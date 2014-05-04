{-# LANGUAGE BangPatterns #-}
module Interpreter (expandInterpreter, evalInterpreter, tailCallOptimize) where

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Data
import Data.Char
import Data.Tuple
import Safe

import Types
import Builtins
import Environment

-- Evaluation
evalInterpreter :: (LispVal, Environment) -> (LispVal, Environment)

-- Just return the value
evalInterpreter !state@(Boolean _, env) = state
evalInterpreter !state@(Int _, env) = state
evalInterpreter !state@(Float _, env) = state
evalInterpreter !state@(String _, env) = state
evalInterpreter !state@(Keyword _, env) = state
evalInterpreter !state@(Quote _, env) = state
evalInterpreter !state@(Function _ _, env) = state

-- More complex forms
evalInterpreter !state@(List lis, env) = lstate'
    where
        !(values, e) = swap $ mapAccumL' (flip $ curry $ swap . evalInterpreter) env lis
        !lstate' = (List values, copyClosures env e)

evalInterpreter !state@(Var name, env) = vstate'
    where
        !v = fromMaybe (LispError $ NotDefined [name, show env]) (lookupSymbolValue env name)
        !vstate' = (v, env)

evalInterpreter !state@(If pred then' else', env) = istate'
    where
        !(cond, e) = evalInterpreter (pred, env)
        !v = case truthy cond of
            err@(LispError _) -> err
            (Boolean True) -> then'
            (Boolean False) -> else'
        !istate' = evalInterpreter (v, e)

evalInterpreter !state@(Definition name expr, env) = dstate'
    where
        !e = if env `containsSymbol` name
            then env
            else addSymbol env name $ Keyword "__builtin__definition__helper__"
        !(v, e') = evalInterpreter (expr, e)
        !e'' = setSymbolValue e' name v
        !dstate' = (v, e'')

evalInterpreter !state@(Assignment name expr, env) = astate'
    where
        !astate' = case lookupSymbolValue env name of
            Nothing -> (LispError $ NotDefined [name], env)
            Just _ -> result
                where
                    !(v, e) = evalInterpreter (expr, env)
                    !e' = setSymbolValue e name v
                    !result = (v, e')

evalInterpreter !state@(Sequence es, env) = sstate'
    where
        !es' = fromList es
        !(values, e) = swap $ mapAccumL' (flip $ curry $ swap . evalInterpreter) env es'
        !sstate' = (last values, copyClosures env e)

evalInterpreter !state@(MakeClosure args expr freevars, env) = mcstate'
    where
        !mcstate' = case makeClosureEnvironment env freevars of
            Left val -> (val, env)
            Right (cid, cenv) -> (Closure args expr cid, cenv)

evalInterpreter !state@(Call expr args, env) = evalCall expr args env handleClosureCall handleFunctionCall
    where
        handleClosureCall (Closure args' expr cid) env args values = (ccret, env')
            where
                !cenv = forkEnvironmentForClosureCall env cid $ zip args values
                !(ccret, e) = evalInterpreter (expr, cenv)
                !env' = forkEnvironmentFromClosureCall e
        handleFunctionCall (Function args' expr) env args values = (cfret, env')
            where
                !fenv = forkEnvironmentForFunctionCall env $ zip args values
                !(cfret, e) = evalInterpreter (expr, fenv)
                !env' = forkEnvironmentFromFunctionCall e

evalInterpreter !state@(TailCall expr args, env) = case parent env of
    Nothing -> evalInterpreter (Call expr args, env)
    Just _ -> evalCall expr args env handleClosureCall handleFunctionCall
    where
        handleClosureCall (Closure args' expr cid) env args values = evalInterpreter (expr, env')
            where
                !env' = forkEnvironmentForTailClosureCall env cid $ zip args values
        handleFunctionCall (Function args' expr) env args values = evalInterpreter (expr, env')
            where
                !env' = forkEnvironmentForTailFunctionCall env $ zip args values

evalInterpreter !state@(TailRecursiveCall expr args, env) = case parent env of
    Nothing -> evalInterpreter (Call expr args, env)
    Just _ -> evalCall expr args env handleClosureCall handleFunctionCall
        where
            handleClosureCall (Closure args' expr cid) env args values = evalInterpreter (expr, env')
                where
                    !env' = forkEnvironmentForTailRecursiveClosureCall env $ zip args values
            handleFunctionCall (Function args' expr) env args values = evalInterpreter (expr, env')
                where
                    !env' = forkEnvironmentForTailRecursiveFunctionCall env $ zip args values

-- Fallthrough case
evalInterpreter !state@(Quasiquote q, env) = (expandQuasiquote env q, env) -- needed for macros to work correctly
evalInterpreter !state@(Macro{}, env) = state
evalInterpreter !state@(LispError _, env) = state
evalInterpreter !state@(unhandled, env) = (LispError $ NotImplemented "evalInterpreter" unhandled, env)

-- Evaluate a call

evalCall :: LispVal -> [LispVal] -> Environment -> (LispVal -> Environment -> [Name] -> [LispVal] -> (LispVal, Environment)) -> (LispVal -> Environment -> [Name] -> [LispVal] -> (LispVal, Environment)) -> (LispVal, Environment)
evalCall !expr !args !env handleClosure handleFunction = ecstate'
    where
        !(fn, e) = {-# SCC "evalCallFn" #-} evalInterpreter (expr, env)
        !cc = copyClosures env e
        !(!e', !values) = {-# SCC "evalCallValues" #-} mapAccumL' (flip $ curry $ swap . evalInterpreter) cc args
        !e'' = copyClosures env e'
        !ecstate' = case fn of
            (BuiltinFunction bname bid arity) -> (cbret, e')
                where
                    !cbret = {-# SCC "evalCallBuiltin" #-} if bid < V.length builtinFunctionVector1
                        then case builtinFunctionVector1 V.!? bid of 
                            Nothing -> LispError $ NotDefined [bname]
                            Just bfn -> if length args /= 1
                                then LispError $ NumArgs (toInteger $ length args) values
                                else case arity of
                                    1 -> bfn x 
                                    where 
                                        (a:[]) = values
                                        !x = a
                        else case builtinFunctionVector2 V.!? (bid - V.length builtinFunctionVector1) of 
                            Nothing -> LispError $ NotDefined [bname]
                            Just bfn -> if length args /= 2
                                then LispError $ NumArgs (toInteger $ length args) values
                                else case arity of
                                    2 -> bfn x y 
                                        where 
                                            (a:b:[]) = values
                                            !x = a
                                            !y = b

            cls@(Closure args expr cid) -> state''
                where
                    !state'' = {-# SCC "evalCallClosure" #-} if length args /= length values
                        then (LispError $ NumArgs (toInteger $ length args) values, env)
                        else handleClosure cls e'' args values
            fun@(Function args expr) -> state''
                where
                    !state'' = {-# SCC "evalCallFunction" #-} if length args /= length values
                        then (LispError $ NumArgs (toInteger $ length args) values, env)
                        else handleFunction fun e'' args values
            err@(LispError _) -> (err, env)
            _ -> (LispError $ NotCallable fn, env)

-- Expansion

expandQuasiquote' :: Environment -> LispVal -> LispVal
expandQuasiquote' env (Unquote val) = case val of
    (Var name) -> fromMaybe (LispError $ NotDefined [name]) (lookupSymbolValue env name)
    _ -> val
expandQuasiquote' env (UnquoteSplicing val) = case val of
    (List !lis) -> List $ map' (expandQuasiquote' env) lis
    !val -> expandQuasiquote' env val
expandQuasiquote' env (List !lis) = List $ map' (expandQuasiquote' env) lis
expandQuasiquote' env (If pred then' else') = If (expandQuasiquote' env pred) (expandQuasiquote' env then') (expandQuasiquote' env else')
expandQuasiquote' env (Definition name expr) = Definition name (expandQuasiquote' env expr)
expandQuasiquote' env (Assignment name expr) = Assignment name (expandQuasiquote' env expr)
expandQuasiquote' env (Sequence seq) = Sequence $ expandQuasiquote' env seq
expandQuasiquote' env (Function args expr) = Function args (expandQuasiquote' env expr)
expandQuasiquote' env (Call expr args) = case expr of
    Var name -> case lookupSymbolValue env name of
        Nothing -> base
        Just value -> case value of
            (Macro mname margs mexpr) -> expanded
                where !expanded = fst $ expandInterpreter (Call expr (map' (expandQuasiquote' env) args), env)
            _ -> base
    _ -> base
    where !base = Call (expandQuasiquote' env expr) (map' (expandQuasiquote' env) args)
expandQuasiquote' env (TailCall expr args) = expandQuasiquote' env (Call expr args)
expandQuasiquote' env (TailRecursiveCall expr args) = expandQuasiquote' env (Call expr args)
expandQuasiquote' env (Var name) = case lookupSymbolValue env name of
    Nothing -> Var name
    Just value -> case value of
        (Function _ _) -> Var name
        (BuiltinFunction{}) -> Var name
        (Closure{}) -> Var name
        _ -> expandQuasiquote' env value
expandQuasiquote' env (BuiltinFunction name _ _) = Var name
expandQuasiquote' env (Quasiquote val) = expandQuasiquote env val
expandQuasiquote' env val = val

expandQuasiquote :: Environment -> LispVal -> LispVal
expandQuasiquote env val@(Unquote _) = expandQuasiquote' env val
expandQuasiquote env val@(UnquoteSplicing _) = expandQuasiquote' env val
expandQuasiquote env val@(List _) = expandQuasiquote' env val
expandQuasiquote env val@(If{}) = expandQuasiquote' env val
expandQuasiquote env val@(Definition _ _) = expandQuasiquote' env val
expandQuasiquote env val@(Assignment _ _) = expandQuasiquote' env val
expandQuasiquote env val@(Sequence _) = expandQuasiquote' env val
expandQuasiquote env val@(Function _ _) = expandQuasiquote' env val
expandQuasiquote env val@(Call _ _) = expandQuasiquote' env val
expandQuasiquote env val@(TailCall _ _) = expandQuasiquote' env val
expandQuasiquote env val@(TailRecursiveCall _ _) = expandQuasiquote' env val
expandQuasiquote env val@(Var _) = expandQuasiquote' env val
expandQuasiquote env val@(BuiltinFunction{}) = expandQuasiquote' env val
expandQuasiquote env val@(Quasiquote _) = expandQuasiquote' env val
expandQuasiquote env val = Quote val

expandInterpreter :: (LispVal, Environment) -> (LispVal, Environment)

expandInterpreter !state@(List lis, env) = state'
    where
        !(lis', e) = swap $ mapAccumL' (flip $ curry $ swap . expandInterpreter) env lis
        !state' = (List lis', e)

expandInterpreter !state@(If pred then' else', env) = state'
    where
        !(pred', e) = expandInterpreter (pred, env)
        !(then'', e') = expandInterpreter (then', e)
        !(else'', e'') = expandInterpreter (else', e')
        !state' = (If pred' then'' else'', e'')

expandInterpreter !state@(Definition name expr, env) = state'
    where
        !env' = if env `containsSymbol` name
            then addSymbol env name $ Keyword "__builtin__definition__helper__"
            else env
        !(expr', e) = expandInterpreter (expr, env')
        !state' = (Definition name expr', e)

expandInterpreter !state@(Assignment name expr, env) = state'
    where
        !(expr', e) = expandInterpreter (expr, env)
        !state' = (Assignment name expr', e)

expandInterpreter !state@(Sequence es, env) = state'
    where
        !es' = fromList es
        !(exprs, e) = swap $ mapAccumL' (flip $ curry $ swap . expandInterpreter) env es'
        !state' = (Sequence $ List exprs, e)

expandInterpreter !state@(Quasiquote expr, env) = state'
    where
        !expr' = expandQuasiquote env expr
        !state' = (expr', env)

expandInterpreter !state@(Quote expr, env) = state'
    where
        !(expr', e) = expandInterpreter (expr, env)
        !state' = (Quote expr', e)

expandInterpreter !state@(Macro name args expr, env) = state'
    where
        !m = Macro name args expr
        !e = addSymbol env name m
        !state' = (m, e)

expandInterpreter !state@(Function args expr, env) = state'
    where
        !(expr', e) = expandInterpreter (expr, env)
        !state' = (fn, e)
        !freevars = freeVariables env args expr' \\ args
        !fn = if null freevars
            then Function args expr'
            else MakeClosure args expr' freevars

expandInterpreter !state@(Call expr args, env) = state'
    where
        !found = case expr of
            Var name -> lookupSymbolValue env name
            _ -> Nothing
        !state' = case found of
            Nothing -> state''
            Just fn -> case fn of
                (Macro name margs expr) -> mstate
                    where
                        !mstate = case length args /= length margs of
                            True -> (LispError $ NumArgs (toInteger $ length margs) args, env)
                            False -> mstate'
                                where
                                    !e'' = forkEnvironmentForFunctionCall env $ zip margs args --makeChildEnvironmentWith env $ zip margs args
                                    !(!expr', !env') = evalInterpreter (expr, e'')
                                    !env'' = forkEnvironmentFromFunctionCall env'
                                    !mstate' = (expr', env'')
                _ -> state''
            where
                !(expr', e) = expandInterpreter (expr, env)
                !(args', e') = swap $ mapAccumL' (flip $ curry $ swap . expandInterpreter) e args
                !state'' = (Call expr' args', e')

expandInterpreter !state@(TailCall expr args, env) = expandInterpreter (TailCall expr args, env)
expandInterpreter !state@(TailRecursiveCall expr args, env) = expandInterpreter (TailRecursiveCall expr args, env)
expandInterpreter !state@(_ , _) = state

tailCallOptimize :: Environment -> LispVal -> LispVal
tailCallOptimize !env (Call expr args) = result where !result = Call (tailCallOptimize env expr) (map' (tailCallOptimize env) args)
tailCallOptimize !env !val = result where !result = tailCallOptimize' env val

tailCallOptimize' :: Environment -> LispVal -> LispVal
tailCallOptimize' !env (Call expr args) = case lookupSymbolValue env "__builtin__tco__definition__" of
    Nothing -> TailCall (tailCallOptimize' env expr) (map' (tailCallOptimize env) args)
    Just expr' -> if expr == expr'
            then TailRecursiveCall (tailCallOptimize' env expr) (map' (tailCallOptimize env) args)
            else TailCall (tailCallOptimize' env expr) (map' (tailCallOptimize env) args)
tailCallOptimize' !env (List !lis) = List $ map' (tailCallOptimize env) lis
tailCallOptimize' !env (Sequence !seq) = Sequence $ List $ map' (tailCallOptimize env) $ fromList seq
tailCallOptimize' !env (If pred then' else') = If (tailCallOptimize env pred) (tailCallOptimize' env then') (tailCallOptimize' env else')
tailCallOptimize' !env (Definition name expr) = Definition name (tailCallOptimize env' expr)
    where
        env' = addSymbol env "__builtin__tco__definition__" $ Var name
tailCallOptimize' !env (Assignment name expr) = Assignment name (tailCallOptimize env expr)
tailCallOptimize' !env (Function args expr) = Function args (tailCallOptimize env expr)
tailCallOptimize' !env (Closure args expr cid) = Closure args (tailCallOptimize env expr) cid
tailCallOptimize' !env (MakeClosure args expr freevars) = MakeClosure args (tailCallOptimize env expr) freevars
tailCallOptimize' !env !val = val

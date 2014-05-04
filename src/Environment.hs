{-# LANGUAGE BangPatterns #-}
module Environment (blankEnvironment, makeChildEnvironment, makeChildEnvironmentWith, 
                    makeClosureEnvironment,
                    forkEnvironmentForClosureCall, forkEnvironmentFromClosureCall,
                    forkEnvironmentForFunctionCall, forkEnvironmentFromFunctionCall,
                    addSymbol, addSymbols, lookupSymbolValue, containsSymbol,
                    setSymbolValue, freeVariables,
                    forkEnvironmentForTailClosureCall, forkEnvironmentForTailRecursiveClosureCall,
                    forkEnvironmentForTailFunctionCall, forkEnvironmentForTailRecursiveFunctionCall,
                    copyClosures, getParentEnvironment) where

import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Arrow
import Safe

import Types

blankEnvironment :: Environment
blankEnvironment = Environment {symbols = H.empty, closures = H.empty, parent = Nothing}

makeChildEnvironment :: Environment -> Environment
makeChildEnvironment !env = Environment {symbols = symbols env, closures = closures env, parent = Just env}

makeChildEnvironmentWith :: Environment -> [(Name, LispVal)] -> Environment
makeChildEnvironmentWith !env !syms = addSymbols (makeChildEnvironment env) syms

makeClosureEnvironment :: Environment -> [Name] -> Either LispVal (Int, Environment)
makeClosureEnvironment !env !names = result
    where
        !values = map' (lookupSymbolValue env) names
        !(found, notfound') = partition (isJust . snd) $ zip names values
        !notfound = map' fst notfound'
        !result = if null notfound
            then Right (cid, env')
            else Left $ LispError $ NotDefined notfound
                where
                    !cid = H.size $ closures env
                    !syms = H.fromList $ filter ((/= Keyword "__builtin__definition__helper__") . snd) $ map' (second (fromJustNote "makeClosureEnvironment")) found
                    !closures' = H.insert cid syms (closures env)
                    !env' = Environment {symbols = symbols env, closures = closures', parent = parent env}

copyClosures :: Environment -> Environment -> Environment
copyClosures !env !cenv = env'
    where
        !env' = Environment {symbols = symbols env, closures = closures cenv, parent = parent env}

forkEnvironmentForClosureCall :: Environment -> Int -> [(Name, LispVal)] -> Environment
forkEnvironmentForClosureCall !env !closure !syms = env'
    where
        !cenv = fromJustNote "forkEnvironmentForClosureCall" $ H.lookup closure (closures env)
        !csyms = zip (H.keys cenv) $ repeat $ Left closure
        !syms' = map' (second Right) $ filter (not . flip (H.member . fst) cenv) syms 
        !env' = addSymbols' (makeChildEnvironment env) $ csyms ++ syms'

forkEnvironmentFromClosureCall :: Environment -> Environment
forkEnvironmentFromClosureCall !cenv = getParentEnvironment cenv

forkEnvironmentForFunctionCall :: Environment -> [(Name, LispVal)] -> Environment
forkEnvironmentForFunctionCall !env !syms = makeChildEnvironmentWith env syms

forkEnvironmentFromFunctionCall :: Environment -> Environment
forkEnvironmentFromFunctionCall !fenv = getParentEnvironment fenv

forkEnvironmentForTailClosureCall :: Environment -> Int -> [(Name, LispVal)] -> Environment
forkEnvironmentForTailClosureCall !env !closure !syms = env'
    where
        !cenv = fromJustNote "forkEnvironmentForTailClosureCall" $ H.lookup closure (closures env)
        !csyms = zip (H.keys cenv) $ repeat $ Left closure
        !syms' = map' (second Right) $ filter (not . flip (H.member . fst) cenv) $ filter ((/= Keyword "__builtin__definition__helper__") . snd) syms 
        !e' = makeChildEnvironment $ getParentEnvironment env
        !env' = addSymbols' e' $ csyms ++ syms'

forkEnvironmentForTailRecursiveClosureCall :: Environment -> [(Name, LispVal)] -> Environment
forkEnvironmentForTailRecursiveClosureCall !env !syms = setSymbolValues env syms'
    where
        !syms' = filter ((/= Keyword "__builtin__definition__helper__") . snd) syms

forkEnvironmentForTailFunctionCall :: Environment -> [(Name, LispVal)] -> Environment
forkEnvironmentForTailFunctionCall !env !syms = makeChildEnvironmentWith (getParentEnvironment env) syms

forkEnvironmentForTailRecursiveFunctionCall :: Environment -> [(Name, LispVal)] -> Environment
forkEnvironmentForTailRecursiveFunctionCall !env !syms = setSymbolValues env syms

addSymbol :: Environment -> Name -> LispVal -> Environment
addSymbol !env !name !value = addSymbol' env name (Right value)

addSymbol' :: Environment -> Name -> Either Int LispVal -> Environment
addSymbol' !env !name !value = Environment {symbols = symbols', closures = closures env, parent = parent env}
    where
        !symbols' = H.insert name value (symbols env)

addSymbols :: Environment -> [(Name, LispVal)] -> Environment
addSymbols !env !syms = env'
    where
        helper !e !(n,v) = addSymbol e n v
        !env' = foldl' helper env syms

addSymbols' :: Environment -> [(Name, Either Int LispVal)] -> Environment
addSymbols' !env !syms = env'
    where
        helper !e !(n,v) = addSymbol' e n v
        !env' = foldl' helper env syms

lookupSymbolValue :: Environment -> Name -> Maybe LispVal
lookupSymbolValue !env !name = result
    where
        !result = {-# SCC "lookupSymbolValueResult" #-} case found of
            Nothing -> Nothing
            Just value -> case value of
                Right v -> Just v
                Left cid -> clookup
                    where
                        !cenv = {-# SCC "lookupSymbolValueCenv" #-} fromJustNote "lookupSymbolValue1" $ H.lookup cid $ closures env
                        !clookup = {-# SCC "lookupSymbolValueClookup" #-} H.lookup name cenv
            where
                !found = {-# SCC "lookupSymbolValueFound" #-} H.lookup name (symbols env)

containsSymbol :: Environment -> Name -> Bool
containsSymbol !env !name = isJust $ lookupSymbolValue env name

setSymbolValue :: Environment -> Name -> LispVal -> Environment
setSymbolValue !env !name !value = env'
    where
        !old = fromJustNote ("setSymbolValue1 " ++ name ++ " = " ++ show value ++ show env) $ H.lookup name (symbols env)
        !symbols' = {-# SCC "setSymbolValueSymbols" #-} case old of
            Left _ -> symbols env
            Right _ -> result where !result = H.insert name (Right value) (symbols env)
        !closures' = {-# SCC "setSymbolValueClosures" #-} case old of
            Left !cid -> H.insert cid cenv' $ closures env
                where
                    !cenv = fromJustNote "setSymbolValue2" $ H.lookup cid $ closures env
                    !cenv' = H.insert name value cenv
            Right _ -> closures env
        !env' = {-# SCC "setSymbolValueEnv'" #-} Environment {symbols = symbols', closures = closures', parent = parent env} 

setSymbolValues :: Environment -> [(Name, LispVal)] -> Environment
setSymbolValues !env !syms = env'
    where
        helper !e !(!n,!v) = result where !result = setSymbolValue e n v
        !env' = foldl' helper env syms

getParentEnvironment :: Environment -> Environment 
getParentEnvironment !env = env'
    where
        !par = fromJustNote "getParentEnvironment" $ parent env
        !env' = Environment {symbols = symbols par, closures = closures env, parent = parent par }


-- Closure Conversion

freeVariables :: Environment -> [Name] -> LispVal -> [Name]
freeVariables !env !args !expr = nub $ fst $ freeVariables' (expr, addSymbols env $ zip args $ repeat $ Keyword "__builtin__free_variables__helper__")

freeVariables' :: (LispVal, Environment) -> ([Name], Environment)

freeVariables' !(Var name, env) = (free', env)
    where
        !free' = if env `containsSymbol` name
            then []
            else [name]

freeVariables' !(Boolean _, env) = ([], env)
freeVariables' !(Int _, env) = ([], env)
freeVariables' !(Float _, env) = ([], env)
freeVariables' !(String _, env) = ([], env)
freeVariables' !(Keyword _, env) = ([], env)
freeVariables' !(Quote _, env) = ([], env)
freeVariables' !(BuiltinFunction{}, env) = ([], env)
freeVariables' !(MakeClosure{}, env) = ([], env)

freeVariables' !(List lis, env) = (free', env)
    where
        (free', env') = case lis of
            [] -> ([], env)
            (v:vs) -> (f ++ f', e')
                where
                    (f, e) = freeVariables' (v, env)
                    (f', e') = freeVariables' (List vs, e)

freeVariables' !(If pred then' else', env) = (free', env)
    where
        (f, e) = freeVariables' (pred, env)
        (f', _) = freeVariables' (then', e)
        (f'', _) = freeVariables' (else', e)
        free' = f ++ f' ++ f''

freeVariables' !(Definition name value, env) = (free', env')
    where
        env' = addSymbol env name value
        (free', _) = freeVariables' (value, env')

freeVariables' !(Assignment name value, env) = freeVariables' (value, env)
freeVariables' !(Sequence lis, env) = freeVariables' (lis, env)

freeVariables' !(Function args expr, env) = (free', env)
    where
        e = addSymbols env $ zip args $ repeat $ Keyword "__builtin__free_variables__helper__"
        (free', _) = freeVariables' (expr, e)

freeVariables' !(Call expr args, env) = (free', env)
    where
        (f, e) = freeVariables' (expr, env)
        (f', _) = freeVariables' (List args, env)
        free' = f ++ f'

freeVariables' !(LispError err, env) = (free', env)
    where
        free' = case err of
            (SyntaxError _) -> []
            (Parser _) -> []
            (NotDefined _) -> []
            (Default _) -> []
            (NumArgs _ args) -> fst $ freeVariables' (List args, env)
            (TypeMismatch _ types) -> fst $ freeVariables' (List types, env)
            (NotImplemented _ expr) -> fst $ freeVariables' (expr, env)
            (NotCallable expr) -> fst $ freeVariables' (expr, env)
            (Custom expr _) -> fst $ freeVariables' (expr, env)

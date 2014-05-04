{-# LANGUAGE Rank2Types, BangPatterns #-}
module Builtins (builtinFunctionVector1, builtinFunctionVector2, makeBuiltinEnvironment) where

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Data
import Data.Char

import Types
import Environment

-- Invokers
invoke1 :: String -> (LispVal -> Bool) -> (LispVal -> a) -> (a -> b) -> (b -> LispVal) -> LispVal -> LispVal
invoke1 name checker unwrapper fn wrapper = helper
    where
    strictfn !x = result
        where
            !u = unwrapper x
            !f = fn u
            !result = wrapper f
    helper !x = if checker x
        then result 
        else LispError $ TypeMismatch name [x] 
        where !result = strictfn x

invoke2 :: String -> (LispVal -> Bool) -> (LispVal -> a) -> (a -> a -> b) -> (b -> LispVal) -> LispVal -> LispVal -> LispVal
invoke2 name checker unwrapper fn wrapper = helper
    where
    strictfn !x !y = result
        where
            !a = unwrapper x
            !b = unwrapper y
            !f = fn a b
            !result = wrapper f
    helper !x !y = if checker x && checker y
        then result
        else LispError $ TypeMismatch name [x, y]
        where !result = strictfn x y

invoke2' :: String -> (LispVal -> Bool, LispVal -> Bool) -> (LispVal -> a, LispVal -> b) -> (a -> b -> c) -> (c -> LispVal) -> LispVal -> LispVal -> LispVal
invoke2' name checkers@(c1, c2) unwrappers@(u1, u2) fn wrapper = helper
    where
    strictfn !x !y = result
        where
            !a = u1 x
            !b = u2 y
            !f = fn a b
            !result = wrapper f
    helper !x !y = if c1 x && c2 y
        then result
        else LispError $ TypeMismatch name [x, y]
        where !result = strictfn x y

-- Arithmetic functions
unaryIntArithOp :: String -> (Integer -> Integer) -> LispVal -> LispVal
unaryIntArithOp name op = invoke1 name isInt fromInt op Int

binaryIntArithOp :: String -> (Integer -> Integer -> Integer) -> LispVal -> LispVal -> LispVal
binaryIntArithOp name op = invoke2 name isInt fromInt op Int

unaryFloatArithOp :: String -> (Double -> Double) -> LispVal -> LispVal
unaryFloatArithOp name op = invoke1 name isFloat fromFloat op Float

binaryFloatArithOp :: String -> (Double -> Double -> Double) -> LispVal -> LispVal -> LispVal
binaryFloatArithOp name op = invoke2 name isFloat fromFloat op Float

unaryArithOp :: String -> (forall a. (Num a) => a -> a) -> [(Name, LispVal -> LispVal)]
unaryArithOp basename op = [
    ("__builtin__" ++ basename ++ "_int", unaryIntArithOp basename op),
    ("__builtin__" ++ basename ++ "_float", unaryFloatArithOp basename op)
    ]

binaryArithOp :: String -> (forall a. (Num a) => a -> a -> a) -> [(Name, LispVal -> LispVal -> LispVal)]
binaryArithOp basename op = [
    ("__builtin__" ++ basename ++ "_int", binaryIntArithOp basename op),
    ("__builtin__" ++ basename ++ "_float", binaryFloatArithOp basename op)
    ]

arithOps1 :: [(Name, LispVal -> LispVal)]
arithOps1 = unaryArithOp "abs" abs


arithOps2 :: [(Name, LispVal -> LispVal -> LispVal)]
arithOps2 = binaryArithOp "add" (+) ++ binaryArithOp "sub" (-) ++ binaryArithOp "mul" (*) ++ 
    [ ("__builtin__div_int", binaryIntArithOp "div" quot), ("__builtin__div_float", binaryFloatArithOp "div" (/)) ]

-- List Functions
listCar :: LispVal -> LispVal
listCar val@(List []) = LispError $ IllegalArgument "car" val
listCar val@(List (x:xs)) = result where !result = x

listCdr :: LispVal -> LispVal
listCdr val@(List []) = LispError $ IllegalArgument "cdr" val
listCdr val@(List (x:xs)) = result where !result = List xs

listOps1 :: [(Name, LispVal -> LispVal)]
listOps1 = [
    ("__builtin__car", invoke1 "car" isList id listCar id),
    ("__builtin__cdr", invoke1 "cdr" isList id listCdr id),
    ("__builtin__length", invoke1 "length" isList fromList length (Int . toInteger)),
    ("__builtin__list", invoke1 "list" isLispVal id (\v -> List [v]) id),
    ("__builtin__null", invoke1 "null" isList fromList null Boolean)
    ]

listOps2 :: [(Name, LispVal -> LispVal -> LispVal)]
listOps2 = [
    ("__builtin__cons", invoke2' "cons" (isLispVal, isList) (id, fromList) (:) List),
    ("__builtin__append", invoke2 "append" isList fromList (++) List)
    ]

-- Comparison Functions
binaryIntComparisonOp :: String -> (Integer -> Integer -> Bool) -> LispVal -> LispVal -> LispVal
binaryIntComparisonOp name op = invoke2 name isInt fromInt op Boolean

binaryFloatComparisonOp :: String -> (Double -> Double -> Bool) -> LispVal -> LispVal -> LispVal
binaryFloatComparisonOp name op = invoke2 name isFloat fromFloat op Boolean

binaryComparisonOp :: String -> (forall a. (Ord a) => a -> a -> Bool) -> [(Name, LispVal -> LispVal -> LispVal)]
binaryComparisonOp basename op = [
    ("__builtin__" ++ basename ++ "_int", binaryIntComparisonOp basename op),
    ("__builtin__" ++ basename ++ "_float", binaryFloatComparisonOp basename op)
    ]

comparisonOps1 :: [(Name, LispVal -> LispVal)]
comparisonOps1 = []

comparisonOps2 :: [(Name, LispVal -> LispVal -> LispVal)]
comparisonOps2 = binaryComparisonOp "gt" (>) ++ binaryComparisonOp "gte" (>=) ++ 
                binaryComparisonOp "lt" (<) ++ binaryComparisonOp "lte" (<=) ++ 
    [ ("__builtin__eq", invoke2 "eq" isLispVal id (==) Boolean) ]

-- Error creation operations
makeNumArgsError :: Integer -> [LispVal] -> LispVal
makeNumArgsError expected found = LispError $ NumArgs expected found

makeTypeMistmatchError :: String -> [LispVal] -> LispVal
makeTypeMistmatchError message values = LispError $ TypeMismatch message values

makeNotImplementedError :: String -> LispVal -> LispVal
makeNotImplementedError message unhandled = LispError $ NotImplemented message unhandled

makeNotDefinedError :: [String] -> LispVal
makeNotDefinedError names = LispError $ NotDefined names

makeNotCallableError :: LispVal -> LispVal
makeNotCallableError name = LispError $ NotCallable name

makeIllegalArgumentError :: String -> LispVal -> LispVal
makeIllegalArgumentError name val = LispError $ IllegalArgument name val

makeCustomError :: String -> String -> LispVal
makeCustomError name message = LispError $ Custom (Keyword name) message

makeParserError :: String -> LispVal
makeParserError message = LispError $ Parser message

makeDefaultError :: String -> LispVal
makeDefaultError message = LispError $ Default message

errorOps1 :: [(Name, LispVal -> LispVal)]
errorOps1 = [ 
    ("__builtin__make_error_not_defined", invoke1 "make-error" (\k -> isList k && all isString (fromList k)) (map fromString . fromList) makeNotDefinedError id),
    ("__builtin__make_error_not_callable", invoke1 "make-error" isLispVal id makeNotCallableError id),
    ("__builtin__make_error_parser", invoke1 "make-error" isString fromString makeParserError id),
    ("__builtin__make_error_default", invoke1 "make-error" isString fromString makeDefaultError id)
    ]

errorOps2 :: [(Name, LispVal -> LispVal -> LispVal)]
errorOps2 = [
    ("__builtin__make_error_num_args", invoke2' "make-error" (isInt, isList) (fromInt, fromList) makeNumArgsError id),
    ("__builtin__make_error_type_mismatch", invoke2' "make-error" (isString, isList) (fromString, fromList) makeTypeMistmatchError id),
    ("__builtin__make_error_not_implemented", invoke2' "make-error" (isString, isLispVal) (fromString, id) makeNotImplementedError id),
    ("__builtin__make_error_illegal_argument", invoke2' "make-error" (isString, isLispVal) (fromString, id) makeIllegalArgumentError id),
    ("__builtin__make_error_custom", invoke2 "make-error" isString fromString makeCustomError id)
    ]

-- Boolean functions

boolOps1 :: [(Name, LispVal -> LispVal)]
boolOps1 = [
    ("__builtin__not", invoke1 "not" isBoolean fromBoolean not Boolean)
    ]

boolOps2 :: [(Name, LispVal -> LispVal -> LispVal)]
boolOps2 = [
    ("__builtin__and", invoke2 "and" isBoolean fromBoolean (&&) Boolean),
    ("__builtin__or", invoke2 "or" isBoolean fromBoolean (||) Boolean)
    ]

-- Miscellaneous functions
miscOps1 :: [(Name, LispVal -> LispVal)]
miscOps1 = [ 
    ("__builtin__typeof", invoke1 "typeof" isLispVal id (map toLower . show . toConstr) Keyword)
    ]

miscOps2 :: [(Name, LispVal -> LispVal -> LispVal)]
miscOps2 = []

-- Builtin environment
builtinFunctionList1 :: [(Name, LispVal -> LispVal)]
builtinFunctionList1 = arithOps1 ++ listOps1 ++ comparisonOps1 ++ errorOps1 ++ boolOps1 ++ miscOps1

builtinSymbols1 :: [(Name, LispVal)]
builtinSymbols1 = zipWith (\(n, f) i -> (n, BuiltinFunction n i 1)) builtinFunctionList1 [0 .. ]

builtinFunctionVector1 :: V.Vector (LispVal -> LispVal)
builtinFunctionVector1 = V.fromList $ map snd builtinFunctionList1

builtinFunctionList2 :: [(Name, LispVal -> LispVal -> LispVal)]
builtinFunctionList2 = arithOps2 ++ listOps2 ++ comparisonOps2 ++ errorOps2 ++ boolOps2 ++ miscOps2

builtinSymbols2 :: [(Name, LispVal)]
builtinSymbols2 = zipWith (\(n, f) i -> (n, BuiltinFunction n i 2)) builtinFunctionList2 [(V.length builtinFunctionVector1) .. ]

builtinFunctionVector2 :: V.Vector (LispVal -> LispVal -> LispVal)
builtinFunctionVector2 = V.fromList $ map snd builtinFunctionList2

makeBuiltinEnvironment :: Environment
makeBuiltinEnvironment = addSymbols blankEnvironment $ builtinSymbols1 ++ builtinSymbols2

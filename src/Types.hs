{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, BangPatterns #-}
module Types where

import Data.DeriveTH
import Data.Data
import Data.HashMap.Strict hiding (map, foldr)
import Language.Haskell.TH hiding (Name)
import Control.DeepSeq
import Control.DeepSeq.TH
import Control.Monad.State
import Safe

-- Fully strict versions
mapAccumL' :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y]) 
mapAccumL' _ !s ![]     =  (s, []) 
mapAccumL' f !s (x:xs) =  (s'',y:ys) 
    where 
      !(!s', !y) = f s x 
      !(!s'', !ys) = s' `seq` mapAccumL' f s' xs 

map' :: (a -> b) -> [a] -> [b]
map' _ ![] = []
map' f (x:xs) = a : b
    where
        !a = f x
        !b = map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ ![]    = []
filter' pred (x:xs)
  | pred x         = addRecurse
  | otherwise      = recurse
  where 
    !recurse = filter' pred xs
    !addRecurse = x : recurse

-- Errors
data LError   = NumArgs !Integer ![LispVal]
              | TypeMismatch !Name ![LispVal]
              | NotImplemented !Name !LispVal
              | NotDefined ![Name]
              | NotCallable !LispVal
              | IllegalArgument !Name !LispVal
              | Custom !LispVal !String
              | Parser !String
              | SyntaxError !String
              | Default !String
              deriving (Eq, Data, Typeable)

showError :: LError -> String
showError (NumArgs expected found) = "((make-error :num-args) " ++ show expected ++ " [" ++ initSafe (concatMap (\l -> show l ++ " ") found) ++ "])"
showError (TypeMismatch message values) = "((make-error :type-mismatch) " ++ show message++ " [" ++ initSafe (concatMap (\l -> show l ++ " ") values) ++ "])"
showError (NotImplemented message unhandled) = "((make-error :not-implemented) " ++ show message ++ " " ++ show unhandled ++ ")"
showError (NotDefined names) = "((make-error :not-defined) " ++ " [" ++ initSafe (concatMap (\l -> show l ++ " ") names) ++ "])"
showError (NotCallable name) = "((make-error :not-callable) " ++ show name ++ ")"
showError (IllegalArgument name val) = "((make-error :illegal-argument) " ++ show name ++ " " ++ show val ++ ")"
showError (Custom name message) = "((make-error :custom) " ++ show name ++ " " ++ show message ++ ")"
showError (Parser parseErr) = "Parse Error: " ++ parseErr ++ ""
showError (SyntaxError message) = "((make-error :syntax-error) " ++ show message ++ ")"
showError (Default message) = "((make-error :custom) " ++ show message ++ ")"

instance Show LError where show = showError

-- Truthiness
truthy :: LispVal -> LispVal
truthy val@(Boolean _) = val
truthy val@(LispError _) = val
truthy _ = Boolean False

isLispVal :: LispVal -> Bool
isLispVal _ = True

-- Environments 
data Environment = Environment {
  symbols :: !(HashMap Name (Either Int LispVal)),
  closures :: !(HashMap Int (HashMap Name LispVal)),
  parent :: !(Maybe Environment)
} deriving (Eq, Show)

-- Values
type Name = String
data LispVal
  = Var !Name
  | Boolean !Bool
  | Int !Integer
  | Float !Double
  | String !String
  | Keyword !String
  | List ![LispVal]
  | Quote !LispVal
  | Quasiquote !LispVal
  | Unquote !LispVal
  | UnquoteSplicing !LispVal
  | If !LispVal !LispVal !LispVal
  | Definition !Name !LispVal
  | Assignment !Name !LispVal
  | Sequence !LispVal
  | Macro !Name ![Name] !LispVal
  | Function ![Name] !LispVal
  | Closure ![Name] !LispVal !Int
  | MakeClosure ![Name] !LispVal ![Name]
  | BuiltinFunction !Name !Int !Int
  | Call !LispVal ![LispVal]
  | TailCall !LispVal ![LispVal]
  | TailRecursiveCall !LispVal ![LispVal]
  | LispError !LError
  deriving (Eq, Data, Typeable)

showLispVal :: LispVal -> String
showLispVal (Var name) = name
showLispVal (Boolean bool) = if bool then "#t" else "#f"
showLispVal (Int val) = show val
showLispVal (Float val) = show val
showLispVal (String str) = str
showLispVal (Keyword kw) = ":" ++ kw
showLispVal (List lis) = "[" ++ initSafe (concatMap (\l -> show l ++ " ") lis) ++ "]"
showLispVal (Quote val) = "'" ++ show val
showLispVal (Quasiquote val) = "`" ++ show val
showLispVal (Unquote val) = "," ++ show val
showLispVal (UnquoteSplicing val) = ",@" ++ show val
showLispVal (If pred then' else') = "(if " ++ show pred ++ " " ++ show then' ++ " " ++ show else' ++ ")"
showLispVal (Definition name val) = "(define " ++ name ++ " " ++ show val ++ ")"
showLispVal (Assignment name val) = "(set! " ++ name ++ " " ++ show val ++ ")"
showLispVal (Sequence seq) = "(begin " ++ tail (init $ show seq) ++ ")"
showLispVal (Macro name args expr) = "(defmacro " ++ name ++ " (" ++ initSafe (concatMap (++ " ") args) ++ ") " ++ show expr ++ ")"
showLispVal (Function args expr) = "(lambda (" ++ initSafe (concatMap (++ " ") args) ++ ") " ++ show expr ++ ")"
showLispVal (Closure args expr num) = "(closure (" ++ initSafe (concatMap (++ " ") args) ++ ") " ++ show expr ++ " " ++ show num ++ ")"
showLispVal (MakeClosure args expr freevars) = "(make-closure (" ++ initSafe (concatMap (++ " ") args) ++ ") " ++ show expr ++ " [" ++ initSafe (concatMap (++ " ") freevars) ++ "])"
showLispVal (BuiltinFunction name _ _) = name
showLispVal (Call expr args) = "(" ++ show expr ++ " " ++ initSafe (concatMap (\l -> show l ++ " ") args) ++ ")"
showLispVal (TailCall expr args) = "(tail " ++ show expr ++ " " ++ initSafe (concatMap (\l -> show l ++ " ") args) ++ ")"
showLispVal (TailRecursiveCall expr args) = "(tailrec " ++ show expr ++ " " ++ initSafe (concatMap (\l -> show l ++ " ") args) ++ ")"
showLispVal (LispError err) = show err

instance Show LispVal where show = showLispVal

$( derive makeIs ''LispVal )
$( derive makeFrom ''LispVal )
$( deriveNFData ''Environment )
$( deriveNFData ''LError )
$( deriveNFData ''LispVal )

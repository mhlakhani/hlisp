{-# Language BangPatterns #-}
module Parser (parseForm, parseTopLevel) where

import qualified Data.ByteString as B
import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.ByteString
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Lexer as L
import Types

-- Primitives
booleanTrue :: Parser LispVal
booleanTrue = do
  L.reserved "#t"
  return $ Boolean True

booleanFalse :: Parser LispVal
booleanFalse = do
  L.reserved "#f"
  return $ Boolean False

boolean :: Parser LispVal
boolean = try booleanTrue <|> try booleanFalse

integral :: Parser LispVal
integral = Int <$> L.integer

floating :: Parser LispVal
floating = Float <$> L.float

string' :: Parser LispVal
string' = String <$> L.string

symbol :: Parser LispVal
symbol = Var <$> L.identifier

keyword :: Parser LispVal
keyword = do
  L.reservedOp ":"
  !name <- L.identifier
  return $ Keyword name

operator :: Parser LispVal
operator = do
  !op <- L.operator
  return $ Var op

atom :: Parser LispVal
atom  =   try boolean
     <|>  try floating
     <|>  try integral
     <|>  try string'
     <|>  try symbol
     <|>  try keyword
     <|>  try operator

-- Slightly more complicated things
list :: Parser LispVal
list = do
  !r <- L.brackets $ many form
  return $ List r

quote :: Parser LispVal
quote = do
  L.reservedOp "'"
  !r <- form
  return $ Quote r

quotekw :: Parser LispVal
quotekw = do
  L.reserved "quote"
  !r <- form
  return $ Quote r

quasiquote :: Parser LispVal
quasiquote = do
  L.reservedOp "`"
  !r <- form
  return $ Quasiquote r

unquote :: Parser LispVal
unquote = do
  L.reservedOp ","
  !r <- form
  return $ Unquote r

unquoteSplicing :: Parser LispVal
unquoteSplicing = do
  L.reservedOp ",@"
  !r <- form
  return $ UnquoteSplicing r

if' :: Parser LispVal
if' = do 
  L.reserved "if"
  !cond <- form
  !w <- L.whitespace
  !then' <- form
  !w2 <- L.whitespace
  !else' <- form
  return $ If cond then' else'

definition :: Parser LispVal
definition = do
  L.reserved "define"
  !name <- try L.identifier <|> L.operator
  !expr <- form
  return $ Definition name expr

assignment :: Parser LispVal
assignment = do
  L.reserved "set!"
  !name <- L.identifier
  !expr <- form
  return $ Assignment name expr

sequence' :: Parser LispVal
sequence' = do
  L.reserved "begin"
  !exprs <- many form
  return $ Sequence $ List exprs

macro :: Parser LispVal
macro = do
  L.reserved "defmacro"
  !name <- L.identifier
  !args <- L.parens $ many L.identifier
  !expr <- form
  return $ Macro name args expr

function :: Parser LispVal
function = do
  L.reserved "lambda"
  !args <- L.parens $ many L.identifier
  !expr <- form
  return $ Function args expr

callName :: Parser LispVal
callName = do
  !name <- try L.identifier <|> try L.operator
  return $ Var name

call :: Parser LispVal
call = do
  !expr <- try callName <|> try (L.parens function) <|> try (L.parens call) <|> try form
  !args <- many form
  return $ Call expr args

-- Combinations
form :: Parser LispVal
form   =   try list
      <|>  try quote
      <|>  try quasiquote
      <|>  try unquote
      <|>  try unquoteSplicing
      <|>  try (L.parens quotekw)
      <|>  try (L.parens if')
      <|>  try (L.parens definition)
      <|>  try (L.parens assignment)
      <|>  try (L.parens sequence')
      <|>  try (L.parens macro)
      <|>  try (L.parens function)
      <|>  try (L.parens call)
      <|>  try atom

-- Parsers

contents :: Parser a -> Parser a
contents p = do
  !w <- L.whitespace
  !r <- p
  eof
  return r

topLevel :: Parser [LispVal]
topLevel = do
    !w <- L.whitespace
    !forms <- sepEndBy form (try L.whitespace <|> try eof)
    eof
    return forms

-- Exported Parse Functions

parseAtom :: String -> B.ByteString -> LispVal
parseAtom source !s = case parse (contents atom) source s of
  Left !err -> LispError $ Parser $ show err
  Right !val -> val

parseForm :: String -> B.ByteString -> LispVal
parseForm source !s = case parse (contents form) source s of
  Left !err -> LispError $ Parser $ show err
  Right !val -> val

parseTopLevel :: String -> B.ByteString -> Either LispVal [LispVal]
parseTopLevel source !s = case parse topLevel source s of
  Left !err -> Left $ LispError $ Parser $ show err
  Right !vals -> Right vals

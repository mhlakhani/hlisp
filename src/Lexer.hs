{-# LANGUAGE NoMonomorphismRestriction #-}
module Lexer where

import qualified Data.ByteString as B
import Text.Parsec hiding (State)
import Text.Parsec.ByteString hiding (Parser)
import Text.Parsec.Language (emptyDef, GenLanguageDef)
import Text.Parsec.Pos (SourcePos)
import qualified Text.Parsec.Token as Tok 
import Text.Parsec.Char

import Control.Monad.State

intDef = Tok.LanguageDef {
            Tok.commentStart = "",
            Tok.commentEnd = "",
            Tok.commentLine = ";",
            Tok.nestedComments = True,
            Tok.identStart = letter <|> char '_',
            Tok.identLetter = alphaNum <|> oneOf "_'?-",
            Tok.opStart = Tok.opLetter intDef,
            Tok.opLetter = oneOf "+*-/<>=!:?,`@",
            Tok.reservedOpNames = ["'", ":", "`", ",", ",@"],
            Tok.reservedNames = [
                                    "quote", 
                                    "if",
                                    "set!",
                                    "define",
                                    "lambda",
                                    "begin",
                                    "#t",
                                    "#f",
                                    "defmacro"
                                ],
            Tok.caseSensitive = True
        }

lexer           = Tok.makeTokenParser intDef
string          = Tok.stringLiteral lexer
integer         = Tok.integer lexer
float           = Tok.float lexer
parens          = Tok.parens lexer
brackets        = Tok.brackets lexer
commaSep        = Tok.commaSep lexer
identifier      = Tok.identifier lexer
whitespace      = Tok.whiteSpace lexer
reserved        = Tok.reserved lexer
reservedOp      = Tok.reservedOp lexer
operator        = Tok.operator lexer
naturalOrFloat  = Tok.naturalOrFloat lexer
charLiteral     = Tok.charLiteral lexer

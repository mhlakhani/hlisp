{-# Language BangPatterns #-}
module Main where

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Control.DeepSeq
import Data.Either

import Types
import Parser
import Interpreter
import Builtins
import Environment

processExpr :: LispVal -> Environment -> (LispVal, Environment, LispVal)
processExpr !expr !env = result
  where
    (!expr', !env') = expandInterpreter (expr, env)
    !tco = tailCallOptimize env' expr' 
    (!ret, !env'') = evalInterpreter (tco, env')
    !result = (ret, env'', tco)

processRepl :: String -> Environment -> IO Environment
processRepl line env = do
  let expr = parseForm "<stdin>" $ C.pack line
  let (ret, env'', tco) = processExpr expr env
  print ret
  return env''

runRepl :: Environment -> IO ()
runRepl env = runInputT defaultSettings (loop env)
  where 
  loop env = do
    minput <- getInputLine "hl> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        env' <- liftIO $ processRepl input env
        loop env'

processFile :: Bool -> [LispVal] -> Environment -> IO Environment
processFile verbose !exprs !env = loop exprs env
  where
  loop ![] !env = return env
  loop (expr:exprs) !env = do
    let (ret, env'', tco) = processExpr expr env
    if isLispError ret
      then do
        print "Error: " 
        print ret
        return env''
      else do
        when verbose $ case tco of
          (Definition _ _) -> return ()
          _ -> print ret
        loop exprs env''

runFile :: String -> Bool -> Environment -> IO Environment
runFile fname verbose !env = do
  !source <- B.readFile fname 
  let !res = parseTopLevel fname source
  case res of
    Left err -> do
      print err
      return env
    Right !exprs -> result where !result = processFile verbose exprs env

main :: IO ()
main = do
  args <- getArgs
  env <- liftM force runFile "stdlib.hl" False makeBuiltinEnvironment
  let !env' = makeChildEnvironment env
  case args of
    []      ->  runRepl env'
    [fname] -> void $ runFile fname True env'

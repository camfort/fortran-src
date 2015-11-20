module Main where

import Forpar.Lexer.FixedForm
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then fail "Needs two arguments"
  else do
    let flag = args !! 0
    let path = args !! 1
    contents <- readFile path
    case flag of
      "--fixed-form-lexer" -> do
        let tokens = collectFixedFormTokens contents
        putStrLn $ show tokens

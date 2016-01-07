module Main where

import System.Environment
import Text.PrettyPrint.GenericPretty (pp)

import Forpar.Lexer.FixedForm
import Forpar.Parser.Fortran66 (fortran66Parser)

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
        case tokens of
          Just tokens' -> putStrLn $ show tokens'
          Nothing -> putStrLn "Cannot lex the file"
      "--66-parser" -> pp $ fortran66Parser contents path

module Main where

import System.Console.GetOpt

import System.Environment
import Text.PrettyPrint.GenericPretty (pp)
import Data.List (isInfixOf, isSuffixOf)
import Data.Char (toLower)

import Forpar.ParserMonad (FortranVersion(..))
import Forpar.Lexer.FixedForm (collectFixedTokens)
import Forpar.Parser.Fortran66 (fortran66Parser)
import Forpar.Parser.Fortran77 (fortran77Parser)

programName = "Forpar"

main :: IO ()
main = do
  args <- getArgs
  (opts, parsedArgs) <- compileArgs args
  if length parsedArgs /= 1
  then fail $ usageInfo programName options 
  else do
    let path = parsedArgs !! 0
    contents <- readFile path
    let version = case fortranVersion opts of { Just v -> v; Nothing -> deduceVersion path }
    case action opts of
      Lex -> 
        if version `elem` [Fortran66, Fortran77]
        then do
            let tokens = collectFixedTokens version contents
            case tokens of
              Just tokens' -> putStrLn $ show tokens'
              Nothing -> putStrLn "Cannot lex the file"
        else if version `elem` [Fortran90, Fortran2003, Fortran2008]
        then fail "for now"
        else ioError $ userError $ usageInfo programName options
      Parse ->
        case version of 
          Fortran66 -> pp $ fortran66Parser contents path
          Fortran77 -> pp $ fortran77Parser contents path

data Action = Lex | Parse

instance Read Action where
  readsPrec _ value = 
    let options = [ ("lex", Lex) , ("parse", Parse) ] in
      tryTypes options
      where
        tryTypes [] = []
        tryTypes ((attempt,result):xs) = 
          if map toLower value == attempt then [(result, "")] else tryTypes xs

data Options = Options 
  { fortranVersion  :: Maybe FortranVersion
  , action   :: Action }

initOptions = Options Nothing Parse

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] 
      ["fortranVersion"] 
      (ReqArg (\v opts -> opts { fortranVersion = Just $ read v }) "VERSION") 
      "fortran fortranVersion to be used"
  , Option ['a']
      ["action"]
      (ReqArg (\a opts -> opts { action = read a }) "ACTION")
      "lex or parse action" ]

compileArgs :: [ String ] -> IO (Options, [ String ])
compileArgs args = 
  case getOpt Permute options args of
    (o, n, []) -> return $ (foldl (flip id) initOptions o, n)
    (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where
    header = "Usage: forpar [OPTION...] <lex|parse> <file>"

deduceVersion :: String -> FortranVersion
deduceVersion path
  | isExtensionOf ".f"      = Fortran77
  | isExtensionOf ".for"    = Fortran77
  | isExtensionOf ".fpp"    = Fortran77
  | isExtensionOf ".ftn"    = Fortran77
  | isExtensionOf ".f90"    = Fortran90
  | isExtensionOf ".f95"    = Fortran95
  | isExtensionOf ".f03"    = Fortran2003
  | isExtensionOf ".f2003"  = Fortran2003
  | isExtensionOf ".f08"    = Fortran2008
  | isExtensionOf ".f2008"  = Fortran2008
  where
    isExtensionOf = flip isSuffixOf $ map toLower path

instance Read FortranVersion where
  readsPrec _ value = 
    let options = [ ("66", Fortran66)
                  , ("77", Fortran77)
                  , ("90", Fortran90)
                  , ("95", Fortran95)
                  , ("03", Fortran2003)
                  , ("08", Fortran2008)] in
      tryTypes options
      where
        tryTypes [] = []
        tryTypes ((attempt,result):xs) = 
          if isInfixOf attempt value then [(result, "")] else tryTypes xs

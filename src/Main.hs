{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Main where

import System.Console.GetOpt

import System.Environment
import Text.PrettyPrint.GenericPretty (pp)
import Data.List (isInfixOf, isSuffixOf, intercalate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import Forpar.ParserMonad (FortranVersion(..))
import qualified Forpar.Lexer.FixedForm as FixedForm (collectFixedTokens, Token(..))
import qualified Forpar.Lexer.FreeForm as FreeForm (collectFreeTokens, Token(..))
import Forpar.Parser.Fortran66 (fortran66Parser)
import Forpar.Parser.Fortran77 (fortran77Parser, extended77Parser)
import Forpar.Analysis.Types (TypeScope(..), inferTypes, IDType(..))
import Forpar.Analysis.BBlocks ( analyseBBlocks, showBBlocks, showAnalysedBBGr, bbgrToDOT
                               , genBBlockMap, genSuperBBGr, showSuperBBGr, superBBGrToDOT )
import Forpar.Analysis.DataFlow (showDataFlow)
import Forpar.Analysis.Renaming (renameAndStrip, analyseRenames)
import Forpar.Analysis (initAnalysis)

import qualified Data.Map as M
import Control.Monad
import Text.Printf

programName = "Forpar"

main :: IO ()
main = do
  args <- getArgs
  (opts, parsedArgs) <- compileArgs args
  if length parsedArgs /= 1
  then fail $ usageInfo programName options
  else do
    let path = head parsedArgs
    contents <- readFile path
    let version = fromMaybe (deduceVersion path) (fortranVersion opts)
    let Just parserF = lookup version
                              [ (Fortran66, fortran66Parser)
                              , (Fortran77, fortran77Parser)
                              , (Fortran77Extended, extended77Parser) ]
    let outfmt = outputFormat opts

    let runRenamer = fst . renameAndStrip . analyseRenames . initAnalysis
    let runBBlocks pf = showBBlocks pf' ++ "\n\n" ++ showDataFlow pf'
          where pf' = analyseBBlocks (initAnalysis pf)
    let runSuperGraph pf | outfmt == DOT = superBBGrToDOT sgr
                         | otherwise     = showSuperBBGr sgr
          where pf' = analyseBBlocks (initAnalysis pf)
                bbm = genBBlockMap pf'
                sgr = genSuperBBGr bbm

    case action opts of
      Lex | version `elem` [ Fortran66, Fortran77, Fortran77Extended ] -> do
        print $ FixedForm.collectFixedTokens version contents
      Lex | version `elem` [Fortran90, Fortran2003, Fortran2008] -> do
        print $ FreeForm.collectFreeTokens version contents
      Lex        -> ioError $ userError $ usageInfo programName options
      Parse      -> pp $ parserF contents path
      Typecheck  -> printTypes . inferTypes $ parserF contents path
      Rename     -> pp . runRenamer $ parserF contents path
      BBlocks    -> putStrLn . runBBlocks $ parserF contents path
      SuperGraph -> putStrLn . runSuperGraph $ parserF contents path

printTypes tenv = forM_ (M.toList tenv) $ \ (scope, tmap) -> do
  putStrLn $ "Scope: " ++ (case scope of Global -> "Global"; Local n -> show n)
  forM_ (M.toList tmap) $ \ (name, IDType { idVType = vt, idCType = ct }) ->
    printf "%s\t\t%s %s\n" name (drop 2 $ maybe "  -" show vt) (drop 2 $ maybe "   " show ct)

data Action = Lex | Parse | Typecheck | Rename | BBlocks | SuperGraph

instance Read Action where
  readsPrec _ value =
    let options = [ ("lex", Lex) , ("parse", Parse) ] in
      tryTypes options
      where
        tryTypes [] = []
        tryTypes ((attempt,result):xs) =
          if map toLower value == attempt then [(result, "")] else tryTypes xs

data OutputFormat = Default | DOT deriving Eq

data Options = Options
  { fortranVersion  :: Maybe FortranVersion
  , action          :: Action
  , outputFormat    :: OutputFormat }

initOptions = Options Nothing Parse Default

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v']
      ["fortranVersion"]
      (ReqArg (\v opts -> opts { fortranVersion = Just $ read v }) "VERSION")
      "fortran fortranVersion to be used"
  , Option ['a']
      ["action"]
      (ReqArg (\a opts -> opts { action = read a }) "ACTION")
      "lex or parse action"
  , Option ['t']
      ["typecheck"]
      (NoArg $ \ opts -> opts { action = Typecheck })
      "parse and run typechecker"
  , Option ['R']
      ["rename"]
      (NoArg $ \ opts -> opts { action = Rename })
      "parse and rename variables"
  , Option ['B']
      ["bblocks"]
      (NoArg $ \ opts -> opts { action = BBlocks })
      "analyse basic blocks"
  , Option ['S']
      ["supergraph"]
      (NoArg $ \ opts -> opts { action = SuperGraph })
      "analyse super graph of basic blocks"
  , Option []
      ["dot"]
      (NoArg $ \ opts -> opts { outputFormat = DOT })
      "output graphs in GraphViz DOT format"
  ]

compileArgs :: [ String ] -> IO (Options, [ String ])
compileArgs args =
  case getOpt Permute options args of
    (o, n, []) -> return (foldl (flip id) initOptions o, n)
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
                  , ("77e", Fortran77Extended)
                  , ("77", Fortran77)
                  , ("90", Fortran90)
                  , ("95", Fortran95)
                  , ("03", Fortran2003)
                  , ("08", Fortran2008)] in
      tryTypes options
      where
        tryTypes [] = []
        tryTypes ((attempt,result):xs) =
          if attempt `isInfixOf` value then [(result, "")] else tryTypes xs

instance {-# OVERLAPPING #-} Show [ FixedForm.Token ] where
  show = unlines . lines'
    where
      lines' [] = []
      lines' xs =
        let (x, xs') = break isNewline xs
        in case xs' of
             (nl@(FixedForm.TNewline _):xs'') -> ('\t' : (intercalate ", " . map show $ x ++ [nl])) : lines' xs''
             xs'' -> [ show xs'' ]
      isNewline (FixedForm.TNewline _) = True
      isNewline _ = False

instance {-# OVERLAPPING #-} Show [ FreeForm.Token ] where
  show = unlines . lines'
    where
      lines' [] = []
      lines' xs =
        let (x, xs') = break isNewline xs
        in case xs' of
             (nl@(FreeForm.TNewline _):xs'') -> ('\t' : (intercalate ", " . map show $ x ++ [nl])) : lines' xs''
             xs'' -> [ show xs'' ]
      isNewline (FreeForm.TNewline _) = True
      isNewline _ = False

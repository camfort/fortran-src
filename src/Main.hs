{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module Main where

import Prelude hiding (readFile)
import qualified Data.ByteString.Char8 as B
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

import Text.PrettyPrint (render)

import System.Console.GetOpt

import System.Environment
import Text.PrettyPrint.GenericPretty (pp, pretty, Out)
import Data.List (isInfixOf, isSuffixOf, intercalate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

import Language.Fortran.ParserMonad (FortranVersion(..))
import qualified Language.Fortran.Lexer.FixedForm as FixedForm (collectFixedTokens, Token(..))
import qualified Language.Fortran.Lexer.FreeForm as FreeForm (collectFreeTokens, Token(..))

import Language.Fortran.Parser.Fortran66 (fortran66Parser)
import Language.Fortran.Parser.Fortran77 (fortran77Parser, extended77Parser)
import Language.Fortran.Parser.Fortran90 (fortran90Parser)
import Language.Fortran.Parser.Any

import Language.Fortran.PrettyPrint
import Language.Fortran.Analysis
import Language.Fortran.AST
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.DataFlow
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Analysis (initAnalysis)
import Data.Graph.Inductive hiding (trc)
import Data.Graph.Inductive.PatriciaTree (Gr)

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad
import Text.Printf

programName = "fortran-src"

main :: IO ()
main = do
  args <- getArgs
  (opts, parsedArgs) <- compileArgs args
  if length parsedArgs /= 1
  then fail $ usageInfo programName options
  else do
    let path = head parsedArgs
    contents <- flexReadFile path
    let version = fromMaybe (deduceVersion path) (fortranVersion opts)
    let Just parserF = lookup version parserVersions
    let outfmt = outputFormat opts

    let runInfer pf = analyseTypes . analyseRenames . initAnalysis $ pf
    let runRenamer = stripAnalysis . rename . analyseRenames . initAnalysis
    let runBBlocks pf = showBBlocks pf' ++ "\n\n" ++ showDataFlow pf'
          where pf' = analyseBBlocks . analyseRenames . initAnalysis $ pf
    let runSuperGraph pf | outfmt == DOT = superBBGrToDOT sgr
                         | otherwise     = superGraphDataFlow pf' sgr
          where pf' = analyseBBlocks . analyseRenames . initAnalysis $ pf
                bbm = genBBlockMap pf'
                sgr = genSuperBBGr bbm

    case action opts of
      Lex | version `elem` [ Fortran66, Fortran77, Fortran77Extended ] ->
        print $ FixedForm.collectFixedTokens version contents
      Lex | version `elem` [Fortran90, Fortran2003, Fortran2008] ->
        print $ FreeForm.collectFreeTokens version contents
      Lex        -> ioError $ userError $ usageInfo programName options
      Parse      -> pp $ parserF contents path
      Typecheck  -> printTypes . extractTypeEnv . fst . runInfer $ parserF contents path
      Rename     -> pp . runRenamer $ parserF contents path
      BBlocks    -> putStrLn . runBBlocks $ parserF contents path
      SuperGraph -> putStrLn . runSuperGraph $ parserF contents path
      Reprint    -> putStrLn . render . flip (pprint version) (Just 0) $ parserF contents path

superGraphDataFlow :: forall a. (Out a, Data a) => ProgramFile (Analysis a) -> SuperBBGr (Analysis a) -> String
superGraphDataFlow pf sgr = showBBGr (nmap (map (fmap insLabel)) gr) ++ "\n\n" ++ replicate 50 '-' ++ "\n\n" ++
                            show entries ++ "\n\n" ++ replicate 50 '-' ++ "\n\n" ++
                            dfStr gr
  where
    gr = superBBGrGraph sgr
    entries = superBBGrEntries sgr
    dfStr gr = (\ (l, x) -> '\n':l ++ ": " ++ x) =<< [
                 ("callMap",      show cm)
               , ("postOrder",    show (postOrder gr))
               , ("revPostOrder", show (revPostOrder gr))
               , ("revPreOrder",  show (revPreOrder gr))
               , ("dominators",   show (dominators gr))
               , ("iDominators",  show (iDominators gr))
               , ("defMap",       show dm)
               , ("lva",          show (IM.toList $ lva gr))
               , ("rd",           show (IM.toList rDefs))
               , ("backEdges",    show bedges)
               , ("topsort",      show (topsort gr))
               , ("scc ",         show (scc gr))
               , ("loopNodes",    show (loopNodes bedges gr))
               , ("duMap",        show (genDUMap bm dm gr rDefs))
               , ("udMap",        show (genUDMap bm dm gr rDefs))
               , ("flowsTo",      show (edges flTo))
               , ("varFlowsTo",   show (genVarFlowsToMap dm flTo))
               , ("ivMap",        show (genInductionVarMap bedges gr))
               , ("noPredNodes",  show (noPredNodes gr))
               , ("blockMap",     unlines [ "AST-block " ++ show i ++ ":\n" ++ pretty b | (i, b) <- IM.toList bm ])
               , ("derivedInd",   unlines [ "Expression " ++ show i ++ " (IE: " ++ show ie ++ "):\n" ++ pretty e
                                          | e <- universeBi bm :: [Expression (Analysis a)]
                                          , i <- maybeToList (insLabel (getAnnotation e))
                                          , let ie = IM.lookup i diMap ])
               ] where
                   bedges = genBackEdgeMap (dominators gr) gr
                   flTo   = genFlowsToGraph bm dm gr rDefs
                   rDefs  = rd gr
                   diMap  = genDerivedInductionMap bedges gr
    lva = liveVariableAnalysis
    bm = genBlockMap pf
    dm = genDefMap bm
    rd = reachingDefinitions dm
    cm = genCallMap pf

printTypes :: TypeEnv -> IO ()
printTypes tenv = do
  forM_ (M.toList tenv) $ \ (name, IDType { idVType = vt, idCType = ct }) ->
    printf "%s\t\t%s %s\n" name (drop 4 $ maybe "  -" show vt) (drop 2 $ maybe "   " show ct)

data Action = Lex | Parse | Typecheck | Rename | BBlocks | SuperGraph | Reprint

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
      "Fortran version to use, format: Fortran[66/77/77Extended/90]"
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
  , Option ['r']
      ["reprint"]
      (NoArg $ \ opts -> opts { action = Reprint })
      "Parse and output using pretty printer"
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

flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile

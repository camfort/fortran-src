{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module Main where

import Prelude hiding (readFile)
import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

import Text.PrettyPrint (render)
import Text.Read

import System.Console.GetOpt

import System.Environment
import System.Directory
import System.FilePath
import Text.PrettyPrint.GenericPretty (pp, pretty, Out)
import Data.List (isInfixOf, intercalate, (\\), isSuffixOf)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Data
import Data.Binary
import Data.Generics.Uniplate.Data

import Language.Fortran.ParserMonad (FortranVersion(..), fromRight)
import qualified Language.Fortran.Lexer.FixedForm as FixedForm (collectFixedTokens, Token(..))
import qualified Language.Fortran.Lexer.FreeForm as FreeForm (collectFreeTokens, Token(..))

import Language.Fortran.Parser.Any

import Language.Fortran.Util.ModFile

import Language.Fortran.PrettyPrint
import Language.Fortran.Analysis
import Language.Fortran.AST
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.DataFlow
import Language.Fortran.Analysis.Renaming
import Data.Graph.Inductive hiding (trc, mf, version)

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad
import Text.Printf

programName :: String
programName = "fortran-src"

main :: IO ()
main = do
  args <- getArgs
  (opts, parsedArgs) <- compileArgs args
  case (parsedArgs, action opts) of
    ([path], DumpModFile) -> do
      let path' = if modFileSuffix `isSuffixOf` path then path else path <.> modFileSuffix
      contents <- B.readFile path'
      case decodeModFile contents of
        Left msg -> putStrLn $ "Error: " ++ msg
        Right mf -> putStrLn $ "Filename: " ++ moduleFilename mf ++
                               "\n\nModuleMap:\n" ++ showModuleMap (combinedModuleMap [mf]) ++
                               "\n\nTypeEnv:\n" ++ showTypes (combinedTypeEnv [mf]) ++
                               "\n\nDeclMap:\n" ++ showGenericMap (combinedDeclMap [mf]) ++
                               "\n\nOther Data Labels: " ++ show (getLabelsModFileData mf)

    ([path], actionOpt) -> do
      contents <- flexReadFile path
      let version = fromMaybe (deduceVersion path) (fortranVersion opts)
      let (Just parserF0) = lookup version parserWithModFilesVersions
      let parserF m b s = fromRight (parserF0 m b s)
      let outfmt = outputFormat opts
      mods <- decodeModFiles $ includeDirs opts
      let mmap = combinedModuleMap mods
      let tenv = combinedTypeEnv mods

      let runInfer = analyseTypesWithEnv tenv . analyseRenamesWithModuleMap mmap . initAnalysis
      let runRenamer = stripAnalysis . rename . analyseRenamesWithModuleMap mmap . initAnalysis
      let runBBlocks pf = showBBlocks pf' ++ "\n\n" ++ showDataFlow pf'
            where pf' = analyseBBlocks . analyseRenamesWithModuleMap mmap . initAnalysis $ pf
      let runSuperGraph pf | outfmt == DOT = superBBGrToDOT sgr
                           | otherwise     = superGraphDataFlow pf' sgr
            where pf' = analyseBBlocks . analyseRenamesWithModuleMap mmap . initAnalysis $ pf
                  bbm = genBBlockMap pf'
                  sgr = genSuperBBGr bbm
      let runCompile = encodeModFile . genModFile . fst . analyseTypesWithEnv tenv . analyseRenamesWithModuleMap mmap . initAnalysis

      case actionOpt of
        Lex | version `elem` [ Fortran66, Fortran77, Fortran77Extended, Fortran77Legacy ] ->
          print $ FixedForm.collectFixedTokens version contents
        Lex | version `elem` [Fortran90, Fortran2003, Fortran2008] ->
          print $ FreeForm.collectFreeTokens version contents
        Lex        -> ioError $ userError $ usageInfo programName options
        Parse      -> pp $ parserF mods contents path
        Typecheck  -> printTypes . extractTypeEnv . fst . runInfer $ parserF mods contents path
        Rename     -> pp . runRenamer $ parserF mods contents path
        BBlocks    -> putStrLn . runBBlocks $ parserF mods contents path
        SuperGraph -> putStrLn . runSuperGraph $ parserF mods contents path
        Reprint    -> putStrLn . render . flip (pprint version) (Just 0) $ parserF mods contents path
        Compile    -> do
          let bytes = runCompile $ parserF mods contents path
          let fspath = path <.> modFileSuffix
          B.writeFile fspath bytes

    _ -> fail $ usageInfo programName options

-- List files in dir recursively
rGetDirContents :: String -> IO [String]
rGetDirContents d = canonicalizePath d >>= \d' -> go [d'] d'
  where
  go seen d'' = do
    ds <- getDirectoryContents d''
    fmap concat . mapM f $ ds \\ [".", ".."] -- remove '.' and '..' entries
      where
        f x = do
          path <- canonicalizePath $ d ++ "/" ++ x
          g <- doesDirectoryExist path
          if g && notElem path seen then do
            x' <- go (path : seen) path
            return $ map (\ y -> x ++ "/" ++ y) x'
          else return [x]

-- List files in dir
getDirContents :: String -> IO [String]
getDirContents d = do
  d' <- canonicalizePath d
  map (d' </>) `fmap` listDirectory d'

decodeModFiles :: [String] -> IO ModFiles
decodeModFiles = foldM (\ modFiles d -> do
      -- Figure out the camfort mod files and parse them.
      modFileNames <- filter isModFile `fmap` getDirContents d
      addedModFiles <- forM modFileNames $ \ modFileName -> do
        eResult <- decodeFileOrFail (d </> modFileName)
        case eResult of
          Left (offset, msg) -> do
            putStrLn $ modFileName ++ ": Error at offset " ++ show offset ++ ": " ++ msg
            return emptyModFile
          Right modFile -> do
            putStrLn $ modFileName ++ ": successfully parsed precompiled file."
            return modFile
      return $ addedModFiles ++ modFiles
    ) emptyModFiles

isModFile :: FilePath -> Bool
isModFile = (== modFileSuffix) . takeExtension

superGraphDataFlow :: forall a. (Out a, Data a) => ProgramFile (Analysis a) -> SuperBBGr (Analysis a) -> String
superGraphDataFlow pf sgr = showBBGr (nmap (map (fmap insLabel)) gr') ++ "\n\n" ++ replicate 50 '-' ++ "\n\n" ++
                            show entries ++ "\n\n" ++ replicate 50 '-' ++ "\n\n" ++
                            dfStr gr'
  where
    gr' = superBBGrGraph sgr
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
               , ("constExpMap",  show (genConstExpMap pf))
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

showGenericMap :: (Show a, Show b) => M.Map a b -> String
showGenericMap = unlines . map (\ (k, v) -> show k ++ " : " ++ show v) . M.toList

showModuleMap :: ModuleMap -> String
showModuleMap = concatMap (\ (n, m) -> show n ++ ":\n" ++ (unlines . map ("  "++) . lines . showGenericMap $ m)) . M.toList
showTypes :: TypeEnv -> String
showTypes tenv = flip concatMap (M.toList tenv) $ \ (name, IDType { idVType = vt, idCType = ct }) ->
    printf "%s\t\t%s %s\n" name (drop 4 $ maybe "  -" show vt) (drop 2 $ maybe "   " show ct)
printTypes :: TypeEnv -> IO ()
printTypes = putStrLn . showTypes

data Action = Lex | Parse | Typecheck | Rename | BBlocks | SuperGraph | Reprint | DumpModFile | Compile deriving Eq

instance Read Action where
  readsPrec _ value =
    let options' = [ ("lex", Lex) , ("parse", Parse) ] in
      tryTypes options'
      where
        tryTypes [] = []
        tryTypes ((attempt,result):xs) =
          if map toLower value == attempt then [(result, "")] else tryTypes xs

data OutputFormat = Default | DOT deriving Eq

data Options = Options
  { fortranVersion  :: Maybe FortranVersion
  , action          :: Action
  , outputFormat    :: OutputFormat
  , includeDirs     :: [String] }

initOptions :: Options
initOptions = Options Nothing Parse Default []

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v']
      ["fortranVersion"]
      (ReqArg (\v opts -> opts { fortranVersion = readMaybe v }) "VERSION")
      "Fortran version to use, format: Fortran[66/77/77Legacy/77Extended/90]"
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
  , Option []
      ["dump-mod-file"]
      (NoArg $ \ opts -> opts { action = DumpModFile })
      "dump the information contained within mod files"
  , Option ['I']
      ["include-dir"]
      (ReqArg (\ d opts -> opts { includeDirs = d:includeDirs opts }) "DIR")
      "directory to search for precompiled 'mod files'"
  , Option ['c']
      ["compile"]
      (NoArg $ \ opts -> opts { action = Compile })
      "compile an .fsmod file from the input"
  ]

compileArgs :: [ String ] -> IO (Options, [ String ])
compileArgs args =
  case getOpt Permute options args of
    (o, n, []) -> return (foldl (flip id) initOptions o, n)
    (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where
    header = "Usage: " ++ programName ++ " [OPTION...] <file>"

instance Read FortranVersion where
  readsPrec _ value = tryTypes options'
    where
      value' = map toLower value
      options' = [ ("66" , Fortran66)
                , ("77e", Fortran77Extended)
                , ("77l", Fortran77Legacy)
                , ("77" , Fortran77)
                , ("90" , Fortran90)
                , ("95" , Fortran95)
                , ("03" , Fortran2003)
                , ("08" , Fortran2008) ]
      tryTypes [] = []
      tryTypes ((attempt,result):xs)
          | attempt `isInfixOf` value' = [(result, "")]
          | otherwise = tryTypes xs

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

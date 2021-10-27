{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main ( main ) where

import Prelude hiding (readFile, mod)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import Text.PrettyPrint (render)

import System.Console.GetOpt
import System.IO
import System.Environment
import System.Directory
import System.FilePath
import Text.PrettyPrint.GenericPretty (pp, pretty, Out)
import Text.Read (readMaybe)
import Data.List (sortBy, intercalate, isSuffixOf)
import Data.Ord (comparing)
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe, maybeToList)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Graph.Inductive hiding (trc, mf, version)
import Data.Either.Combinators ( fromRight' )

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad
import Text.Printf

import Language.Fortran.Parser
import Language.Fortran.Version
import Language.Fortran.Util.ModFile
import Language.Fortran.Util.Position
import Language.Fortran.Util.Files
import Language.Fortran.PrettyPrint
import Language.Fortran.Analysis
import Language.Fortran.AST
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.ModGraph
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.DataFlow
import Language.Fortran.Analysis.Renaming
import qualified Language.Fortran.Parser as Parser
import qualified Language.Fortran.Parser.Fixed.Lexer as Fixed
import qualified Language.Fortran.Parser.Free.Lexer  as Free

programName :: String
programName = "fortran-src"

main :: IO ()
main = do
  args <- getArgs
  (opts, parsedArgs) <- compileArgs args
  case (parsedArgs, action opts) of
    (paths, ShowMakeGraph) -> do
      paths' <- expandDirs paths
      mg <- genModGraph (fortranVersion opts) (includeDirs opts) paths'
      putStrLn $ modGraphToDOT mg
    -- make: construct a build-dep graph and follow it
    (paths, Make) -> do
      let mvers = fortranVersion opts
      paths' <- expandDirs paths
      -- Build the graph of module dependencies
      mg0 <- genModGraph mvers (includeDirs opts) paths'
      -- Start the list of mods with those from the command line
      mods0 <- decodeModFiles' $ includeDirs opts
      -- Loop through the dependency graph until it is empty
      let loop mg mods
            | nxt <- takeNextMods mg
            , not (null nxt) = do
                let fnPaths = [ fn | (_, Just (MOFile fn)) <- nxt ]
                newMods <- fmap concat . forM fnPaths $ \ fnPath -> do
                  tsStatus <- checkTimestamps fnPath
                  case tsStatus of
                    NoSuchFile -> do
                      putStr $ "Does not exist: " ++ fnPath
                      pure [emptyModFile]
                    ModFileExists modPath -> do
                      putStrLn $ "Loading mod file " ++ modPath ++ "."
                      decodeOneModFile modPath
                    CompileFile -> do
                      putStr $ "Summarising " ++ fnPath ++ "..."
                      mod <- compileFileToMod mvers mods fnPath Nothing
                      putStrLn "done"
                      pure [mod]

                let ns  = map fst nxt
                let mg' = delModNodes ns mg
                loop mg' $ newMods ++ mods
          loop _ mods = pure mods

      allMods <- loop mg0 mods0
      case outputFile opts of
        Nothing -> pure ()
        Just f  -> LB.writeFile f $ encodeModFile allMods

    (paths, Compile) -> do
      mods <- decodeModFiles' $ includeDirs opts
      mapM_ (\ p -> compileFileToMod (fortranVersion opts) mods p (outputFile opts)) paths
    (path:_, actionOpt) -> do
      contents <- flexReadFile path
      mods <- decodeModFiles' $ includeDirs opts
      let version   = fromMaybe (deduceFortranVersion path) (fortranVersion opts)
          parsedPF  = case (Parser.byVerWithMods mods version) path contents of
                        Left  a -> error $ show a
                        Right a -> a
          outfmt    = outputFormat opts
          mmap      = combinedModuleMap mods
          tenv      = combinedTypeEnv mods
          pvm       = combinedParamVarMap mods

      let runTypes = analyseAndCheckTypesWithEnv tenv . analyseRenamesWithModuleMap mmap . initAnalysis
      let runRenamer = stripAnalysis . rename . analyseRenamesWithModuleMap mmap . initAnalysis
      let runBBlocks pf = showBBlocks pf' ++ "\n\n" ++ showDataFlow pf'
            where pf' = analyseParameterVars pvm . analyseBBlocks . analyseRenamesWithModuleMap mmap . initAnalysis $ pf
      let runSuperGraph pf | outfmt == DOT = superBBGrToDOT sgr
                           | otherwise     = superGraphDataFlow pf' sgr
            where pf' = analyseParameterVars pvm . analyseBBlocks . analyseRenamesWithModuleMap mmap . initAnalysis $ pf
                  bbm = genBBlockMap pf'
                  sgr = genSuperBBGr bbm
      let findBlockPU pf astBlockId = listToMaybe
            [ pu | pu <- universeBi pf :: [ProgramUnit (Analysis A0)]
                 , bbgr <- maybeToList (bBlocks (getAnnotation pu))
                 , b <- concatMap snd $ labNodes (bbgrGr bbgr)
                 , insLabel (getAnnotation b) == Just astBlockId ]
      case actionOpt of
        Lex | version `elem` [ Fortran66, Fortran77, Fortran77Extended, Fortran77Legacy ] ->
          print $ Parser.collectTokens Fixed.lexer' $ initParseStateFixed "<unknown>" version contents
        Lex | version `elem` [Fortran90, Fortran2003, Fortran2008] ->
          print $ Parser.collectTokens Free.lexer'  $ initParseStateFree "<unknown>" version contents
        Lex        -> ioError $ userError $ usageInfo programName options
        Parse      -> pp parsedPF
{-
        Typecheck  -> let (pf, _, errs) = runTypes parsedPF in
                        printTypeErrors errs >> printTypes (extractTypeEnv pf)
        Typecheck  -> let (pf, _, errs) = runTypes (parserF mods contents path) in
                        printTypeErrors errs >> printTypes (regenerateTypeEnv pf)
-}
        Rename     -> pp $ runRenamer parsedPF
        BBlocks    -> putStrLn $ runBBlocks parsedPF
        SuperGraph -> putStrLn $ runSuperGraph parsedPF
        Reprint    ->
          let prettyContents = render . flip (pprint version) (Just 0) $ parsedPF
           in putStrLn $
                if   useContinuationReformatter opts
                then reformatMixedFormInsertContinuations prettyContents
                else prettyContents
        DumpModFile -> do
          let path' = if modFileSuffix `isSuffixOf` path then path else path <.> modFileSuffix
          contents' <- LB.readFile path'
          case decodeModFile contents' of
            Left msg  -> putStrLn $ "Error: " ++ msg
            Right mfs -> forM_ mfs $ \ mf ->
              putStrLn $ "Filename: " ++ moduleFilename mf ++
                       "\n\nStringMap:\n" ++ showStringMap (combinedStringMap [mf]) ++
                       "\n\nModuleMap:\n" ++ showModuleMap (combinedModuleMap [mf]) ++
                       "\n\nDeclMap:\n" ++ showGenericMap (combinedDeclMap [mf]) ++
                       "\n\nTypeEnv:\n" ++ showTypes (combinedTypeEnv [mf]) ++
                       "\n\nParamVarMap:\n" ++ showGenericMap (combinedParamVarMap [mf]) ++
                       "\n\nOther Data Labels: " ++ show (getLabelsModFileData mf)
        ShowFlows isFrom isSuper astBlockId -> do
          let pf = analyseParameterVars pvm .
                   analyseBBlocks .
                   analyseRenamesWithModuleMap mmap .
                   initAnalysis $ parsedPF
          let bbm = genBBlockMap pf
          case (isSuper, findBlockPU pf astBlockId) of
            (False, Nothing) -> fail "Couldn't find given AST block ID number."
            (False, Just pu)
              | Just bbgr <- M.lookup (puName pu) bbm ->
                  putStrLn $ showFlowsDOT pf bbgr astBlockId isFrom
              | otherwise -> do
                  print $ M.keys bbm
                  fail $ "Internal error: Program Unit " ++ show (puName pu) ++ " is lacking a basic block graph."
            (True, _) -> do
              let sgr = genSuperBBGr bbm
              putStrLn $ showFlowsDOT pf (superBBGrGraph sgr) astBlockId isFrom
        ShowBlocks mlinenum -> do
          let pf = analyseBBlocks .
                   analyseRenamesWithModuleMap mmap .
                   initAnalysis $ parsedPF
          let f :: ([ASTBlockNode], Int) -> ([ASTBlockNode], Int) -> ([ASTBlockNode], Int)
              f (nodes1, len1) (nodes2, len2)
                | len1 < len2 = (nodes1, len1)
                | len2 < len1 = (nodes2, len2)
                | otherwise   = (nodes1 ++ nodes2, len1)
          let lineMap :: IM.IntMap ([ASTBlockNode], Int)  -- ([list of IDs], line-distance of span)
              lineMap = IM.fromListWith f [
                (l, ([i], lineDistance ss))
                | b <- universeBi pf :: [Block (Analysis A0)]
                , i <- maybeToList . insLabel $ getAnnotation b
                , let ss = getSpan b
                , l <- spannedLines ss
                ]
          case mlinenum of
            Just l -> putStrLn . unwords . map show $ fromMaybe [] (fst <$> IM.lookup l lineMap)
            Nothing -> do
              let lineBs = B.lines contents
              let maxLen = maximum (0:map B.length lineBs)
              forM_ (zip lineBs [1..]) $ \ (line, l) -> do
                let nodeIDs = fromMaybe [] (fst <$> IM.lookup l lineMap)
                let nodeStr = B.intercalate "," (map (B.pack . ('B':) . show) nodeIDs)
                let suffix | null nodeIDs = ""
                           | otherwise    = B.replicate (maxLen - B.length line + 1) ' ' <> "!" <> nodeStr
                B.putStrLn $ line <> suffix
        _ -> fail $ usageInfo programName options
    _ -> fail $ usageInfo programName options


-- | Expand all paths that are directories into a list of Fortran
-- files from a recursive directory listing.
expandDirs :: [FilePath] -> IO [FilePath]
expandDirs = fmap concat . mapM each
  where
    each path = do
      isDir <- doesDirectoryExist path
      if isDir
        then listFortranFiles path
        else pure [path]

-- | Get a list of Fortran files under the given directory.
listFortranFiles :: FilePath -> IO [FilePath]
listFortranFiles dir = filter isFortran <$> listDirectoryRecursively dir
  where
    -- | True if the file has a valid fortran extension.
    isFortran :: FilePath -> Bool
    isFortran x = map toLower (takeExtension x) `elem` exts
      where exts = [".f", ".f90", ".f77", ".f03"]

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = listDirectoryRec dir ""
  where
    listDirectoryRec :: FilePath -> FilePath -> IO [FilePath]
    listDirectoryRec d f = do
      let fullPath = d </> f
      isDir <- doesDirectoryExist fullPath
      if isDir
      then do
        conts <- listDirectory fullPath
        concat <$> mapM (listDirectoryRec fullPath) conts
      else pure [fullPath]

compileFileToMod :: Maybe FortranVersion -> ModFiles -> FilePath -> Maybe FilePath -> IO ModFile
compileFileToMod mvers mods path moutfile = do
  contents <- flexReadFile path
  let version = fromMaybe (deduceFortranVersion path) mvers
      mmap = combinedModuleMap mods
      tenv = combinedTypeEnv mods
      runCompile = genModFile . fst . analyseTypesWithEnv tenv . analyseRenamesWithModuleMap mmap . initAnalysis
      parsedPF  = fromRight' $ (Parser.byVerWithMods mods version) path contents
      mod = runCompile parsedPF
      fspath = path -<.> modFileSuffix `fromMaybe` moutfile
  LB.writeFile fspath $ encodeModFile [mod]
  return mod

decodeOneModFile :: FilePath -> IO ModFiles
decodeOneModFile path = do
  contents <- LB.readFile path
  case decodeModFile contents of
    Left msg -> do
      hPutStrLn stderr $ path ++ ": Error: " ++ msg
      return []
    Right modFiles -> do
      hPutStrLn stderr $ path ++ ": successfully parsed summary file."
      return modFiles

-- TODO almost replicated at Analysis.DataFlow.showDataFlow
superGraphDataFlow :: forall a. (Out a, Data a) => ProgramFile (Analysis a) -> SuperBBGr (Analysis a) -> String
superGraphDataFlow pf sgr = showBBGr (bbgrMap (nmap (map (fmap insLabel))) gr') ++ "\n\n" ++ replicate 50 '-' ++ "\n\n" ++
                            show entries ++ "\n\n" ++ replicate 50 '-' ++ "\n\n" ++
                            dfStr gr'
  where
    gr' = superBBGrGraph sgr
    entries = superBBGrEntries sgr
    dfStr gr = (\ (l, x) -> '\n':l ++ ": " ++ x) =<< [
                 ("callMap",      show cm)
               , ("entries",      show (bbgrEntries gr))
               , ("exits",        show (bbgrExits gr))
               , ("postOrder",    show (postOrder gr))
               , ("revPostOrder", show (revPostOrder gr))
               , ("revPreOrder",  show (revPreOrder gr))
               , ("dominators",   show (dominators gr))
               , ("iDominators",  show (iDominators gr))
               , ("defMap",       show dm)
               , ("lva",          show (IM.toList $ lva gr))
               , ("rd",           show (IM.toList rDefs))
               , ("backEdges",    show bedges)
               , ("topsort",      show (topsort $ bbgrGr gr))
               , ("scc ",         show (scc $ bbgrGr gr))
               , ("loopNodes",    show (loopNodes bedges $ bbgrGr gr))
               , ("duMap",        show (genDUMap bm dm gr rDefs))
               , ("udMap",        show (genUDMap bm dm gr rDefs))
               , ("flowsTo",      show (edges flTo))
               , ("varFlowsTo",   show (genVarFlowsToMap dm flTo))
               , ("ivMap",        show (genInductionVarMap bedges gr))
               , ("blockMap",     unlines [ "AST-block " ++ show i ++ ":\n" ++ pretty b | (i, b) <- IM.toList bm ])
               , ("derivedInd",   unlines [ "Expression " ++ show i ++ " (IE: " ++ show ie ++ "):\n" ++ pretty e
                                          | e <- universeBi bm :: [Expression (Analysis a)]
                                          , i <- maybeToList (insLabel (getAnnotation e))
                                          , let ie = IM.lookup i diMap ])
               , ("constExpMap",  show (genConstExpMap pf))
               ] where
                   bedges = genBackEdgeMap (dominators gr) $ bbgrGr gr
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
showStringMap :: StringMap -> String
showStringMap = showGenericMap
showModuleMap :: ModuleMap -> String
showModuleMap = concatMap (\ (n, m) -> show n ++ ":\n" ++ (unlines . map ("  "++) . lines . showGenericMap $ m)) . M.toList
showTypes :: TypeEnv -> String
showTypes tenv =
    flip concatMap (M.toList tenv) $
      \ (name, IDType { idVType = vt, idCType = ct }) ->
        printf "%s\t\t%s %s\n" name (drop 1 $ maybe "  -" show vt) (drop 2 $ maybe "   " show ct)
printTypes :: TypeEnv -> IO ()
printTypes = putStrLn . showTypes
showTypeErrors :: [TypeError] -> String
showTypeErrors errs = unlines [ show ss ++ ": " ++ msg | (msg, ss) <- sortBy (comparing snd) errs ]
printTypeErrors :: [TypeError] -> IO ()
printTypeErrors = putStrLn . showTypeErrors

data Action
  = Lex | Parse | Typecheck | Rename | BBlocks | SuperGraph | Reprint | DumpModFile | Compile
  | ShowFlows Bool Bool Int | ShowBlocks (Maybe Int) | ShowMakeGraph | Make
  deriving Eq

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
  , outputFile      :: Maybe FilePath
  , includeDirs     :: [String]
  , useContinuationReformatter :: Bool
  }

initOptions :: Options
initOptions = Options Nothing Parse Default Nothing [] False

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v','F']
      ["fortranVersion"]
      (ReqArg (\v opts -> opts { fortranVersion = selectFortranVersion v }) "VERSION")
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
      ["split-long"]
      (NoArg $ \ opts -> opts { useContinuationReformatter = True })
      "when using pretty printer, split long lines via continuations"
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
      ["summarise", "compile-mod"]
      (NoArg $ \ opts -> opts { action = Compile })
      "build an .fsmod file from the input"
  , Option ['o']
      ["output-file"]
      (ReqArg (\ f opts -> opts { outputFile = Just f }) "FILE")
      "name of output file (e.g. name of generated fsmod file)"
  , Option []
      ["make-mods", "make"]
      (NoArg $ \ opts -> opts { action = Make })
      "determine dependency order of modules and automatically build .fsmod files"
  , Option []
      ["show-make-graph"]
      (NoArg $ \ opts -> opts { action = ShowMakeGraph })
      "dump a graph showing the build structure of modules"
  , Option []
      ["show-block-numbers"]
      (OptArg (\a opts -> opts { action = ShowBlocks (a >>= readMaybe) }
              ) "LINE-NUM")
      "Show the corresponding AST-block identifier number next to every line of code."
  , Option []
      ["show-flows-to"]
      (ReqArg (\a opts -> case a of s:num | toLower s == 's' -> opts { action = ShowFlows False True (read num) }
                                    b:num | toLower b == 'b' -> opts { action = ShowFlows False False (read num) }
                                    num                      -> opts { action = ShowFlows False False (read num) }
              ) "AST-BLOCK-ID")
      "dump a graph showing flows-to information from the given AST-block ID; prefix with 's' for supergraph"
  , Option []
      ["show-flows-from"]
      (ReqArg (\a opts -> case a of s:num | toLower s == 's' -> opts { action = ShowFlows True True (read num) }
                                    b:num | toLower b == 'b' -> opts { action = ShowFlows True False (read num) }
                                    num                      -> opts { action = ShowFlows True False (read num) }
              ) "AST-BLOCK-ID")
      "dump a graph showing flows-from information from the given AST-block ID; prefix with 's' for supergraph"
  ]

compileArgs :: [ String ] -> IO (Options, [ String ])
compileArgs args =
  case getOpt Permute options args of
    (o, n, []) -> return (foldl (flip id) initOptions o, n)
    (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where
    header = "Usage: " ++ programName ++ " [OPTION...] <file...>"

instance {-# OVERLAPPING #-} Show [ Fixed.Token ] where
  show = unlines . lines'
    where
      lines' [] = []
      lines' xs =
        let (x, xs') = break isNewline xs
        in case xs' of
             (nl@(Fixed.TNewline _):xs'') -> ('\t' : (intercalate ", " . map show $ x ++ [nl])) : lines' xs''
             xs'' -> [ show xs'' ]
      isNewline (Fixed.TNewline _) = True
      isNewline _ = False

instance {-# OVERLAPPING #-} Show [ Free.Token ] where
  show = unlines . lines'
    where
      lines' [] = []
      lines' xs =
        let (x, xs') = break isNewline xs
        in case xs' of
             (nl@(Free.TNewline _):xs'') -> ('\t' : (intercalate ", " . map show $ x ++ [nl])) : lines' xs''
             xs'' -> [ show xs'' ]
      isNewline (Free.TNewline _) = True
      isNewline _ = False

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, PatternGuards, TupleSections #-}

-- | Generate a module use-graph.
module Language.Fortran.Analysis.ModGraph
  (genModGraph, ModGraph(..), ModOrigin(..), modGraphToDOT, takeNextMods, delModNodes)
where

import Prelude hiding (mod)
import Control.Monad
import Control.Monad.State.Strict
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Graph.Inductive hiding (version)
import Data.Maybe
import Language.Fortran.AST hiding (setName)
import Language.Fortran.Version (FortranVersion(..), deduceFortranVersion)
import Language.Fortran.Parser.Any (parserWithModFilesVersions)
import Language.Fortran.ParserMonad (fromRight)
import Language.Fortran.Util.ModFile
import Language.Fortran.Util.Files
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as M
import System.IO
import System.FilePath

--------------------------------------------------

data ModOrigin = MOFile FilePath | MOFSMod FilePath
  deriving (Eq, Data, Show)

instance Ord ModOrigin where
  MOFSMod _ <= MOFSMod _ = True
  a <= b = a == b

data ModGraph = ModGraph { mgModNodeMap :: M.Map String (Node, Maybe ModOrigin)
                         , mgGraph      :: Gr String ()
                         , mgNumNodes   :: Int }
  deriving (Eq, Data)

modGraph0 :: ModGraph
modGraph0 = ModGraph M.empty empty 0

type ModGrapher a = StateT ModGraph IO a

maybeAddModName :: String -> Maybe ModOrigin -> ModGrapher Node
maybeAddModName modName org = do
  mg@ModGraph { mgModNodeMap = mnmap, mgGraph = gr, mgNumNodes = numNodes } <- get
  case M.lookup modName mnmap of
    Just (i, org') | org <= org' -> pure i
                   | otherwise   -> do
                       let mnmap' = M.insert modName (i, org) mnmap
                       put $ mg { mgModNodeMap = mnmap' }
                       pure i
    Nothing -> do
      let i = numNodes + 1
      let mnmap' = M.insert modName (i, org) mnmap
      let gr' = insNode (i, modName) gr
      put $ mg { mgModNodeMap = mnmap', mgGraph = gr', mgNumNodes = i }
      pure i

addModDep :: String -> String -> ModGrapher ()
addModDep modName depName = do
  i <- maybeAddModName modName Nothing
  j <- maybeAddModName depName Nothing
  mg@ModGraph { mgGraph = gr } <- get
  put $ mg { mgGraph = insEdge (i, j, ()) gr }

genModGraph :: Maybe FortranVersion -> [FilePath] -> [FilePath] -> IO ModGraph
genModGraph mversion includeDirs paths = do
  let perModule path pu@(PUModule _ _ modName _ _) = do
        _ <- maybeAddModName modName (Just $ MOFile path)
        let uses = [ usedName | StUse _ _ (ExpValue _ _ (ValVariable usedName)) _ _ _ <-
                                universeBi pu :: [Statement ()] ]
        forM_ uses $ \ usedName -> do
          _ <- maybeAddModName usedName Nothing
          addModDep modName usedName
      perModule path pu | Named puName <- getName pu = do
        _ <- maybeAddModName puName (Just $ MOFile path)
        let uses = [ usedName | StUse _ _ (ExpValue _ _ (ValVariable usedName)) _ _ _ <-
                                universeBi pu :: [Statement ()] ]
        forM_ uses $ \ usedName -> do
          _ <- maybeAddModName usedName Nothing
          addModDep puName usedName
      perModule _ _ = pure ()
  let iter :: FilePath -> ModGrapher ()
      iter path = do
        contents <- liftIO $ flexReadFile path
        let version = fromMaybe (deduceFortranVersion path) mversion
        let parserF0 = parserWithModFilesVersions version
        let parserF m b s = fromRight (parserF0 m b s)
        fileMods <- liftIO $ decodeModFiles includeDirs
        let mods = map snd fileMods
        forM_ fileMods $ \ (fileName, mod) -> do
          forM_ [ name | Named name <- M.keys (combinedModuleMap [mod]) ] $ \ name -> do
            _ <- maybeAddModName name . Just $ MOFSMod fileName
            pure ()
        let pf = parserF mods contents path
        mapM_ (perModule path) (childrenBi pf :: [ProgramUnit ()])
        pure ()
  execStateT (mapM_ iter paths) modGraph0

modGraphToDOT :: ModGraph -> String
modGraphToDOT ModGraph { mgGraph = gr } = unlines dot
  where
    dot = [ "strict digraph {\n"
          , "node [shape=box,fontname=\"Courier New\"]\n" ] ++
          concatMap (\ (i, name) ->
                        [ "n" ++ show i ++ "[label=\"" ++ name ++ "\"]\n"
                        , "n" ++ show i ++ " -> {" ] ++
                        [ " n" ++ show j | j <- suc gr i ] ++
                        ["}\n"])
                    (labNodes gr) ++
          [ "}\n" ]

takeNextMods :: ModGraph -> [(Node, Maybe ModOrigin)]
takeNextMods ModGraph { mgModNodeMap = mnmap, mgGraph = gr } = noDepFiles
  where
    noDeps = [ (i, modName) | (i, modName) <- labNodes gr, null (suc gr i) ]
    noDepFiles = [ (i, mo) | (i, modName) <- noDeps
                           , (_, mo) <- maybeToList (M.lookup modName mnmap) ]

delModNodes :: [Node] -> ModGraph -> ModGraph
delModNodes ns mg@ModGraph { mgGraph = gr } = mg'
  where
    mg' = mg { mgGraph = delNodes ns gr }

--------------------------------------------------

decodeModFiles :: [FilePath] -> IO [(FilePath, ModFile)]
decodeModFiles = foldM (\ modFiles d -> do
      -- Figure out the camfort mod files and parse them.
      modFileNames <- filter isModFile `fmap` getDirContents d
      addedModFiles <- fmap concat . forM modFileNames $ \ modFileName -> do
        contents <- LB.readFile (d </> modFileName)
        case decodeModFile contents of
          Left msg -> do
            hPutStrLn stderr $ modFileName ++ ": Error: " ++ msg
            return [(modFileName, emptyModFile)]
          Right mods -> do
            hPutStrLn stderr $ modFileName ++ ": successfully parsed precompiled file."
            return $ map (modFileName,) mods
      return $ addedModFiles ++ modFiles
    ) [] -- can't use emptyModFiles

isModFile :: FilePath -> Bool
isModFile = (== modFileSuffix) . takeExtension

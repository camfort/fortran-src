{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|

Format of Camfort precompiled files with information about Fortran
modules. The 'ModuleMap' stores information important to the
renamer. The other data is up to you.

Note that the encoder and decoder work on lists of ModFile so that one
fsmod-file may contain information about multiple Fortran files.

One typical usage might look like:

> let modFile1 = genModFile programFile
> let modFile2 = alterModFileData (const (Just ...)) "mydata" modFile1
> let bytes    = encodeModFile [modFile2]
> ...
> case decodeModFile bytes of
>   Left error -> print error
>   Right modFile3:otherModuleFiles -> ...
>     where
>       moduleMap = combinedModuleMap (modFile3:otherModuleFiles)
>       myData    = lookupModFileData "mydata" modFile3
>       renamedPF = analyseRenamesWithModuleMap moduleMap programFile

-}

module Language.Fortran.Util.ModFile
  ( modFileSuffix, ModFile, ModFiles, emptyModFile, emptyModFiles
  , lookupModFileData, getLabelsModFileData, alterModFileData -- , alterModFileDataF
  , genModFile, regenModFile, encodeModFile, decodeModFile
  , StringMap, DeclMap, ParamVarMap, DeclContext(..), extractModuleMap, extractDeclMap
  , moduleFilename, combinedStringMap, combinedDeclMap, combinedModuleMap, combinedTypeEnv, combinedParamVarMap
  , genUniqNameToFilenameMap
  , TimestampStatus(..), checkTimestamps )
where

import Control.Monad.State
import Data.Binary (Binary, encode, decodeOrFail)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Generics.Uniplate.DataOnly
import qualified Data.Map.Strict as M
import Data.Maybe
import GHC.Generics (Generic)
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Util.Position as P
import System.Directory
import System.FilePath

--------------------------------------------------

-- | Standard ending of fortran-src-format "mod files"
modFileSuffix :: String
modFileSuffix = ".fsmod"

-- | Context of a declaration: the ProgramUnit where it was declared.
data DeclContext = DCMain | DCBlockData | DCModule F.ProgramUnitName
                 | DCFunction (F.ProgramUnitName, F.ProgramUnitName)    -- ^ (uniqName, srcName)
                 | DCSubroutine (F.ProgramUnitName, F.ProgramUnitName)  -- ^ (uniqName, srcName)
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary DeclContext

-- | Map of unique variable name to the unique name of the program
-- unit where it was defined, and the corresponding SrcSpan.
type DeclMap = M.Map F.Name (DeclContext, P.SrcSpan)

-- | A map of aliases => strings, in order to save space and share
-- structure for repeated strings.
type StringMap = M.Map String String

-- | A map of variables => their constant expression if known
type ParamVarMap = FAD.ParameterVarMap

-- | The data stored in the "mod files"
data ModFile = ModFile { mfFilename    :: String
                       , mfStringMap   :: StringMap
                       , mfModuleMap   :: FAR.ModuleMap
                       , mfDeclMap     :: DeclMap
                       , mfTypeEnv     :: FAT.TypeEnv
                       , mfParamVarMap :: ParamVarMap
                       , mfOtherData   :: M.Map String LB.ByteString }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Binary ModFile

-- | A set of decoded mod files.
type ModFiles = [ModFile]

-- | Empty set of mod files. (future proof: may not always be a list)
emptyModFiles :: ModFiles
emptyModFiles = []

-- | Starting point.
emptyModFile :: ModFile
emptyModFile = ModFile "" M.empty M.empty M.empty M.empty M.empty M.empty

-- | Extracts the module map, declaration map and type analysis from
-- an analysed and renamed ProgramFile, then inserts it into the
-- ModFile.
regenModFile :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> ModFile -> ModFile
regenModFile pf mf = mf { mfModuleMap   = extractModuleMap pf
                        , mfDeclMap     = extractDeclMap pf
                        , mfTypeEnv     = FAT.extractTypeEnv pf
                        , mfParamVarMap = extractParamVarMap pf
                        , mfFilename    = F.pfGetFilename pf }

-- | Generate a fresh ModFile from the module map, declaration map and
-- type analysis of a given analysed and renamed ProgramFile.
genModFile :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> ModFile
genModFile = flip regenModFile emptyModFile

-- | Looks up the raw "other data" that may be stored in a ModFile by
-- applications that make use of fortran-src.
lookupModFileData :: String -> ModFile -> Maybe LB.ByteString
lookupModFileData k = M.lookup k . mfOtherData

-- | Get a list of the labels present in the "other data" of a
-- ModFile. More of a meta-programming / debugging feature.
getLabelsModFileData :: ModFile -> [String]
getLabelsModFileData = M.keys . mfOtherData

-- | Allows modification/insertion/deletion of "other data" that may
-- be stored in a ModFile by applications that make use of
-- fortran-src. See 'Data.Map.Strict.alter' for more information about
-- the interface of this function.
alterModFileData :: (Maybe LB.ByteString -> Maybe LB.ByteString) -> String -> ModFile -> ModFile
alterModFileData f k mf = mf { mfOtherData = M.alter f k . mfOtherData $ mf }

-- For when stackage gets containers-0.5.8.1:
-- alterModFileDataF :: Functor f => (Maybe B.ByteString -> f (Maybe B.ByteString)) -> String -> ModFile -> f ModFile
-- alterModFileDataF f k mf = (\ od -> mf { mfOtherData = od }) <$> M.alterF f k (mfOtherData mf)

-- | Convert ModFiles to a strict ByteString for writing to file.
encodeModFile :: [ModFile] -> LB.ByteString
encodeModFile = encode . map each
  where
    each mf = mf' { mfStringMap = sm }
      where
        (mf', sm) = extractStringMap (mf { mfStringMap = M.empty })

-- | Convert a strict ByteString to ModFiles, if possible. Revert the
-- String aliases according to the StringMap.
decodeModFile :: LB.ByteString -> Either String [ModFile]
decodeModFile bs = case decodeOrFail bs of
  Left (_, _, s)    -> Left s
  Right (_, _, mfs) -> Right (map each mfs)
    where
      each mf = (revertStringMap sm mf { mfStringMap = M.empty }) { mfStringMap = sm }
        where sm = mfStringMap mf

-- | Extract the combined module map from a set of ModFiles. Useful
-- for parsing a Fortran file in a large context of other modules.
combinedModuleMap :: ModFiles -> FAR.ModuleMap
combinedModuleMap = M.unions . map mfModuleMap

-- | Extract the combined module map from a set of ModFiles. Useful
-- for parsing a Fortran file in a large context of other modules.
combinedTypeEnv :: ModFiles -> FAT.TypeEnv
combinedTypeEnv = M.unions . map mfTypeEnv

-- | Extract the combined declaration map from a set of
-- ModFiles. Useful for parsing a Fortran file in a large context of
-- other modules.
combinedDeclMap :: ModFiles -> DeclMap
combinedDeclMap = M.unions . map mfDeclMap

-- | Extract the combined string map of ModFiles. Mainly internal use.
combinedStringMap :: ModFiles -> StringMap
combinedStringMap = M.unions . map mfStringMap

-- | Extract the combined string map of ModFiles. Mainly internal use.
combinedParamVarMap :: ModFiles -> ParamVarMap
combinedParamVarMap = M.unions . map mfParamVarMap

-- | Get the associated Fortran filename that was used to compile the
-- ModFile.
moduleFilename :: ModFile -> String
moduleFilename = mfFilename

--------------------------------------------------

-- | Create a map that links all unique variable/function names in the
-- ModFiles to their corresponding filename.
genUniqNameToFilenameMap :: ModFiles -> M.Map F.Name String
genUniqNameToFilenameMap = M.unions . map perMF
  where
    perMF mf = M.fromList [ (n, fname) | modEnv <- M.elems (mfModuleMap mf)
                                       , (n, _) <- M.elems modEnv ]
      where
        fname = mfFilename mf

--------------------------------------------------

-- | Extract all module maps (name -> environment) by collecting all
-- of the stored module maps within the PUModule annotation.
extractModuleMap :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> FAR.ModuleMap
extractModuleMap pf
  -- in case there are no modules, store global program unit names under the name 'NamelessMain'
  | null mmap = M.singleton F.NamelessMain $ M.unions combinedEnv
  | otherwise = M.fromList mmap
  where
    mmap = [ (n, env) | pu@F.PUModule{} <- childrenBi pf :: [F.ProgramUnit (FA.Analysis a)]
                      , let a = F.getAnnotation pu
                      , let n = F.getName pu
                      , env <- maybeToList (FA.moduleEnv a) ]
    combinedEnv = [ env | pu <- childrenBi pf :: [F.ProgramUnit (FA.Analysis a)]
                        , let a = F.getAnnotation pu
                        , env <- maybeToList (FA.moduleEnv a) ]

-- | Extract map of declared variables with their associated program
-- unit and source span.
extractDeclMap :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> DeclMap
extractDeclMap pf = M.fromList . concatMap (blockDecls . nameAndBlocks) $ universeBi pf
  where
    -- Extract variable names, source spans from declarations (and
    -- from function return variable if present)
    blockDecls :: (DeclContext, Maybe (F.Name, P.SrcSpan), [F.Block (FA.Analysis a)]) -> [(F.Name, (DeclContext, P.SrcSpan))]
    blockDecls (dc, mret, bs)
      | Nothing        <- mret = map decls (universeBi bs)
      | Just (ret, ss) <- mret = (ret, (dc, ss)):map decls (universeBi bs)
      where
        decls d = let (v, ss) = declVarName d in (v, (dc, ss))

    -- Extract variable name and source span from declaration
    declVarName :: F.Declarator (FA.Analysis a) -> (F.Name, P.SrcSpan)
    declVarName (F.DeclVariable _ _ e _ _) = (FA.varName e, P.getSpan e)
    declVarName (F.DeclArray _ _ e _ _ _)  = (FA.varName e, P.getSpan e)

    -- Extract context identifier, a function return value (+ source
    -- span) if present, and a list of contained blocks
    nameAndBlocks :: F.ProgramUnit (FA.Analysis a) -> (DeclContext, Maybe (F.Name, P.SrcSpan), [F.Block (FA.Analysis a)])
    nameAndBlocks pu = case pu of
      F.PUMain       _ _ _ b _            -> (DCMain, Nothing, b)
      F.PUModule     _ _ _ b _            -> (DCModule $ FA.puName pu, Nothing, b)
      F.PUSubroutine _ _ _ _ _ b _        -> (DCSubroutine (FA.puName pu, FA.puSrcName pu), Nothing, b)
      F.PUFunction   _ _ _ _ _ _ mret b _
        | Nothing   <- mret
        , F.Named n <- FA.puName pu       -> (DCFunction (FA.puName pu, FA.puSrcName pu), Just (n, P.getSpan pu), b)
        | Just ret <- mret                -> (DCFunction (FA.puName pu, FA.puSrcName pu), Just (FA.varName ret, P.getSpan ret), b)
        | otherwise                       -> error $ "nameAndBlocks: un-named function with no return value! " ++ show (FA.puName pu) ++ " at source-span " ++ show (P.getSpan pu)
      F.PUBlockData  _ _ _ b              -> (DCBlockData, Nothing, b)
      F.PUComment    {}                   -> (DCBlockData, Nothing, []) -- no decls inside of comments, so ignore it

-- | Extract a string map from the given data, leaving behind aliased
-- values in place of strings in the returned version.
extractStringMap :: Data a => a -> (a, StringMap)
extractStringMap x = fmap (inv . fst) . flip runState (M.empty, 0) $ descendBiM f x
  where
    inv = M.fromList . map (\ (a,b) -> (b,a)) . M.toList
    f :: String -> State (StringMap, Int) String
    f s = do
      (m, n) <- get
      case M.lookup s m of
        Just s' -> return s'
        Nothing -> do
          let s' = '@':show n
          put (M.insert s s' m, n + 1)
          return s'

-- | Rewrite the data with the string map aliases replaced by the
-- actual values (implicitly sharing structure).
revertStringMap :: Data a => StringMap -> a -> a
revertStringMap sm = descendBi (\ s -> s `fromMaybe` M.lookup s sm)

-- | Extract a map of variables assigned to constant values.
extractParamVarMap :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> ParamVarMap
extractParamVarMap pf = M.fromList cvm
  where
    pf' = FAD.analyseConstExps $ FAB.analyseBBlocks pf
    cvm = [ (FA.varName v, con)
          | F.PUModule _ _ _ bs _                             <- universeBi pf' :: [F.ProgramUnit (FA.Analysis a)]
          , st@(F.StDeclaration _ _ (F.TypeSpec _ _ _ _) _ _) <- universeBi bs  :: [F.Statement (FA.Analysis a)]
          , F.AttrParameter _ _                               <- universeBi st  :: [F.Attribute (FA.Analysis a)]
          , (F.DeclVariable _ _ v _ _)                        <- universeBi st  :: [F.Declarator (FA.Analysis a)]
          , Just con                                          <- [FA.constExp (F.getAnnotation v)] ] ++
          [ (FA.varName v, con)
          | F.PUModule _ _ _ bs _                             <- universeBi pf' :: [F.ProgramUnit (FA.Analysis a)]
          , st@F.StParameter {}                               <- universeBi bs  :: [F.Statement (FA.Analysis a)]
          , (F.DeclVariable _ _ v _ _)                        <- universeBi st  :: [F.Declarator (FA.Analysis a)]
          , Just con                                          <- [FA.constExp (F.getAnnotation v)] ]

-- | Status of mod-file compared to Fortran file.
data TimestampStatus = NoSuchFile | CompileFile | ModFileExists FilePath

-- | Compare the source file timestamp to the fsmod file timestamp, if
-- it exists.
checkTimestamps :: FilePath -> IO TimestampStatus
checkTimestamps path = do
  pathExists <- doesFileExist path
  modExists <- doesFileExist $ path -<.> modFileSuffix
  case (pathExists, modExists) of
    (False, _)    -> pure NoSuchFile
    (True, False) -> pure CompileFile
    (True, True)  -> do
      let modPath = path -<.> modFileSuffix
      pathModTime <- getModificationTime path
      modModTime  <- getModificationTime modPath
      if pathModTime < modModTime
        then pure $ ModFileExists modPath
        else pure CompileFile

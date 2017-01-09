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

One typical usage might look like:

> let modFile1 = genModFile programFile
> let modFile2 = alterModFileData (const (Just ...)) "mydata" modFile1
> let bytes    = encodeModFile modFile1
> ...
> case decodeModFile bytes of
>   Left error -> print error
>   Right modFile3 -> ...
>     where
>       moduleMap = combinedModuleMap (modFile3:otherModuleFiles)
>       myData    = lookupModFileData "mydata" modFile3
>       renamedPF = analyseRenamesWithModuleMap moduleMap programFile

-}

module Language.Fortran.Util.ModFile
  ( modFileSuffix, ModFile, ModFiles, emptyModFile, emptyModFiles
  , lookupModFileData, getLabelsModFileData, alterModFileData -- , alterModFileDataF
  , genModFile, regenModFile, encodeModFile, decodeModFile
  , DeclMap, DeclContext(..), extractModuleMap, extractDeclMap
  , moduleFilename, combinedDeclMap, combinedModuleMap, combinedTypeEnv
  , genUniqNameToFilenameMap )
where

import qualified Debug.Trace as D

import Data.Data
import Data.List
import Data.Char
import Data.Maybe
import Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict as M
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import qualified Language.Fortran.Util.Position as P
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT

--------------------------------------------------

-- | Standard ending of fortran-src-format "mod files"
modFileSuffix :: String
modFileSuffix = ".fsmod"

-- | Context of a declaration: the ProgramUnit where it was declared.
data DeclContext = DCMain | DCBlockData | DCModule F.ProgramUnitName
                 | DCFunction F.ProgramUnitName | DCSubroutine F.ProgramUnitName
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary DeclContext

-- | Map of unique variable name to the unique name of the program
-- unit where it was defined, and the corresponding SrcSpan.
type DeclMap = M.Map F.Name (DeclContext, P.SrcSpan)

-- | The data stored in the "mod files"
data ModFile = ModFile { mfFilename  :: String
                       , mfModuleMap :: FAR.ModuleMap
                       , mfDeclMap   :: DeclMap
                       , mfTypeEnv   :: FAT.TypeEnv
                       , mfOtherData :: M.Map String B.ByteString }
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary ModFile

-- | A set of decoded mod files.
type ModFiles = [ModFile]

-- | Empty set of mod files. (future proof: may not always be a list)
emptyModFiles :: ModFiles
emptyModFiles = []

-- | Starting point.
emptyModFile :: ModFile
emptyModFile = ModFile "" M.empty M.empty M.empty M.empty

-- | Extracts the module map, declaration map and type analysis from
-- an analysed and renamed ProgramFile, then inserts it into the
-- ModFile.
regenModFile :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> ModFile -> ModFile
regenModFile pf mf = mf
  { mfModuleMap = extractModuleMap pf
  , mfDeclMap   = extractDeclMap pf
  , mfTypeEnv   = FAT.extractTypeEnv pf
  , mfFilename  = F.pfGetFilename pf }

-- | Generate a fresh ModFile from the module map, declaration map and
-- type analysis of a given analysed and renamed ProgramFile.
genModFile :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> ModFile
genModFile = flip regenModFile emptyModFile

-- | Looks up the raw "other data" that may be stored in a ModFile by
-- applications that make use of fortran-src.
lookupModFileData :: String -> ModFile -> Maybe B.ByteString
lookupModFileData k = M.lookup k . mfOtherData

-- | Get a list of the labels present in the "other data" of a
-- ModFile. More of a meta-programming / debugging feature.
getLabelsModFileData :: ModFile -> [String]
getLabelsModFileData = M.keys . mfOtherData

-- | Allows modification/insertion/deletion of "other data" that may
-- be stored in a ModFile by applications that make use of
-- fortran-src. See 'Data.Map.Strict.alter' for more information about
-- the interface of this function.
alterModFileData :: (Maybe B.ByteString -> Maybe B.ByteString) -> String -> ModFile -> ModFile
alterModFileData f k mf = mf { mfOtherData = M.alter f k . mfOtherData $ mf }

-- For when stackage gets containers-0.5.8.1:
-- alterModFileDataF :: Functor f => (Maybe B.ByteString -> f (Maybe B.ByteString)) -> String -> ModFile -> f ModFile
-- alterModFileDataF f k mf = (\ od -> mf { mfOtherData = od }) <$> M.alterF f k (mfOtherData mf)

-- | Convert ModFile to a strict ByteString for writing to file.
encodeModFile :: ModFile -> B.ByteString
encodeModFile = LB.toStrict . encode

-- | Convert a strict ByteString to a ModFile, if possible
decodeModFile :: Binary a => B.ByteString -> Either String a
decodeModFile bs = case decodeOrFail (LB.fromStrict bs) of
  Left (_, _, s) -> Left s
  Right (_, _, mf) -> Right mf

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

extractModuleMap :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> FAR.ModuleMap
extractModuleMap pf = M.fromList [ (n, env) | pu@(F.PUModule {}) <- universeBi pf :: [F.ProgramUnit (FA.Analysis a)]
                                            , let a = F.getAnnotation pu
                                            , let n = F.getName pu
                                            , env <- maybeToList (FA.moduleEnv a) ]
                                        
extractDeclMap :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> DeclMap
extractDeclMap pf = M.fromList . concatMap (blockDecls . nameAndBlocks) $ universeBi pf
  where
    blockDecls :: (DeclContext, [F.Block (FA.Analysis a)]) -> [(F.Name, (DeclContext, P.SrcSpan))]
    blockDecls (dc, bs) = flip map (universeBi bs) $ \ d ->
                            let (v, ss) = declVarName d in (v, (dc, ss))

    declVarName :: F.Declarator (FA.Analysis a) -> (F.Name, P.SrcSpan)
    declVarName (F.DeclVariable _ _ e _ _) = (FA.varName e, P.getSpan e)
    declVarName (F.DeclArray _ _ e _ _ _)  = (FA.varName e, P.getSpan e)

    nameAndBlocks :: F.ProgramUnit (FA.Analysis a) -> (DeclContext, [F.Block (FA.Analysis a)])
    nameAndBlocks pu = case pu of
      F.PUMain       _ _ _ b _         -> (DCMain, b)
      F.PUModule     _ _ _ b _         -> (DCModule $ FA.puName pu, b)
      F.PUSubroutine _ _ _ _ _ b _     -> (DCSubroutine $ FA.puName pu, b)
      F.PUFunction   _ _ _ _ _ _ _ b _ -> (DCFunction $ FA.puName pu, b)
      F.PUBlockData  _ _ _ b           -> (DCBlockData, b)

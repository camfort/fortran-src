{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Transformer
  ( transform
  , transformWithModFiles
  , Transformation(..)
  , defaultTransformations
  ) where

import Data.Map (empty)
import Data.Data

import Language.Fortran.Util.ModFile
import Language.Fortran.Transformation.TransformMonad (Transform, runTransform)
import Language.Fortran.Transformation.Disambiguation.Function
import Language.Fortran.Transformation.Disambiguation.Intrinsic
import Language.Fortran.Transformation.Grouping
import Language.Fortran.AST (ProgramFile)
import Language.Fortran.Version (FortranVersion(..))

data Transformation =
    GroupForall
  | GroupCase
  | GroupDo
  | GroupLabeledDo
  | DisambiguateFunction
  | DisambiguateIntrinsic
  deriving (Eq)

transformationMapping :: Data a => Transformation -> Transform a ()
transformationMapping = \case
  GroupForall           -> groupForall
  GroupCase             -> groupCase
  GroupDo               -> groupDo
  GroupLabeledDo        -> groupLabeledDo
  DisambiguateFunction  -> disambiguateFunction
  DisambiguateIntrinsic -> disambiguateIntrinsic

transformWithModFiles :: Data a => ModFiles -> [ Transformation ] -> ProgramFile a -> ProgramFile a
transformWithModFiles mods trs = runTransform (combinedTypeEnv mods) (combinedModuleMap mods) trans
  where
    trans = mapM_ transformationMapping trs

transform :: Data a => [ Transformation ] -> ProgramFile a -> ProgramFile a
transform trs = runTransform empty empty trans
  where
    trans = mapM_ transformationMapping trs

-- | The default post-parse AST transformations for each Fortran version.
defaultTransformations :: FortranVersion -> [Transformation]
defaultTransformations = \case
  Fortran66 ->
    [ GroupLabeledDo
    , DisambiguateIntrinsic
    , DisambiguateFunction
    ]
  Fortran77         -> defaultTransformations Fortran66
  Fortran77Legacy   -> GroupDo   : defaultTransformations Fortran77
  Fortran77Extended -> GroupCase : defaultTransformations Fortran77Legacy
  Fortran90   -> defaultTransformations Fortran77Extended
  Fortran95   -> defaultTransformations Fortran77Extended
  Fortran2003 -> defaultTransformations Fortran77Extended
  Fortran2008 -> defaultTransformations Fortran2003

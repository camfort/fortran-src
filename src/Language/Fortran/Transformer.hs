module Language.Fortran.Transformer ( transform, transformWithModFiles
                                    , Transformation(..) ) where

import Control.Monad
import Data.Maybe (fromJust)
import Data.Map (Map, empty)
import Data.Data

import Language.Fortran.Util.ModFile
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Transformation.TransformMonad (Transform, runTransform)
import Language.Fortran.Transformation.Disambiguation.Function
import Language.Fortran.Transformation.Disambiguation.Intrinsic
import Language.Fortran.Transformation.Grouping
import Language.Fortran.AST (ProgramFile, ProgramUnitName)

data Transformation =
    GroupForall
  | GroupIf
  | GroupCase
  | GroupDo
  | GroupLabeledDo
  | DisambiguateFunction
  | DisambiguateIntrinsic
  deriving (Eq)

transformationMapping :: Data a => [ (Transformation, Transform a ()) ]
transformationMapping =
  [ (GroupForall, groupForall)
  , (GroupIf, groupIf)
  , (GroupCase, groupCase)
  , (GroupDo, groupDo)
  , (GroupLabeledDo, groupLabeledDo)
  , (DisambiguateFunction, disambiguateFunction)
  , (DisambiguateIntrinsic, disambiguateIntrinsic)
  ]

transformWithModFiles :: Data a => ModFiles -> [ Transformation ] -> ProgramFile a -> ProgramFile a
transformWithModFiles mods trs = runTransform (combinedTypeEnv mods) (combinedModuleMap mods) trans
  where
    trans = mapM_ (\t -> fromJust $ lookup t transformationMapping) trs

transform :: Data a => [ Transformation ] -> ProgramFile a -> ProgramFile a
transform trs = runTransform empty empty trans
  where
    trans = mapM_ (\t -> fromJust $ lookup t transformationMapping) trs

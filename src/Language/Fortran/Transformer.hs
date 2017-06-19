module Language.Fortran.Transformer ( transform, transformWithModFiles
                                    , Transformation(..) ) where

import Data.Maybe (fromJust)
import Data.Map (empty)
import Data.Data

import Language.Fortran.Util.ModFile
import Language.Fortran.Transformation.TransformMonad (Transform, runTransform)
import Language.Fortran.Transformation.Disambiguation.Function
import Language.Fortran.Transformation.Disambiguation.Intrinsic
import Language.Fortran.Transformation.Grouping
import Language.Fortran.AST (ProgramFile)

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

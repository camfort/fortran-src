module Language.Fortran.Transformer ( transform
                                    , Transformation(..) ) where

import Control.Monad
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Data

import Language.Fortran.Analysis.Types (IDType, TypeScope, inferTypes)
import Language.Fortran.Transformation.TransformMonad (Transform, runTransform)
import Language.Fortran.Transformation.Disambiguation.Function
import Language.Fortran.Transformation.Grouping
import Language.Fortran.AST (ProgramFile, ProgramUnitName)

data Transformation =
    GroupIf
  | GroupCase
  | GroupDo
  | GroupLabeledDo
  | DisambiguateFunction
  deriving (Eq)

transformationMapping :: Data a => [ (Transformation, Transform a ()) ]
transformationMapping =
  [ (GroupIf, groupIf)
  , (GroupCase, groupCase)
  , (GroupDo, groupDo)
  , (GroupLabeledDo, groupLabeledDo)
  , (DisambiguateFunction, disambiguateFunction)
  ]

transform :: Data a => [ Transformation ] -> ProgramFile a -> ProgramFile a
transform trs = runTransform trans
  where
    trans = mapM_ (\t -> fromJust $ lookup t transformationMapping) trs

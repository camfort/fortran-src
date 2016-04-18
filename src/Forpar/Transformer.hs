module Forpar.Transformer ( transform
                          , Transformation(..) ) where

import Control.Monad
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Data

import Forpar.Analysis.Types (IDType, TypeScope, inferTypes)
import Forpar.Transformation.TransformMonad (Transform, runTransform)
import Forpar.Transformation.Disambiguation.Function
import Forpar.Transformation.Grouping
import Forpar.AST (ProgramFile, ProgramUnitName)

data Transformation =
    GroupIf
  | GroupDo
  | GroupLabeledDo
  | DisambiguateFunction
  deriving (Eq)

transformationMapping :: Data a => [ (Transformation, Transform a ()) ]
transformationMapping =
  [ (GroupIf, groupIf)
  , (GroupDo, groupDo)
  , (GroupLabeledDo, groupLabeledDo)
  , (DisambiguateFunction, disambiguateFunction)
  ]

transform :: Data a => [ Transformation ] -> ProgramFile a -> ProgramFile a
transform trs = runTransform trans
  where
    trans = mapM_ (\t -> fromJust $ lookup t transformationMapping) trs

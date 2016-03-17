module Forpar.Transformer ( transform
                          , Transformation(..) ) where

import Control.Monad
import Data.Maybe (fromJust)
import Data.Map (Map)

import Forpar.Analysis.Types (IDType, TypeScope, inferTypes)
import Forpar.Transformation.TransformMonad (Transform, runTransform)
import Forpar.Transformation.Disambiguation.Function
import Forpar.Transformation.Disambiguation.Array
import Forpar.Transformation.Grouping
import Forpar.AST (ProgramFile, ProgramUnitName)

data Transformation = 
    GroupIf
  | DisambiguateFunction
  | DisambiguateArray
  deriving (Eq)

transformationMapping :: [ (Transformation, Transform ()) ]
transformationMapping = 
  [ (GroupIf, groupIf)
  , (DisambiguateFunction, disambiguateFunction)
  , (DisambiguateArray, disambiguateArray) ]
  
transform :: [ Transformation ] -> ProgramFile () -> ProgramFile ()
transform trs = runTransform trans
  where
    trans = mapM_ (\t -> fromJust $ lookup t transformationMapping) trs

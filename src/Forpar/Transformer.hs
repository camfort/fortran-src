module Forpar.Transformer (Transformation(..), transform) where

import Data.Maybe (fromJust)

import Forpar.Transformations.Grouping
import Forpar.AST (ProgramUnit)

data Transformation = 
  GroupIf
  deriving (Eq)

transformationMapping :: [ (Transformation, [ ProgramUnit a ] -> [ ProgramUnit a ]) ]
transformationMapping = 
  [ (GroupIf, groupIf) ]
  
transform :: [ Transformation ] -> [ ProgramUnit a ] -> [ ProgramUnit a ]
transform trs pus = 
    foldr (\t acc -> t acc) pus transformations 
  where
    transformations = map (\t -> fromJust $ lookup t transformationMapping) trs

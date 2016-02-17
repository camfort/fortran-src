module Forpar.Transformer (Transformation(..), transform) where

import Data.Maybe (fromJust)

import Forpar.Transformation.Grouping
import Forpar.AST (ProgramFile)

data Transformation = 
  GroupIf
  deriving (Eq)

transformationMapping :: [ (Transformation, ProgramFile a -> ProgramFile a) ]
transformationMapping = 
  [ (GroupIf, groupIf) ]
  
transform :: [ Transformation ] -> ProgramFile a -> ProgramFile a
transform trs pus = 
    foldr (\t acc -> t acc) pus transformations 
  where
    transformations = map (\t -> fromJust $ lookup t transformationMapping) trs

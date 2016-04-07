{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Forpar.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..), lhsExprs, BB, BBGr )
where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Forpar.AST
import Data.Graph.Inductive.PatriciaTree (Gr)

--------------------------------------------------

type BB a = [Block a]
type BBGr a = Gr (BB a) ()

-- Allow graphs to reside inside of annotations
deriving instance (Typeable a, Typeable b) => Typeable (Gr a b)
instance (Typeable a, Typeable b) => Data (Gr a b) where
    gfoldl _k z v = z v -- make graphs opaque to Uniplate
    toConstr _    = error "toConstr"
    gunfold _ _   = error "gunfold"
    dataTypeOf _  = mkNoRepType "Gr"

--------------------------------------------------

data Analysis a = Analysis
  { prevAnnotation :: a -- ^ original annotation
  , uniqueName     :: Maybe String -- ^ unique name for function/variable, after variable renaming phase
  , bBlocks        :: Maybe (BBGr (Analysis a)) -- ^ basic block graph
  , insLabel       :: Maybe Int -- ^ unique number for each block during dataflow analysis
  }
  deriving (Data, Show, Eq)

analysis0 a = Analysis { prevAnnotation = a
                       , uniqueName     = Nothing
                       , bBlocks        = Nothing
                       , insLabel       = Nothing }

-- | Create analysis annotations for the program, saving the original
-- annotations.
initAnalysis :: ProgramFile a -> ProgramFile (Analysis a)
initAnalysis = fmap analysis0

-- | Remove analysis annotations from the program, restoring the
-- original annotations.
stripAnalysis :: ProgramFile (Analysis a) -> ProgramFile a
stripAnalysis = fmap prevAnnotation

--------------------------------------------------

-- | Return list of expressions used as the left-hand-side of
-- assignment statements (including for-loops).
lhsExprs :: (Data a, Data (b a)) => b a -> [Expression a]
lhsExprs x = [e1 | (StExpressionAssign _ _ e1 _) <- universeBi x]
-- FIXME: do parameters to functions/subroutines need to be included (because call-by-ref)?

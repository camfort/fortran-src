{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Forpar.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..), lhsExprs )
where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Forpar.AST

--------------------------------------------------

data Analysis a = Analysis
  { prevAnnotation :: a -- ^ original annotation
  , uniqueName :: Maybe String -- ^ unique name for function/variable, after variable renaming phase
  }
  deriving (Data, Show, Eq)

analysis0 a = Analysis { prevAnnotation = a
                       , uniqueName     = Nothing }

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
lhsExprs :: (Data a, Annotated b, Data (b a)) => b a -> [Expression a]
lhsExprs x = [e1 | (StExpressionAssign _ _ e1 _) <- universeBi x]

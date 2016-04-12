{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Forpar.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..), lhsExprs, isLExpr, BB, BBGr )
where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Forpar.AST
import Data.Graph.Inductive.PatriciaTree (Gr)

--------------------------------------------------

-- | Basic block
type BB a = [Block a]

-- | Basic block graph.
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
-- assignment statements (including for-loops and function-calls by reference).
lhsExprs :: (Data a, Data (b a)) => b a -> [Expression a]
lhsExprs x = [ e1 | StExpressionAssign _ _ e1 _ <- universeBi x ] ++
             concat [ fstLvl aexps | StCall _ _ _ (Just aexps) <- universeBi x ] ++
             concat [ fstLvl aexps | ExpFunctionCall _ _ _ aexps <- universeBi x ]
  where
    fstLvl = filter isLExpr . aStrip

-- | Is this an expression capable of assignment?
isLExpr :: Expression a -> Bool
isLExpr (ExpValue _ _ (ValVariable _ _)) = True
isLExpr (ExpValue _ _ (ValArray _ _))    = True
isLExpr (ExpSubscript _ _ _ _)           = True
isLExpr _                                = False

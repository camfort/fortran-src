{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Forpar.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..)
  , lhsExprs, isLExpr, allVars, allLhsVars, blockVarUses, blockVarDefs
  , BB, BBGr )
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
lhsExprs x = [ e | StExpressionAssign _ _ e _  <- universeBi x                    ] ++
             [ e | StCall _ _ _ (Just aexps)   <- universeBi x, e <- fstLvl aexps ] ++
             [ e | ExpFunctionCall _ _ _ aexps <- universeBi x, e <- fstLvl aexps ]
  where
    fstLvl = filter isLExpr . aStrip

-- | Is this an expression capable of assignment?
isLExpr :: Expression a -> Bool
isLExpr (ExpValue _ _ (ValVariable _ _)) = True
isLExpr (ExpValue _ _ (ValArray _ _))    = True
isLExpr (ExpSubscript _ _ _ _)           = True
isLExpr _                                = False

-- | Set of names found in an AST node.
allVars :: (Data a, Data (b a)) => b a -> [Name]
allVars b = [ v | ExpValue _ _ (ValArray _ v)    <- uniBi b ] ++
            [ v | ExpValue _ _ (ValVariable _ v) <- uniBi b ]
  where
    uniBi :: (Data a, Data (b a)) => b a -> [Expression a]
    uniBi = universeBi

-- | Set of names found in the parts of an AST that are the target of
-- an assignment statement.
allLhsVars :: (Data a, Data (b a)) => b a -> [Name]
allLhsVars b = [ v | ExpValue _ _ (ValArray _ v)                      <- lhsExprs b ] ++
               [ v | ExpValue _ _ (ValVariable _ v)                   <- lhsExprs b ] ++
               [ v | ExpSubscript _ _ (ExpValue _ _ (ValArray _ v)) _ <- lhsExprs b ]

-- | Set of names used -- not defined -- by an AST-block.
blockVarUses :: Data a => Block a -> [Name]
blockVarUses (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ allVars subs
  | otherwise                      = allVars rhs
blockVarUses (BlDo _ _ _ (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2) _)
  | ExpSubscript _ _ _ subs <- lhs = allVars (rhs, e1, e2) ++ allVars subs
  | otherwise                      = allVars (rhs, e1, e2)
blockVarUses (BlStatement _ _ _ (StDeclaration {})) = []
blockVarUses (BlDoWhile _ _ e1 e2 _)   = allVars (e1, e2)
blockVarUses (BlIf _ _ e1 e2 _)        = allVars (e1, e2)
blockVarUses b                         = allVars b

-- | Set of names defined by an AST-block.
blockVarDefs :: Data a => Block a -> [Name]
blockVarDefs (BlStatement _ _ _ st) = allLhsVars st
blockVarDefs (BlDo _ _ _ doSpec _)  = allLhsVars doSpec
blockVarDefs _                      = []

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

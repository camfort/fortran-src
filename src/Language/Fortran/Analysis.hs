{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Language.Fortran.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..), varName, genVar, puName, blockRhsExprs, rhsExprs
  , ModEnv, NameType(..), IDType(..), ConstructType(..), BaseType(..)
  , lhsExprs, isLExpr, allVars, allLhsVars, blockVarUses, blockVarDefs
  , BB, BBGr
  , TransFunc, TransFuncM )
where

import Language.Fortran.Util.Position (SrcSpan)
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Language.Fortran.AST
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map as M

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

-- | The type of "transformBi"-family functions
type TransFunc f g a = (f (Analysis a) -> f (Analysis a)) -> g (Analysis a) -> g (Analysis a)
-- | The type of "transformBiM"-family functions
type TransFuncM m f g a = (f (Analysis a) -> m (f (Analysis a))) -> g (Analysis a) -> m (g (Analysis a))

data NameType = NTSubprogram | NTVariable deriving (Show, Eq, Ord, Data, Typeable)
type ModEnv = M.Map String (String, NameType)

data ConstructType =
    CTFunction
  | CTSubroutine
  | CTVariable
  | CTArray
  | CTParameter
  deriving (Data, Show, Eq)

data IDType = IDType
  { idVType :: Maybe BaseType
  , idCType :: Maybe ConstructType }
  deriving (Data, Show, Eq)

data Analysis a = Analysis
  { prevAnnotation :: a -- ^ original annotation
  , uniqueName     :: Maybe String -- ^ unique name for function/variable, after variable renaming phase
  , bBlocks        :: Maybe (BBGr (Analysis a)) -- ^ basic block graph
  , insLabel       :: Maybe Int -- ^ unique number for each block during dataflow analysis
  , moduleEnv      :: Maybe ModEnv
  , idType         :: Maybe IDType
  }
  deriving (Data, Show, Eq)
analysis0 a = Analysis { prevAnnotation = a
                       , uniqueName     = Nothing
                       , bBlocks        = Nothing
                       , insLabel       = Nothing
                       , moduleEnv      = Nothing
                       , idType         = Nothing }

-- | Obtain either uniqueName or source name from an ExpValue variable.
varName :: Expression (Analysis a) -> String
varName (ExpValue (Analysis { uniqueName = Just n }) _ (ValVariable {})) = n
varName (ExpValue (Analysis { uniqueName = Nothing }) _ (ValVariable n)) = n
varName _ = error "Use of varName on non-variable."

-- | Generate an ExpValue variable with its source name == to its uniqueName.
genVar :: Analysis a -> SrcSpan -> String -> Expression (Analysis a)
genVar a s n = ExpValue (a { uniqueName = Just n }) s (ValVariable n)

-- | Obtain either uniqueName or source program unit name.
puName :: ProgramUnit (Analysis a) -> ProgramUnitName
puName pu
  | Just n <- uniqueName (getAnnotation pu) = Named n
  | otherwise                               = getName pu

-- | Create analysis annotations for the program, saving the original
-- annotations.
initAnalysis :: Functor b => b a -> b (Analysis a)
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
             [ e | ExpFunctionCall _ _ _ (Just aexps) <- universeBi x, e <- fstLvl aexps ]
  where
    fstLvl = filter isLExpr . map extractExp . aStrip
    extractExp (Argument _ _ _ exp) = exp

-- | Return list of expressions that are not "left-hand-side" of
-- assignment statements.
rhsExprs :: (Data a, Data (b a)) => b a -> [Expression a]
rhsExprs x = concat [ blockRhsExprs b | b <- universeBi x ]

-- | Is this an expression capable of assignment?
isLExpr :: Expression a -> Bool
isLExpr (ExpValue _ _ (ValVariable {}))  = True
isLExpr (ExpSubscript _ _ _ _)           = True
isLExpr _                                = False

-- | Set of names found in an AST node.
allVars :: forall a b. (Data a, Data (b (Analysis a))) => b (Analysis a) -> [Name]
allVars b = [ varName v | v@(ExpValue _ _ (ValVariable _)) <- uniBi b ]
  where
    uniBi x = universeBi x :: [Expression (Analysis a)]

-- | Set of names found in the parts of an AST that are the target of
-- an assignment statement.
allLhsVars :: (Data a, Data (b (Analysis a))) => b (Analysis a) -> [Name]
allLhsVars b = [ varName v | v@(ExpValue _ _ (ValVariable {})) <- lhsExprs b ] ++
               [ varName v | ExpSubscript _ _ v@(ExpValue _ _ (ValVariable {})) _ <- lhsExprs b ]

-- | Set of expressions used -- not defined -- by an AST-block.
blockRhsExprs :: Data a => Block a -> [Expression a]
blockRhsExprs (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = universeBi rhs ++ universeBi subs
  | otherwise                      = universeBi rhs
blockRhsExprs (BlDo _ _ _ (Just (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2)) _)
  | ExpSubscript _ _ _ subs <- lhs = universeBi (rhs, e1, e2) ++ universeBi subs
  | otherwise                      = universeBi (rhs, e1, e2)
blockRhsExprs (BlStatement _ _ _ (StDeclaration {})) = []
blockRhsExprs (BlDoWhile _ _ e1 e2 _)   = universeBi (e1, e2)
blockRhsExprs (BlIf _ _ e1 e2 _)        = universeBi (e1, e2)
blockRhsExprs b                         = universeBi b

-- | Set of names used -- not defined -- by an AST-block.
blockVarUses :: Data a => Block (Analysis a) -> [Name]
blockVarUses (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ concatMap allVars (aStrip subs)
  | otherwise                      = allVars rhs
blockVarUses (BlDo _ _ _ (Just (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2)) _)
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ allVars e1 ++ maybe [] allVars e2 ++ concatMap allVars (aStrip subs)
  | otherwise                      = allVars rhs ++ allVars e1 ++ maybe [] allVars e2
blockVarUses (BlStatement _ _ _ (StDeclaration {})) = []
blockVarUses (BlDoWhile _ _ e1 e2 _)   = maybe [] allVars e1 ++ allVars e2
blockVarUses (BlIf _ _ e1 e2 _)        = maybe [] allVars e1 ++ concatMap (maybe [] allVars) e2
blockVarUses b                         = allVars b

-- | Set of names defined by an AST-block.
blockVarDefs :: Data a => Block (Analysis a) -> [Name]
blockVarDefs (BlStatement _ _ _ st) = allLhsVars st
blockVarDefs (BlDo _ _ _ (Just doSpec) _)  = allLhsVars doSpec
blockVarDefs _                      = []

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving, DeriveGeneric #-}

-- |
-- Common data structures and functions supporting analysis of the AST.
module Language.Fortran.Analysis
  ( initAnalysis, stripAnalysis, Analysis(..), varName, srcName, genVar, puName, puSrcName, blockRhsExprs, rhsExprs
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
import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Binary

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

-- Describe a Fortran name as either a program unit or a variable.
data NameType = NTSubprogram | NTVariable deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance Binary NameType
instance Out NameType

-- Module environments are associations between source name and
-- (unique name, name type) in a specific module.
type ModEnv = M.Map String (String, NameType)

-- Fortran data types are broken down into 'construct type' and 'base type'.
data ConstructType =
    CTFunction
  | CTSubroutine
  | CTVariable
  | CTArray
  | CTParameter
  deriving (Data, Show, Eq, Generic)

instance Out ConstructType

data IDType = IDType
  { idVType :: Maybe BaseType
  , idCType :: Maybe ConstructType }
  deriving (Data, Show, Eq, Generic)

instance Out IDType

data Analysis a = Analysis
  { prevAnnotation :: a -- ^ original annotation
  , uniqueName     :: Maybe String -- ^ unique name for function/variable, after variable renaming phase
  , sourceName     :: Maybe String -- ^ original name for function/variable found in source text
  , bBlocks        :: Maybe (BBGr (Analysis a)) -- ^ basic block graph
  , insLabel       :: Maybe Int -- ^ unique number for each block during dataflow analysis
  , moduleEnv      :: Maybe ModEnv
  , idType         :: Maybe IDType
  }
  deriving (Data, Show, Eq, Generic)

instance Out (Analysis a) where
  doc a = parens . text . unwords . map (uncurry (++) . fmap fromJust) . filter (isJust . snd) $
            [ ("uniqueName: ", uniqueName a)
            , ("sourceName: ", sourceName a)
            , ("insLabel: ", fmap show (insLabel a))
            , ("idType: ", fmap show (idType a)) ]
  docPrec _ = doc

analysis0 a = Analysis { prevAnnotation = a
                       , uniqueName     = Nothing
                       , sourceName     = Nothing
                       , bBlocks        = Nothing
                       , insLabel       = Nothing
                       , moduleEnv      = Nothing
                       , idType         = Nothing }

-- | Obtain either uniqueName or source name from an ExpValue variable.
varName :: Expression (Analysis a) -> String
varName (ExpValue (Analysis { uniqueName = Just n }) _ (ValVariable {})) = n
varName (ExpValue (Analysis { sourceName = Just n }) _ (ValVariable {})) = n
varName (ExpValue _ _ (ValVariable n))                                   = n
varName _ = error "Use of varName on non-variable."

-- | Obtain the source name from an ExpValue variable.
srcName :: Expression (Analysis a) -> String
srcName (ExpValue (Analysis { sourceName = Just n }) _ (ValVariable {})) = n
srcName (ExpValue _ _ (ValVariable n))                                   = n
srcName _ = error "Use of srcName on non-variable."

-- | Generate an ExpValue variable with its source name == to its uniqueName.
genVar :: Analysis a -> SrcSpan -> String -> Expression (Analysis a)
genVar a s n = ExpValue (a { uniqueName = Just n, sourceName = Just n }) s (ValVariable n)

-- | Obtain either ProgramUnit uniqueName or whatever is in the AST.
puName :: ProgramUnit (Analysis a) -> ProgramUnitName
puName pu
  | Just n <- uniqueName (getAnnotation pu) = Named n
  | otherwise                               = getName pu

-- | Obtain either ProgramUnit sourceName or whatever is in the AST.
puSrcName :: ProgramUnit (Analysis a) -> ProgramUnitName
puSrcName pu
  | Just n <- sourceName (getAnnotation pu) = Named n
  | otherwise                               = getName pu

-- | Create analysis annotations for the program, saving the original
-- annotations.
initAnalysis :: Functor b => b a -> b (Analysis a)
initAnalysis = fmap analysis0

-- | Remove analysis annotations from the program, restoring the
-- original annotations.
stripAnalysis :: Functor b => b (Analysis a) -> b a
stripAnalysis = fmap prevAnnotation

--------------------------------------------------

-- | Return list of expressions used as the left-hand-side of
-- assignment statements (including for-loops and function-calls by reference).
lhsExprs :: (Data a, Data (b a)) => b a -> [Expression a]
lhsExprs x = concatMap lhsOfStmt (universeBi x) ++ concatMap lhsOfExp (universeBi x)
  where
    lhsOfStmt (StExpressionAssign _ _ e _) = [e]
    lhsOfStmt (StCall _ _ _ (Just aexps)) = fstLvl aexps
    lhsOfStmt _ = []

    lhsOfExp (ExpFunctionCall _ _ _ (Just aexps)) = fstLvl aexps
    lhsOfExp _ = []

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
blockRhsExprs (BlDo _ _ _ _ _ (Just (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2)) _ _)
  | ExpSubscript _ _ _ subs <- lhs = universeBi (rhs, e1, e2) ++ universeBi subs
  | otherwise                      = universeBi (rhs, e1, e2)
blockRhsExprs (BlStatement _ _ _ (StDeclaration {})) = []
blockRhsExprs (BlDoWhile _ _ e1 _ e2 _ _)   = universeBi (e1, e2)
blockRhsExprs (BlIf _ _ e1 _ e2 _ _)        = universeBi (e1, e2)
blockRhsExprs b                         = universeBi b

-- | Set of names used -- not defined -- by an AST-block.
blockVarUses :: Data a => Block (Analysis a) -> [Name]
blockVarUses (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ concatMap allVars (aStrip subs)
  | otherwise                      = allVars rhs
blockVarUses (BlDo _ _ _ _ _ (Just (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2)) _ _)
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ allVars e1 ++ maybe [] allVars e2 ++ concatMap allVars (aStrip subs)
  | otherwise                      = allVars rhs ++ allVars e1 ++ maybe [] allVars e2
blockVarUses (BlStatement _ _ _ (StDeclaration {})) = []
blockVarUses (BlDoWhile _ _ e1 _ e2 _ _)   = maybe [] allVars e1 ++ allVars e2
blockVarUses (BlIf _ _ e1 _ e2 _ _)        = maybe [] allVars e1 ++ concatMap (maybe [] allVars) e2
blockVarUses b                         = allVars b

-- | Set of names defined by an AST-block.
blockVarDefs :: Data a => Block (Analysis a) -> [Name]
blockVarDefs (BlStatement _ _ _ st) = allLhsVars st
blockVarDefs (BlDo _ _ _ _ _ (Just doSpec) _ _)  = allLhsVars doSpec
blockVarDefs _                      = []

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

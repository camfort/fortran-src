-- TODO dislike having to use String
-- TODO unlike fortran-src, we're not using each expression's unique label.
-- would be good for efficiency
--
-- SymValMap has to be built up via Statements (parameter declarations,
-- assignments). Then can be used in expressions. fortran-src currently does
-- these the other way round -- unclear if swapping them will impact anything
-- (appears unlikely?)
--
-- The overall approach here is rewriting the fortran-vars eval story (some
-- constructors in SymbolTable, an Eval module) into a classy interface that
-- provides the eval function, which SymbolTable can implement, and we can
-- improve fortran-src to also do similar work.
--
-- F90 ISO spec is great for this. See pg.38.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Parameters where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Repr

import           Data.Data
import           Data.Generics.Uniplate.Operations
import qualified Data.Map                   as Map
import           Data.Map                   ( Map )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Function              ( on )

type ConstMap = Map Name ScalarVal

-- | Gather labeled constants (PARAMETERS).
--
-- The AST must have gone through a variable renaming pass. (That's the only use
-- of 'Analysis a' here.)
--
-- TODO: F90 ISO pg.193 init expr. Their definition appears a little wider than
-- params, but it's pretty much what we're going for?
gatherConsts :: forall a. Data a => ProgramFile (Analysis a) -> ConstMap
gatherConsts = foldr go Map.empty . universeBi
  where
    go :: Statement (Analysis a) -> ConstMap -> ConstMap
    go (StParameter _ _ decls) m = foldr handleParamDecl m (aStrip decls)
    go (StDeclaration _ _ _ (Just attrs) decls) m =
        case getAttr (AttrParameter undefined undefined) (aStrip attrs) of
          Just{}  -> foldr handleParamDecl m (aStrip decls)
          Nothing -> m
    go _ m = m

-- Only checks the constructor, not the fields inside.
getAttr :: Data a => Attribute a -> [Attribute a] -> Maybe (Attribute a)
getAttr aCmp = go
  where go = \case
               []   -> Nothing
               a:as -> if sameConstructor aCmp a then Just a else go as

sameConstructor :: Data a => a -> a -> Bool
sameConstructor = (==) `on` toConstr

handleParamDecl :: Data a => Declarator (Analysis a) -> ConstMap -> ConstMap
handleParamDecl (Declarator _ _ varExpr mDims _ mInitExpr) m =
    case mDims of
      Just{}  -> error "impossible parse: array declarator in parameter declarator list"
      Nothing ->
        case mInitExpr of
          Nothing -> error "impossible parse: no init expr in parameter declarator"
          Just initExpr ->
            let var = varName varExpr
                val = evalInitExpr initExpr m
             in execState (assignConst var val) m

assignConst :: MonadState ConstMap m => Name -> ScalarVal -> m ()
assignConst var val = modify go
  where go cm = case Map.member var cm of
                  True  -> error "parameter defined multiple times"
                  False -> Map.insert var val cm

-- | F90 ISO R505 pg.38
--
-- TODO: also must be >0 (+ve, non-zero)
evalKindParam :: MonadReader ConstMap m => Expression a -> m Int
evalKindParam = evalScalarIntInitExpr

-- | F90 ISO R505 pg.41
--
-- Does *not* handle @CHARACTER(*)@.
evalCharLengthInt :: MonadReader ConstMap m => Expression a -> m Int
evalCharLengthInt = evalScalarIntInitExpr

-- | Used for various things, including kind parameters.
--
-- TODO: ideal: return Either handling why it failed (e.g. wrong type)
evalScalarIntInitExpr :: MonadReader ConstMap m => Expression a -> m Int
evalScalarIntInitExpr e =
    evalInitExpr e >>= \case
      SVInt i -> return i
      _       -> error "scalar-int-expr (F90 ISO pg.77) was not an integer"

-- | (F90 ISO R504 pg.38) Limited initialization expression evaluation function.
--   Only handles some some expressions, some types and no intrinsics.
--
--   TODO eval from fortran-vars
evalInitExpr :: MonadReader ConstMap m => Expression a -> m ScalarVal
evalInitExpr = \case
  ExpValue _ _ valExpr ->
    case valExpr of
      ValVariable v -> do
        cm <- ask
        case Map.lookup v cm of
          Nothing  -> error $ "variable specified in init expr is not a known parameter: " <> v
          Just val -> return val
      _ -> return $ evalScalarValue valExpr
  _ -> undefined

-- TODO primarily placeholder values
evalScalarValue :: Value a -> ScalarVal
evalScalarValue = \case
  ValInteger i   -> SVInt (read i) -- TODO kind param
  ValReal    r   -> SVReal (read r) -- TODO kind param
  ValString  s   -> SVStr  s
  ValLogical b _ -> SVLogical b
  _              -> error "unsupported value"

{-

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information.
analyseTypes :: Data a => ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypes = analyseTypesWithEnv Map.empty

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information; provided with a
-- starting type environment.
analyseTypesWithEnv :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypesWithEnv env pf = (pf', tenv)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState

-- | Annotate AST nodes with type information, return a type
-- environment mapping names to type information and return any type
-- errors found; provided with a starting type environment.
analyseAndCheckTypesWithEnv
  :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv, [TypeError])
analyseAndCheckTypesWithEnv env pf = (pf', tenv, terrs)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState
    terrs           = typeErrors endState

analyseTypesWithEnv' :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), InferState)
analyseTypesWithEnv' env pf@(ProgramFile mi _) = runInfer (miVersion mi) env $ do
  -- Gather information.
  mapM_ intrinsicsExp (allExpressions pf)
  mapM_ programUnit (allProgramUnits pf)
  mapM_ declarator (allDeclarators pf)
  mapM_ statement (allStatements pf)

  -- Gather types for known entry points.
  eps <- gets (Map.toList . entryPoints)
  forM_ eps $ \ (eName, (fName, mRetName)) -> do
    mFType <- getRecordedType fName
    case mFType of
      Just (IDType fVType fCType) -> do
        recordMType fVType fCType eName
        -- FIXME: what about functions that return arrays?
        maybe (return ()) (error "Entry points with result variables unsupported" >> recordMType fVType Nothing) mRetName
      _                           -> return ()

  annotateTypes pf              -- Annotate AST nodes with their types.

-}

{-

type Symbol = F.Name

-- | Provides a map of symbol name to its constant value (if known).
--
-- @Nothing :: Maybe Reason@ means unknown (yet-registered) var.
class SymValMap map val where
  symValMapGet :: Symbol -> map -> Either (Maybe Reason) val
  symValMapPut :: Symbol -> Either Reason val -> map

eval :: (SymValMap sm, MonadState sm m) => F.Expression a -> m (Either (Maybe Reason) Val)
eval = \case
  F.ExpValue _ _ (F.ValVariable name) ->
    gets (symValMapGet name) >>= \case
      Right v     -> return $ Right v
      Left reason -> error $ show reason
  F.ExpValue _ _ fVal -> error "TODO F.Value -> ???"
  e -> unableReason $ "Unsupported expression at: " ++ show (getSpan e)
   where
    unableReason = return $ Left $ Just $ ReasonUnsupportedExpression

data SymbolTable
--instance SymValMap SymbolTable where
instance SymValMap (Map Symbol Val) where
  symValMapGet v m =
    case Map.lookup v m of
      Nothing -> Left Nothing
      Just x  -> Right d

-}

{-

data EvalErr
  = EvalErrNotAParam
  | EvalErrUnsupportedExpression
  | EvalErrLitConv LitConvErr

-}

data LitConvErr
  = LitConvErr

{-

-- TODO also provide a monadic one that works over a SymValMap?
tryAstValueToScalarVal :: F.Value a -> Maybe ScalarVal
tryAstValueToScalarVal = \case
  F.ValInteger i -> case readMaybe i of
    Nothing -> {- BOZ -} return $ SVBoz i
    Just i' ->           return $ SVInt i'
  F.ValString s -> return $ SVStr s
  F.ValHollerith h -> return $ SVHollerith h
  F.ValLogical _ -> return $ SVLogical False -- TODO
  _ -> return $ SVStr "TODO"

-}

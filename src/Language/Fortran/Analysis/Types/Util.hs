{-# LANGUAGE RankNTypes #-}

module Language.Fortran.Analysis.Types.Util where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Version
import           Language.Fortran.Intrinsics
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Types.Internal

import           Data.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map               as Map

inferState0 :: InferState
inferState0 = InferState
  { environ     = Map.empty
  , structs     = Map.empty
  , entryPoints = Map.empty
  , typeErrors  = []
  , constMap    = Map.empty
  }

inferConfig0 :: FortranVersion -> InferConfig
inferConfig0 v = InferConfig
  { inferConfigAcceptNonCharLengthAsKind = True
  , inferConfigConstantOps               = Map.fromList
    [
    ]
  , inferConfigLangVersion               = v
  , inferConfigIntrinsics                = getVersionIntrinsics v
  }

runInfer :: FortranVersion -> TypeEnv -> Infer a -> (a, InferState)
runInfer v env f = flip runReader (inferConfig0 v) $ flip runStateT (inferState0 { environ = env }) f

typeError :: MonadState InferState m => String -> SrcSpan -> m ()
typeError msg ss = modify $ \ s -> s { typeErrors = (msg, ss):typeErrors s }

recordStruct :: MonadState InferState m => StructMemberTypeEnv -> Name -> m ()
recordStruct mt n = modify $ \s -> s { structs = Map.insert n mt (structs s) }

-- Record the CType of the given name.
recordCType :: MonadState InferState m => ConstructType -> Name -> m ()
recordCType ct n =
    modify $ \s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just $ IDType (mIDType >>= idScalarType) (mIDType >>= idArrayInfo) (Just ct)
    {-
recordType :: MonadState InferState m => Name -> FType -> m ()
recordType n (FType sty maty) =
    modify $ \s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just sty) maty (mIDType >>= idCType))

recordScalarType :: MonadState InferState m => Name -> FTypeScalar -> m ()
recordScalarType n sty =
    modify $ \s -> s { environ = Map.alter changeFunc n (environ s) }
  where
    changeFunc = \case
      Nothing   -> Just $ IDType (Just sty) Nothing Nothing
      Just idty -> Just $ idty { idScalarType = Just sty }

recordArrayInfo :: MonadState InferState m => Name -> ArrayShape -> m ()
recordArrayInfo n aty =
    modify $ \s -> s { environ = Map.alter changeFunc n (environ s) }
  where
    changeFunc = \case
      Nothing   -> Just $ IDType Nothing (Just aty) Nothing
      Just idty -> Just $ idty { idArrayInfo = Just aty }
-}

recordEntryPoint :: MonadState InferState m => Name -> Name -> Maybe Name -> m ()
recordEntryPoint fn en mRetName = modify $ \ s -> s { entryPoints = Map.insert en (fn, mRetName) (entryPoints s) }

getRecordedType :: MonadState InferState m => Name -> m (Maybe IDType)
getRecordedType n = gets (Map.lookup n . environ)

-- ???
getExprRecordedType
    :: (MonadState InferState m, Data a)
    => Expression (Analysis a) -> m (Maybe IDType)
getExprRecordedType e@(ExpValue _ _ (ValVariable _)) = getRecordedType $ varName e

-- get type of subscripted array
getExprRecordedType (ExpSubscript _ _ base _) = do
  mTy <- getExprRecordedType base
  case mTy of
    Just (IDType (Just sty) (Just _aty) _) ->
      pure . Just $ IDType (Just sty) Nothing (Just CTVariable)
    _ -> pure Nothing

{-
getExprRecordedType (ExpDataRef _ _ base ref) = do
  mTy <- getExprRecordedType base
  case mTy of
    Just (IDType (Just (FTypeScalarCustom n)) _ _) -> do
      mStructEnv <- gets (Map.lookup n . structs)
      case mStructEnv of
        Nothing -> pure Nothing
        Just env -> pure $ Map.lookup (varName ref) env
    x -> pure x
getExprRecordedType _ = pure Nothing
-}

-- Set the idType annotation
setIDType :: Annotated f => IDType -> f (Analysis a) -> f (Analysis a)
setIDType ty x =
    let a = getAnnotation x
     in setAnnotation (a { idType = Just ty }) x

-- Get the idType annotation
getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
getIDType x = idType (getAnnotation x)

{-
makeEvalEnv
    :: (MonadState InferState m, MonadReader InferConfig m)
    => m (Eval.Env FVal)
makeEvalEnv = do scalarConsts <- gets constMap
                 let consts = Map.map FValScalar scalarConsts
                 ops <- asks inferConfigConstantOps
                 --let ops' = Map.map _ ops
                 return $ Eval.Env consts ops
-}

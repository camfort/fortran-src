{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Fortran.Analysis.Types.Util where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Version
import           Language.Fortran.Intrinsics
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Types.Internal
import           Language.Fortran.Repr.Type

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
  , inferConfigConstantIntrinsics        = Map.empty
  , inferConfigLangVersion               = v
  , inferConfigIntrinsics                = getVersionIntrinsics v
  }

runInfer :: FortranVersion -> TypeEnv -> Infer a -> (a, InferState)
runInfer v env f = flip runReader (inferConfig0 v) $ flip runStateT (inferState0 { environ = env }) f

typeError :: MonadState InferState m => String -> SrcSpan -> m ()
typeError msg ss = modify $ \ s -> s { typeErrors = (msg, ss):typeErrors s }

emptyType :: IDType
emptyType = IDType Nothing Nothing

-- Record the type of the given name.
recordType
    :: MonadState InferState m
    => FTypeScalar -> ConstructType -> Name -> m ()
recordType st ct n = modify $ \ s -> s { environ = Map.insert n (IDType (Just st) (Just ct)) (environ s) }

recordType'
    :: MonadState InferState m => Name -> IDType -> m ()
recordType' n idt = modify $ \ s -> s { environ = Map.insert n idt (environ s) }

recordStruct :: MonadState InferState m => StructMemberTypeEnv -> Name -> m ()
recordStruct mt n = modify $ \s -> s { structs = Map.insert n mt (structs s) }

-- Record the type (maybe) of the given name.
recordMType
    :: MonadState InferState m
    => Maybe FTypeScalar -> Maybe ConstructType -> Name -> m ()
recordMType st ct n = modify $ \ s -> s { environ = Map.insert n (IDType st ct) (environ s) }

-- Record the CType of the given name.
recordCType :: MonadState InferState m => ConstructType -> Name -> m ()
recordCType ct n = modify $ \ s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (mIDType >>= idVType) (Just ct))

recordScalarType :: MonadState InferState m => Name -> FTypeScalar -> m ()
recordScalarType n ft =
    modify $ \s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just ft) (mIDType >>= idCType))

recordEntryPoint :: MonadState InferState m => Name -> Name -> Maybe Name -> m ()
recordEntryPoint fn en mRetName = modify $ \ s -> s { entryPoints = Map.insert en (fn, mRetName) (entryPoints s) }

getRecordedType :: MonadState InferState m => Name -> m (Maybe IDType)
getRecordedType n = gets (Map.lookup n . environ)

getExprRecordedType
    :: (MonadState InferState m, Data a)
    => Expression (Analysis a) -> m (Maybe IDType)
getExprRecordedType e@(ExpValue _ _ (ValVariable _)) = getRecordedType $ varName e
getExprRecordedType (ExpSubscript _ _ base _) = do
  mTy <- getExprRecordedType base
  case mTy of
    Just (IDType semTy (Just CTArray{})) -> pure . Just $ IDType semTy (Just CTVariable)
    _ -> pure Nothing
getExprRecordedType (ExpDataRef _ _ base ref) = do
  mTy <- getExprRecordedType base
  case mTy of
    Just (IDType (Just (FTypeScalarCustom n)) _) -> do
      mStructEnv <- gets (Map.lookup n . structs)
      case mStructEnv of
        Nothing -> pure Nothing
        Just env -> pure $ Map.lookup (varName ref) env
    x -> pure x
getExprRecordedType _ = pure Nothing

-- Set the idType annotation
setIDType :: Annotated f => IDType -> f (Analysis a) -> f (Analysis a)
setIDType ty x =
    let a = getAnnotation x
     in setAnnotation (a { idType = Just ty }) x

-- Get the idType annotation
getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
getIDType x = idType (getAnnotation x)

-- | For all types holding an 'IDType' (in an 'Analysis'), set the 'SemType'
--   field of the 'IDType'.
setSemType :: (Annotated f, Data a) => FTypeScalar -> f (Analysis a) -> f (Analysis a)
setSemType st x =
    let anno  = getAnnotation x
        idt   = idType anno
        anno' = anno { idType = Just (setIDTypeSemType idt) }
     in setAnnotation anno' x
  where
    setIDTypeSemType :: Maybe IDType -> IDType
    setIDTypeSemType (Just (IDType _ mCt)) = IDType (Just st) mCt
    setIDTypeSemType Nothing               = IDType (Just st) Nothing

-- Set the CType part of idType annotation
--setCType :: (Annotated f, Data a) => ConstructType -> f (Analysis a) -> f (Analysis a)
--setCType ct x
--  | a@(Analysis { idType = Nothing }) <- getAnnotation x = setAnnotation (a { idType = Just (IDType Nothing (Just ct)) }) x
--  | a@(Analysis { idType = Just it }) <- getAnnotation x = setAnnotation (a { idType = Just (it { idCType = Just ct }) }) x

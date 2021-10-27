{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Types
  ( module Language.Fortran.Analysis.Types
  , TypeEnv
  , TypeError
  ) where

import Prelude hiding ( EQ, LT, GT )

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Analysis.Types.Internal
import           Language.Fortran.Analysis.Types.Util
import qualified Language.Fortran.Analysis.Types.Traverse   as Traverse
import           Language.Fortran.Analysis.Constants
import           Language.Fortran.Util.Position

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)

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
  cm <- lift (withReaderT inferConfigConstantIntrinsics $ gatherConsts pf) >>= \case
          Right cm -> return cm
          Left err -> do
            typeError ("bad constants: " <> show err) (getSpan pf)
            return Map.empty
  modify $ \s -> s { constMap = cm }

  mapM_ Traverse.intrinsicsExp $ allExpressions  pf
  mapM_ Traverse.programUnit   $ allProgramUnits pf
  mapM_ Traverse.declarator    $ allDeclarators  pf
  mapM_ Traverse.statement     $ allStatements   pf

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

type TransType f g a = (f (Analysis a) -> Infer (f (Analysis a))) -> g (Analysis a) -> Infer (g (Analysis a))
annotateTypes :: Data a => ProgramFile (Analysis a) -> Infer (ProgramFile (Analysis a))
annotateTypes pf = (transformBiM :: Data a => TransType Expression ProgramFile a) annotateExpression pf >>=
                   (transformBiM :: Data a => TransType ProgramUnit ProgramFile a) annotateProgramUnit

-- TODO here, we should parse values into a Repr.Value type, allowing us to
-- check for initial safety. we can't put them into the parameter map, but we
-- should be able to store their validated/strong repr value in the annotation!
-- TODO shouldn't be a state monad.
annotateExpression
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Expression (Analysis a) -> m (Expression (Analysis a))
annotateExpression = return

annotateProgramUnit :: Data a => ProgramUnit (Analysis a) -> Infer (ProgramUnit (Analysis a))
annotateProgramUnit pu | Named n <- puName pu = maybe pu (`setIDType` pu) `fmap` getRecordedType n
annotateProgramUnit pu                        = return pu

-- | Regenerate the 'TypeEnv' for the given type-annotated 'ProgramFile'.
--
-- The type analysis builds a 'TypeEnv' as it traverses the program. When
-- complete, the program is re-traversed to annotate AST nodes with their type
-- where relevant.
--
-- The 'TypeEnv' is returned along with the annotated program, so you should use
-- that where possible. In cases where you don't have the 'TypeEnv', but you do
-- have the type-annotated AST, you can use this function to regenerate the
-- 'TypeEnv' faster than rebuilding it via re-traversal.
regenerateTypeEnv :: forall a. Data a => ProgramFile (Analysis a) -> TypeEnv
regenerateTypeEnv pf = Map.union puEnv expEnv
  where
    puEnv  = Map.fromList [ (n, ty) | pu <- universeBi pf :: [ProgramUnit (Analysis a)]
                                    , Named n <- [puName pu]
                                    , ty <- maybeToList (idType (getAnnotation pu)) ]
    expEnv = Map.fromList [ (n, ty) | e@(ExpValue _ _ ValVariable{}) <- universeBi pf :: [Expression (Analysis a)]
                                    , let n = varName e
                                    , ty <- maybeToList (idType (getAnnotation e)) ]

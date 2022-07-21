{-# LANGUAGE ScopedTypeVariables #-}

{- TODO
  * return more generated info (ConstMap, from more functions)
-}

module Language.Fortran.Analysis.Types
  ( module Language.Fortran.Analysis.Types
  , TypeEnv
  , TypeError
  ) where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Analysis.Types.Internal
import           Language.Fortran.Analysis.Types.Util
import qualified Language.Fortran.Analysis.Types.Traverse   as Traverse
import qualified Language.Fortran.Analysis.Types.Annotate   as Annotate
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
analyseTypes
    :: Data a => ProgramFile (Analysis a)
    -> (ProgramFile (Analysis a), TypeEnv, ConstMap)
analyseTypes = analyseTypesWithEnv Map.empty

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information; provided with a
-- starting type environment.
analyseTypesWithEnv
    :: Data a => TypeEnv -> ProgramFile (Analysis a)
    -> (ProgramFile (Analysis a), TypeEnv, ConstMap)
analyseTypesWithEnv env pf = (pf', tenv, cm)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState
    cm              = constMap endState

-- | Annotate AST nodes with type information, return a type environment mapping
--   names to type information and return any type errors found; provided with a
--   starting type environment.
analyseAndCheckTypesWithEnv
  :: Data a
  => TypeEnv -> ProgramFile (Analysis a)
  -> (ProgramFile (Analysis a), TypeEnv, ConstMap, [TypeError])
analyseAndCheckTypesWithEnv env pf = (pf', tenv, cm, terrs)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv   = environ    endState
    terrs  = typeErrors endState
    cm     = constMap   endState

analyseTypesWithEnv'
    :: Data a
    => TypeEnv -> ProgramFile (Analysis a)
    -> (ProgramFile (Analysis a), InferState)
analyseTypesWithEnv' env pf@(ProgramFile mi _) = runInfer (miVersion mi) env $ do
  cm <- lift (withReaderT inferConfigConstantOps $ gatherConsts pf) >>= \case
          Right cm -> return cm
          Left err -> do
            typeError ("bad constants: " <> show err) (getSpan pf)
            return Map.empty
  modify $ \s -> s { constMap = cm }

  mapM_ Traverse.intrinsicsExp $ allExpressions  pf
  mapM_ Traverse.programUnit   $ allProgramUnits pf
  mapM_ Traverse.declarator    $ allDeclarators  pf -- TODO needed? rewritten from @recordArrayDecl@
  mapM_ Traverse.statement     $ allStatements   pf

  -- Gather types for known entry points.
  eps <- gets (Map.toList . entryPoints)
  forM_ eps $ \ (eName, (fName, _mRetName)) -> do
    mFType <- getRecordedType fName
    case mFType of
      Just idty -> do
        modify $ \s -> s { environ = Map.insert eName idty (environ s) }
        -- FIXME: what about functions that return arrays?
        -- TODO
        --maybe (return ()) (error "Entry points with result variables unsupported" >> recordMType fVType Nothing) mRetName
      _                           -> return ()

  -- Annotate AST nodes with their types.
  tenv <- gets environ
  return $ Annotate.annotateProgramFile tenv pf

--------------------------------------------------------------------------------

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

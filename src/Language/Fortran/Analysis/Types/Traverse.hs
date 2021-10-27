{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Analysis.Types.Traverse where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Analysis.Types.Util
import           Language.Fortran.Analysis.Types.Internal
import qualified Language.Fortran.Analysis.Types.Derive     as Derive
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Value
import qualified Language.Fortran.Repr.Eval.Scalar          as Eval
import qualified Language.Fortran.Repr.Coerce               as Coerce
import           Language.Fortran.Intrinsics

import           Data.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map                                   as Map
import qualified Data.List                                  as List

programUnit
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => ProgramUnit (Analysis a)
    -> m ()
programUnit pu@(PUFunction _ _ mRetType _ _ _ mRetVar blocks _)
  | Named n <- puName pu   = do
    -- record some type information that we can glean
    recordCType CTFunction n
    case (mRetType, mRetVar) of
      (Just ts@(TypeSpec _ _ _ _), Just v) -> do
        tryDeriveTypeVia Derive.fromTypeSpec ts $ \ft -> do
          recordScalarType n           ft
          recordScalarType (varName v) ft
      (Just ts@(TypeSpec _ _ _ _), _)      -> do
        tryDeriveTypeVia Derive.fromTypeSpec ts $ \ft -> do
          recordScalarType n ft
      _                                        -> return ()
    -- record entry points for later annotation
    forM_ blocks $ \ block ->
      sequence_ [ recordEntryPoint n (varName v) (fmap varName mRetVar') | (StEntry _ _ v _ mRetVar') <- allStatements block ]
programUnit pu@(PUSubroutine _ _ _ _ _ blocks _) | Named n <- puName pu = do
  -- record the fact that this is a subroutine
  recordCType CTSubroutine n
  -- record entry points for later annotation
  forM_ blocks $ \ block ->
    sequence_ [ recordEntryPoint n (varName v) Nothing | (StEntry _ _ v _ _) <- allStatements block ]
programUnit _                                           = return ()

declarator :: (MonadState InferState m, Data a) => Declarator (Analysis a) -> m ()
declarator (Declarator _ _ v (Just ddAList) _ _) =
    recordCType (CTArray $ dimDeclarator ddAList) (varName v)
declarator _ = return ()

-- TODO const eval them, don't limit to ints!!
dimDeclarator :: AList DimensionDeclarator a -> [(Maybe Int, Maybe Int)]
dimDeclarator ddAList = [ (lb, ub) | DimensionDeclarator _ _ lbExp ubExp <- aStrip ddAList
                                   , let lb = do ExpValue _ _ (ValInteger i _) <- lbExp
                                                 return $ read i
                                   , let ub = do ExpValue _ _ (ValInteger i _) <- ubExp
                                                 return $ read i ]

intrinsicsExp
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Expression (Analysis a) -> m ()
intrinsicsExp = \case
  ExpSubscript _ _ nexp _    -> go nexp
  ExpFunctionCall _ _ nexp _ -> go nexp
  _ -> return ()
  where
    go nexp | isNamedExpression nexp = do
      itab <- asks inferConfigIntrinsics
      case getIntrinsicReturnType (srcName nexp) itab of
        Just _ -> do
          let n = varName nexp
          recordCType CTIntrinsic n
          -- recordBaseType _  n -- FIXME: going to skip base types for the moment
        _             -> return ()
    go _ = return ()

statement
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Statement (Analysis a) -> m ()
statement (StDeclaration _ _ ts mAttrAList declAList) = do
  mapM_ (handleDeclarator ts (aStrip' mAttrAList)) (aStrip declAList)
statement (StExternal _ _ varAList) = do
  let vars = aStrip varAList
  mapM_ (recordCType CTExternal . varName) vars
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  --  | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    mIDType <- getExprRecordedType v
    case mIDType of
      Just (IDType _ (Just CTArray{})) -> return ()                -- do nothing, it's already known to be an array
      _                                -> recordCType CTFunction (varName v) -- assume it's a function statement

-- FIXME: if StFunctions can only be identified after types analysis
-- is complete and disambiguation is performed, then how do we get
-- them in the first place? (iterate until fixed point?)
statement (StFunction _ _ v _ _) = recordCType CTFunction (varName v)
-- (part of answer to above is) nullary function statement: foo() = ...
statement (StExpressionAssign _ _ (ExpFunctionCall _ _ v Nothing) _) = recordCType CTFunction (varName v)

statement (StDimension _ _ declAList) =
  forM_ (aStrip declAList) $ \(Declarator _ _ v declType _ _) ->
      case declType of
        ScalarDecl     -> return ()
        ArrayDecl dims -> recordCType (CTArray $ dimDeclarator dims) (varName v)

-- TODO
--statement (StStructure _ _ mName itemAList) = handleStructure mName itemAList

statement _ = return ()

handleDeclarator
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => TypeSpec (Analysis a)
    -> [Attribute (Analysis a)]
    -> Declarator (Analysis a)
    -> m ()
handleDeclarator _ _ (Declarator _ _ _ ArrayDecl{} _ _) =
    error "handle array declarator unimplemented"
handleDeclarator ts attrs decl@(Declarator _ _ v ScalarDecl _ _) =
    handleCType >>= \case
      Just CTParameter -> handleScalarConstDecl ts decl
      _ -> return ()
  where
    wrapCType cty = recordCType cty (varName v) >> return (Just cty)
    handleCType
      | any isAttrExternal attrs = wrapCType CTExternal
      | Just (AttrDimension _ _ _dims) <- List.find isAttrDimension attrs =
          error "handle dims in declarator"
      | any isAttrParameter attrs = wrapCType CTParameter
      | otherwise = do
          getRecordedType (varName v) >>= \case
            Just (IDType _ (Just cty)) ->
              if cty /= CTIntrinsic then wrapCType cty else wrapCType CTVariable
            _ -> wrapCType CTVariable

-- | Try to derive the scalar type information for a variable and record if
--   successful.
tryRecordScalarType
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Name -> Maybe (Expression (Analysis a)) -> TypeSpec (Analysis a)
    -> m ()
tryRecordScalarType v mDeclExpr ts = do
    tryDeriveTypeVia (Derive.fromDeclaration mDeclExpr) ts $ recordScalarType v

-- | Try to record the type and initial value of a scalar constant (PARAMETER)
--   declaration.
--
-- TODO use len if character
handleScalarConstDecl
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => TypeSpec (Analysis a)
    -> Declarator (Analysis a)
    -> m ()
handleScalarConstDecl _ (Declarator _ _ _ _ _ Nothing) =
    -- Should be an impossible parse, so the function has been used incorrectly.
    error "PARAMETER declarator missing initialization expression"
handleScalarConstDecl _ (Declarator _ _ _ ArrayDecl{}  _ _) =
    error "scalar const handler received array const"
handleScalarConstDecl ts (Declarator _ ss v ScalarDecl mLenExpr (Just initExpr)) = do
    Derive.fromDeclaration mLenExpr ts >>= \case
      Left  err -> do
        typeError ("error while deriving a type: " <> show err) ss
      Right fty -> do
        recordScalarType (varName v) fty
        evalExpr initExpr >>= \case
          Left  err  ->
            typeError ("failed to evaluate initialization expression: " <> show err) (getSpan initExpr)
          Right fval -> do
            case Coerce.coerceScalar fval fty of
              Left  err   -> do
                typeError ("error while coercing PARAMETER initial value to its type: " <> show err) (getSpan initExpr)
              Right fval' -> assignConst (varName v) fval' ss

{-
-- | Create a structure env from the list of fields and add it to the InferState
handleStructure
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Maybe String -> AList StructureItem (Analysis a) -> m ()
handleStructure mName itemAList = do
  case mName of
    Just n -> do
      structEnv <- foldM handleStructureItem Map.empty (aStrip itemAList)
      recordStruct structEnv n
    Nothing -> pure ()

handleStructureItem
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => StructMemberTypeEnv -> StructureItem (Analysis a) -> m StructMemberTypeEnv
handleStructureItem mt (StructFields _ _ ts mAttrAList declAList) = do
  ds <- handleDeclaration ts (aStrip' mAttrAList) (aStrip declAList)
  pure $ List.foldl' (\m (n, idt) -> Map.insert n idt m) mt ds
-- TODO: These should eventually be implemented
handleStructureItem mt StructUnion{} = pure mt
handleStructureItem mt StructStructure{} = pure mt
-}

--------------------------------------------------------------------------------

-- Wrapper around Derive functions to use the 'Spanned' instance from the
-- 'TypeSpec' you pass. (Generalized to all 'Spanned' because why not.)
tryDeriveTypeVia
    :: (MonadState InferState m, Spanned a)
    => (a -> m (Either Derive.Error FTypeScalar))
    -> a
    -> (FTypeScalar -> m ())
    -> m ()
tryDeriveTypeVia fDerive a useType =
    fDerive a >>= \case
      Left err -> typeError ("error while deriving a type: " <> show err) (getSpan a)
      Right ft -> useType ft

data Error = ErrorEval Eval.Error deriving (Eq, Show)

-- Doesn't touch state.
evalExpr
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Expression a -> m (Either Error FValScalar)
evalExpr e = do
    evalEnv <- makeEvalEnv
    case Eval.eval evalEnv e of
      Left  err -> return $ Left $ ErrorEval err
      Right val -> return $ Right val
  where makeEvalEnv = do cm <- gets constMap
                         im <- asks inferConfigConstantIntrinsics
                         return $ Eval.Env cm im

assignConst :: MonadState InferState m => Name -> FValScalar -> SrcSpan -> m ()
assignConst var val ss = do
    cm <- gets constMap
    case Map.member var cm of
      True  -> typeError ("PARAMETER redefined (using new value)") ss
      False -> return ()
    let cm' = Map.insert var val cm
    modify $ \s -> s { constMap = cm' }

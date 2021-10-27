-- | Most, but not all deriving functions can report type errors. So most of
-- these functions are in the 'Infer' monad.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Types.Derive where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Analysis.Types.Internal
import           Language.Fortran.Analysis.Types.Util
import           Language.Fortran.Repr.Type
import qualified Language.Fortran.Repr.Eval.Scalar          as Eval

import           Control.Monad.State.Strict
import           Control.Monad.Reader

data Error
  = ErrorInvalidSyntax String
  | ErrorEval Eval.Error
  | ErrorTypeKindNotSupported String Integer -- TODO ?
  | ErrorUnimplemented String
    deriving (Eq, Show)

-- | Attempt to derive a variable's 'ScalarTy' from the relevant parts of its
--   surrounding 'StDeclaration'.
--
-- A declaration looks as follows:
--
--     INTEGER(8) :: var_name
--
-- In the AST, this is split into a LHS 'TypeSpec', and a RHS list of
-- 'Declarator's. Note that @CHARACTER@ variables are allowed to specify their
-- length via special syntax on the RHS:
--
--     CHARACTER :: string*10
--
-- so to handle that, this function takes that length as a Maybe Expression (as
-- provided in 'StDeclaration'). (Note that @CHARACTER :: string(10)@ is array
-- dimension declarator syntax, not the same. This function does not handle
-- array type information.)
--
-- If a length was defined on both sides, the declaration length (RHS) is used.
-- This matches gfortran's behaviour, though even with -Wall they don't warn on
-- this rather confusing syntax usage. We report a (soft) type error.
--
-- The same syntax is parsed regardless of type, and an RHS length can be used
-- as a kind (and override any LHS kind) if configured. This matches gfortran's
-- behaviour.
fromDeclaration
    :: forall a m
    . (MonadState InferState m, MonadReader InferConfig m)
    => Maybe (Expression a) -> TypeSpec a -> m (Either Error FTypeScalar)
fromDeclaration mLenExpr (TypeSpec _ _ bt mSel) =
    case bt of TypeCharacter -> go chooseLenExpr
               _             -> go chooseKindParamExpr
  where go f = do expr <- f mSel mLenExpr
                  fromBaseTypeMaybeKindParam bt expr

-- | Named 'fromDeclaration' shortcut.
fromTypeSpec
    :: forall a m
    . (MonadState InferState m, MonadReader InferConfig m)
    => TypeSpec a -> m (Either Error FTypeScalar)
fromTypeSpec = fromDeclaration Nothing

fromBaseTypeMaybeKindParam
    :: Monad m
    => BaseType -> Maybe (Expression a) -> m (Either Error FTypeScalar)
fromBaseTypeMaybeKindParam bt mkp =
    case bt of
      TypeCustom s ->
        case mkp of
          Nothing -> return $ Right $ FTypeScalarCustom s
          Just _  -> return $ Left $ ErrorInvalidSyntax "TypeCustom had a kind parameter"
      TypeInteger ->
        case mkp of
          Nothing -> return $ Right $ FTypeScalarInt FTypeInt4 -- TODO default kind selection
          Just _  -> return $ Left $ ErrorUnimplemented "int with kind param"
      -- etc...
      _ -> return $ Left $ ErrorUnimplemented $ "unhandled basetype: " <> show bt

-- CHARACTER-specific (checks length field). Needs state for 'typeError'.
chooseLenExpr
    :: MonadState InferState m
    => Maybe (Selector a) -> Maybe (Expression a) -> m (Maybe (Expression a))
chooseLenExpr mSel mLenExpr =
    case mLenExpr of
      Nothing -> return mSelLenExpr
      Just lenExpr ->
        case mSelLenExpr of
          Nothing -> return $ Just lenExpr
          Just _ -> do
            flip typeError (getSpan lenExpr) $
                "note: CHARACTER variable given both LHS length and RHS length"
             <> ": specific RHS declarator overrides"
            return $ Just lenExpr
  where mSelLenExpr = case mSel of Nothing -> Nothing; Just (Selector _ _ x _) -> x

-- All except CHARACTER (doesn't check length field).
chooseKindParamExpr
    :: forall a m
     . (MonadReader InferConfig m, MonadState InferState m)
    => Maybe (Selector a) -> Maybe (Expression a) -> m (Maybe (Expression a))
chooseKindParamExpr mSel mRHSkp =
    checkRHSkp >>= \case
      Nothing -> return mSelKp
      Just rhsKp ->
        case mSelKp of
          Nothing -> return $ Just rhsKp
          Just _ -> do
            flip typeError (getSpan rhsKp) $
                "note: non-CHARACTER variable given both"
             <> " LHS kind param and RHS nonstandard kind param"
             <> ": specific RHS declarator overrides"
            return $ Just rhsKp
  where
    checkRHSkp =
        case mRHSkp of
          Nothing -> return Nothing
          Just rhsKp ->
            -- nonstandard kind param syntax @INTEGER x*2@
            asks inferConfigAcceptNonCharLengthAsKind >>= \case
              False -> do
                flip typeError (getSpan rhsKp) $
                    "warning: non-CHARACTER variable given a length: ignoring"
                return Nothing
              True -> do
                flip typeError (getSpan rhsKp) $
                    "warning: non-CHARACTER variable given a length"
                 <> ": treating as nonstandard kind parameter syntax"
                return $ Just rhsKp
    mSelKp = case mSel of Nothing -> Nothing; Just (Selector _ _ _ x) -> x

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fortran type and value representations.
--
-- TODO: Naming scheme would be nicer as Ty, STy, ATy; Val, SVal, AVal. (BaseTy
-- could be SBTy or something. Not just BTy.)

module Language.Fortran.Repr
  (
  -- * Types
    Ty(..)
  , ScalarTy(..)
  , IntrinsicTy(..)
  , BaseTy(..)
  , ArrayTy(..)
  , Dimensions
  , recoverTyTypeSpec

  -- * Values
  , Val(..)
  , ScalarVal(..)

  ) where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Version

import           Data.Void
import           Data.Data                      ( Data, Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

-- | A fully-evaluated Fortran type.
data Ty
  = TyScalarTy ScalarTy
  | TyArrayTy  ArrayTy
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Out    Ty
instance Binary Ty

data ScalarTy
  = ScalarTyIntrinsic IntrinsicTy
  | ScalarTyCustom String           -- use for F77 structures, F90 DDTs
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Out    ScalarTy
instance Binary ScalarTy

data IntrinsicTy = IntrinsicTy
  { iTyBase :: BaseTy
  , iTyKind :: Int
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Out    IntrinsicTy
instance Binary IntrinsicTy

-- | TODO the nonstandard BYTE type is often is encoded @LOGICAL(1)@, though it
--   may be more sensible as @INTEGER(1)@? and it's perma-signed anyway
data BaseTy
  = BTyInteger
  | BTyReal
  | BTyComplex              -- TODO: encode via ArrayTy?? (maybe too different?)
  | BTyLogical
  | BTyCharacter Int
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Out    BaseTy
instance Binary BaseTy

data ArrayTy = ArrayTy
  { aTyScalar :: ScalarTy
  , aTyDims   :: Dimensions
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Out    ArrayTy
instance Binary ArrayTy

-- | The declared dimensions of a staticically typed array variable
-- type is of the form [(dim1_lower, dim1_upper), (dim2_lower, dim2_upper)]
--
-- TODO but now with maybes too, so defaults can be delayed
type Dimensions = [(Maybe Int, Maybe Int)]

-- | A fully-evaluated Fortran value.
data Val
  = ValScalar ScalarVal
  | ValArray Void -- TODO ExpInitialisation (note that when using F90 attribs,
                  -- it will be in a DeclVariable)

-- The fully evaluated value of a scalar Fortran expression.
data ScalarVal
  = SVInt       Int
  | SVReal      Double
  | SVComplex   (Double, Double)
  | SVStr       String
  | SVLogical   Bool
  | SVBoz       String
  | SVHollerith String
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Recover the most appropriate 'TypeSpec' for the given 'Ty', depending on
--   the given 'FortranVersion'.
--
-- Kinds weren't formalized as a syntactic feature until Fortran 90, so we ask
-- for a context. If possible (>=F90), we prefer the more explicit
-- representation e.g. @REAL(8)@. For older versions, for specific type-kind
-- combinations, @DOUBLE PRECISION@ and @DOUBLE COMPLEX@ are used instead.
-- However, we otherwise don't shy away from adding kind info regardless of
-- theoretical version support.
--
-- This is a "best effort": 'TypeSpec' is a highly syntactic representation and
-- doesn't store array type information. Perhaps better would be to recover a
-- full 'StDeclaration'.
recoverTyTypeSpec
    :: forall a. a -> SrcSpan -> FortranVersion -> Ty -> TypeSpec a
recoverTyTypeSpec a ss v = \case
  TyArrayTy  aTy -> -- nowhere to put array info, so only recover scalar info
    recoverScalarTyTypeSpec (aTyScalar aTy)
  TyScalarTy sTy -> recoverScalarTyTypeSpec sTy
  where
    ts  = TypeSpec a ss
    sel = Selector a ss
    intValExpr :: Int -> Expression a
    intValExpr x = ExpValue a ss (ValInteger (show x))
    recoverScalarTyTypeSpec = \case
      ScalarTyIntrinsic iTy -> recoverIntrinsicTyTypeSpec iTy
      ScalarTyCustom ty' -> ts (TypeCustom ty') Nothing
    recoverIntrinsicTyTypeSpec (IntrinsicTy bTy k) =
        case bTy of
          BTyInteger -> buildTs (Just (k, 4)) TypeInteger Nothing
          BTyLogical -> buildTs (Just (k, 4)) TypeLogical Nothing
          BTyReal    ->
            if   v < Fortran90 && k == 8
            then buildTs Nothing TypeDoublePrecision Nothing
            else buildTs (Just (k, 4)) TypeReal Nothing
          BTyComplex ->
            if   v < Fortran90 && k == 16
            then buildTs Nothing TypeDoubleComplex Nothing
            else buildTs (Just (k, 8)) TypeComplex Nothing
          BTyCharacter len ->
            if   len == 1
            then buildTs (Just (k, 1)) TypeCharacter Nothing
            else buildTs (Just (k, 1)) TypeCharacter (Just len)
    buildTs (Just (k, kDef)) ty mLen =
        if   k == kDef
        then buildTs Nothing ty mLen
        else ts ty (Just (sel (intValExpr <$> mLen) (Just (intValExpr k))))
    buildTs Nothing ty (Just len) = ts ty (Just (sel (Just (intValExpr len)) Nothing))
    buildTs Nothing ty Nothing = ts ty Nothing

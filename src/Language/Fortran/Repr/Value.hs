{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Language.Fortran.Repr.Value where

import           Language.Fortran.Repr.Type

import           Data.Data                      ( Data, Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

data FValScalar
  = FValScalarInt FValInt
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- TODO hide constructor, make smart constructor
-- | Fortran INTEGER value.
data FValInt = FValInt FTypeInt Integer
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | A bounds-checked operation on 'ty'. True is valid, False is invalid.
type CheckedOp ty = ty -> ty -> (ty, Bool)

fValIntIsWellBounded :: FValInt -> Bool
fValIntIsWellBounded (FValInt t v) = not $ v > fTypeIntMax t || v < fTypeIntMin t

-- | Apply a binary operator to two 'FValInt's and check that no bounds issues may
--   have been present.
--
-- I think we always do the same checks? For integers, we have to check both
-- bounds every time. So pulled this out.
fValIntSafeBinOp :: (Integer -> Integer -> Integer) -> CheckedOp FValInt
fValIntSafeBinOp op (FValInt t1 v1) (FValInt t2 v2) = (FValInt t v, isInBound)
  where
    v = v1 `op` v2
    t = max t1 t2
    isInBound = fValIntIsWellBounded $ FValInt t v

fValIntSafeAdd :: CheckedOp FValInt
fValIntSafeAdd = fValIntSafeBinOp (+)

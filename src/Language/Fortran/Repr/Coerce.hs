-- | Very rough.

module Language.Fortran.Repr.Coerce where

import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.Repr.Value.Scalar

coerceScalar :: FValScalar -> FTypeScalar -> Either String FValScalar
coerceScalar (FValScalarInt fvint) (FTypeScalarInt ftint) =
    FValScalarInt <$> coerceInt fvint ftint
scalar _ _ = Left "unimplemented scalar coerce"

coerceInt :: FValInt -> FTypeInt -> Either String FValInt
coerceInt (FValInt _vt vv) t =
    if   fValIntIsWellBounded coerced
    then Right coerced
    else Left "tried to coerce int to a too-small type"
  where coerced = FValInt t vv

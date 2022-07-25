module Language.Fortran.Repr.Value.Scalar.Logical.Idealized where

import Language.Fortran.Repr.Type.Scalar.Int

newtype FLogical (k :: FTInt) = FLogical Bool
    deriving stock (Show, Eq, Ord)

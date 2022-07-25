module Language.Fortran.Repr.Type where

import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Array
import GHC.Generics ( Generic )

-- | A Fortran type (scalar or array).
data FType = MkFScalarType FScalarType | MkFArrayType FArrayType
    deriving stock (Generic, Eq, Show)

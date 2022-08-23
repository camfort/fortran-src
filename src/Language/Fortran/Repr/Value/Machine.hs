module Language.Fortran.Repr.Value.Machine where

import Language.Fortran.Repr.Value.Scalar.Machine
import Language.Fortran.Repr.Value.Array.Machine
import Language.Fortran.Repr.Type

-- | A Fortran value (scalar or array).
data FValue = MkFArrayValue FArrayValue | MkFScalarValue FScalarValue
    deriving stock Show

fValueType :: FValue -> FType
fValueType = \case
  MkFScalarValue a -> MkFScalarType $ fScalarValueType a
  MkFArrayValue  a -> MkFArrayType  $ fArrayValueType  a

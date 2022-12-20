module Language.Fortran.Repr.Value.Machine where

import Language.Fortran.Repr.Value.Scalar.Machine
import Language.Fortran.Repr.Value.Array.Machine
import Language.Fortran.Repr.Type

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint (text)
import Data.Data
import Data.Binary


-- | A Fortran value (scalar or array).
data FValue = MkFArrayValue FArrayValue | MkFScalarValue FScalarValue
    deriving stock (Show, Eq, Ord, Generic)

instance Binary FValue

instance Data FValue where
  --TODO: do we need this?

-- TODO: The following is not a very pretty output.
instance Out FValue where
  doc = text . show
  docPrec _ x = text . show $ x


fValueType :: FValue -> FType
fValueType = \case
  MkFScalarValue a -> MkFScalarType $ fScalarValueType a
  MkFArrayValue  a -> MkFArrayType  $ fArrayValueType  a

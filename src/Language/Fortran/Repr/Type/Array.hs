module Language.Fortran.Repr.Type.Array where

import Language.Fortran.Repr.Type.Scalar
import Numeric.Natural

import GHC.Generics ( Generic )
import Data.Data ( Data )

-- | A Fortran array type.
--
-- An array type is defined by a scalar type together with a shape.
data FArrayType = FArrayType
  { fatScalar :: FScalarType
  , fatShape  :: Shape
  } deriving stock (Generic, Data, Show, Eq, Ord)

newtype Shape = Shape { getShape :: [Natural] }
    deriving stock (Generic, Data, Show, Eq, Ord)

fatSize :: FArrayType -> Natural
fatSize = sum . getShape . fatShape

module Language.Fortran.Repr.Type.Array where

import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Compat.Natural

import GHC.Generics ( Generic )
import Data.Data ( Data )

-- | A Fortran array type.
--
-- An array type is defined by a scalar type together with a shape.
data FArrayType = FArrayType
  { fatScalar :: FScalarType
  , fatShape  :: Shape
  } deriving stock (Generic, Data, Show, Eq, Ord)

-- | The shape of a Fortran array is a list of extents. (The rank of the array
--   is length of the list.)
--
-- Note that the F90 standard limits maximum array rank to 7 (R512).
--
-- TODO
--   * An empty list here feels nonsensical. Perhaps this should be NonEmpty.
--   * List type is inefficient here, since we don't care about pushing/popping,
--     and list length is important. Use a vector type instead.
newtype Shape = Shape { getShape :: [Natural] }
    deriving stock (Generic, Data, Show, Eq, Ord)

fatSize :: FArrayType -> Natural
fatSize = sum . getShape . fatShape

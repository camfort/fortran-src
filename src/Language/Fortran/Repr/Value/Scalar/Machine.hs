module Language.Fortran.Repr.Value.Scalar.Machine
  (
  -- * Note on type coercion implementation
  -- $type-coercion-implementation

    FScalarValue(..)
  , fScalarValueType
  ) where

import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Complex
import Language.Fortran.Repr.Value.Scalar.String
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Type.Scalar.Real
import GHC.Generics ( Generic )

{- $type-coercion-implementation

When you run a binary operation on two Fortran values, type coercion may take
place depending on the types of the values. This complicates evaluation code,
because now we have to export two sets of functions for operating on values: one
for returning a kinded value (e.g. addition returns the same type), and one for
non-kinded values (e.g. equality returns a boolean).

On the lowest level, e.g. for operating over @INTEGER(x)@ and @INTEGER(y)@, we
resolve this by doing the coercion in an internal function which is polymorphic
over the result type, and using that in both sets of functions. To operate
kinded, we use the relevant type. To operate unkinded, we use
@'Data.Functor.Const' r@, which ignores the kind and just stores a value of type
'r'.
-}

-- | A Fortran scalar value.
data FScalarValue
  = FSVInt     SomeFInt
  | FSVReal    SomeFReal
  | FSVComplex SomeFComplex
  | FSVLogical SomeFInt
  | FSVString  SomeFString
    deriving stock (Generic, Show, Eq)

-- | Recover a Fortran scalar value's type.
fScalarValueType :: FScalarValue -> FScalarType
fScalarValueType = \case
  FSVInt     a -> FSTInt     $ someFKindedKind a
  FSVReal    a -> FSTReal    $ someFKindedKind a
  FSVComplex a -> FSTComplex $ someFKindedKind a
  FSVLogical a -> FSTLogical $ someFKindedKind a
  FSVString  a -> FSTString  $ someFStringLen  a

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
import Language.Fortran.Repr.Type.Scalar

import Data.Text ( Text )
import qualified Data.Text as Text

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )
import Text.PrettyPrint.GenericPretty.Orphans()

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
  = FSVInt     FInt
  | FSVReal    FReal
  | FSVComplex FComplex
  | FSVLogical FInt
  | FSVString  Text
    deriving stock (Show, Generic, Data, Eq)
    deriving anyclass (Binary, Out)

-- | Recover a Fortran scalar value's type.
fScalarValueType :: FScalarValue -> FScalarType
fScalarValueType = \case
  FSVInt     a -> FSTInt     $ fKind a
  FSVReal    a -> FSTReal    $ fKind a
  FSVComplex a -> FSTComplex $ fKind a
  FSVLogical a -> FSTLogical $ fKind a
  FSVString  a -> FSTString  $ fromIntegral $ Text.length a

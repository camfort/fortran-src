{- | Fortran complex data type.

The complex data type is a simple layer on top of reals. We reuse the type and
value representation from reals, but for convenience, we provide a newtype
wrapper to enable writing a 'FKinded' instance for the complex type.

TODO candidate for improving. other ways of writing, name is long & poor.
alternatively, could enforce usage of this
-}

module Language.Fortran.Repr.Type.Scalar.Complex where

import Language.Fortran.Repr.Type.Scalar.Common
import Language.Fortran.Repr.Type.Scalar.Real

import GHC.Generics ( Generic )
import Data.Data ( Data )

newtype FTComplexWrapper = FTComplexWrapper { unFTComplexWrapper :: FTReal }
    deriving stock (Generic, Data, Show, Eq, Ord)

instance FKinded FTComplexWrapper where
    type FKindOf ('FTComplexWrapper 'FTReal4) = 8
    type FKindOf ('FTComplexWrapper 'FTReal8) = 16
    type FKindDefault = 'FTComplexWrapper 'FTReal4
    parseFKind = \case 8  -> Just $ FTComplexWrapper FTReal4
                       16 -> Just $ FTComplexWrapper FTReal8
                       _ -> Nothing
    printFKind = \case
      FTComplexWrapper FTReal4 -> 8
      FTComplexWrapper FTReal8 -> 16

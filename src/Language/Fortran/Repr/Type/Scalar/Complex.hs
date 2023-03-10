{- | Fortran complex data type.

The complex data type is a simple layer on top of reals. We reuse the type and
value representation from reals, but for convenience, we provide a newtype
wrapper to enable writing a 'FKinded' instance for the complex type.

TODO candidate for improving. other ways of writing, name is long & poor.
alternatively, could enforce usage of this
-}

{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Repr.Type.Scalar.Complex where

import Language.Fortran.Repr.Type.Scalar.Common
import Language.Fortran.Repr.Type.Scalar.Real

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

newtype FTComplexWrapper = FTComplexWrapper { unFTComplexWrapper :: FTReal }
    deriving stock (Show, Generic, Data)
    deriving (Enum, Eq, Ord) via FTReal
    deriving anyclass (Binary, Out)

instance FKind FTComplexWrapper where
    parseFKind = \case 8  -> Just $ FTComplexWrapper FTReal4
                       16 -> Just $ FTComplexWrapper FTReal8
                       _ -> Nothing
    printFKind = \case FTComplexWrapper FTReal4 -> 8
                       FTComplexWrapper FTReal8 -> 16

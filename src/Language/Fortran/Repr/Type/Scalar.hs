{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, RankNTypes #-}

module Language.Fortran.Repr.Type.Scalar where

import Data.Int

import Data.Data                      ( Data, Typeable )
import GHC.Generics                   ( Generic )
import Data.Binary                    ( Binary )
import Text.PrettyPrint.GenericPretty ( Out(..) )

-- | Fortran scalar type.
data FTypeScalar
  = FTypeScalarInt     FTypeInt
  | FTypeScalarReal    FTypeReal
  | FTypeScalarComplex FTypeComplex
  | FTypeScalarLogical FTypeInt
  | FTypeScalarChar    FTypeChar
  | FTypeScalarCustom  String    -- ^ F77 structure, F90 DDT (non-intrinsic scalar)
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | Fortran INTEGER type.
--
-- TODO INTEGER(16) (gfortran supports)
data FTypeInt
  = FTypeInt1
  | FTypeInt2
  | FTypeInt4
  | FTypeInt8
    deriving stock    (Eq, Ord, Show, Data, Enum, Typeable, Generic)
    deriving anyclass (Out, Binary)

parseKindInt :: String -> Maybe FTypeInt
parseKindInt = \case "1" -> Just FTypeInt1
                     "2" -> Just FTypeInt2
                     "4" -> Just FTypeInt4
                     "8" -> Just FTypeInt8
                     _   -> Nothing

parseKindInt' :: (Num a, Eq a) => a -> Maybe FTypeInt
parseKindInt' = \case 1 -> Just FTypeInt1
                      2 -> Just FTypeInt2
                      4 -> Just FTypeInt4
                      8 -> Just FTypeInt8
                      _ -> Nothing

prettyKindInt :: Integral a => FTypeInt -> a
prettyKindInt = \case FTypeInt1 -> 1
                      FTypeInt2 -> 2
                      FTypeInt4 -> 4
                      FTypeInt8 -> 8

fTypeIntMax :: FTypeInt -> Integer
fTypeIntMax = \case FTypeInt1 -> toInteger (maxBound @Int8)
                    FTypeInt2 -> toInteger (maxBound @Int16)
                    FTypeInt4 -> toInteger (maxBound @Int32)
                    FTypeInt8 -> toInteger (maxBound @Int64)

fTypeIntMin :: FTypeInt -> Integer
fTypeIntMin = \case FTypeInt1 -> toInteger (minBound @Int8)
                    FTypeInt2 -> toInteger (minBound @Int16)
                    FTypeInt4 -> toInteger (minBound @Int32)
                    FTypeInt8 -> toInteger (minBound @Int64)

-- | Fortran REAL type.
data FTypeReal
  = FTypeReal4
  | FTypeReal8
    deriving stock    (Eq, Ord, Show, Enum, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

parseKindReal :: String -> Maybe FTypeReal
parseKindReal = \case "4" -> Just FTypeReal4
                      "8" -> Just FTypeReal8
                      _   -> Nothing

prettyKindReal :: Integral a => FTypeReal -> a
prettyKindReal = \case FTypeReal4 -> 4
                       FTypeReal8 -> 8

-- | Fortran COMPLEX type (= 2 REALs).
data FTypeComplex
  = FTypeComplex8
  | FTypeComplex16
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

parseKindComplex :: Integer -> Maybe FTypeComplex
parseKindComplex = \case
  4 -> Just FTypeComplex8
  8 -> Just FTypeComplex16
  _ -> Nothing

-- | Fortran CHARACTER type.
data FTypeChar = FTypeChar CharLen
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | The length of a CHARACTER value.
--
-- IanH provides a great reference on StackOverflow:
-- https://stackoverflow.com/a/25051522/2246637
data CharLen
  = CharLen Integer
  -- ^ @CHARACTER(LEN=x)@ (where @x@ is a constant integer expression). Value
  --   has the given static length.

  | CharLenAssumed
  -- ^ @CHARACTER(LEN=*)@. F90. Value has assumed length. For a dummy argument,
  --   the length is assumed from the actual argument. For a PARAMETER named
  --   constant, the length is assumed from the length of the initializing
  --   expression.

  | CharLenDeferred
  -- ^ @CHARACTER(LEN=:)@. F2003. Value has deferred length. Must have the
  --   ALLOCATABLE or POINTER attribute.

    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

prettyScalarType :: FTypeScalar -> String
prettyScalarType = \case
  FTypeScalarInt     k -> "INTEGER" <> bracket (show (prettyKindInt  k))
  FTypeScalarReal    k -> "REAL"    <> bracket (show (prettyKindReal k))
  --FTypeScalarComplex FTypeComplex
  FTypeScalarLogical k -> "LOGICAL" <> bracket (show (prettyKindInt  k))
  --FTypeScalarChar    FTypeChar
  ty -> show ty
  where
    bracket x = "(" <> x <> ")"

{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Repr.Value.Machine where

import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Machine
import Language.Fortran.Repr.Type

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

-- | A Fortran value (scalar only currently).
data FValue = MkFScalarValue FScalarValue
    deriving stock (Show, Generic, Data, Eq)
    deriving anyclass (Binary, Out)

fValueType :: FValue -> FType
fValueType = \case
  MkFScalarValue a -> MkFScalarType $ fScalarValueType a

fromConstInt :: FValue -> Maybe Integer
fromConstInt (MkFScalarValue (FSVInt a)) = Just $ withFInt a
fromConstInt _          = Nothing

fromConstReal :: FValue -> Maybe Double
fromConstReal (MkFScalarValue (FSVReal (FReal4 a))) = Just $ floatToDouble a
  where
    floatToDouble :: Float -> Double
    floatToDouble = realToFrac
fromConstReal (MkFScalarValue (FSVReal (FReal8 a))) = Just $ a

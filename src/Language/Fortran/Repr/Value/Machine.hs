{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Repr.Value.Machine where

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

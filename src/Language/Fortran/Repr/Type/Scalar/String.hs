{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures, UndecidableInstances #-}

module Language.Fortran.Repr.Type.Scalar.String where

import Language.Fortran.Repr.Compat.Natural

import GHC.Generics ( Generic )
import Data.Data ( Data )

--import Data.Singletons.TH
-- required for deriving instances (seems like bug)
--import Prelude.Singletons
--import Data.Ord.Singletons

-- $(singletons [d|
-- | The length of a CHARACTER value.
--
-- IanH provides a great reference on StackOverflow:
-- https://stackoverflow.com/a/25051522/2246637
data CharLen
  = CharLen Natural
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

    deriving stock (Eq, Ord, Show)
--    |])

deriving stock instance Generic CharLen
deriving stock instance Data    CharLen

prettyCharLen :: Natural -> String
prettyCharLen l = "LEN="<>show l

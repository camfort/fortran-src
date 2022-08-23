{-# LANGUAGE CPP #-}

{- | Compatibility definitions for working with term and type level natural
     numbers across multiple GHC versions.

Prior to GHC 9.2:

  * Term level natural numbers: @Natural :: Type@
  * Type level natural numbers: @n :: Nat@

As of GHC 9.2:

  * Term level natural numbers: @Natural :: Type@
  * Type level natural numbers: @n :: Natural@

To avoid issues, we export a 'NaturalK' kind that will refer to the correct
definition for your platform.
-}
module Language.Fortran.Repr.Compat.Natural ( Natural, NaturalK ) where

-- exports 'Natural' >= 9.2
import GHC.TypeNats

#if __GLASGOW_HASKELL__ >= 902
type NaturalK = Natural
#else
import Numeric.Natural
type NaturalK = Nat
#endif

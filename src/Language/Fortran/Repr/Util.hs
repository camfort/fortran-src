{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Fortran.Repr.Util where

import Language.Fortran.Repr.Compat.Natural

import GHC.TypeNats
import GHC.Exts

natVal'' :: forall (a :: NaturalK). KnownNat a => Natural
natVal'' = natVal' (proxy# :: Proxy# a)

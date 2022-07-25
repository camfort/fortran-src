{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Fortran.Repr.Util where

import GHC.TypeNats
import GHC.Exts

natVal'' :: forall a. KnownNat a => Natural
natVal'' = natVal' (proxy# :: Proxy# a)

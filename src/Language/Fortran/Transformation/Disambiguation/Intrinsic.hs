{-# LANGUAGE ScopedTypeVariables #-}

-- | Uses 'ConstructType' from analysis.

module Language.Fortran.Transformation.Disambiguation.Intrinsic (disambiguateIntrinsic) where

import Data.Generics.Uniplate.Data
import Data.Data

import Language.Fortran.Analysis ( Analysis(..), ConstructType(..), TransFunc )
import Language.Fortran.AST
import Language.Fortran.Transformation.Monad

disambiguateIntrinsic :: forall a. Data a => Transform a ()
disambiguateIntrinsic = modifyProgramFile (trans expression)
  where
    trans = transformBi :: TransFunc Expression ProgramFile a
    expression (ExpValue a s (ValVariable v))
      | Just CTIntrinsic <- constructType a = ExpValue a s (ValIntrinsic v)
    expression e                            = e

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

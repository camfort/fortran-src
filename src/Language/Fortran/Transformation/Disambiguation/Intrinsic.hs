{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.Fortran.Transformation.Disambiguation.Intrinsic (disambiguateIntrinsic) where

import Prelude hiding (lookup)
import Data.Generics.Uniplate.Data
import Data.Map ((!), lookup, Map)
import Data.Maybe (isJust, fromJust)
import Data.Data

import Language.Fortran.Util.Position (getSpan)
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Types
import Language.Fortran.AST
import Language.Fortran.Transformation.TransformMonad

import Debug.Trace

disambiguateIntrinsic :: Data a => Transform a ()
disambiguateIntrinsic = modifyProgramFile (trans expression)
  where
    trans = (transformBi :: Data a => TransFunc Expression ProgramFile a)
    expression e@(ExpValue a s (ValVariable v))
      | Just (IDType _ (Just CTIntrinsic)) <- idType a = ExpValue a s (ValIntrinsic v)
    expression e                                      = e

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

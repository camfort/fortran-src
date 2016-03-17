{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Forpar.Transformation.Disambiguation.Array (disambiguateArray) where

import Prelude hiding (lookup)
import Data.Generics.Uniplate.Data
import Data.Map ((!), lookup)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Control.Monad (mplus)

import Forpar.Analysis.Types (IDType(..), ConstructType(..), TypeScope(..))
import Forpar.AST
import Forpar.Transformation.TransformMonad

import Debug.Trace

disambiguateArray :: Transform ()
disambiguateArray = do
  renameProgramFile
  tenv <- getTypes
  let perValue :: ProgramUnitName -> Value () -> Value ()
      perValue name (ValVariable a v) | isArrayType tenv name v = ValArray a v
                                      | otherwise               = ValVariable a v
      perValue _ x = x
  let perPU :: ProgramUnit () -> ProgramUnit ()
      perPU pu = transformBi (perValue (getName pu)) pu
  modifyProgramFile $ transformBi perPU
  unrenameProgramFile

isArrayType tenv name v = fromMaybe False $ do
  tmap <- lookup (Local name) tenv `mplus` lookup Global tenv
  idty <- lookup v tmap
  cty  <- idCType idty
  return $ cty == CTArray

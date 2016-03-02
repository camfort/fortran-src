{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Forpar.Transformation.Disambiguation.Function (disambiguateFunction) where

import Prelude hiding (lookup)
import Data.Generics.Uniplate.Data
import Data.Map ((!), lookup)
import Data.Maybe (isJust, fromJust)

import Forpar.Analysis.Types (IDType(..), ConstructType(..), TypeScope(..))
import Forpar.AST
import Forpar.Transformation.TransformMonad

import Debug.Trace

disambiguateFunction :: Transform ()
disambiguateFunction = do
  disambiguateFunctionStatements
  disambiguateFunctionCalls

disambiguateFunctionStatements :: Transform ()
disambiguateFunctionStatements = do
  pf <- getProgramFile
  mapping <- getTypes
  putProgramFile $ descendBi (transform' mapping) pf
  where
    transform' mapping (pu :: ProgramUnit ()) =
      descendBi (transform'' $ mapping ! Local (getName pu)) pu
    transform''
      innerMapping
      st@(StExpressionAssign () s
        (ExpSubscript () _
          (ExpValue () s'' (ValArray () n)) indicies)
        e2)
      | Just (IDType _ (Just CTFunction)) <- lookup n innerMapping =
          StFunction () s
            (ExpValue () s'' (ValFunctionName n))
            indicies
            e2
      | otherwise = st
    transform'' _ (x :: Statement ()) = x

disambiguateFunctionCalls :: Transform ()
disambiguateFunctionCalls = do
  pf <- getProgramFile
  mapping <- getTypes
  putProgramFile $ descendBi (transform' mapping) pf
  where
    transform' mapping (pu :: ProgramUnit ()) =
      descendBi (transform'' (lookup Global mapping) (mapping ! Local (getName pu))) pu
    transform''
        mGlobalMapping
        innerMapping
        exp@(ExpSubscript () s (ExpValue () s' (ValArray () n)) l)
      -- Check if it is a function statement
      -- | Just (IDType _ (Just CTFunction)) <- lookup n innerMapping
      | Just (IDType _ (Just CTFunction)) <- lookup n innerMapping
      = ExpFunctionCall () s (ExpValue () s' (ValFunctionName n)) l
      -- Check if it is a function subprogram
      | isJust mGlobalMapping
      , Just (IDType _ (Just CTFunction)) <- lookup n (fromJust mGlobalMapping)
      = ExpFunctionCall () s (ExpValue () s' (ValFunctionName n)) l
      | otherwise = exp
    transform'' mGlobalMapping localMapping (x :: Expression ()) =
      descend (transform'' mGlobalMapping localMapping) x

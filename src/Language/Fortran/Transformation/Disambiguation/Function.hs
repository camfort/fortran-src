{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.Fortran.Transformation.Disambiguation.Function (disambiguateFunction) where

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

disambiguateFunction :: Data a => Transform a ()
disambiguateFunction = do
  disambiguateFunctionStatements
  disambiguateFunctionCalls
  pf <- getAPF
  putProgramFile (fmap prevAnnotation pf)

disambiguateFunctionStatements :: Data a => Transform a ()
disambiguateFunctionStatements = modifyAPF (trans statement)
  where
    trans = (transformBi :: Data a => TransFunc Statement ProgramFile a)
    statement st@(StExpressionAssign a1 s (ExpSubscript _ _ v@(ExpValue a _ (ValVariable _)) indicies) e2)
      | Just (IDType _ (Just CTFunction)) <- idType a = StFunction a1 s v (aMap fromIndex indicies) e2
    statement st                                      = st

disambiguateFunctionCalls :: Data a => Transform a ()
disambiguateFunctionCalls = modifyAPF (trans expression)
  where
    trans = (transformBi :: Data a => TransFunc Expression ProgramFile a)
    expression e@(ExpSubscript a1 s v@(ExpValue a _ (ValVariable _)) indicies)
      | Just (IDType _ (Just CTFunction)) <- idType a = ExpFunctionCall a1 s v (Just $ aMap fromIndex indicies)
    expression e                                      = e

class Indexed a where
  fromIndex :: Index b -> a b

instance Indexed Argument where
  fromIndex (IxSingle a s mKey e) = Argument a s mKey e
  fromIndex IxRange{} =
    error "Deduced a function but argument is not an expression."

instance Indexed Expression where
  fromIndex (IxSingle _ _ _ e) = e
  fromIndex IxRange{} =
    error "Deduced a function but argument is not an expression."

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

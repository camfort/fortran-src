{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.Fortran.Transformation.Disambiguation.Function (disambiguateFunction) where

import Prelude hiding (lookup)
import Data.Generics.Uniplate.Data
import Data.Data

import Language.Fortran.Analysis
import Language.Fortran.AST
import Language.Fortran.Transformation.TransformMonad


disambiguateFunction :: Data a => Transform a ()
disambiguateFunction = do
  disambiguateFunctionStatements
  disambiguateFunctionCalls

disambiguateFunctionStatements :: Data a => Transform a ()
disambiguateFunctionStatements = modifyProgramFile (trans statement)
  where
    trans = transformBi :: Data a => TransFunc Statement ProgramFile a
    statement (StExpressionAssign a1 s (ExpSubscript _ _ v@(ExpValue a _ (ValVariable _)) indicies) e2)
      | Just (IDType _ (Just CTFunction)) <- idType a
      , indiciesRangeFree indicies = StFunction a1 s v (aMap fromIndex indicies) e2
    -- nullary statement function
    statement (StExpressionAssign a1 s1 (ExpFunctionCall _ _ v@(ExpValue a s (ValVariable _)) Nothing) e2)
      = StFunction a1 s1 v (AList a s []) e2
    statement st                                      = st

disambiguateFunctionCalls :: Data a => Transform a ()
disambiguateFunctionCalls = modifyProgramFile (trans expression)
  where
    trans = transformBi :: Data a => TransFunc Expression ProgramFile a
    expression (ExpSubscript a1 s v@(ExpValue a _ (ValVariable _)) indicies)
      | Just (IDType _ (Just CTFunction)) <- idType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (Just $ aMap fromIndex indicies)
      | Just (IDType _ (Just CTExternal)) <- idType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (Just $ aMap fromIndex indicies)
      | Just (IDType _ (Just CTVariable)) <- idType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (Just $ aMap fromIndex indicies)
      | Nothing <- idType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (Just $ aMap fromIndex indicies)
    expression (ExpSubscript a1 s v@(ExpValue a _ (ValIntrinsic _)) indicies)
      | Just (IDType _ (Just CTIntrinsic)) <- idType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (Just $ aMap fromIndex indicies)
    expression e                                      = e

-- BEGIN: TODO STRICTLY TO BE REMOVED LATER TODO
indiciesRangeFree :: AList Index a -> Bool
indiciesRangeFree aIndicies = cRange $ aStrip aIndicies
  where
    cRange [] = True
    cRange (IxSingle{}:xs) = cRange xs
    cRange (IxRange{}:_) = False
-- END: TODO STRICTLY TO BE REMOVED LATER TODO

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

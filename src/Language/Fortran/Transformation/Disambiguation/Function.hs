{-# LANGUAGE ScopedTypeVariables #-}

-- | Uses 'ConstructType' from analysis.

module Language.Fortran.Transformation.Disambiguation.Function (disambiguateFunction) where

import Prelude hiding (lookup)
import Data.Generics.Uniplate.Data
import Data.Data

import Language.Fortran.Analysis ( Analysis(..), ConstructType(..), TransFunc )
import Language.Fortran.AST
import Language.Fortran.Transformation.Monad


disambiguateFunction :: Data a => Transform a ()
disambiguateFunction = do
  disambiguateFunctionStatements
  disambiguateFunctionCalls

disambiguateFunctionStatements :: Data a => Transform a ()
disambiguateFunctionStatements = modifyProgramFile (trans statement)
  where
    trans = transformBi :: Data a => TransFunc Statement ProgramFile a
    statement (StExpressionAssign a1 s (ExpSubscript _ _ v@(ExpValue a _ (ValVariable _)) indicies) e2)
      | Just CTFunction <- constructType a
      , indiciesRangeFree indicies = StFunction a1 s v (aMap fromIndex indicies) e2
    -- nullary statement function (no args): TODO handled above...?
    statement st                                      = st

disambiguateFunctionCalls :: Data a => Transform a ()
disambiguateFunctionCalls = modifyProgramFile (trans expression)
  where
    trans = transformBi :: Data a => TransFunc Expression ProgramFile a
    expression (ExpSubscript a1 s v@(ExpValue a _ (ValVariable _)) indicies)
      | Just CTFunction <- constructType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (aMap fromIndex indicies)
      | Just CTExternal <- constructType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (aMap fromIndex indicies)
      | Just CTVariable <- constructType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (aMap fromIndex indicies)
      | Nothing <- constructType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (aMap fromIndex indicies)
    expression (ExpSubscript a1 s v@(ExpValue a _ (ValIntrinsic _)) indicies)
      | Just CTIntrinsic <- constructType a
      , indiciesRangeFree indicies = ExpFunctionCall a1 s v (aMap fromIndex indicies)
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
  fromIndex (IxSingle a s mKey e) = Argument a s mKey (ArgExpr e)
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

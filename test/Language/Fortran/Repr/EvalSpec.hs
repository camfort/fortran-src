module Language.Fortran.Repr.EvalSpec where

import Test.Hspec
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck ( NonNegative(..) )

import TestUtil ( u )

import Language.Fortran.AST
import Language.Fortran.Repr
import Language.Fortran.Repr.Eval.Value

import Language.Fortran.Analysis

import Data.Int

spec :: Spec
spec =
  describe "exponentiation" $
    prop "integer exponentation (+ve exponent) (INTEGER(4))" $
      \base (NonNegative (expo :: Int32)) ->
        let expr = expBinary Exponentiation (expValInt base) (expValInt expo)
         in shouldEvalTo (FSVInt (FInt4 (base^expo))) (evalExpr expr)

shouldEvalTo :: FScalarValue -> FEvalValuePure FValue -> Expectation
shouldEvalTo checkVal prog =
    case runEvalFValuePure mempty prog of
      Right (a, _msgs) ->
        case a of
          MkFScalarValue a' -> a' `shouldBe` checkVal
          -- _ -> expectationFailure "not a scalar"
      Left e -> expectationFailure (show e)

expBinary :: BinaryOp -> Expression (Analysis ()) -> Expression (Analysis ()) -> Expression (Analysis ())
expBinary = ExpBinary (analysis0 ()) u

expValue :: Value (Analysis ()) -> Expression (Analysis ())
expValue = ExpValue (analysis0 ()) u

-- | default kind. take integral-like over String because nicer to write :)
valInteger :: (Integral a, Show a) => a -> Value (Analysis ())
valInteger i = ValInteger (show i) Nothing

expValInt :: (Integral a, Show a) => a -> Expression (Analysis ())
expValInt = expValue . valInteger

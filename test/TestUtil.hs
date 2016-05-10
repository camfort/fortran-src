{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestUtil where

import Test.Hspec
import Data.Data
import Data.Generics.Uniplate.Data

import Language.Fortran.AST
import Language.Fortran.Util.Position

u = undefined

valTrue = ExpValue () u $ ValLogical ".true."
valFalse = ExpValue () u $ ValLogical ".false."

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable () str

intGen :: (Show a, Integral a) => a -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

realGen :: (Fractional a, Show a) => a -> Expression ()
realGen i = ExpValue () u $ ValReal $ show i

strGen :: String -> Expression ()
strGen str = ExpValue () u $ ValString str

labelGen :: Integer -> Expression ()
labelGen i = ExpValue () u $ ValLabel $ show i

starVal :: Expression ()
starVal = ExpValue () u ValStar

opGen :: String -> Expression ()
opGen s = ExpValue () u (ValOperator s)

assVal :: Expression ()
assVal = ExpValue () u ValAssignment

ixSinGen i = IxSingle () u (intGen i)
ixRanGen i j = IxRange () u (Just $ intGen i) (Just $ intGen j) Nothing

shouldBe' a b = resetSrcSpan a `shouldBe` resetSrcSpan b

-- To be used in testing it reverts the SrcSpans in AST to dummy initial
-- SrcSpan value.
resetSrcSpan :: Data a => a -> a
resetSrcSpan = transformBi f
  where
    f x = case cast x :: Maybe SrcSpan of
      Just _ -> initSrcSpan
      Nothing -> x

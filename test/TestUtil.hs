module TestUtil where

import Test.Hspec
import Data.Data
import Data.Generics.Uniplate.Data

import Forpar.AST
import Forpar.Util.Position

u = undefined

valTrue = ExpValue () u ValTrue
valFalse = ExpValue () u ValFalse

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable () str

parGen :: String -> Expression ()
parGen str = ExpValue () u $ ValParameter str

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

strGen :: String -> Expression ()
strGen str = ExpValue () u $ ValString $ str

labelGen :: Integer -> Expression ()
labelGen i = ExpValue () u $ ValLabel $ show i

arrGen :: String -> Expression ()
arrGen str = ExpValue () u $ ValArray () str

cbNameGen :: String -> Expression ()
cbNameGen str = ExpValue () u $ ValCommonName str

starVal :: Expression ()
starVal = ExpValue () u ValStar

shouldBe' a b = resetSrcSpan a `shouldBe` resetSrcSpan b

-- To be used in testing it reverts the SrcSpans in AST to dummy initial
-- SrcSpan value.
resetSrcSpan :: Data a => a -> a
resetSrcSpan = transformBi f
  where 
    f x = case cast x :: Maybe SrcSpan of 
      Just _ -> initSrcSpan
      Nothing -> x

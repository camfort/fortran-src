{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Fortran.PrettySpec where

import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Operations
import Data.Maybe (catMaybes)
--import Data.DeriveTH

import Control.Monad (void)

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Pretty
import Language.Fortran.Parser.Any
import Language.Fortran.Util.Position
import Language.Fortran.Util.SecondParameter

import System.FilePath
import System.Directory
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

import Test.Hspec

--derive makeArbitrary ''Expression
--derive makeArbitrary ''Statement
--derive makeArbitrary ''Value

checkAll :: forall a b c . (Out c, Data c, Data a, Data b)
         => (b -> Maybe c) -> (c -> Spec) -> a -> Spec
checkAll restrict check t =
    describe ("Testing on " ++ show (length inputs) ++ " nodes")
      $ mapM_ check inputs
  where
    inputs = catMaybes [ restrict b | b <- universeBi t :: [b] ]

samplesBase :: FilePath
samplesBase = "test" </> "Language" </> "Fortran" </> "samples"

spec :: Spec
spec =
  describe "Pretty printer tests" $ do
    let path = samplesBase </> "simple.f90"
    contents <- runIO $ flexReadFile path
    let version = deduceVersion path
    let Just parserF = lookup version parserVersions
    let ast = void (parserF contents path)

    describe "Size-related invariants (values in expressions)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Expression () -> Spec
     checkAll valueExpressions (ppr version) ast

    describe "Size-related invariants (do specifications)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> DoSpecification () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (indices)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Index () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (expressions)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Expression () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (arguments)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Argument () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (statements)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Statement () -> Spec
     checkAll Just (ppr version) ast

valueExpressions :: Expression () -> Maybe (Expression ())
valueExpressions e@ExpValue{} = Just e
valueExpressions _ = Nothing

prop_pprintsize :: (SecondParameter (e ()) SrcSpan, Pretty (e ()))
                   => FortranVersion -> e () -> Spec
prop_pprintsize v e = do
  let pp = render $ pprint v e
  let span = getSecondParameter e
  it ("line length on: " ++ pp) $ lineDistance span `shouldBe` 0
  it ("col length on: " ++ pp)  $ length pp `shouldBe` (columnDistance span + 1)

flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile

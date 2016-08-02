{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import TestUtil

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

    describe "Size-related invariants (dimension declarator)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> DimensionDeclarator ()
            -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (selector)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> DimensionDeclarator ()
            -> Spec
     checkAll Just (ppr version) ast

    describe "Dimension declarator" $ do
      it "Prints left bound dimension declarator" $ do
        let dd = DimensionDeclarator () u (Just $ intGen 42) Nothing
        pprint Fortran90 dd `shouldBe` "42:"

      it "Prints right bound dimension declarator" $ do
        let dd = DimensionDeclarator () u Nothing (Just $ intGen 42)
        pprint Fortran90 dd `shouldBe` "42"

      it "Prints bounded dimension declarator" $ do
        let dd = DimensionDeclarator () u (Just $ intGen 24) (Just $ intGen 42)
        pprint Fortran90 dd `shouldBe` "24:42"

    describe "Selector" $ do
      it "prints Fortran 77 selector" $ do
        let sel = Selector () u (Just $ intGen 42) Nothing
        pprint Fortran77 sel `shouldBe` "* (42)"

      it "prints Fortran 90 selector" $ do
        let sel = Selector () u (Just $ intGen 42) (Just $ intGen 24)
        pprint Fortran90 sel `shouldBe` "(len=42, kind=24)"

    describe "Use" $
      it "prints renaming" $ do
        let renaming = UseRename () u (varGen "x") (varGen "y")
        pprint Fortran90 renaming `shouldBe` "x => y"

    describe "Control pair" $
      it "prints named control pair" $ do
        let cp = ControlPair () u (Just "errno") (intGen 42)
        pprint Fortran77Extended cp `shouldBe` "errno = 42"

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

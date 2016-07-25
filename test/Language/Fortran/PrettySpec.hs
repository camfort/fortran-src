{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fortran.PrettySpec where

import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Operations
--import Data.DeriveTH

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Pretty
import Language.Fortran.Parser.Any
import Language.Fortran.Util.Position

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
    inputs = [c | Just c <- [restrict b | b <- (universeBi t) :: [b]]]

samplesBase :: FilePath
samplesBase = "test" </> "Language" </> "Fortran" </> "samples"

spec :: Spec
spec =
  describe "Pretty printer tests" $ do
    let path = samplesBase </> "simple.f90"
    contents <- runIO $ flexReadFile path
    let version = deduceVersion path
    let Just parserF = lookup version parserVersions
    let ast = fmap (const ()) (parserF contents path)

    describe "Size-related invariants" $ do
     checkAll valueExpressions (prop_valpprintsize version) ast

valueExpressions :: Expression () -> Maybe (Expression ())
valueExpressions e@(ExpValue {}) = Just e
valueExpressions _ = Nothing

prop_valpprintsize :: FortranVersion -> Expression () -> Spec
prop_valpprintsize v e@(ExpValue _ span val) = do
  let pp = render $ pprint v val
  it ("line length on: " ++ pp) $ (lineDistance span) `shouldBe` 0
  it ("col length on: " ++ pp)  $ (length pp) `shouldBe` (columnDistance span + 1)

flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile

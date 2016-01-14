module Forpar.Parser.Fortran77Spec where

import Test.Hspec

import Forpar.Parser.Fortran77
import Forpar.AST

u = undefined

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable str

pParser :: String -> [ProgramUnit ()]
pParser source = fortran77Parser source "<unknown>"

spec :: Spec
spec = 
  describe "Fortran 77 Parser" $ do
    it "parses main program unit" $ do
      let st = StDeclaration () u TypeInteger (AList () u [varGen "x"])
      let bl = BlStatement () u st []
      let pu = resetSrcSpan $ PUMain () u (Just "hello") [(Nothing, bl)] []
      resetSrcSpan (pParser exampleProgram) `shouldBe` [pu]

exampleProgram = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

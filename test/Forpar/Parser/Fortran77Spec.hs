module Forpar.Parser.Fortran77Spec where

import Test.Hspec

import Forpar.Parser.Fortran77
import Forpar.Lexer.FixedForm (initParseState)
import Forpar.ParserMonad (FortranVersion(..), evalParse)
import Forpar.AST

u = undefined

eParser :: String -> Expression ()
eParser sourceCode = 
  evalParse expressionParser $ initParseState sourceCode Fortran77 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode = 
  evalParse statementParser $ initParseState sourceCode Fortran77 "<unknown>"

pParser :: String -> [ProgramUnit ()]
pParser source = fortran77Parser source "<unknown>"

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable str

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

spec :: Spec
spec = 
  describe "Fortran 77 Parser" $ do
    it "parses main program unit" $ do
      let st = StDeclaration () u (TypeInteger () u) (AList () u [varGen "x"])
      let bl = BlStatement () u st []
      let pu = resetSrcSpan $ PUMain () u (Just "hello") [(Nothing, bl)] []
      resetSrcSpan (pParser exampleProgram) `shouldBe` [pu]

    describe "IMPLICIT" $ do
      it "parses 'implicit none'" $ do
        let st = resetSrcSpan $ StImplicit () u Nothing
        resetSrcSpan (sParser "      implicit none") `shouldBe` st

      it "parses 'implicit character*30 (a, b, c), integer (a-z, l)" $ do
        let imp1 = ImpList () u (TypeCharacter () u (Just $ intGen 30)) $ AList () u [ImpCharacter () u "a", ImpCharacter () u "b", ImpCharacter () u "c"]
        let imp2 = ImpList () u (TypeInteger () u) $ AList () u [ImpRange () u "a" "z", ImpCharacter () u "l"]
        let st = resetSrcSpan $ StImplicit () u $ Just $ AList () u [imp1, imp2]
        resetSrcSpan (sParser "      implicit character*30 (a, b, c), integer (a-z, l)") `shouldBe` st

exampleProgram = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

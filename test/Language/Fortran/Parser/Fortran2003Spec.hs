module Language.Fortran.Parser.Fortran2003Spec where


import Prelude hiding (GT, EQ, exp, pred)

import TestUtil
import Test.Hspec

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Lexer.FreeForm
import Language.Fortran.Parser.Fortran2003
import qualified Data.ByteString.Char8 as B

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
    _ -> error "unhandled evalParse"
  where
    paddedSourceCode = B.pack $ "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran2003 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran2003 "<unknown>"

fParser :: String -> ProgramUnit ()
fParser sourceCode =
  evalParse functionParser $ initParseState (B.pack sourceCode) Fortran2003 "<unknown>"

spec :: Spec
spec =
  describe "Fortran 2003 Parser" $ do
    describe "Modules" $ do
      it "parses use statement, intrinsic module" $ do
        let renames = fromList ()
              [ UseRename () u (varGen "sprod") (varGen "prod")
              , UseRename () u (varGen "a") (varGen "b") ]
        let st = StUse () u (varGen "mod") (Just ModIntrinsic) Permissive (Just renames)
        sParser "use, intrinsic :: mod, sprod => prod, a => b" `shouldBe'` st

      it "parses use statement, non_intrinsic module" $ do
        let renames = fromList ()
              [ UseRename () u (varGen "sprod") (varGen "prod")
              , UseRename () u (varGen "a") (varGen "b") ]
        let st = StUse () u (varGen "mod") (Just ModNonIntrinsic) Exclusive (Just renames)
        sParser "use, non_intrinsic :: mod, only: sprod => prod, a => b" `shouldBe'` st

      it "parses use statement, unspecified nature of module" $ do
        let renames = fromList ()
              [ UseRename () u (varGen "sprod") (varGen "prod")
              , UseRename () u (varGen "a") (varGen "b") ]
        let st = StUse () u (varGen "mod") Nothing Permissive (Just renames)
        sParser "use :: mod, sprod => prod, a => b" `shouldBe'` st

      it "parses procedure (interface-name, attribute, proc-decl)" $ do
        let call = ExpFunctionCall () u (varGen "c") Nothing
        let st = StProcedure () u (Just (ProcInterfaceName () u (varGen "a")))
                                  (Just (AttrSave () u))
                                  (AList () u [ProcDecl () u (varGen "b") (Just call)])
        sParser "PROCEDURE(a), SAVE :: b => c()" `shouldBe'` st

      it "parses procedure (class-star, bind-name, proc-decls)" $ do
        let call = ExpFunctionCall () u (varGen "c") Nothing
        let clas = TypeSpec () u ClassStar Nothing
        let st = StProcedure () u (Just (ProcInterfaceType () u clas))
                                  (Just (AttrBind () u (Just (ExpValue () u (ValString "e")))))
                                  (AList () u [ProcDecl () u (varGen "b") (Just call)
                                              ,ProcDecl () u (varGen "d") (Just call)])
        sParser "PROCEDURE(CLASS(*)), BIND(C, NAME=\"e\") :: b => c(), d => c()" `shouldBe'` st

      it "parses procedure (class-custom, bind, proc-decls)" $ do
        let call = ExpFunctionCall () u (varGen "c") Nothing
        let clas = TypeSpec () u (ClassCustom "e") Nothing
        let st = StProcedure () u (Just (ProcInterfaceType () u clas))
                                  (Just (AttrBind () u Nothing))
                                  (AList () u [ProcDecl () u (varGen "b") (Just call)
                                              ,ProcDecl () u (varGen "d") (Just call)])
        sParser "PROCEDURE(CLASS(e)), BIND(C) :: b => c(), d => c()" `shouldBe'` st

      it "import statements" $ do
        let st = StImport () u (AList () u [varGen "a", varGen "b"])
        sParser "import a, b" `shouldBe'` st
        sParser "import :: a, b" `shouldBe'` st

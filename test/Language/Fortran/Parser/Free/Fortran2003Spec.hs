module Language.Fortran.Parser.Free.Fortran2003Spec ( spec ) where

import Prelude hiding (GT, EQ, exp, pred)

import Test.Hspec
import TestUtil
import Language.Fortran.Parser.Free.Common

import Language.Fortran.AST
import Language.Fortran.Version
import Language.Fortran.Parser
import Language.Fortran.Parser.Monad ( Parse )
import qualified Language.Fortran.Parser.Free.Fortran2003 as F2003
import qualified Language.Fortran.Parser.Free.Lexer       as Free

import qualified Data.ByteString.Char8 as B

parseWith :: Parse Free.AlexInput Free.Token a -> String -> a
parseWith p = parseUnsafe (makeParserFree p Fortran2003) . B.pack

eParser :: String -> Expression ()
eParser = parseUnsafe p . B.pack
  where p = makeParser initParseStateFreeExpr F2003.expressionParser Fortran2003

sParser :: String -> Statement ()
sParser = parseWith F2003.statementParser

bParser :: String -> Block ()
bParser = parseWith F2003.blockParser

fParser :: String -> ProgramUnit ()
fParser = parseWith F2003.functionParser

spec :: Spec
spec =
  describe "Fortran 2003 Parser" $ do
    describe "Modules" $ do
      it "parses use statement, intrinsic module" $ do
        let renames = fromList ()
              [ UseRename () u (varGen "sprod") (varGen "prod")
              , UseRename () u (varGen "a") (varGen "b") ]
            st = StUse () u (varGen "mod") (Just ModIntrinsic) Permissive (Just renames)
        sParser "use, intrinsic :: mod, sprod => prod, a => b" `shouldBe'` st

      it "parses use statement, non_intrinsic module" $ do
        let renames = fromList ()
              [ UseRename () u (varGen "sprod") (varGen "prod")
              , UseRename () u (varGen "a") (varGen "b") ]
            st = StUse () u (varGen "mod") (Just ModNonIntrinsic) Exclusive (Just renames)
        sParser "use, non_intrinsic :: mod, only: sprod => prod, a => b" `shouldBe'` st

      it "parses use statement, unspecified nature of module" $ do
        let renames = fromList ()
              [ UseRename () u (varGen "sprod") (varGen "prod")
              , UseRename () u (varGen "a") (varGen "b") ]
            st = StUse () u (varGen "mod") Nothing Permissive (Just renames)
        sParser "use :: mod, sprod => prod, a => b" `shouldBe'` st

      it "parses simple procedure with no args" $ do
        let st = StProcedure () u (Just (ProcInterfaceName () u (varGen "name")))
                                  Nothing
                                  (AList () u [ProcDecl () u (varGen "other_name") Nothing] )
        sParser "procedure(name) :: other_name" `shouldBe'` st


      it "parses procedure (interface-name, attribute, proc-decl)" $ do
        let call = ExpFunctionCall () u (varGen "c") (aEmpty () u)
            st = StProcedure () u (Just (ProcInterfaceName () u (varGen "a")))
                                  (Just (AList () u [AttrSave () u]))
                                  (AList () u [ProcDecl () u (varGen "b") (Just call)])
        sParser "PROCEDURE(a), SAVE :: b => c()" `shouldBe'` st

      it "parses procedure (class-star, bind-name, proc-decls)" $ do
        let call = ExpFunctionCall () u (varGen "c") (aEmpty () u)
            clas = TypeSpec () u ClassStar Nothing
            st = StProcedure () u (Just (ProcInterfaceType () u clas))
                                  (Just (AList () u [AttrSuffix () u (SfxBind () u (Just (ExpValue () u (ValString "e"))))]))
                                  (AList () u [ProcDecl () u (varGen "b") (Just call)
                                              ,ProcDecl () u (varGen "d") (Just call)])
        sParser "PROCEDURE(CLASS(*)), BIND(C, NAME=\"e\") :: b => c(), d => c()" `shouldBe'` st

      it "parses procedure (class-custom, bind, proc-decls)" $ do
        let call = ExpFunctionCall () u (varGen "c") (aEmpty () u)
            clas = TypeSpec () u (ClassCustom "e") Nothing
            st = StProcedure () u (Just (ProcInterfaceType () u clas))
                                  (Just (AList () u [AttrSuffix () u (SfxBind () u Nothing)]))
                                  (AList () u [ProcDecl () u (varGen "b") (Just call)
                                              ,ProcDecl () u (varGen "d") (Just call)])
        sParser "PROCEDURE(CLASS(e)), BIND(C) :: b => c(), d => c()" `shouldBe'` st

      it "import statements" $ do
        let st = StImport () u (AList () u [varGen "a", varGen "b"])
        sParser "import a, b" `shouldBe'` st
        sParser "import :: a, b" `shouldBe'` st

      it "parses function with bind" $ do
          let puFunction = PUFunction () u
              fType = Nothing
              fPre = emptyPrefixes
              fSuf = fromList' () [SfxBind () u (Just $ ExpValue () u (ValString "f"))]
              fName = "f"
              fArgs = Nothing
              fRes = Nothing
              fBody = []
              fSub = Nothing
              fStr = init $ unlines ["function f() bind(c,name=\"f\")"
                                    , "end function f" ]
          let expected = puFunction fType (fPre, fSuf) fName fArgs fRes fBody fSub
          fParser fStr `shouldBe'` expected

      it "parses asynchronous decl" $ do
        let decls = [declVarGen "a", declVarGen "b"]
            st = StAsynchronous () u (AList () u decls)
        sParser "asynchronous a, b" `shouldBe'` st
        sParser "asynchronous :: a, b" `shouldBe'` st

      it "parses asynchronous attribute" $ do
        let decls = [declVarGen "a", declVarGen "b"]
            ty = TypeSpec () u TypeInteger Nothing
            attrs = [AttrAsynchronous () u]
            st = StDeclaration () u ty (Just (AList () u attrs)) (AList () u decls)
        sParser "integer, asynchronous :: a, b" `shouldBe'` st

      it "parses enumerators" $ do
        let decls = [ declVariable () u (varGen "a") Nothing (Just (intGen 1))
                    , declVariable () u (varGen "b") Nothing Nothing ]
            st = StEnumerator () u (AList () u decls)
        sParser "enum, bind(c)" `shouldBe'` StEnum () u
        sParser "enumerator :: a = 1, b" `shouldBe'` st
        sParser "end enum" `shouldBe'` StEndEnum () u

      it "parses allocate with type_spec" $ do
        let sel = Selector () u (Just (ExpValue () u ValColon)) (Just (varGen "foo"))
            ty = TypeSpec () u TypeCharacter (Just sel)
            decls = AList () u [declVarGen "s"]
            st = StDeclaration () u ty (Just (AList () u [AttrAllocatable () u])) decls
        sParser "character(len=:,kind=foo), allocatable :: s" `shouldBe'` st

      it "parses allocate with type_spec" $ do
        let sel = Selector () u (Just (intGen 3)) (Just (varGen "foo"))
            ty = TypeSpec () u TypeCharacter (Just sel)
            st = StAllocate () u (Just ty) (AList () u [varGen "s"]) Nothing
        sParser "allocate(character(len=3,kind=foo) :: s)" `shouldBe'` st

      it "parses protected" $ do
        let ty = TypeSpec () u TypeReal Nothing
            decls = AList () u [declVarGen "x"]
            st1 = StDeclaration () u ty (Just (AList () u [AttrProtected () u, AttrPublic () u])) decls
            st2 = StProtected () u (Just (AList () u [varGen "x"]))
        sParser "real, protected, public :: x" `shouldBe'` st1
        sParser "protected x" `shouldBe'` st2

    describe "labelled where" $ do
      it "parses where construct statement" $
        sParser "foo: where (.true.)" `shouldBe'` StWhereConstruct () u (Just "foo") valTrue

      it "parses elsewhere statement" $
        sParser "elsewhere ab101" `shouldBe'` StElsewhere () u (Just "ab101") Nothing

      it "parses elsewhere statement" $ do
        let exp = ExpBinary () u GT (varGen "a") (varGen "b")
        sParser "elsewhere (a > b) A123" `shouldBe'` StElsewhere () u (Just "a123") (Just exp)

      it "parses endwhere statement" $
        sParser "endwhere foo1" `shouldBe'` StEndWhere () u (Just "foo1")

    describe "associate block" $ do
      it "parses multiple assignment associate block" $ do
        let text = unlines [ "associate (x => a, y => (a * b))"
                           , "  print *, x"
                           , "  print *, y"
                           , "end associate" ]
            expected = BlAssociate () u Nothing Nothing abbrevs body' Nothing
            body'   = [blStmtPrint "x", blStmtPrint "y"]
            blStmtPrint x = BlStatement () u Nothing (stmtPrint x)
            stmtPrint x = StPrint () u starVal (Just $ AList () u [ varGen x ])
            abbrevs = AList () u [abbrev "x" (expValVar "a"), abbrev "y" (expBinVars Multiplication "a" "b")]
            abbrev var expr = ATuple () u (expValVar var) expr
            expValVar x = ExpValue () u (ValVariable x)
            expBinVars op x1 x2 = ExpBinary () u op (expValVar x1) (expValVar x2)
        bParser text `shouldBe'` expected

    specFreeCommon bParser sParser eParser

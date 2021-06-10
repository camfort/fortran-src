module Language.Fortran.Parser.Fortran77.ParserSpec where

import Test.Hspec
import TestUtil

import Prelude hiding (exp)
import Language.Fortran.Parser.Fortran77
import Language.Fortran.Lexer.FixedForm (initParseState)
import Language.Fortran.ParserMonad (FortranVersion(..), evalParse, fromParseResultUnsafe)
import Language.Fortran.AST
import qualified Data.ByteString.Char8 as B

{-# ANN module "HLint: ignore Reduce duplication" #-}

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
    _ -> error "unhandled evalParse"
  where
    paddedSourceCode = B.pack $ "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran77 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran77 "<unknown>"

slParser :: String -> Statement ()
slParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran77Legacy "<unknown>"

blParser :: String -> Block ()
blParser src = evalParse blockParser $ initParseState (B.pack src) Fortran77Legacy "<unknown>"

iParser :: String -> [Block ()]
iParser sourceCode =
  fromParseResultUnsafe $ includeParser Fortran77Legacy (B.pack sourceCode) "<unknown>"

pParser :: String -> ProgramFile ()
pParser source = fromParseResultUnsafe $ legacy77Parser (B.pack source) "<unknown>"

spec :: Spec
spec =
  describe "Fortran 77 Parser" $ do
    describe "IO" $ do
      it "parses 'print *, 9000" $ do
        let expectedSt = StPrint () u starVal $ Just (AList () u [ intGen 9000 ])
        sParser "      print *, 9000" `shouldBe'` expectedSt

      it "parses 'write (UNIT=6, FORMAT=*)" $ do
        let cp1 = ControlPair () u (Just "unit") (intGen 6)
            cp2 = ControlPair () u (Just "format") starVal
            expectedSt = StWrite () u (AList () u [cp1, cp2]) Nothing
        sParser "      write (UNIT=6, FORMAT=*)" `shouldBe'` expectedSt

      it "parses 'endfile i" $
        sParser "      endfile i" `shouldBe'` StEndfile2 () u (varGen "i")

      it "parses 'read *, (x, y(i), i = 1, 10, 2)'" $ do
        let stAssign = StExpressionAssign () u (varGen "i") (intGen 1)
            doSpec = DoSpecification () u stAssign (intGen 10) (Just $ intGen 2)
            impliedDoVars = AList () u [ varGen "x", ExpSubscript () u (varGen "y") (AList () u [ IxSingle () u Nothing $ varGen "i" ])]
            impliedDo = ExpImpliedDo () u impliedDoVars doSpec
            iolist = AList () u [ impliedDo ]
            expectedSt = StRead2 () u starVal (Just iolist)
        sParser "      read *, (x, y(i), i = 1, 10, 2)" `shouldBe'` expectedSt

    it "parses '(x, y(i), i = 1, 10, 2)'" $ do
      let stAssign = StExpressionAssign () u (varGen "i") (intGen 1)
          doSpec = DoSpecification () u stAssign (intGen 10) (Just $ intGen 2)
          impliedDoVars = AList () u [ varGen "x", ExpSubscript () u (varGen "y") (AList () u [ IxSingle () u Nothing $ varGen "i" ])]
          impliedDo = ExpImpliedDo () u impliedDoVars doSpec
      eParser "(x, y(i), i = 1, 10, 2)" `shouldBe'` impliedDo

    it "parses main program unit" $ do
      let decl = DeclVariable () u (varGen "x") Nothing Nothing
          st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
          bl = BlStatement () u Nothing st
          pu = ProgramFile mi77 [ PUMain () u (Just "hello") [ bl ] Nothing ]
      pParser exampleProgram1 `shouldBe'` pu

    it "parses block data unit" $ do
      let decl = DeclVariable () u (varGen "x") Nothing Nothing
          st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
          bl = BlStatement () u Nothing st
          pu = ProgramFile mi77 [ PUBlockData () u (Just "hello") [ bl ] ]
      pParser exampleProgram2 `shouldBe'` pu

    it "parses 'intrinsic cosh, sin'" $ do
      let fun1 = ExpValue () u (ValVariable "cosh")
          fun2 = ExpValue () u (ValVariable "sin")
          st = StIntrinsic () u (AList () u [ fun1, fun2 ])
      sParser "      intrinsic cosh, sin" `shouldBe'` st

    it "parses 'intrinsic real" $ do
      let fun = ExpValue () u (ValVariable "real")
          st = StIntrinsic () u (AList () u [ fun ])
      sParser "      intrinsic real" `shouldBe'` st

    describe "CHARACTER" $ do
      it "parses character literal assignment" $ do
        let rhs = ExpValue () u (ValString "hello 'baby")
            st = StExpressionAssign () u (varGen "xyz") rhs
        sParser "      xyz = 'hello ''baby'" `shouldBe'` st

      it "string concatenation" $ do
        let str1 = ExpValue () u (ValString "hello ")
            str2 = ExpValue () u (ValString "world")
            exp = ExpBinary () u Concatenation str1 str2
        eParser "'hello ' // 'world'" `shouldBe'` exp

    describe "Subscript like" $ do
      it "parses vanilla subscript" $ do
        let exp = ExpSubscript () u (varGen "a") (AList () u [ IxSingle () u Nothing $ varGen "x", IxSingle () u Nothing $ intGen 2, IxSingle () u Nothing $ intGen 3 ])
        eParser "a(x, 2, 3)" `shouldBe'` exp

      it "parses array declarator" $ do
        let dimDecls = [ DimensionDeclarator () u (Just $ intGen 1) (Just $ intGen 2)
                       , DimensionDeclarator () u Nothing (Just $ intGen 15)
                       , DimensionDeclarator () u (Just $ varGen "x") (Just starVal) ]
            decl = DeclArray () u (varGen "a") (AList () u dimDecls) Nothing Nothing
            st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
        sParser "      integer a(1:2, 15, x:*)" `shouldBe'` st

      it "parses character substring" $ do
        let indicies = [ ixSinGen 1, ixSinGen 2, ixSinGen 3 ]
            subExp = ExpSubscript () u (varGen "a")  (AList () u indicies)
            range = IxRange () u Nothing (Just $ intGen 10) Nothing
            exp = ExpSubscript () u subExp (AList () u [ range ])
        eParser "a(1, 2, 3)(:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let exp = ExpSubscript () u (varGen "a") (AList () u [ ixRanGen 5 10 ])
        eParser "a(5:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let range = IxRange () u (Just $ intGen 5) Nothing Nothing
            exp = ExpSubscript () u (varGen "a") (AList () u [ range ])
        eParser "a(5:)" `shouldBe'` exp

      it "parses literal string subscript" $ do
        let range = IxRange () u (Just $ intGen 1) (Just $ intGen 2) Nothing
            exp = ExpSubscript () u (strGen "abc") (AList () u [ range ])
        eParser "'abc'(1:2)" `shouldBe'` exp

    describe "GOTO" $ do
      it "parses computed GOTO with integer expression" $ do
        let exp = ExpBinary () u Multiplication (intGen 42) (intGen 24)
            st = StGotoComputed () u (AList () u [labelGen 10, labelGen 20, labelGen 30]) exp
        sParser "      GOTO (10, 20, 30), 42 * 24" `shouldBe'` st

      let gotoSt = StGotoAssigned () u (varGen "v") (Just (AList () u [labelGen 10, labelGen 20, labelGen 30]))
      it "parses assigned GOTO with comma" $
        sParser "      GOTO v, (10, 20, 30)" `shouldBe'` gotoSt

      it "parses assigned GOTO without comma" $
        sParser "      GOTO v (10, 20, 30)" `shouldBe'` gotoSt

    describe "IMPLICIT" $ do
      it "parses 'implicit none'" $ do
        let st = resetSrcSpan $ StImplicit () u Nothing
        sParser "      implicit none" `shouldBe'` st

      it "parses 'implicit character*30 (a, b, c), integer (a-z, l)" $ do
        let impEls = [ImpCharacter () u "a", ImpCharacter () u "b", ImpCharacter () u "c"]
            sel = Selector () u (Just (intGen 30)) Nothing
            imp1 = ImpList () u (TypeSpec () u TypeCharacter (Just sel)) $ AList () u impEls
            imp2 = ImpList () u (TypeSpec () u TypeInteger Nothing) $ AList () u [ImpRange () u "a" "z", ImpCharacter () u "l"]
            st = StImplicit () u $ Just $ AList () u [imp1, imp2]
        sParser "      implicit character*30 (a, b, c), integer (a-z, l)" `shouldBe'` st

    it "parses 'parameter (pi = 3.14, b = 'X' // 'O', d = k) '" $ do
      let sts = [ DeclVariable () u (varGen "pi") Nothing (Just $ realGen (3.14::Double))
                , let e = ExpBinary () u Concatenation (strGen "X") (strGen "O")
                  in DeclVariable () u (varGen "b") Nothing (Just e)
                , DeclVariable () u (varGen "d") Nothing (Just $ varGen "k") ]
          st = StParameter () u (AList () u sts)
      sParser "      parameter (pi = 3.14, b = 'X' // 'O', d = k)" `shouldBe'` st

    it "parses 'pause 'hello world''" $ do
      let st = StPause () u $ Just $ strGen "hello world"
      sParser "      pause 'hello world'" `shouldBe'` st

    describe "SAVE" $ do
      it "parses 'save /cb/, var, /key/'" $ do
        let saveArgs = [ varGen "cb", varGen "var", varGen "key" ]
            st = StSave () u (Just $ AList () u saveArgs)
        sParser "      save /cb/, var, /key/" `shouldBe'` st

      it "parses 'save'" $
        sParser "      save" `shouldBe'` StSave () u Nothing

    it "parses '.true. .eqv. f(42) .neqv. x'" $ do
      let arg2 = ExpSubscript () u (varGen "f") $ AList () u [ ixSinGen 42 ]
          arg3 = varGen "x"
          subexp = ExpBinary () u Equivalent valTrue arg2
          exp = ExpBinary () u NotEquivalent subexp arg3
      eParser ".true. .eqv. f(42) .neqv. x" `shouldBe'` exp

    it "parses 'entry me (a,b,*)'" $ do
      let func = ExpValue () u (ValVariable "me")
          args = [ varGen "a", varGen "b", starVal ]
          st = StEntry () u func (Just $ AList () u args) Nothing
      sParser "      entry me (a,b,*)" `shouldBe'` st

    it "parses 'character a*8'" $ do
      let decl = DeclVariable () u (varGen "a") (Just $ intGen 8) Nothing
          typeSpec = TypeSpec () u TypeCharacter Nothing
          st = StDeclaration () u typeSpec Nothing (AList () u [ decl ])
      sParser "      character a*8" `shouldBe'` st

    it "parses 'character c*(ichar('A'))" $ do
      let args = AList () u [ IxSingle () u Nothing (ExpValue () u (ValString "A")) ]
          lenExpr = ExpSubscript () u (ExpValue () u (ValVariable "ichar")) args
          decl = DeclVariable () u (varGen "c") (Just $ lenExpr) Nothing
          typeSpec = TypeSpec () u TypeCharacter Nothing
          st = StDeclaration () u typeSpec Nothing (AList () u [ decl ])
      sParser "      character c*(ichar('A'))" `shouldBe'` st

    it "parses included files" $ do
      let decl = DeclVariable () u (varGen "a") Nothing Nothing
          typeSpec = TypeSpec () u TypeInteger Nothing
          st = StDeclaration () u typeSpec Nothing (AList () u [ decl ])
          bl = BlStatement () u Nothing st
      iParser "      integer a" `shouldBe'` [bl]

    describe "parses if blocks" $ do
      let printArgs  = Just $ AList () u [ExpValue () u $ ValString "foo"]
          printStmt  = StPrint () u (ExpValue () u ValStar) printArgs
          printBlock = BlStatement () u Nothing printStmt
          trueLit = ExpValue () u $ ValLogical ".true."
      it "unlabelled" $ do
        let bl = BlIf () u Nothing Nothing [ Just trueLit, Nothing ] [[printBlock], [printBlock]]  Nothing
            src = unlines [ "      if (.true.) then"
                          , "        print *, 'foo'"
                          , "      else"
                          , "        print *, 'foo'"
                          , "       endif"
                          ]
        blParser src `shouldBe'` bl
      it "labelled" $ do
        let label str = Just $ ExpValue () u $ ValInteger str
            bl = BlIf () u (label "10")  Nothing [Just trueLit, Nothing] [[printBlock], [printBlock]] (label "30")
            src = unlines [ "10    if (.true.) then"
                          , "        print *, 'foo'"
                          , "20    else"
                          , "        print *, 'foo'"
                          , "30     endif"
                          ]
        blParser src `shouldBe'` bl

    describe "Legacy Extensions" $ do
      it "parses structure/union/map blocks" $ do
        let src = init
                $ unlines [ "      structure /foo/"
                          , "        union"
                          , "          map"
                          , "            integer i"
                          , "          end map"
                          , "          map"
                          , "            real r"
                          , "          end map"
                          , "        end union"
                          , "      end structure"]
            ds = [ UnionMap () u $ AList () u
                   [StructFields () u (TypeSpec () u TypeInteger Nothing) Nothing $
                    AList () u [DeclVariable () u (varGen "i") Nothing Nothing]]
                 , UnionMap () u $ AList () u
                   [StructFields () u (TypeSpec () u TypeReal Nothing) Nothing $
                    AList () u [DeclVariable () u (varGen "r") Nothing Nothing]]
                 ]
            st = StStructure () u (Just "foo") $ AList () u [StructUnion () u $ AList () u ds]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses structure/union/map blocks with comments" $ do
        let src = init
                $ unlines [ "      structure /foo/"
                          , "C       comment before union"
                          , "        union"
                          , "C         comment inside union, before map"
                          , "          map"
                          , "C           comment inside map"
                          , "            integer i ! more comment"
                          , "          end map"
                          , "C         comment between maps"
                          , "          map"
                          , "            real r    ! more comment"
                          , "          end map"
                          , "C         comment after map"
                          , "        end union"
                          , "C       comment after union"
                          , "      end structure"]
            ds = [ UnionMap () u $ AList () u
                   [StructFields () u (TypeSpec () u TypeInteger Nothing) Nothing $
                    AList () u [DeclVariable () u (varGen "i") Nothing Nothing]]
                 , UnionMap () u $ AList () u
                   [StructFields () u (TypeSpec () u TypeReal Nothing) Nothing $
                    AList () u [DeclVariable () u (varGen "r") Nothing Nothing]]
                 ]
            st = StStructure () u (Just "foo") $ AList () u [StructUnion () u $ AList () u ds]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses nested structure blocks" $ do
        let src = init
                $ unlines [ "      structure /foo/"
                          , "        structure /bar/ baz"
                          , "          integer qux"
                          , "        end structure"
                          , "      end structure"]
            var = DeclVariable () u (varGen "qux") Nothing Nothing
            innerst = StructStructure () u (Just "bar") ("baz")
              $ AList () u [StructFields () u (TypeSpec () u TypeInteger Nothing) Nothing
                $ AList () u [var]]
            st = StStructure () u (Just "foo") $ AList () u [innerst]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses structure data references " $ do
        let src = init $ unlines [ "      print *, foo % bar"
                                 , "      print *, foo.bar" ]
            expStar = ExpValue () u ValStar
            foobar = ExpDataRef () u (ExpValue () u (ValVariable "foo")) (ExpValue () u (ValVariable "bar"))
            blStmt = BlStatement () u Nothing $ StPrint () u expStar $ Just $ AList () u [foobar]
        resetSrcSpan (iParser src) `shouldBe` [ blStmt, blStmt ]

      it "parse special intrinsics to arguments" $ do
        let blStmt stmt = BlStatement () u Nothing stmt
            var = ExpValue () u . ValVariable
            ext = blStmt $ StExternal () u $ AList () u [var "bar"]
            arg = Just . AList () u . pure . Argument () u Nothing
            valBar = ExpFunctionCall () u (ExpValue () u (ValIntrinsic "%val"))
                     $ arg $ var "baz"
            call = blStmt $ StCall () u (var "bar") $ arg valBar
            pu = ProgramFile mi77 [ PUSubroutine () u (Nothing, Nothing) "foo"
                                   (Just $ AList () u [var "baz"]) [ ext, call ] Nothing ]
        resetSrcSpan (pParser exampleProgram3) `shouldBe` pu

      it "parses character declarations with unspecfied lengths" $ do
        let src = "      character s*(*)"
            st = StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing $
                 AList () u [DeclVariable () u
                               (ExpValue () u (ValVariable "s"))
                               (Just (ExpValue () u ValStar))
                               Nothing]
        resetSrcSpan (slParser src) `shouldBe` st

      it "parses array initializers" $ do
        let src = "      integer xs(3) / 1, 2, 3 /"
            inits = [ExpValue () u (ValInteger "1"), ExpValue () u (ValInteger "2"), ExpValue () u (ValInteger "3")]
            st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing $
                 AList () u [DeclArray () u
                               (ExpValue () u (ValVariable "xs"))
                               (AList () u [DimensionDeclarator () u Nothing (Just (ExpValue () u (ValInteger "3")))])
                               Nothing
                               (Just (ExpInitialisation () u $ AList () u inits))]
        resetSrcSpan (slParser src) `shouldBe` st

        let src1 = "      character xs(2)*5 / 'hello', 'world' /"
            inits1 = [ExpValue () u (ValString "hello"), ExpValue () u (ValString "world")]
            st1 = StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing $
                 AList () u [DeclArray () u
                               (ExpValue () u (ValVariable "xs"))
                               (AList () u [DimensionDeclarator () u Nothing (Just (ExpValue () u (ValInteger "2")))])
                               (Just (ExpValue () u (ValInteger "5")))
                               (Just (ExpInitialisation () u $ AList () u inits1))]
        resetSrcSpan (slParser src1) `shouldBe` st1

        let src2 = "      character xs*5(2) / 'hello', 'world' /"
            inits2 = [ExpValue () u (ValString "hello"), ExpValue () u (ValString "world")]
            st2 = StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing $
                 AList () u [DeclArray () u
                               (ExpValue () u (ValVariable "xs"))
                               (AList () u [DimensionDeclarator () u Nothing (Just (ExpValue () u (ValInteger "2")))])
                               (Just (ExpValue () u (ValInteger "5")))
                               (Just (ExpInitialisation () u $ AList () u inits2))]
        resetSrcSpan (slParser src2) `shouldBe` st2

      it "parses subscripts in assignments" $ do
        let mkIdx i = IxSingle () u Nothing (ExpValue () u (ValInteger i))

            src = "      x(0,1) = 0"
            tgt = ExpSubscript () u (ExpValue () u (ValVariable "x")) (AList () u [mkIdx "0", mkIdx "1"])
            st = StExpressionAssign () u tgt (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src) `shouldBe` st

        let src1 = "      x(0).foo = 0"
            tgt1 = ExpDataRef () u (ExpSubscript () u (ExpValue () u (ValVariable "x")) (AList () u [mkIdx "0"])) (ExpValue () u (ValVariable "foo"))
            st1 = StExpressionAssign () u tgt1 (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src1) `shouldBe` st1

        let src2 = "      x.foo = 0"
            tgt2 = ExpDataRef () u (ExpValue () u (ValVariable "x")) (ExpValue () u (ValVariable "foo"))
            st2 = StExpressionAssign () u tgt2 (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src2) `shouldBe` st2

        let src3 = "      x.foo(0) = 0"
            tgt3 = ExpSubscript () u (ExpDataRef () u (ExpValue () u (ValVariable "x")) (ExpValue () u (ValVariable "foo"))) (AList () u [mkIdx "0"])
            st3 = StExpressionAssign () u tgt3 (ExpValue () u (ValInteger "0"))
        resetSrcSpan (slParser src3) `shouldBe` st3
      it "parses automatic and static statements" $ do
        let decl = DeclVariable () u (varGen "x") Nothing Nothing
            autoStmt = StAutomatic () u (AList () u [decl])
            staticStmt = StStatic () u (AList () u [decl])
            autoSrc =  "      automatic x"
            staticSrc = "      static x"
        resetSrcSpan (slParser autoSrc) `shouldBe` autoStmt
        resetSrcSpan (slParser staticSrc) `shouldBe` staticStmt

exampleProgram1 :: String
exampleProgram1 = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

exampleProgram2 :: String
exampleProgram2 = unlines
  [ "      block data hello"
  , "      integer x"
  , "      end" ]

exampleProgram3 :: String
exampleProgram3 = unlines
  [ "      subroutine foo(baz)"
  , "      external bar"
  , "      call bar(%val(baz))"
  , "      end subroutine foo"]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:

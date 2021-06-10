module Language.Fortran.Parser.Fortran95Spec (spec) where

import Prelude hiding (GT, EQ, exp, pred)

import TestUtil
import Test.Hspec
import Control.Exception (evaluate)

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Lexer.FreeForm
import Language.Fortran.Parser.Fortran95
import qualified Data.List as List
import Data.Foldable(forM_)
import qualified Data.ByteString.Char8 as B

{-# ANN module "HLint: ignore Reduce duplication" #-}

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
    _ -> error "unhandled evalParse"
  where
    paddedSourceCode = B.pack $ "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran95 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran95 "<unknown>"

blParser :: String -> Block ()
blParser sourceCode =
  evalParse blockParser $ initParseState (B.pack sourceCode) Fortran95 "<unknown>"

fParser :: String -> ProgramUnit ()
fParser sourceCode =
  evalParse functionParser $ initParseState (B.pack sourceCode) Fortran95 "<unknown>"

{- Useful for parser debugging; Lexes the given source code.
fTok :: String -> [Token]
fTok sourceCode = collectFreeTokens Fortran95 $ B.pack sourceCode
-}

{-
 - Given a list of values, find every combination of those values:
 - combination [1,2] = [[], [1], [2], [1,2], [2,1]]
 -}
combination :: [a] -> [[a]]
combination = foldr ((++) . List.permutations) [] . List.subsequences

spec :: Spec
spec =
  describe "Fortran 95 Parser" $ do
    describe "Function" $ do
      let puFunction = PUFunction () u
          fType = Nothing
          fSuf = emptySuffixes
          fPreSuf = emptyPrefixSuffix
          fName = "f"
          fArgs = Nothing
          fRes = Nothing
          fBody = []
          fSub = Nothing

      describe "End" $ do
        it "parses simple functions ending with \"end function [function name]\"" $ do
          let expected = puFunction fType fPreSuf fName fArgs fRes fBody fSub
              fStr = init $ unlines ["function f()"
                               , "end function f" ]
          fParser fStr `shouldBe'` expected

        it "parses simple functions ending with \"end\"" $ do
          let expected = puFunction fType fPreSuf fName fArgs fRes fBody fSub
              fStr = init $ unlines ["function f()"
                               , "end" ]
          fParser fStr `shouldBe'` expected

        it "parses simple functions ending with \"end function\"" $ do
          let expected = puFunction fType fPreSuf fName fArgs fRes fBody fSub
              fStr = init $ unlines ["function f()"
                               , "end function" ]
          fParser fStr `shouldBe'` expected


        it "parses functions with return type specs" $ do
          let fType' = Just $ TypeSpec () u TypeInteger Nothing
              expected = puFunction fType' fPreSuf fName fArgs fRes fBody fSub
              fStr = init $ unlines ["integer function f()"
                               , "end function f" ]
          fParser fStr `shouldBe'` expected

      describe "parses function options (recursive, pure, elemental)" $ do
        let options_list = map unzip $ combination
                                        [ ("recursive ", PfxRecursive () u)
                                        , ("pure ", PfxPure () u)
                                        , ("elemental ", PfxElemental () u) ]

        forM_ options_list (\(strs, opts) -> do
          let isElem (PfxElemental {}) = True; isElem _ = False
              isRec  (PfxRecursive {}) = True; isRec _  = False
              str = concat strs
              fStr = str ++ init (unlines ["function f()", "end"])
              pfx = fromList' () opts
          --let expected = puFunction fType
          if any isElem opts && any isRec opts
            then
              it ("Shouldn't parse: " ++ show fStr ++ ": " ++ show opts) $
                evaluate (fParser fStr) `shouldThrow` anyIOException
            else
              it ("Should parse: " ++ show fStr ++ ": " ++ show opts) $ do
                let expected' = puFunction fType (pfx, fSuf) fName fArgs fRes fBody fSub
                fParser fStr `shouldBe'` expected'
          )

      it "parses functions with a list of arguments" $ do
        let fArgs' = Just $ AList () u [ varGen "x", varGen "y", varGen "z" ]
            expected = puFunction fType fPreSuf fName fArgs' fRes fBody fSub
            fStr = init $ unlines ["function f(x, y, z)"
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

      it "parses functions with a result variable" $ do
        let fRes' = Just $ varGen "i"
            expected = puFunction fType fPreSuf fName fArgs fRes' fBody fSub
            fStr = init $ unlines ["function f() result(i)"
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

      it "parses functions with function bodies" $ do
        let decrementRHS = ExpBinary () u Subtraction (varGen "i") (intGen 1)
            f1 = StPrint () u starVal (Just $ AList () u [ varGen "i" ])
            f2 = StExpressionAssign () u (varGen "i") decrementRHS
            fBody' = [ BlStatement () u Nothing f1 , BlStatement () u Nothing f2 ]
            expected = puFunction fType fPreSuf fName fArgs fRes fBody' fSub
            fStr = init $ unlines ["function f()"
                             , "  print *, i"
                             , "  i = (i - 1)"
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

      it "parses complex functions" $ do
        let fType' = Just $ TypeSpec () u TypeInteger Nothing
            fArgs' = Just $ AList () u [ varGen "x", varGen "y", varGen "z" ]
            fRes' = Just $ varGen "i"
            decrementRHS = ExpBinary () u Subtraction (varGen "i") (intGen 1)
            f1 = StPrint () u starVal (Just $ AList () u [ varGen "i" ])
            f2 = StExpressionAssign () u (varGen "i") decrementRHS
            fBody' = [ BlStatement () u Nothing f1 , BlStatement () u Nothing f2 ]
            expected = puFunction fType' fPreSuf fName fArgs' fRes' fBody' fSub
            fStr = init $ unlines [ "integer function f(x, y, z) result(i)"
                             , "  print *, i"
                             , "  i = (i - 1)"
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

    describe "Expression" $ do
      it "parses logial literals with kind" $ do
        let expected = ExpValue () u (ValLogical ".true._kind")
        eParser ".true._kind" `shouldBe'` expected

      it "parses array initialisation exp" $ do
        let list = AList () u [ intGen 1, intGen 2, intGen 3, intGen 4 ]
        eParser "(/ 1, 2, 3, 4 /)" `shouldBe'` ExpInitialisation () u list

      describe "Custom operator" $ do
        let unOp = UnCustom ".inverse."
            unExp = ExpUnary () u unOp $ intGen 42

        it "parses unary custom operator" $
          eParser ".inverse. 42" `shouldBe'` unExp

        let binOp = BinCustom ".xor."
        it "parses binary custom operator" $ do
          let expected = ExpBinary () u binOp (intGen 24) (intGen 42)
          eParser "24 .xor. 42" `shouldBe'` expected

        it "parses mixed unary custom operator" $ do
          let binExp = ExpBinary () u binOp unExp (intGen 24)
          eParser ".inverse. 42 .xor. 24" `shouldBe'` binExp

        it "parses data ref" $ do
          let range = fromList () [ IxSingle () u Nothing $ intGen 10 ]
              sub = ExpSubscript () u (varGen "y") range
              innerRefExp = ExpDataRef () u (varGen "x") sub
              exp = ExpDataRef () u innerRefExp (varGen "z")
          eParser "x % y(10) % z" `shouldBe'` exp

        it "parses section subscript" $ do
          let range = [ IxSingle () u Nothing $ intGen 10
                      , IxRange () u Nothing (Just $ intGen 1) (Just $ intGen 2)
                      , IxSingle () u Nothing $ varGen "y" ]
              exp = ExpSubscript () u (varGen "x") (fromList () range)
          eParser "x (10, : 1 : 2, y)" `shouldBe'` exp

    describe "Statement" $ do
      it "data ref assignment" $ do
        let indicies = AList () u [ IxSingle () u Nothing (intGen 1) ]
            subs = ExpSubscript () u (varGen "x") indicies
            lhs = ExpDataRef () u subs (varGen "y")
            st = StExpressionAssign () u lhs (intGen 1)
        sParser "x(1) % y = 1" `shouldBe'` st

      it "doesn't parse assign statements" $ do
        let stStr = "ASSIGN 1 \"LABEL\""
        evaluate (sParser stStr) `shouldThrow` anyIOException

      it "doesn't parse pause statements" $ do
        let stStr = "PAUSE"
        evaluate (sParser stStr) `shouldThrow` anyIOException

      it "doesn't parse pause statements with expression" $ do
        let stStr = "PAUSE \"MESSAGE\""
        evaluate (sParser stStr) `shouldThrow` anyIOException

      it "parses declaration with attributes" $ do
        let typeSpec = TypeSpec () u TypeReal Nothing
            attrs = AList () u [ AttrExternal () u
                               , AttrIntent () u Out
                               , AttrDimension () u $ AList () u
                                  [ DimensionDeclarator () u
                                      (Just $ intGen 3) (Just $ intGen 10)
                                  ]
                               ]
            declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing
              , DeclVariable () u (varGen "y") Nothing Nothing ]
            expected = StDeclaration () u typeSpec (Just attrs) declarators
            stStr = "real, external, intent (out), dimension (3:10) :: x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with old syntax" $ do
        let typeSpec = TypeSpec () u TypeLogical Nothing
            declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing
              , DeclVariable () u (varGen "y") Nothing Nothing ]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "logical x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with initialisation" $ do
        let typeSpec = TypeSpec () u TypeComplex Nothing
            init' = ExpValue () u (ValComplex (intGen 24) (realGen (42.0::Double)))
            declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing (Just init') ]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "complex :: x = (24, 42.0)"
        sParser stStr `shouldBe'` expected

      it "parses declaration of custom type" $ do
        let typeSpec = TypeSpec () u (TypeCustom "meinetype") Nothing
            declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing ]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "type (MeineType) :: x"
        sParser stStr `shouldBe'` expected

      it "parses declaration type with kind selector" $ do
        let selector = Selector () u Nothing (Just $ varGen "hello")
            typeSpec = TypeSpec () u TypeInteger (Just selector)
            declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing ]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "integer (hello) :: x"
        sParser stStr `shouldBe'` expected

      it "parses intent statement" $ do
        let stStr = "intent (inout) :: a"
            expected = StIntent () u InOut (fromList () [ varGen "a" ])
        sParser stStr `shouldBe'` expected

      it "parses optional statement" $ do
        let stStr = "optional x"
            expected = StOptional () u (fromList () [ varGen "x" ])
        sParser stStr `shouldBe'` expected

      it "parses public statement" $ do
        let stStr = "public :: x"
            expected = StPublic () u (Just $ fromList () [ varGen "x" ])
        sParser stStr `shouldBe'` expected

      it "parses public assignment" $ do
        let expected = StPublic () u (Just $ fromList () [ assVal ])
        sParser "public :: assignment (=)" `shouldBe'` expected

      it "parses private statement" $
        sParser "private" `shouldBe'` StPrivate () u Nothing

      it "parses private operator" $ do
        let expected = StPrivate () u (Just $ fromList () [ opGen "*" ])
        sParser "private operator ( * )" `shouldBe'` expected

      it "parses save statement" $ do
        let list = [ varGen "hello", varGen "bye" ]
            expected = StSave () u (Just $ fromList () list)
            stStr = "save /hello/, bye"
        sParser stStr `shouldBe'` expected

      it "parses parameter statement" $ do
        let ass1 = DeclVariable () u (varGen "x") Nothing (Just $ intGen 10)
            ass2 = DeclVariable () u (varGen "y") Nothing (Just $ intGen 20)
            expected = StParameter () u (fromList () [ ass1, ass2 ])
        sParser "parameter (x = 10, y = 20)" `shouldBe'` expected

      describe "FORALL blocks" $ do
        let stride = Just $ ExpBinary () u NE (varGen "i") (intGen 2)
            tripletSpecList = [("i", intGen 1, varGen "n", stride)]

        it "parses basic FORALL blocks" $ do
          let stStr = "FORALL (I=1:N, I /= 2)"
              expected = StForall () u Nothing (ForallHeader tripletSpecList Nothing)
          sParser stStr `shouldBe'` expected

      describe "FORALL statements" $ do
        let stride = Just $ ExpBinary () u NE (varGen "i") (intGen 2)
            tripletSpecList = [("i", intGen 1, varGen "n", stride)]
        --let varI = IxSingle () u Nothing (varGen "i")
        --let expSub1 = ExpSubscript () u (varGen "a") (AList () u [varI, varI])
        --let expSub2 = ExpSubscript () u (varGen "x") (AList () u [varI])
        --let eAssign = StExpressionAssign () u expSub1 expSub2

        it "parses basic FORALL statements" $ do
          let stStr = "FORALL (I=1:N, I /= 2)" -- A(I,I) = X(I)"
              expected = StForall () u Nothing (ForallHeader tripletSpecList Nothing)-- eAssign
          sParser stStr `shouldBe'` expected

      describe "ENDFORALL statements" $ do
        it "parses FORALL end statements" $ do
          let stStr = "ENDFORALL"
              expected = StEndForall () u Nothing
          sParser stStr `shouldBe'` expected

        it "parses FORALL end statements with label" $ do
          let stStr = "ENDFORALL A"
              expected = StEndForall () u $ Just "a"
          sParser stStr `shouldBe'` expected

      describe "Implicit" $ do
        it "parses implicit none" $ do
          let st = StImplicit () u Nothing
          sParser "implicit none" `shouldBe'` st

        it "parses implicit with single" $ do
          let typeSpec = TypeSpec () u TypeCharacter Nothing
              impEls = [ ImpCharacter () u "k" ]
              impLists = [ ImpList () u typeSpec (fromList () impEls) ]
              st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit character (k)" `shouldBe'` st

        it "parses implicit with range" $ do
          let typeSpec = TypeSpec () u TypeLogical Nothing
              impEls = [ ImpRange () u "x" "z" ]
              impLists = [ ImpList () u typeSpec (fromList () impEls) ]
              st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit logical (x-z)" `shouldBe'` st

        it "parses implicit statement" $ do
          let typeSpec1 = TypeSpec () u TypeCharacter Nothing
              typeSpec2 = TypeSpec () u TypeInteger Nothing
              impEls1 = [ ImpCharacter () u "s", ImpCharacter () u "a" ]
              impEls2 = [ ImpRange () u "x" "z" ]
              impLists = [ ImpList () u typeSpec1 (fromList () impEls1)
                         , ImpList () u typeSpec2 (fromList () impEls2) ]
              st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit character (s, a), integer (x-z)" `shouldBe'` st

      describe "Data" $ do
        it "parses vanilla" $ do
          let nlist = fromList () [ varGen "x", varGen "y" ]
              vlist = fromList () [ intGen 1, intGen 2 ]
              list = [ DataGroup () u nlist vlist ]
              expected = StData () u (fromList () list)
              stStr = "data x,y/1,2/"
          sParser stStr `shouldBe'` expected

        describe "Delimeter" $ do
          let [ nlist1, vlist1 ] =
                map (fromList () . return) [ varGen "x", intGen 1 ]
              [ nlist2, vlist2 ] =
                map (fromList () . return) [ varGen "y", intGen 2 ]
              list = [ DataGroup () u nlist1 vlist1
                     , DataGroup () u nlist2 vlist2 ]
              expected = StData () u (fromList () list)

          it "parses comma delimited init groups" $
            sParser "data x/1/, y/2/" `shouldBe'` expected

          it "parses non-comma delimited init groups" $
            sParser "data x/1/ y/2/" `shouldBe'` expected

      describe "Namelist" $ do
        let groupNames = [ ExpValue () u (ValVariable "something")
                         , ExpValue () u (ValVariable "other") ]
            itemss = [ fromList () [ varGen "a", varGen "b", varGen "c" ]
                     , fromList () [ varGen "y" ] ]
            st = StNamelist () u $
              fromList () [ Namelist () u (head groupNames) (head itemss)
                          , Namelist () u (last groupNames) (last itemss) ]

        it "parses namelist statement (comma delimited) (1)" $
          sParser "namelist /something/a,b,c,/other/y" `shouldBe'` st

        it "parses namelist statement (2)" $
          sParser "namelist /something/a,b,c/other/y" `shouldBe'` st

      describe "Common" $ do
        let commonNames = [ ExpValue () u (ValVariable "something")
                          , ExpValue () u (ValVariable "other") ]
            itemss = [ fromList () [ varGen "a", varGen "b", varGen "c" ]
                     , fromList () [ varGen "y" ] ]
            st = StCommon () u $ fromList ()
              [ CommonGroup () u Nothing (fromList () [ varGen "q" ])
              , CommonGroup () u (Just $ head commonNames) (head itemss)
              , CommonGroup () u (Just $ last commonNames) (last itemss) ]

        it "parses common statement (comma delimited) (1)" $
          sParser "common q /something/a,b,c, /other/y" `shouldBe'` st

        it "parses common statement (2)" $
          sParser "common q /something/a,b,c /other/y" `shouldBe'` st

      it "parses equivalence statement" $ do
        let eqALists = fromList ()
              [ fromList ()
                  [ let indicies = fromList () [ IxSingle () u Nothing (intGen 1) ]
                    in ExpSubscript () u (varGen "a") indicies
                  , varGen "x"
                  ]
              , fromList ()
                  [ varGen "y"
                  , varGen "z"
                  , let indicies = fromList () [ IxRange () u (Just $ intGen 1)
                                                              (Just $ intGen 42)
                                                              Nothing ]
                    in ExpSubscript () u (varGen "d") indicies
                  ]
              ]
            st = StEquivalence () u eqALists
        sParser "equivalence (a(1), x), (y, z, d(1:42))" `shouldBe'` st

      describe "Dynamic allocation" $ do
        it "parses allocate statement" $ do
          let opt = AOStat () u (varGen "a")
              allocs = fromList ()
                [ varGen "x"
                , ExpDataRef () u (varGen "st") (varGen "part")
                ]
              s = StAllocate () u Nothing allocs (Just (AList () u [opt]))
          sParser "allocate (x, st % part, STAT = a)" `shouldBe'` s

        it "parses deallocate statement" $ do
          let allocs = fromList ()
                [ let indicies = fromList () [ IxSingle () u Nothing (intGen 20) ]
                  in ExpSubscript () u (varGen "smt") indicies
                ]
              s = StDeallocate () u allocs Nothing
          sParser "deallocate (smt ( 20 ))" `shouldBe'` s

        it "parses nullify statement" $ do
          let s = StNullify () u (fromList () [ varGen "x" ])
          sParser "nullify (x)" `shouldBe'` s

      it "parses pointer assignment" $ do
        let src = ExpDataRef () u (varGen "x") (varGen "y")
            st = StPointerAssign () u src (varGen "exp")
        sParser "x % y => exp" `shouldBe'` st

      describe "Where" $ do
        it "parses where statement" $ do
          let exp = ExpBinary () u Subtraction (varGen "temp") (varGen "r_temp")
              pred = ExpBinary () u GT (varGen "temp") (intGen 100)
              assignment = StExpressionAssign () u (varGen "temp") exp
              st = StWhere () u pred assignment
          sParser "where (temp > 100) temp = temp - r_temp"`shouldBe'` st

        describe "Where block" $ do
          it "parses where construct statement" $
            sParser "where (.true.)" `shouldBe'` StWhereConstruct () u Nothing valTrue

          it "parses elsewhere statement" $
            sParser "elsewhere" `shouldBe'` StElsewhere () u Nothing Nothing

          it "parses elsewhere statement" $ do
            let exp = ExpBinary () u GT (varGen "a") (varGen "b")
            sParser "elsewhere (a > b)" `shouldBe'` StElsewhere () u Nothing (Just exp)

          it "parses endwhere statement" $
            sParser "endwhere" `shouldBe'` StEndWhere () u Nothing

    describe "If" $ do
      let stPrint = StPrint () u starVal (Just $ fromList () [ ExpValue () u (ValString "foo")])
      it "parser if block" $
        let ifBlockSrc = unlines [ "if (.false.) then", "print *, 'foo'", "end if"]
            falseLit = ExpValue () u (ValLogical ".false.")
        in blParser ifBlockSrc `shouldBe'` BlIf () u Nothing Nothing [Just falseLit] [[BlStatement () u Nothing stPrint]] Nothing

      it "parses named if block" $ do
        let ifBlockSrc = unlines [ "mylabel : if (.true.) then", "print *, 'foo'", "end if mylabel"]
            trueLit = ExpValue () u (ValLogical ".true.")
            ifBlock = BlIf () u Nothing (Just "mylabel") [Just trueLit] [[BlStatement () u Nothing stPrint]] Nothing
        blParser ifBlockSrc `shouldBe'` ifBlock

      it "parses logical if statement" $ do
        let assignment = StExpressionAssign () u (varGen "a") (varGen "b")
            stIf = StIfLogical () u valTrue assignment
        sParser "if (.true.) a = b" `shouldBe'` stIf

      it "parses arithmetic if statement" $ do
        let stIf = StIfArithmetic () u (varGen "x") (intGen 1)
                                                    (intGen 2)
                                                    (intGen 3)
        sParser "if (x) 1, 2, 3" `shouldBe'` stIf

    describe "Case" $ do
      it "parses select case statement" $ do
        let st = StSelectCase () u Nothing (varGen "n")
        sParser "select case (n)" `shouldBe'` st

      it "parses select case statement with construct name" $ do
        let st = StSelectCase () u (Just "case") (varGen "n")
        sParser "case: select case (n)" `shouldBe'` st

      it "parses case statement" $ do
        let ranges = AList () u [ IxRange () u (Just $ intGen 42) Nothing Nothing ]
        sParser "case (42:)" `shouldBe'` StCase () u Nothing (Just ranges)

      it "parses case statement" $
        sParser "case default" `shouldBe'` StCase () u Nothing Nothing

      it "parses end select statement" $ do
        let st = StEndcase () u (Just "name")
        sParser "end select name" `shouldBe'` st

    describe "Do" $ do
      it "parses do statement with label" $ do
        let assign = StExpressionAssign () u (varGen "i") (intGen 0)
            doSpec = DoSpecification () u assign (intGen 42) Nothing
            st = StDo () u Nothing (Just $ intGen 24) (Just doSpec)
        sParser "do 24, i = 0, 42" `shouldBe'` st

      it "parses do statement without label" $ do
        let assign = StExpressionAssign () u (varGen "i") (intGen 0)
            doSpec = DoSpecification () u assign (intGen 42) Nothing
            st = StDo () u Nothing Nothing (Just doSpec)
        sParser "do i = 0, 42" `shouldBe'` st

      it "parses infinite do" $ do
        let st = StDo () u Nothing Nothing Nothing
        sParser "do" `shouldBe'` st

      it "parses end do statement" $ do
        let st = StEnddo () u (Just "constructor")
        sParser "end do constructor" `shouldBe'` st

      it "parses end do while statement" $ do
        let st = StDoWhile () u (Just "name") Nothing valTrue
        sParser "name: do while (.true.)" `shouldBe'` st

    describe "Goto" $ do
      it "parses vanilla goto" $ do
        let st = StGotoUnconditional () u (intGen 999)
        sParser "goto 999" `shouldBe'` st

      it "parses computed goto" $ do
        let list = fromList () [ intGen 10, intGen 20, intGen 30 ]
            st = StGotoComputed () u list (intGen 20)
        sParser "goto (10, 20, 30) 20" `shouldBe'` st

      it "doesn't parse assigned goto" $
        evaluate (sParser "goto i, (10, 20, 30)") `shouldThrow` anyIOException

      it "doesn't parse label assignment" $
        evaluate (sParser "assign 20 to l") `shouldThrow` anyIOException

    describe "IO" $ do
      it "parses vanilla print" $ do
        let st = StPrint () u starVal (Just $ fromList () [ varGen "hex" ])
        sParser "print *, hex" `shouldBe'` st

      it "parses write with implied do" $ do
        let cp1 = ControlPair () u Nothing (intGen 10)
            cp2 = ControlPair () u (Just "format") (varGen "x")
            ciList = fromList () [ cp1, cp2 ]
            assign = StExpressionAssign () u (varGen "i") (intGen 1)
            doSpec = DoSpecification () u assign (intGen 42) (Just $ intGen 2)
            alist = fromList () [ varGen "i", varGen "j" ]
            outList = fromList () [ ExpImpliedDo () u alist doSpec ]
            st = StWrite () u ciList (Just outList)
        sParser "write (10, FORMAT = x) (i, j,  i = 1, 42, 2)" `shouldBe'` st

    it "parses use statement" $ do
      let renames = fromList ()
            [ UseRename () u (varGen "sprod") (varGen "prod")
            , UseRename () u (varGen "a") (varGen "b") ]
      let st = StUse () u (varGen "stats_lib") Nothing Permissive (Just renames)
      sParser "use stats_lib, sprod => prod, a => b" `shouldBe'` st

    it "parses value decl" $ do
      let decls = [DeclVariable () u (varGen "a") Nothing Nothing, DeclVariable () u (varGen "b") Nothing Nothing]
      let st = StValue () u (AList () u decls)
      sParser "value a, b" `shouldBe'` st
      sParser "value :: a, b" `shouldBe'` st

    it "parses value attribute" $ do
      let decls = [DeclVariable () u (varGen "a") Nothing Nothing, DeclVariable () u (varGen "b") Nothing Nothing]
      let ty = TypeSpec () u TypeInteger Nothing
      let attrs = [AttrValue () u]
      let st = StDeclaration () u ty (Just (AList () u attrs)) (AList () u decls)
      sParser "integer, value :: a, b" `shouldBe'` st

    it "parses volatile decl" $ do
      let decls = [DeclVariable () u (varGen "a") Nothing Nothing, DeclVariable () u (varGen "b") Nothing Nothing]
      let st = StVolatile () u (AList () u decls)
      sParser "volatile a, b" `shouldBe'` st
      sParser "volatile :: a, b" `shouldBe'` st

    it "parses volatile attribute" $ do
      let decls = [DeclVariable () u (varGen "a") Nothing Nothing, DeclVariable () u (varGen "b") Nothing Nothing]
      let ty = TypeSpec () u TypeInteger Nothing
      let attrs = [AttrVolatile () u]
      let st = StDeclaration () u ty (Just (AList () u attrs)) (AList () u decls)
      sParser "integer, volatile :: a, b" `shouldBe'` st

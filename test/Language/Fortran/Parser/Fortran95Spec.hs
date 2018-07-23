module Language.Fortran.Parser.Fortran95Spec (spec) where

import Prelude hiding (GT, EQ)

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

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
  where
    paddedSourceCode = B.pack $ "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran95 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState (B.pack sourceCode) Fortran95 "<unknown>"

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
      let fType = Nothing
      let fOpt = None () u False
      let fName = "f"
      let fArgs = Nothing
      let fRes = Nothing
      let fBody = []
      let fSub = Nothing

      describe "End" $ do
        it "parses simple functions ending with \"end function [function name]\"" $ do
          let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
          let fStr = init $ unlines ["function f()"
                               , "end function f" ]
          fParser fStr `shouldBe'` expected

        it "parses simple functions ending with \"end\"" $ do
          let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
          let fStr = init $ unlines ["function f()"
                               , "end" ]
          fParser fStr `shouldBe'` expected

        it "parses simple functions ending with \"end function\"" $ do
          let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
          let fStr = init $ unlines ["function f()"
                               , "end function" ]
          fParser fStr `shouldBe'` expected


        it "parses functions with return type specs" $ do
          let fType = Just $ TypeSpec () u TypeInteger Nothing
          let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
          let fStr = init $ unlines ["integer function f()"
                               , "end function f" ]
          fParser fStr `shouldBe'` expected

      describe "parses function options (recursive, pure, elemental)" $ do
        let options_list = map unzip $ combination
                                        [ ("recursive ", None () u True)
                                        , ("pure ", Pure () u False)
                                        , ("elemental ", Elemental () u) ]

        forM_ options_list (\(strs, opts) -> do
          let str = foldr (++) "" strs
          let fStr = str ++ (init $ unlines ["function f()", "end"])
          let opt = buildPUFunctionOpts opts
          let expected = puFunction fType 
          case opt of
            Left _ -> it ("Shouldn't parse: " ++ show fStr ++ ": " ++ show opt) $ evaluate (fParser fStr) `shouldThrow` anyIOException
            Right fOpt ->
              it ("Should parse: " ++ show fStr ++ ": " ++ show opt) $ do
                let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
                fParser fStr `shouldBe'` expected
          )

      it "parses functions with a list of arguments" $ do
        let fArgs = Just $ AList () u [ varGen "x", varGen "y", varGen "z" ]                                                     
        let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
        let fStr = init $ unlines ["function f(x, y, z)"
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

      it "parses functions with a result variable" $ do
        let fRes = Just $ varGen "i"
        let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
        let fStr = init $ unlines ["function f() result(i)"
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

      it "parses functions with function bodies" $ do
        let decrementRHS = ExpBinary () u Subtraction (varGen "i") (intGen 1)
        let f1 = StPrint () u starVal (Just $ AList () u [ varGen "i" ])                                                      
        let f2 = StExpressionAssign () u (varGen "i") decrementRHS
        let fBody = [ BlStatement () u Nothing f1 , BlStatement () u Nothing f2 ]
        let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
        let fStr = init $ unlines ["function f()"
                             , "  print *, i"                                                                          
                             , "  i = (i - 1)"                                                                         
                             , "end function f" ]
        fParser fStr `shouldBe'` expected

      it "parses complex functions" $ do
        let fType = Just $ TypeSpec () u TypeInteger Nothing
        let fArgs = Just $ AList () u [ varGen "x", varGen "y", varGen "z" ]                                                     
        let fRes = Just $ varGen "i"                                                                                      
        let decrementRHS = ExpBinary () u Subtraction (varGen "i") (intGen 1)
        let f1 = StPrint () u starVal (Just $ AList () u [ varGen "i" ])                                                      
        let f2 = StExpressionAssign () u (varGen "i") decrementRHS
        let fBody = [ BlStatement () u Nothing f1 , BlStatement () u Nothing f2 ]
        let expected = puFunction fType fOpt fName fArgs fRes fBody fSub
        let fStr = init $ unlines [ "integer function f(x, y, z) result(i)"                                                 
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
        let unExp = ExpUnary () u unOp $ intGen 42

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
          let sub = ExpSubscript () u (varGen "y") range
          let innerRefExp = ExpDataRef () u (varGen "x") sub
          let exp = ExpDataRef () u innerRefExp (varGen "z")
          eParser "x % y(10) % z" `shouldBe'` exp

        it "parses section subscript" $ do
          let range = [ IxSingle () u Nothing $ intGen 10
                      , IxRange () u Nothing (Just $ intGen 1) (Just $ intGen 2)
                      , IxSingle () u Nothing $ varGen "y" ]
          let exp = ExpSubscript () u (varGen "x") (fromList () range)
          eParser "x (10, : 1 : 2, y)" `shouldBe'` exp

    describe "Statement" $ do
      it "data ref assignment" $ do
        let indicies = AList () u [ IxSingle () u Nothing (intGen 1) ]
        let subs = ExpSubscript () u (varGen "x") indicies
        let lhs = ExpDataRef () u subs (varGen "y")
        let st = StExpressionAssign () u lhs (intGen 1)
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
        let attrs = AList () u [ AttrExternal () u
                               , AttrIntent () u Out
                               , AttrDimension () u $ AList () u
                                  [ DimensionDeclarator () u
                                      (Just $ intGen 3) (Just $ intGen 10)
                                  ]
                               ]
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing
              , DeclVariable () u (varGen "y") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec (Just attrs) declarators
        let stStr = "real, external, intent (out), dimension (3:10) :: x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with old syntax" $ do
        let typeSpec = TypeSpec () u TypeLogical Nothing
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing
              , DeclVariable () u (varGen "y") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "logical x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with initialisation" $ do
        let typeSpec = TypeSpec () u TypeComplex Nothing
        let init = ExpValue () u (ValComplex (intGen 24) (realGen 42.0))
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing (Just init) ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "complex :: x = (24, 42.0)"
        sParser stStr `shouldBe'` expected

      it "parses declaration of custom type" $ do
        let typeSpec = TypeSpec () u (TypeCustom "meinetype") Nothing
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "type (MeineType) :: x"
        sParser stStr `shouldBe'` expected

      it "parses declaration type with kind selector" $ do
        let selector = Selector () u Nothing (Just $ varGen "hello")
        let typeSpec = TypeSpec () u TypeInteger (Just selector)
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "integer (hello) :: x"
        sParser stStr `shouldBe'` expected

      it "parses intent statement" $ do
        let stStr = "intent (inout) :: a"
        let expected = StIntent () u InOut (fromList () [ varGen "a" ])
        sParser stStr `shouldBe'` expected

      it "parses optional statement" $ do
        let stStr = "optional x"
        let expected = StOptional () u (fromList () [ varGen "x" ])
        sParser stStr `shouldBe'` expected

      it "parses public statement" $ do
        let stStr = "public :: x"
        let expected = StPublic () u (Just $ fromList () [ varGen "x" ])
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
        let expected = StSave () u (Just $ fromList () list)
        let stStr = "save /hello/, bye"
        sParser stStr `shouldBe'` expected

      it "parses parameter statement" $ do
        let ass1 = DeclVariable () u (varGen "x") Nothing (Just $ intGen 10)
        let ass2 = DeclVariable () u (varGen "y") Nothing (Just $ intGen 20)
        let expected = StParameter () u (fromList () [ ass1, ass2 ])
        sParser "parameter (x = 10, y = 20)" `shouldBe'` expected

      describe "FORALL blocks" $ do
        let stride = Just $ ExpBinary () u NE (varGen "i") (intGen 2)
        let tripletSpecList = [("i", intGen 1, varGen "n", stride)]

        it "parses basic FORALL blocks" $ do
          let stStr = "FORALL (I=1:N, I /= 2)"
          let expected = StForall () u Nothing (ForallHeader tripletSpecList Nothing) 
          sParser stStr `shouldBe'` expected

      describe "FORALL statements" $ do
        let stride = Just $ ExpBinary () u NE (varGen "i") (intGen 2)
        let tripletSpecList = [("i", intGen 1, varGen "n", stride)]
        let varI = IxSingle () u Nothing (varGen "i")
        let expSub1 = ExpSubscript () u (varGen "a") (AList () u [varI, varI])
        let expSub2 = ExpSubscript () u (varGen "x") (AList () u [varI])
        let eAssign = StExpressionAssign () u expSub1 expSub2

        it "parses basic FORALL statements" $ do
          let stStr = "FORALL (I=1:N, I /= 2)" -- A(I,I) = X(I)"
          let expected = StForall () u Nothing (ForallHeader tripletSpecList Nothing)-- eAssign
          sParser stStr `shouldBe'` expected

      describe "ENDFORALL statements" $ do
        it "parses FORALL end statements" $ do
          let stStr = "ENDFORALL"
          let expected = StEndForall () u Nothing
          sParser stStr `shouldBe'` expected

        it "parses FORALL end statements with label" $ do
          let stStr = "ENDFORALL A"
          let expected = StEndForall () u $ Just "a"
          sParser stStr `shouldBe'` expected

      describe "Implicit" $ do
        it "parses implicit none" $ do
          let st = StImplicit () u Nothing
          sParser "implicit none" `shouldBe'` st

        it "parses implicit with single" $ do
          let typeSpec = TypeSpec () u TypeCharacter Nothing
          let impEls = [ ImpCharacter () u "k" ]
          let impLists = [ ImpList () u typeSpec (fromList () impEls) ]
          let st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit character (k)" `shouldBe'` st

        it "parses implicit with range" $ do
          let typeSpec = TypeSpec () u TypeLogical Nothing
          let impEls = [ ImpRange () u "x" "z" ]
          let impLists = [ ImpList () u typeSpec (fromList () impEls) ]
          let st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit logical (x-z)" `shouldBe'` st

        it "parses implicit statement" $ do
          let typeSpec1 = TypeSpec () u TypeCharacter Nothing
          let typeSpec2 = TypeSpec () u TypeInteger Nothing
          let impEls1 = [ ImpCharacter () u "s", ImpCharacter () u "a" ]
          let impEls2 = [ ImpRange () u "x" "z" ]
          let impLists = [ ImpList () u typeSpec1 (fromList () impEls1)
                         , ImpList () u typeSpec2 (fromList () impEls2) ]
          let st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit character (s, a), integer (x-z)" `shouldBe'` st

      describe "Data" $ do
        it "parses vanilla" $ do
          let nlist = fromList () [ varGen "x", varGen "y" ]
          let vlist = fromList () [ intGen 1, intGen 2 ]
          let list = [ DataGroup () u nlist vlist ]
          let expected = StData () u (fromList () list)
          let stStr = "data x,y/1,2/"
          sParser stStr `shouldBe'` expected

        describe "Delimeter" $ do
          let [ nlist1, vlist1 ] =
                map (fromList () . return) [ varGen "x", intGen 1 ]
          let [ nlist2, vlist2 ] =
                map (fromList () . return) [ varGen "y", intGen 2 ]
          let list = [ DataGroup () u nlist1 vlist1
                     , DataGroup () u nlist2 vlist2 ]
          let expected = StData () u (fromList () list)

          it "parses comma delimited init groups" $
            sParser "data x/1/, y/2/" `shouldBe'` expected

          it "parses non-comma delimited init groups" $
            sParser "data x/1/ y/2/" `shouldBe'` expected

      describe "Namelist" $ do
        let groupNames = [ ExpValue () u (ValVariable "something")
                         , ExpValue () u (ValVariable "other") ]
        let itemss = [ fromList () [ varGen "a", varGen "b", varGen "c" ]
                     , fromList () [ varGen "y" ] ]
        let st = StNamelist () u $
              fromList () [ Namelist () u (head groupNames) (head itemss)
                          , Namelist () u (last groupNames) (last itemss) ]

        it "parses namelist statement (comma delimited) (1)" $
          sParser "namelist /something/a,b,c,/other/y" `shouldBe'` st

        it "parses namelist statement (2)" $
          sParser "namelist /something/a,b,c/other/y" `shouldBe'` st

      describe "Common" $ do
        let commonNames = [ ExpValue () u (ValVariable "something")
                          , ExpValue () u (ValVariable "other") ]
        let itemss = [ fromList () [ varGen "a", varGen "b", varGen "c" ]
                     , fromList () [ varGen "y" ] ]
        let st = StCommon () u $ fromList ()
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
        let st = StEquivalence () u eqALists
        sParser "equivalence (a(1), x), (y, z, d(1:42))" `shouldBe'` st

      describe "Dynamic allocation" $ do
        it "parses allocate statement" $ do
          let controlPair = ControlPair () u (Just "stat") (varGen "a")
          let allocs = fromList ()
                [ varGen "x"
                , ExpDataRef () u (varGen "st") (varGen "part")
                ]
          let s = StAllocate () u allocs (Just controlPair)
          sParser "allocate (x, st % part, STAT = a)" `shouldBe'` s

        it "parses deallocate statement" $ do
          let allocs = fromList ()
                [ let indicies = fromList () [ IxSingle () u Nothing (intGen 20) ]
                  in ExpSubscript () u (varGen "smt") indicies
                ]
          let s = StDeallocate () u allocs Nothing
          sParser "deallocate (smt ( 20 ))" `shouldBe'` s

        it "parses nullify statement" $ do
          let s = StNullify () u (fromList () [ varGen "x" ])
          sParser "nullify (x)" `shouldBe'` s

      it "parses pointer assignment" $ do
        let src = ExpDataRef () u (varGen "x") (varGen "y")
        let st = StPointerAssign () u src (varGen "exp")
        sParser "x % y => exp" `shouldBe'` st

      describe "Where" $ do
        it "parses where statement" $ do
          let exp = ExpBinary () u Subtraction (varGen "temp") (varGen "r_temp")
          let pred = ExpBinary () u GT (varGen "temp") (intGen 100)
          let assignment = StExpressionAssign () u (varGen "temp") exp
          let st = StWhere () u pred assignment
          sParser "where (temp > 100) temp = temp - r_temp"`shouldBe'` st

        describe "Where block" $ do
          it "parses where construct statement" $
            sParser "where (.true.)" `shouldBe'` StWhereConstruct () u valTrue

          it "parses elsewhere statement" $
            sParser "elsewhere" `shouldBe'` StElsewhere () u

          it "parses endwhere statement" $
            sParser "endwhere" `shouldBe'` StEndWhere () u

    describe "If" $ do
      it "parses if-then statement" $
        sParser "if (.false.) then" `shouldBe'` StIfThen () u Nothing valFalse

      it "parses if-then statement with construct name" $ do
        let st = StIfThen () u (Just "my_if") valFalse
        sParser "my_if: if (.false.) then" `shouldBe'` st

      it "parses else statement" $
        sParser "else" `shouldBe'` StElse () u Nothing

      it "parses else-if statement" $
        sParser "else if (.true.) then" `shouldBe'` StElsif () u Nothing valTrue

      it "parses end if statement" $
        sParser "end if" `shouldBe'` StEndif () u Nothing

      it "parses logical if statement" $ do
        let assignment = StExpressionAssign () u (varGen "a") (varGen "b")
        let stIf = StIfLogical () u valTrue assignment
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
        let doSpec = DoSpecification () u assign (intGen 42) Nothing
        let st = StDo () u Nothing (Just $ intGen 24) (Just doSpec)
        sParser "do 24, i = 0, 42" `shouldBe'` st

      it "parses do statement without label" $ do
        let assign = StExpressionAssign () u (varGen "i") (intGen 0)
        let doSpec = DoSpecification () u assign (intGen 42) Nothing
        let st = StDo () u Nothing Nothing (Just doSpec)
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
        let st = StGotoComputed () u list (intGen 20)
        sParser "goto (10, 20, 30) 20" `shouldBe'` st

      it "doesn't parse assigned goto" $ do
        evaluate (sParser "goto i, (10, 20, 30)") `shouldThrow` anyIOException

      it "doesn't parse label assignment" $ do
        evaluate (sParser "assign 20 to l") `shouldThrow` anyIOException

    describe "IO" $ do
      it "parses vanilla print" $ do
        let st = StPrint () u starVal (Just $ fromList () [ varGen "hex" ])
        sParser "print *, hex" `shouldBe'` st

      it "parses write with implied do" $ do
        let cp1 = ControlPair () u Nothing (intGen 10)
        let cp2 = ControlPair () u (Just "format") (varGen "x")
        let ciList = fromList () [ cp1, cp2 ]
        let assign = StExpressionAssign () u (varGen "i") (intGen 1)
        let doSpec = DoSpecification () u assign (intGen 42) (Just $ intGen 2)
        let alist = fromList () [ varGen "i", varGen "j" ]
        let outList = fromList () [ ExpImpliedDo () u alist doSpec ]
        let st = StWrite () u ciList (Just outList)
        sParser "write (10, FORMAT = x) (i, j,  i = 1, 42, 2)" `shouldBe'` st

    it "parses use statement" $ do
      let renames = fromList ()
            [ UseRename () u (varGen "sprod") (varGen "prod")
            , UseRename () u (varGen "a") (varGen "b") ]
      let st = StUse () u (varGen "stats_lib") Permissive (Just renames)
      sParser "use stats_lib, sprod => prod, a => b" `shouldBe'` st

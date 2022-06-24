module Language.Fortran.Parser.Free.Fortran90Spec ( spec ) where

import Prelude hiding (GT, exp, pred)

import Test.Hspec
import TestUtil
import Language.Fortran.Parser.Free.Common

import Language.Fortran.AST
import Language.Fortran.AST.Literal.Real ( parseRealLit )
import Language.Fortran.Version
import Language.Fortran.Parser
import Language.Fortran.Parser.Monad ( Parse )
import qualified Language.Fortran.Parser.Free.Fortran90 as F90
import qualified Language.Fortran.Parser.Free.Lexer     as Free

--import qualified Data.List as List
import qualified Data.ByteString.Char8 as B

parseWith :: Parse Free.AlexInput Free.Token a -> String -> a
parseWith p = parseUnsafe (makeParserFree p Fortran90) . B.pack

eParser :: String -> Expression ()
eParser = parseUnsafe p . B.pack
  where p = makeParser initParseStateFreeExpr F90.expressionParser Fortran90

sParser :: String -> Statement ()
sParser = parseWith F90.statementParser

bParser :: String -> Block ()
bParser = parseWith F90.blockParser

fParser :: String -> ProgramUnit ()
fParser = parseWith F90.functionParser

{- Useful for parser debugging; Lexes the given source code.
fTok :: String -> [Token]
fTok sourceCode = collectFreeTokens Fortran95 $ B.pack sourceCode
-}

{-
 - Given a list of values, find every combination of those values:
 - combination [1,2] = [[], [1], [2], [1,2], [2,1]]
 -}
--combination :: [a] -> [[a]]
--combination = foldr ((++) . List.permutations) [] . List.subsequences

spec :: Spec
spec =
  describe "Fortran 90 Parser" $ do
    describe "Function" $ do
      let puFunction = PUFunction () u
          fType = Nothing
          fPre = emptyPrefixes
          fPreR = Just $ AList () u [PfxRecursive () u]
          fSuf = emptySuffixes
          fPreSuf = (fPre, fSuf)
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

      it "parses recursive functions" $
        let expected = puFunction fType (fPreR, fSuf) fName fArgs fRes fBody fSub
            fStr = init $ unlines ["recursive function f()", "end"]
        in fParser fStr `shouldBe'` expected


      it "parses functions with a list of arguments" $
        let fArgs' = Just $ AList () u [ varGen "x", varGen "y", varGen "z" ]
            expected = puFunction fType fPreSuf fName fArgs' fRes fBody fSub
            fStr = init $ unlines ["function f(x, y, z)"
                             , "end function f" ]
        in fParser fStr `shouldBe'` expected

      it "parses functions with a result variable" $
        let fRes' = Just $ varGen "i"
            expected = puFunction fType fPreSuf fName fArgs fRes' fBody fSub
            fStr = init $ unlines ["function f() result(i)"
                             , "end function f" ]
        in fParser fStr `shouldBe'` expected

      it "parses functions with function bodies" $
        let decrementRHS = ExpBinary () u Subtraction (varGen "i") (intGen 1)
            f1 = StPrint () u starVal (Just $ AList () u [ varGen "i" ])
            f2 = StExpressionAssign () u (varGen "i") decrementRHS
            fBody' = [ BlStatement () u Nothing f1 , BlStatement () u Nothing f2 ]
            expected = puFunction fType fPreSuf fName fArgs fRes fBody' fSub
            fStr = init $ unlines ["function f()"
                             , "  print *, i"
                             , "  i = (i - 1)"
                             , "end function f" ]
        in fParser fStr `shouldBe'` expected

      it "parses complex functions" $
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
        in fParser fStr `shouldBe'` expected

    describe "Expression" $ do
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

      it "parses pause statements" $ do
        let stPause = StPause () u Nothing
            stStr = "PAUSE"
        sParser stStr `shouldBe'` stPause

      it "parses pause statements with expression" $ do
        let stPause = StPause () u (Just (strGen "MESSAGE"))
            stStr = "PAUSE \"MESSAGE\""
        sParser stStr `shouldBe'` stPause

      it "parses declaration with attributes" $ do
        let typeSpec = TypeSpec () u TypeReal Nothing
            attrs = AList () u [ AttrExternal () u
                               , AttrIntent () u Out
                               , AttrDimension () u $ AList () u
                                  [ DimensionDeclarator () u
                                      (Just $ intGen 3) (Just $ intGen 10)
                                  ]
                               ]
            declarators = AList () u [ declVarGen "x", declVarGen "y"]
            expected = StDeclaration () u typeSpec (Just attrs) declarators
            stStr = "real, external, intent (out), dimension (3:10) :: x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with old syntax" $ do
        let typeSpec = TypeSpec () u TypeLogical Nothing
            declarators = AList () u [ declVarGen "x", declVarGen "y"]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "logical x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with initialisation" $
        let typeSpec = TypeSpec () u TypeComplex Nothing
            init' = complexGen (ComplexPartInt () u "24" Nothing)
                               (ComplexPartReal () u (parseRealLit "42.0") Nothing)
            declarators = AList () u
              [ declVariable () u (varGen "x") Nothing (Just init') ]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "complex :: x = (24, 42.0)"
        in sParser stStr `shouldBe'` expected

      it "parses declaration of custom type" $ do
        let typeSpec = TypeSpec () u (TypeCustom "meinetype") Nothing
            declarators = AList () u [declVarGen "x"]
            expected = StDeclaration () u typeSpec Nothing declarators
            stStr = "type (MeineType) :: x"
        sParser stStr `shouldBe'` expected

      it "parses declaration type with kind selector" $ do
        let selector = Selector () u Nothing (Just $ varGen "hello")
            typeSpec = TypeSpec () u TypeInteger (Just selector)
            declarators = AList () u [declVarGen "x"]
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
        let ass1 = declVariable () u (varGen "x") Nothing (Just $ intGen 10)
            ass2 = declVariable () u (varGen "y") Nothing (Just $ intGen 20)
            expected = StParameter () u (fromList () [ ass1, ass2 ])
        sParser "parameter (x = 10, y = 20)" `shouldBe'` expected

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
            itemss = [ fromList () [ declVarGen "a", declVarGen "b", declVarGen "c" ]
                     , fromList () [ declVarGen "y" ] ]
            st = StCommon () u $ fromList ()
              [ CommonGroup () u Nothing (fromList () [ declVarGen "q" ])
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
          let opt = AOStat () u (varGen "a")
              allocs = fromList ()
                [ varGen "x"
                , ExpDataRef () u (varGen "st") (varGen "part")
                ]
          let s = StAllocate () u Nothing allocs (Just (AList () u [opt]))
          sParser "allocate (x, st % part, STAT = a)" `shouldBe'` s

        it "parses deallocate statement" $ do
          let opt = AOStat () u (varGen "a")
              allocs = fromList ()
                [ let indicies = fromList () [ IxSingle () u Nothing (intGen 20) ]
                  in ExpSubscript () u (varGen "smt") indicies
                ]
              s = StDeallocate () u allocs Nothing
              s' = StDeallocate () u allocs (Just (AList () u [opt]))
          sParser "deallocate (smt ( 20 ))" `shouldBe'` s
          sParser "deallocate (smt ( 20 ), stat=a)" `shouldBe'` s'

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

    describe "DO WHILE" $ do
      it "parses unnamed do while statement" $ do
        let st = StDoWhile () u Nothing Nothing valTrue
        sParser "do while (.true.)" `shouldBe'` st

      it "parses named do while statement" $ do
        let st = StDoWhile () u (Just "name") Nothing valTrue
        sParser "name: do while (.true.)" `shouldBe'` st

      it "parses unnamed labelled do while statement" $ do
        let st = StDoWhile () u Nothing (Just (intGen 999)) valTrue
        sParser "do 999 while (.true.)" `shouldBe'` st

    describe "Goto" $ do
      it "parses vanilla goto" $ do
        let st = StGotoUnconditional () u (intGen 999)
        sParser "goto 999" `shouldBe'` st

      it "parses computed goto" $ do
        let list = fromList () [ intGen 10, intGen 20, intGen 30 ]
            st = StGotoComputed () u list (intGen 20)
        sParser "goto (10, 20, 30) 20" `shouldBe'` st

      it "parses assigned goto" $ do
        let list = fromList () [ intGen 10, intGen 20, intGen 30 ]
            st = StGotoAssigned () u (varGen "i") (Just list)
        sParser "goto i, (10, 20, 30)" `shouldBe'` st

      it "parses label assignment" $ do
        let st = StLabelAssign () u (intGen 20) (varGen "l")
        sParser "assign 20 to l" `shouldBe'` st

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

    it "parses use statement with renames" $ do
      let renames = fromList ()
            [ UseRename () u (varGen "sprod") (varGen "prod")
            , UseRename () u (varGen "a") (varGen "b") ]
          st = StUse () u (varGen "stats_lib") Nothing Permissive (Just renames)
      sParser "use stats_lib, sprod => prod, a => b" `shouldBe'` st

    it "parses use statement with only list" $ do
      let onlys = fromList ()
            [ UseID () u (varGen "a")
            , UseRename () u (varGen "b") (varGen "c")
            , UseID () u (ExpValue () u (ValOperator "+"))
            , UseID () u (ExpValue () u ValAssignment) ]
          st = StUse () u (varGen "stats_lib") Nothing Exclusive (Just onlys)
      sParser "use stats_lib, only: a, b => c, operator(+), assignment(=)" `shouldBe'` st

    specFreeCommon bParser sParser eParser

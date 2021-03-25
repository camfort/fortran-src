module Language.Fortran.Analysis.TypesSpec where

import Test.Hspec
import TestUtil

import Data.Map ((!))

import Data.Data
import Data.Generics.Uniplate.Data
import Language.Fortran.AST
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Analysis
import qualified Language.Fortran.Parser.Fortran90 as F90
import Language.Fortran.ParserMonad
import qualified Data.ByteString.Char8 as B

inferTable :: Data a => ProgramFile a -> TypeEnv
inferTable = underRenaming (snd . analyseTypes)

typedProgramFile :: Data a => ProgramFile a -> ProgramFile (Analysis a)
typedProgramFile = fst . analyseTypes . analyseRenames . initAnalysis

fortran90Parser :: String -> String -> ProgramFile A0
fortran90Parser src file = fromParseResultUnsafe $ F90.fortran90Parser (B.pack src) file

uniExpr :: ProgramFile (Analysis A0) -> [Expression (Analysis A0)]
uniExpr = universeBi

spec :: Spec
spec = do
  describe "Global type inference" $ do
    it "types integer returning function" $ do
      let entry = inferTable ex1 ! "f1"
      entry `shouldBe` IDType (Just TypeInteger) (Just CTFunction) Nothing Nothing

    it "types multiples program units" $ do
      let mapping = inferTable ex2
      mapping ! "f1" `shouldBe` IDType (Just TypeInteger) (Just CTFunction) Nothing Nothing
      mapping ! "s1" `shouldBe` IDType Nothing (Just CTSubroutine) Nothing Nothing

    it "types ENTRY points within subprograms" $ do
      let mapping = inferTable ex3
      mapping ! "e1" `shouldBe` IDType Nothing (Just CTSubroutine) Nothing Nothing
      mapping ! "e2" `shouldBe` IDType Nothing (Just CTSubroutine) Nothing Nothing
      mapping ! "e3" `shouldBe` IDType Nothing (Just CTSubroutine) Nothing Nothing

  describe "Local type inference" $ do
    it "infers from type declarations" $ do
      let mapping = inferTable ex4
      let pf = typedProgramFile ex4
      mapping ! "y" `shouldBe` IDType (Just TypeInteger) (Just $ CTArray [(Nothing, Just 10)]) Nothing Nothing
      mapping ! "c" `shouldBe` IDType (Just $ TypeCharacter Nothing Nothing) (Just CTVariable) Nothing Nothing
      mapping ! "log" `shouldBe` IDType (Just TypeLogical) (Just CTVariable) Nothing Nothing
      [ () | ExpValue a _ (ValVariable "x") <- uniExpr pf
           , idType a == Just (IDType (Just TypeInteger) (Just CTVariable) Nothing Nothing) ]
        `shouldNotSatisfy` null
      [ () | ExpValue a _ (ValVariable "y") <- uniExpr pf
           , idType a == Just (IDType (Just TypeInteger) (Just $ CTArray [(Nothing, Just 10)]) Nothing Nothing) ]
        `shouldNotSatisfy` null

    it "infers from dimension declarations" $ do
      let mapping = inferTable ex5
      mapping ! "x" `shouldBe` IDType Nothing (Just $ CTArray [(Nothing, Just 1)]) Nothing Nothing
      mapping ! "y" `shouldBe` IDType Nothing (Just $ CTArray [(Nothing, Just 1)]) Nothing Nothing

    it "infers from function statements" $ do
      let mapping = inferTable ex6
      mapping ! "a" `shouldBe` IDType (Just TypeInteger) (Just $ CTArray [(Nothing, Just 1)]) Nothing Nothing
      mapping ! "b" `shouldBe` IDType (Just TypeInteger) (Just $ CTArray [(Nothing, Just 1)]) Nothing Nothing
      mapping ! "c" `shouldBe` IDType (Just TypeInteger) (Just CTFunction) Nothing Nothing
      mapping ! "d" `shouldBe` IDType Nothing (Just CTFunction) Nothing Nothing

    describe "Intrinsics type analysis" $ do
      it "disambiguates intrinsics from functions and variables" $ do
        let mapping = inferTable intrinsics1
        let pf = typedProgramFile intrinsics1
        [ () | ExpValue a _ (ValVariable "x") <- uniExpr pf
             , idType a == Just (IDType (Just TypeReal) (Just CTVariable) Nothing Nothing) ]
          `shouldSatisfy` ((== 5) . length)

        -- the following are true because dabs and cabs are defined as function and array in this program.
        idCType (mapping ! "dabs") `shouldBe` Just CTFunction
        [ a | ExpValue a _ (ValIntrinsic "dabs") <- uniExpr pf
             ] -- , idType a == Just (IDType (Just TypeReal) (Just CTVariable) Nothing Nothing) ]
          `shouldSatisfy` null

        idCType (mapping ! "cabs") `shouldBe` Just (CTArray [(Nothing, Just 3)])
        [ a | ExpValue a _ (ValIntrinsic "cabs") <- uniExpr pf
             ] -- , idType a == Just (IDType (Just TypeReal) (Just CTVariable) Nothing Nothing) ]
          `shouldSatisfy` null

        -- abs is an actual intrinsic
        idCType (mapping ! "abs") `shouldBe` Just CTIntrinsic
        [ a | ExpFunctionCall a _ (ExpValue _ _ (ValIntrinsic "abs")) _ <- uniExpr pf
            , idType a == Just (IDType (Just TypeInteger) Nothing Nothing Nothing) ]
          `shouldNotSatisfy` null

      it "intrinsics and numeric types" $ do
        let mapping = inferTable intrinsics2
        let pf = typedProgramFile intrinsics2
        idCType (mapping ! "abs") `shouldBe` Just CTIntrinsic
        idCType (mapping ! "cabs") `shouldBe` Just CTIntrinsic
        idCType (mapping ! "dabs") `shouldBe` Just CTIntrinsic
        [ ty | ExpFunctionCall a _ (ExpValue _ _ (ValIntrinsic "abs")) _ <- uniExpr pf
             , Just (IDType (Just ty) Nothing Nothing Nothing) <- [idType a] ]
          `shouldBe` [TypeDoublePrecision, TypeComplex]
        [ a | ExpFunctionCall a _ (ExpValue _ _ (ValIntrinsic "cabs")) _ <- uniExpr pf
            , idType a == Just (IDType (Just TypeComplex) Nothing Nothing Nothing) ]
          `shouldNotSatisfy` null
        [ a | ExpFunctionCall a _ (ExpValue _ _ (ValIntrinsic "dabs")) _ <- uniExpr pf
            , idType a == Just (IDType (Just TypeDoublePrecision) Nothing Nothing Nothing) ]
          `shouldNotSatisfy` null

    describe "Numeric types" $ do
      it "Widening / upgrading" $ do
        let pf = typedProgramFile numerics1
        [ a | ExpFunctionCall a _ (ExpValue _ _ (ValIntrinsic "abs")) _ <- uniExpr pf
            , idType a == Just (IDType (Just TypeReal) Nothing Nothing Nothing) ]
          `shouldNotSatisfy` null
        [ a | ExpBinary a _ Addition (ExpValue _ _ (ValInteger "1")) _ <- uniExpr pf
            , idType a == Just (IDType (Just TypeComplex) Nothing Nothing Nothing) ]
          `shouldNotSatisfy` null
        [ a | ExpBinary a _ Addition (ExpValue _ _ (ValInteger "2")) _ <- uniExpr pf
            , idType a == Just (IDType (Just TypeDoublePrecision) Nothing Nothing Nothing) ]
          `shouldNotSatisfy` null

    describe "Character string types" $
      it "examples of various character variables" $ do
        let mapping = inferTable teststrings1
        idVType (mapping ! "a") `shouldBe` Just (TypeCharacter (Just (CharLenInt 5)) (Just "1"))
        idVType (mapping ! "b") `shouldBe` Just (TypeCharacter (Just (CharLenInt 10)) Nothing)
        idVType (mapping ! "c") `shouldBe` Just (TypeCharacter (Just (CharLenInt 3)) (Just "1"))
        idVType (mapping ! "d") `shouldBe` Just (TypeCharacter (Just CharLenExp) Nothing)
        idCType (mapping ! "d") `shouldBe` Just (CTArray [(Nothing, Just 10)])
        idVType (mapping ! "e") `shouldBe` Just (TypeCharacter (Just (CharLenInt 10)) Nothing)
        idCType (mapping ! "e") `shouldBe` Just (CTArray [(Nothing, Just 20)])
        let pf = typedProgramFile teststrings1
        [ () | ExpValue a _ (ValVariable "e") <- uniExpr pf
             , idType a == Just (IDType (Just (TypeCharacter (Just (CharLenInt 10)) Nothing))
                                        (Just (CTArray [(Nothing, Just 20)]))
                                        Nothing
                                        Nothing) ]
          `shouldNotSatisfy` null

ex1 :: ProgramFile ()
ex1 = ProgramFile mi77 [ ex1pu1 ]
ex1pu1 :: ProgramUnit ()
ex1pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) emptyPrefixSuffix "f1" Nothing Nothing [] Nothing

ex2 :: ProgramFile ()
ex2 = ProgramFile mi77 [ ex2pu1, ex1pu1 ]
ex2pu1 :: ProgramUnit ()
ex2pu1 = PUSubroutine () u emptyPrefixSuffix "s1" Nothing [] Nothing

ex3 :: ProgramFile ()
ex3 = ProgramFile mi77 [ ex3pu1 ]
ex3pu1 :: ProgramUnit ()
ex3pu1 = PUSubroutine () u emptyPrefixSuffix "s1" Nothing ex3pu1bs Nothing
ex3pu1bs :: [Block ()]
ex3pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e1")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e2")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e3")) Nothing Nothing) ]

ex4 :: ProgramFile ()
ex4 = ProgramFile mi77 [ ex4pu1 ]
ex4pu1 :: ProgramUnit ()
ex4pu1 = PUMain () u Nothing ex4pu1bs Nothing
ex4pu1bs :: [Block ()]
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing
      (AList () u
        [ DeclVariable () u (varGen "x") Nothing Nothing
        , DeclArray () u (varGen "y")
            (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 10) ]) Nothing Nothing ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u (TypeCharacter Nothing Nothing) Nothing) Nothing
      (AList () u [ DeclVariable () u (varGen "c") Nothing Nothing ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeLogical Nothing) Nothing
      (AList () u [ DeclVariable () u (varGen "log") Nothing Nothing ])) ]

ex5 :: ProgramFile ()
ex5 = ProgramFile mi77 [ ex5pu1 ]
ex5pu1 :: ProgramUnit ()
ex5pu1 = PUBlockData () u (Just "bd") ex5pu1bs
ex5pu1bs :: [Block ()]
ex5pu1bs =
  [ BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (varGen "x") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclArray () u (varGen "y") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing])) ]

{-
- program Main
- integer a, b(1), c
- dimension a(1)
- a(1) = 1
- b(1) = 1
- c(x) = 1
- d(x) = 1
- end
-}
ex6 :: ProgramFile ()
ex6 = ProgramFile mi77 [ ex6pu1 ]
ex6pu1 :: ProgramUnit ()
ex6pu1 = PUMain () u (Just "main") ex6pu1bs Nothing
ex6pu1bs :: [Block ()]
ex6pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "a") Nothing Nothing
      , DeclArray () u (varGen "b") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclVariable () u (varGen "c") Nothing Nothing ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (varGen "a") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1 ) ]) Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "a") (fromList () [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "b") (fromList () [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "c") (fromList () [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "d") (fromList () [ ixSinGen 1 ])) (intGen 1)) ]

ex11 :: ProgramFile ()
ex11 = ProgramFile mi77 [ ex11pu1 ]
ex11pu1 :: ProgramUnit ()
ex11pu1 = PUFunction () u (Just (TypeSpec () u TypeInteger Nothing)) emptyPrefixSuffix "f1" Nothing (Just (varGen "r1")) ex11pu1bs Nothing
ex11pu1bs :: [Block ()]
ex11pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e1")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e2")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e3")) Nothing (Just (varGen "r2"))) ]

intrinsics1 :: ProgramFile A0
intrinsics1 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module intrinsics"
  , "contains"
  , "  subroutine main()"
  , "    real :: x"
  , "    integer :: y = 1"
  , "    real :: cabs(3)"
  , "    x = dabs(y)"
  , "    x = cabs(y)"
  , "    x = abs(y)"
  , "    print *, x"
  , "  end subroutine main"
  , "  real function dabs(a)"
  , "    integer :: a"
  , "    dabs = a"
  , "  end function dabs"
  , "end module intrinsics"
  ]

intrinsics2 :: ProgramFile A0
intrinsics2 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module intrinsics"
  , "contains"
  , "  subroutine main()"
  , "    double precision :: u"
  , "    complex :: c"
  , "    real :: x"
  , "    integer :: y = 1"
  , "    u = dabs(y + x)"
  , "    c = cabs(y + x)"
  , "    u = abs(y + x * u)"
  , "    c = abs(y + x * c)"
  , "    print *, x"
  , "  end subroutine main"
  , "end module intrinsics"
  ]

numerics1 :: ProgramFile A0
numerics1 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module numerics1"
  , "contains"
  , "  subroutine main()"
  , "    double precision :: u"
  , "    complex :: c"
  , "    real :: x"
  , "    integer :: y = 1"
  , "    print *, 1 + (-u * c + abs(y + x))"
  , "    print *, 2 + f(y)"
  , "  end subroutine main"
  , "  double precision function f(a)"
  , "    integer :: a"
  , "    f = a"
  , "  end function f"
  , "end module numerics1"
  ]


teststrings1 :: ProgramFile A0
teststrings1 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "program teststrings"
  , "  character(5,1) :: a"
  , "  character :: b*10"
  , "  character(kind=1,len=3) :: c"
  , "  integer, parameter :: k = 8"
  , "  character(k), dimension(10) :: d"
  , "  character :: e(20)*10"
  , "end program teststrings"
  ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:

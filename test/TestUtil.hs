module TestUtil where

import Test.Hspec
import Data.Data
import Data.Generics.Uniplate.Data

import Language.Fortran.AST
import Language.Fortran.AST.Literal.Real
import Language.Fortran.AST.Literal.Complex
import Language.Fortran.Version
import Language.Fortran.Util.Position

import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import qualified Data.Map as M
import Data.Maybe

u :: SrcSpan
u = initSrcSpan

-- TODO Filename only gets set with transformations (defaults to the empty
-- string). @Parser.parseUnsafe@ uses @"<unknown>"@. So we have to define two
-- different versions.
--
-- Better would be to make an equality checker that ignores 'MetaInfo'.
mi77, mi77', mi90 :: MetaInfo
mi77  = MetaInfo { miVersion = Fortran77, miFilename = "" }
mi77' = MetaInfo { miVersion = Fortran77, miFilename = "<unknown>" }
mi90  = MetaInfo { miVersion = Fortran90, miFilename = "" }

valTrue, valFalse :: Expression ()
valTrue  = ExpValue () u $ ValLogical True  Nothing
valFalse = ExpValue () u $ ValLogical False Nothing

valTrue', valFalse' :: KindParam' () -> Expression ()
valTrue'  kp = ExpValue () u $ ValLogical True  (Just kp)
valFalse' kp = ExpValue () u $ ValLogical False (Just kp)

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable str

declVarGen :: String -> Declarator ()
declVarGen str = Declarator () u (varGen str) ScalarDecl Nothing Nothing

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger (show i) Nothing

aintGen :: Integer -> ArgumentExpression ()
aintGen = ArgExpr . intGen

initGen :: [Expression ()] -> Expression ()
initGen es = ExpInitialisation () u $ fromList () es

realGen :: (Fractional a, Show a) => a -> Expression ()
realGen i = ExpValue () u $ ValReal (parseRealLit (show i)) Nothing

complexGen :: ComplexPart () -> ComplexPart () -> Expression ()
complexGen cr ci = ExpValue () u $ ValComplex $ ComplexLit () u cr ci

strGen :: String -> Expression ()
strGen str = ExpValue () u $ ValString str

labelGen :: Integer -> Expression ()
labelGen = intGen

starVal :: Expression ()
starVal = ExpValue () u ValStar

opGen :: String -> Expression ()
opGen s = ExpValue () u (ValOperator s)

assVal :: Expression ()
assVal = ExpValue () u ValAssignment

declVariable :: a -> SrcSpan -> Expression a -> Maybe (Expression a) -> Maybe (Expression a) -> Declarator a
declVariable a ss v mLen mVal = Declarator a ss v ScalarDecl mLen mVal

declArray :: a -> SrcSpan -> Expression a -> AList DimensionDeclarator a -> Maybe (Expression a) -> Maybe (Expression a) -> Declarator a
declArray a ss v dims mLen mVal = Declarator a ss v (ArrayDecl dims) mLen mVal

ixSinGen :: Integer -> Index ()
ixSinGen i = IxSingle () u Nothing (intGen i)
ixRanGen :: Integer -> Integer -> Index ()
ixRanGen i j = IxRange () u (Just $ intGen i) (Just $ intGen j) Nothing

shouldBe' :: (Data a, Eq a, Show a) => a -> a -> Expectation
shouldBe' a b = resetSrcSpan a `shouldBe` resetSrcSpan b

shouldMatchList' :: (Data a, Eq a, Show a) => [a] -> [a] -> Expectation
shouldMatchList' a b = resetSrcSpan a `shouldMatchList` resetSrcSpan b

-- To be used in testing it reverts the SrcSpans in AST to dummy initial
-- SrcSpan value.
resetSrcSpan :: Data a => a -> a
resetSrcSpan = transformBi f
  where
    f x = case cast x :: Maybe SrcSpan of
      Just _ -> initSrcSpan
      Nothing -> x

--------------------------------------------------
-- These functions do not work on modules with use-renaming so are
-- only for testing purposes...
underRenaming :: (Data a, Data b) => (ProgramFile (Analysis a) -> b) -> ProgramFile a -> b
underRenaming f pf = tryUnrename `descendBi` f pf'
  where
    pf' = rename . analyseRenames . initAnalysis $ pf
    renameMap = extractNameMap pf'
    tryUnrename n = n `fromMaybe` M.lookup n renameMap

extractNameMap :: Data a => ProgramFile (Analysis a) -> M.Map String String
extractNameMap pf = eMap `M.union` puMap
  where
    eMap  = M.fromList [ (un, n) | ExpValue Analysis { uniqueName = Just un, sourceName = Just n } _ _ <- uniE pf ]
    puMap = M.fromList [ (un, n) | pu <- uniPU pf, Analysis { uniqueName = Just un, sourceName = Just n } <- [getAnnotation pu] ]
    uniE :: Data a => ProgramFile a -> [Expression a]
    uniE = universeBi
    uniPU :: Data a => ProgramFile a -> [ProgramUnit a]
    uniPU = universeBi
--------------------------------------------------

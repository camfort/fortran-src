module Language.Fortran.Generate where

import Language.Fortran.AST
import Test.QuickCheck

import Language.Fortran.Util.Position
import Language.Fortran.PrettyPrint
import Language.Fortran.Version

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------
-- Core generators
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Value a) where
  arbitrary = oneof
    [ do x <- arbitrary :: Gen Integer
         pure $ ValInteger (show x) Nothing
    , do s <- arbitrary :: Gen String
         pure $ ValString s
    , ValLogical <$> arbitrary <*> pure Nothing
    , pure $ ValVariable "myVar"
    ]

instance Arbitrary BaseType where
  arbitrary = oneof
    [ pure TypeInteger
    , pure TypeReal
    , pure TypeLogical
    , pure TypeCharacter
    ]

instance Arbitrary a => Arbitrary (TypeSpec a) where
  arbitrary = do
    annotation <- arbitrary
    base       <- arbitrary
    selector   <- arbitrary
    pure $ TypeSpec annotation nullSpan base selector

instance Arbitrary a => Arbitrary (Selector a) where
  arbitrary = do 
    annotation <- arbitrary
    -- Positive length
    len    :: Integer <- abs <$> arbitrary
    -- Kind, powers of two
    kind   :: Integer <- elements [1,2,4,8]
    -- Wrap into expressions, with a chance of being Nothing
    let kindExpr = ExpValue annotation nullSpan (ValInteger (show kind) Nothing)
    lenExprM   <- maybeWrapper (pure (ExpValue annotation nullSpan (ValInteger (show len) Nothing)))
    kindExprM  <- 
       case lenExprM of 
         Nothing -> return $ Just kindExpr  -- If no length, always include kind (cannot have nothing for both)
         Just{}  -> maybeWrapper (pure kindExpr)
    pure $ Selector annotation nullSpan lenExprM kindExprM

maybeWrapper :: Gen a -> Gen (Maybe a)
maybeWrapper gen = oneof [pure Nothing, Just <$> gen]


nullSpan :: SrcSpan
nullSpan = SrcSpan initPosition initPosition

--------------------------------------------------------------------------------
-- Stateful generation
--------------------------------------------------------------------------------

-- | Environment mapping variable names to their declared types.
type Env = Map Name (TypeSpec A0)

-- | Stateful generator: a 'Gen' action that can read/write an 'Env'.
type GenM a = StateT Env Gen a

-- | Lift a plain 'Gen' action into 'GenM'.
liftGen :: Gen a -> GenM a
liftGen = lift

-- | Generate a fresh variable name based on the current environment size.
freshName :: GenM Name
freshName = do
  env <- get
  pure $ "var" ++ show (Map.size env)

--------------------------------------------------------------------------------
-- Generate typing context and declarations
--------------------------------------------------------------------------------

-- | Generate one declaration statement, adding the variable to the environment.
genDecl :: GenM (Statement A0)
genDecl = do
  name     <- freshName
  typeSpec  <- liftGen arbitrary
  let
      varExpr   = ExpValue () nullSpan (ValVariable name)
      decl      = Declarator () nullSpan varExpr ScalarDecl Nothing Nothing
      declList  = AList () nullSpan [decl]
  modify (Map.insert name typeSpec)
  pure $ StDeclaration () nullSpan typeSpec Nothing declList

-- | Generate @n@ declarations, building up the environment as we go.
genDecls :: Int -> GenM [Statement A0]
genDecls n = replicateM n genDecl

-- | Top-level runner: generate a subroutine with a growing set of declarations.
--   Uses QuickCheck's 'sized' so the number of declarations scales with test size.
genProgramUnit :: Gen (ProgramUnit A0)
genProgramUnit = sized $ \sz -> do
  let numDecls = max 1 (sz `div` 5)
  (decls, env) <- runStateT (genDecls numDecls) Map.empty
  -- env is now available for generating expressions / further statements
  let blocks  = map (\s -> BlStatement () nullSpan Nothing s) decls
      name    = "generated_sub"
  pure $ PUSubroutine () nullSpan emptyPrefixSuffix name Nothing blocks Nothing

genVarRef :: GenM (Expression A0)
genVarRef = do
  env <- get
  (name, _) <- liftGen $ elements (Map.toList env)
  pure $ ExpValue () nullSpan (ValVariable name)

--------------------------------------------------------------------------------
-- Demonstration / expertimentation
--------------------------------------------------------------------------------

-- Generate a list of 10 values and pretty print
-- the results
demoVal :: IO ()
demoVal = do
  values :: [Value ()] <- generate $ vectorOf 10 arbitrary
  let prettyValues = map (pprint' Fortran90) values
  mapM_ (putStrLn . render) prettyValues
  putStrLn $ "Generated " ++ show (length values) ++ " values."

-- | Generate 10 full Fortran programs and print each one.
demoProgram :: IO ()
demoProgram = do
  pus <- generate $ vectorOf 10 genProgramUnit
  let meta = MetaInfo { miVersion = Fortran90, miFilename = "<generated>" }
      programs = map (\pu -> ProgramFile meta [pu]) pus
  mapM_ printOne (zip [1..] programs)
  where
    printOne (i, pf) = do
      putStrLn $ "-- Program " ++ show (i :: Int) ++ " " ++ replicate 60 '-'
      putStrLn $ pprintAndRender Fortran90 pf (Just 2)


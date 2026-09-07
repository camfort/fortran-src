{-# LANGUAGE DefaultSignatures #-}
module Language.Fortran.Generate where

import Language.Fortran.AST
import Language.Fortran.AST.Literal
import Language.Fortran.AST.Literal.Real
import Test.QuickCheck

import Language.Fortran.Util.Position
import Language.Fortran.PrettyPrint
import Language.Fortran.Version

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath ((</>))
import Data.List (partition)

--------------------------------------------------------------------------------
-- Core (stateless) generators
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

instance Arbitrary RealLit where
  arbitrary = do
    float <- arbitrary :: Gen Double
    -- Note: This is a very rough approximation. It generates a valid REAL
    -- literal, but it may not be exactly the same as the original float
    --  due to formatting differences.
    -- (Haskell's 'show' may use scientific notation- issue?)
    let (floatString, _) = break (== 'e') $ show float
    return $ RealLit floatString (Exponent ExpLetterE (show (floor (logBase 10 (abs float))) :: String))

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
    selector <-
      case base of
        TypeReal -> do
          -- For real types, we can optionally include a kind selector.
          kind   :: Integer <- elements [4,8]
          let kindExpr = ExpValue annotation nullSpan (ValInteger (show kind) Nothing)
          return $ Just $ Selector annotation nullSpan Nothing (Just kindExpr)
        -- No selector
        _ -> return Nothing
    pure $ TypeSpec annotation nullSpan base selector

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

-- | Like 'Arbitrary' but generators run in 'GenM', giving access to the
--   typing environment.
--
--   The default implementation lifts 'arbitrary', so any type with an
--   'Arbitrary' instance gets an 'ArbitraryCtxt' instance for free:
--
-- > instance ArbitraryCtxt BaseType  -- uses default
--
--   Override for types whose generation depends on the environment:
--
-- > instance ArbitraryCtxt (Statement A0) where
-- >   arbitraryCtxt = genDecl
class ArbitraryCtxt a where
  arbitraryCtxt :: GenM a
  default arbitraryCtxt :: Arbitrary a => GenM a
  arbitraryCtxt = liftGen arbitrary

instance ArbitraryCtxt BaseType
instance ArbitraryCtxt (TypeSpec A0)

-- | Generate a fresh variable name based on the current environment size.
freshName :: GenM Name
freshName = do
  env <- get
  pure $ "var" ++ show (Map.size env)

--------------------------------------------------------------------------------
-- Generate typing context and declarations
--------------------------------------------------------------------------------

-- | Generate one declaration statement, adding the variable to the environment.
--   The flag controls whether the declaration carries an initializer
--   (dummy arguments must not be initialised).
genDecl :: Bool -> GenM (Statement A0)
genDecl initialise = do
  name      <- freshName
  typeSpec  <- arbitraryCtxt
  initialExpr <- if initialise
                   then Just <$> genTypedValue typeSpec
                   else pure Nothing
  let
      varExpr   = ExpValue () nullSpan (ValVariable name)
      decl      = Declarator () nullSpan varExpr ScalarDecl Nothing initialExpr
      declList  = AList () nullSpan [decl]
  modify (Map.insert name typeSpec)
  pure $ StDeclaration () nullSpan typeSpec Nothing declList

-- | Generate @n@ declarations, building up the environment as we go.
genDecls :: Bool -> Int -> GenM [Statement A0]
genDecls initialise n = replicateM n (genDecl initialise)

-- | Generate a program unit with a growing set of declarations.
instance Arbitrary (ProgramUnit A0) where
  arbitrary = sized $ \sz -> do
    -- Uses QuickCheck's 'sized' so the number of declarations scales with test size.
    let numDecls = max 1 (sz `div` 5)
    -- Generate some declarations
    (decls, env) <- runStateT (genDecls True numDecls) Map.empty
    -- env is now available for generating expressions / further statements
    let declBlocks  = map (\s -> BlStatement () nullSpan Nothing s) decls
        name    = "generated"
    -- Generate some other subroutines and functions
    procs <- evalStateT genProcedures env
    -- Generate main program unit statements
    -- TODO: these need access to the `procs`
    statements <- evalStateT arbitraryCtxt env :: Gen [Statement A0]
    let computeBlocks = map (\s -> BlStatement () nullSpan Nothing s) statements
    let blocks = declBlocks ++ computeBlocks ++ (printAllEnd env)
    pure $ PUMain () nullSpan (Just name) blocks (Just procs)
    where
      -- print out everything in the environment at the end
      printAllEnd env =
        [ BlStatement () nullSpan Nothing (StPrint () nullSpan (ExpValue () nullSpan ValStar) (fromList' () (map (\n -> ExpValue () nullSpan (ValVariable n)) (Map.keys env)))) ]

instance ArbitraryCtxt (Statement A0) where
  -- Bias assignments over print statements
  arbitraryCtxt = oneofCtxt (printer : replicate 3 assignment)
    where
      assignment :: GenM (Statement A0)
      assignment = do
        (lvar, typ) <- pickVar
        expr <- genTypedExpression typ
        pure $ StExpressionAssign () nullSpan (ExpValue () nullSpan (ValVariable lvar)) expr

      printer :: GenM (Statement A0)
      printer = do
        (name, _) <- pickVar
        let expr = ExpValue () nullSpan (ValVariable name)
        pure $ StPrint () nullSpan (ExpValue () nullSpan ValStar) (fromList' () [expr])

genProcedures :: GenM [ProgramUnit A0]
genProcedures = do
  -- For simplicity, we generate a single procedure
  pu <- genProcedure
  pure [pu]

-- Synthesise some procedures (subroutines or functions), which
-- can make use of a global environment passed to it.
genProcedure :: GenM (ProgramUnit A0)
genProcedure = do
  annotation <- liftGen $ arbitrary
  sz <- liftGen getSize
  numArgs <- liftGen $ choose (0, max 1 (sz `div` 5))
  -- Generate the parameters in a fresh local environment (own scope),
  -- reusing genDecls; the resulting env gives us the argument names.
  (argDecls, localEnv) <- liftGen $ runStateT (genDecls False numArgs) Map.empty
  let argNames  = Map.keys localEnv
      args      = fromList' () (map (ExpValue () nullSpan . ValVariable) argNames)
      declBlocks = map (BlStatement () nullSpan Nothing) argDecls
  pure $ PUFunction annotation nullSpan Nothing (Nothing, Nothing)
             "generated_subroutine" args Nothing declBlocks Nothing

-- Synthesise an expression of the given type
genTypedExpression :: TypeSpec A0 -> GenM (Expression A0)
genTypedExpression typeSpec = do
  -- For simplicity, we just generate a variable reference of the correct type.
  -- In a full implementation, we would generate more complex expressions.
  env <- get
  -- See if a variable can fill the hole
  let candidates = [name | (name, t) <- Map.toList env, t == typeSpec]
  -- If not...
  if null candidates
    -- No variables of the correct type, fall back to arbitrary expression
    then genTypedValue typeSpec  
    else do
      -- Otherwise generate extpressions from the variables
      name <- liftGen $ elements candidates
      annotation <- liftGen $ arbitrary
      value <- oneofCtxt [ pure (ExpValue annotation nullSpan $ ValVariable name)
                        , genTypedValue typeSpec ]  -- In a full implementation, we would generate more complex expressions
      pure value

-- Synthesise a value of the given type
genTypedValue :: TypeSpec A0 -> GenM (Expression A0)
genTypedValue (TypeSpec _ _ baseType _) = case baseType of
  TypeInteger -> do
    x <- liftGen (arbitrary :: Gen Integer)
    pure $ ExpValue () nullSpan (ValInteger (show x) Nothing)
  TypeReal -> do
    x <- liftGen arbitrary
    pure $ ExpValue () nullSpan (ValReal x Nothing)
  TypeLogical -> do
    b <- liftGen (arbitrary :: Gen Bool)
    pure $ ExpValue () nullSpan (ValLogical b Nothing)
  TypeCharacter -> do
    n <- liftGen $ choose (0, 20)
    --TODO: consider utf-8 because maybe this is somewhere things break in compilers
    s <- liftGen $ vectorOf n (choose (' ', '~'))
    let s' = concat (map (\c -> if c == '\'' then "" else if c == '\"' then "\\\"" else [c]) s)
    pure $ ExpValue () nullSpan (ValString s')

instance ArbitraryCtxt a => ArbitraryCtxt [a] where
  arbitraryCtxt = do
    n <- liftGen $ choose (0, 2000)  -- Limit list length for simplicity
    replicateM n arbitraryCtxt

oneofCtxt :: [GenM a] -> GenM a
oneofCtxt gens = do
  gen <- liftGen $ elements gens
  gen

pickVar :: GenM (Name, TypeSpec A0)
pickVar = do
  env <- get
  liftGen $ elements (Map.toList env)

--------------------------------------------------------------------------------
-- Demonstration / experimentation
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
  pus <- generate $ vectorOf 10 (arbitrary :: Gen (ProgramUnit A0))
  let meta = MetaInfo { miVersion = Fortran90, miFilename = "<generated>" }
      programs = map (\pu -> ProgramFile meta [pu]) pus
  mapM_ printOne (zip [1..] programs)
  where
    printOne (i, pf) = do
      putStrLn $ "-- Program " ++ show (i :: Int) ++ " " ++ replicate 60 '-'
      putStrLn $ pprintAndRender Fortran90 pf (Just 2)

--------------------------------------------------------------------------------
-- Generate programs to files
--------------------------------------------------------------------------------

-- | Generate @n@ Fortran programs and write each to @dir/<name>.f90@.
--   Filenames are taken from the PUMain program unit name; unnamed programs
--   are numbered @program_1.f90@, @program_2.f90@, etc.
generatePrograms :: Int -> FilePath -> IO ()
generatePrograms n dir = do
  pus <- generate $ vectorOf n (resize n (arbitrary :: Gen (ProgramUnit A0)))
  let meta = MetaInfo { miVersion = Fortran90, miFilename = "<generated>" }
  forM_ (zip [1 :: Int ..] pus) $ \(i, pu) -> do
    let name = "example" ++ show i
        pu'   = updateName name pu
        pf   = ProgramFile (meta { miFilename = name ++ ".f90" }) [pu']
        src  = pprintAndRender Fortran90 pf (Just 2)
        path = dir </> name ++ ".f90"
    writeFile path src
    putStrLn $ "Written: " ++ path
  where
    updateName name (PUMain a src (Just _) blocks subprog) = PUMain a src (Just name) blocks subprog
    -- TODO: maybe want to expand this.
    updateName name p = p


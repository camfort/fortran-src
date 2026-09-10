{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Fortran.Generate where

import Prelude hiding (EQ, GT, LT)

import Language.Fortran.AST
import Language.Fortran.AST.Literal
import Language.Fortran.AST.Literal.Real
import Test.QuickCheck hiding (Fun)

import Language.Fortran.Util.Position
import Language.Fortran.PrettyPrint
import Language.Fortran.Version

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Control.Monad (forM_, replicateM)
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

-- | Typing Environment for the code generator
data Env = Env
  { localVariables :: Map Name (TypeSpec A0)
  , functions      :: Map Name ([TypeSpec A0], TypeSpec A0)
  , subroutines    :: Map Name [TypeSpec A0]
  }

data VarType = Var | Sub | Fun

emptyEnv :: Env
emptyEnv = Env { localVariables = Map.empty
               , functions      = Map.empty
               , subroutines    = Map.empty
               }

instance Show VarType where
     show Var = "var"
     show Sub = "sub"
     show Fun = "fun"

-- | Stateful generator: a 'Gen' action that can read/write an 'Env'.
type GenM a = StateT Env Gen a

-- | Lift a plain 'Gen' action into 'GenM'.
liftGen :: Gen a -> GenM a
liftGen = lift

-- | Like 'Arbitrary' but generators run in 'GenM', giving access to the
--   typing environment.
--
--   The default implementation lifts 'arbitrary', so any type with an
--   'Arbitrary' instance gets an 'ArbitraryInCtxt' instance for free.

class ArbitraryInCtxt a where
  arbitraryInCtxt :: GenM a
  default arbitraryInCtxt :: Arbitrary a => GenM a
  arbitraryInCtxt = liftGen arbitrary

instance ArbitraryInCtxt BaseType
instance ArbitraryInCtxt (TypeSpec A0)

-- | Generate a fresh variable name based on the current environment size.
freshName :: VarType -> GenM Name
freshName varType = do
  env <- get
  let number =
          case varType of
               Var -> Map.size (localVariables env)
               Sub -> Map.size (subroutines env)
               Fun -> Map.size (functions env)
  return $ show varType ++ show number

--------------------------------------------------------------------------------
-- Generate typing context and declarations
--------------------------------------------------------------------------------

-- | Generate one variable declaration statement, adding the variable to the environment.
--   The flag controls whether the declaration carries an initializer
--   (dummy arguments must not be initialised).
genDecl :: Bool -> GenM (TypeSpec A0, Statement A0)
genDecl initialise = do
  name      <- freshName Var
  typeSpec  <- arbitraryInCtxt
  initialExpr <- if initialise
                   then Just <$> genTypedValue typeSpec
                   else pure Nothing
  let
      varExpr   = ExpValue () nullSpan (ValVariable name)
      decl      = Declarator () nullSpan varExpr ScalarDecl Nothing initialExpr
      declList  = AList () nullSpan [decl]
  modify (\st -> st { localVariables = Map.insert name typeSpec (localVariables st) } )
  pure (typeSpec, StDeclaration () nullSpan typeSpec Nothing declList)

-- | Generate @n@ declarations, building up the environment as we go.
genDecls :: Bool -> Int -> GenM [(TypeSpec A0, Statement A0)]
genDecls initialise n = replicateM n (genDecl initialise)

-- | Generate a program unit with a growing set of declarations.
instance Arbitrary (ProgramUnit A0) where
  arbitrary = sized $ \sz -> do
    -- Generate some other subroutines and functions
    (procs, env) <- runStateT genProcedures emptyEnv

    -- Generate some top-level declarations for the main program
    -- Uses QuickCheck's 'sized' so the number of declarations scales with test size.
    let numDecls = max 1 (sz `div` 5)
    (decls, env') <- runStateT (genDecls True numDecls) env
    let declBlocks  = map (\(_, s) -> BlStatement () nullSpan Nothing s) decls
    -- Generate main program unit's statements
    topLevelBlocks <- evalStateT genBodyBlocks env
    let blocks = declBlocks ++ topLevelBlocks ++ printAllEnd env'
    let name = "generated"
    pure $ PUMain () nullSpan (Just name) blocks (Just procs)
    where
      -- print out everything in the environment at the end
      printAllEnd env =
        [ BlStatement () nullSpan Nothing (StPrint () nullSpan (ExpValue () nullSpan ValStar) 
            (fromList' () (map (\n -> ExpValue () nullSpan (ValVariable n)) (Map.keys (localVariables env))))) ]

instance ArbitraryInCtxt (Statement A0) where
  -- Pick 
  arbitraryInCtxt = oneofCtxt ([printer] ++ replicate 2 subroutine ++ replicate 3 assignment)
    where
     assignment :: GenM (Statement A0)
     assignment = do
          (lvar, typ) <- pickVar
          expr <- genTypedExpression typ
          pure $ StExpressionAssign () nullSpan (ExpValue () nullSpan (ValVariable lvar)) expr

     subroutine :: GenM (Statement A0)
     subroutine = do
          -- Pick a subroutine; fall back to assignment if none exist yet
          env <- get
          if Map.null (subroutines env)
            then assignment
            else do
              (subName, subArgTypes) <- liftGen $ elements (Map.toList $ subroutines env)
              -- Generate expressions for each argument
              argExprs <- mapM genTypedExpression subArgTypes
              let subExpr = ExpValue () nullSpan (ValVariable subName)
                  argList = AList () nullSpan (map (Argument () nullSpan Nothing . ArgExpr) argExprs)
              pure $ StCall () nullSpan subExpr argList

     printer :: GenM (Statement A0)
     printer = do
       (name, _) <- pickVar
       let expr = ExpValue () nullSpan (ValVariable name)
       pure $ StPrint () nullSpan (ExpValue () nullSpan ValStar) (fromList' () [expr])

-- | Generate the statements of a body as blocks
genBodyBlocks :: GenM [Block A0]
genBodyBlocks = do
     env <- get
     -- Statements need at least one variable to refer to
     if Map.null (localVariables env)
       then pure []
       else do
         sz <- liftGen getSize
         n <- liftGen $ choose (0, sz)
         statements <- replicateM n (arbitraryInCtxt :: GenM (Statement A0))
         pure $ map (BlStatement () nullSpan Nothing) statements

-- Generate a list of procedures (subroutines or functions)
genProcedures :: GenM [ProgramUnit A0]
genProcedures = do
  sz <- liftGen getSize
  numProcs <- liftGen $ choose (1, max 1 (sz `div` 5))
  -- Names are made unique by freshName
  replicateM numProcs genProcedure

-- Synthesise some procedures (subroutines or functions), which
-- can make use of a global environment passed to it.
genProcedure :: GenM (ProgramUnit A0)
genProcedure = do
     -- Choose if we are generating a subroutine or function
     isSubroutine <- liftGen (arbitrary :: Gen Bool)
     name <- freshName (if isSubroutine then Sub else Fun)

     annotation <- liftGen $ arbitrary
     sz <- liftGen getSize
     numArgs <- liftGen $ choose (0, max 1 (sz `div` 5))
     -- Generate the parameters in a fresh local environment (own scope),
     -- reusing genDecls; the resulting env gives us the argument names.
     env_before <- get
     -- Blank out local variables
     modify (\env -> env { localVariables = Map.empty } )
     argDecls <- genDecls False numArgs
     let argTypes = map fst argDecls

     -- Generate parameters and declaration statements for the parameter
     env <- get
     let argNames   = Map.keys (localVariables env)
         args       = fromList' () (map (ExpValue () nullSpan . ValVariable) argNames)
         declBlocks = map (\(_, s) -> BlStatement () nullSpan Nothing s) argDecls

     -- Generate the body of the procedure
     bodyBlocks <- genBodyBlocks

     -- Produce the final procedure AST node, updating the type environment
     pu <- if isSubroutine
       then do
          modify (\env -> env { subroutines = Map.insert name argTypes (subroutines env) })
          pure $ PUSubroutine annotation nullSpan (Nothing, Nothing) name args (declBlocks ++ bodyBlocks) Nothing
       else do

         -- Decide what the return result will be for a function
         returnType <- liftGen (arbitrary :: Gen (TypeSpec A0))
         returnValue <- genTypedExpression returnType
         -- Functions return by assigning to their own name
         let returnBlock = BlStatement () nullSpan Nothing
               (StExpressionAssign () nullSpan (ExpValue () nullSpan (ValVariable name)) returnValue)

         modify (\env -> env { functions = Map.insert name (argTypes, returnType) (functions env) })

         pure $ PUFunction annotation nullSpan (Just returnType) (Nothing, Nothing)
             name args Nothing (declBlocks ++ bodyBlocks ++ [returnBlock]) Nothing

     -- Restore the caller's local variables so procedure locals don't leak
     modify (\env -> env { localVariables = localVariables env_before })
     pure pu

-- | Signatures of binary operators, restricted to the base types we
--   synthesise over (integer, real, logical): each entry gives an operator
--   together with the base types of its left operand, right operand, and
--   result. Compare with the type-checker's classification of the same
--   operators in 'Language.Fortran.Analysis.Types.binaryOpType'.
binaryOpTable :: [(BinaryOp, BaseType, BaseType, BaseType)]
binaryOpTable =
     -- Arithmetic: operands and result share a numeric type
     [ (op, ty, ty, ty)
     | op <- [Addition, Subtraction, Multiplication, Division, Exponentiation]
     , ty <- [TypeInteger, TypeReal]
     ] ++
     -- Relational: numeric operands, logical result
     [ (op, ty, ty, TypeLogical)
     | op <- [GT, GTE, LT, LTE, EQ, NE]
     , ty <- [TypeInteger, TypeReal]
     ] ++
     -- Logical: logical operands and result
     [ (op, TypeLogical, TypeLogical, TypeLogical)
     | op <- [Or, XOr, And, Equivalent, NotEquivalent]
     ]

-- | Signatures of unary operators, restricted the same way as 'binaryOpTable'.
unaryOpTable :: [(UnaryOp, BaseType, BaseType)]
unaryOpTable =
     [ (op, ty, ty) | op <- [Plus, Minus], ty <- [TypeInteger, TypeReal] ]
     ++ [ (Not, TypeLogical, TypeLogical) ]

-- Synthesise an expression of a given base type (any kind/selector).
genTypedExpressionOfBase :: BaseType -> GenM (Expression A0)
genTypedExpressionOfBase base = liftGen (genTypeSpecOfBase base) >>= genTypedExpression

-- Synthesise an expression of the given type
genTypedExpression :: TypeSpec A0 -> GenM (Expression A0)
genTypedExpression typeSpec = do
     -- Choose a strategy: variable, value, function application, or operator
     oneofCtxt [variable, expression, binaryOpExpr, unaryOpExpr, genTypedValue typeSpec]

     where
          TypeSpec _ _ goalBaseType _ = typeSpec

          {-
               Synthesis based on the sequent calculus style of function application

               G, x : B |- C => t2
               G        |- A => t1
               ------------------------------------
               G, f : A -> B |- C => [f(t1) / x] t2
          -}
          expression :: GenM (Expression A0)
          expression = do
               -- Pick a function from the context
               env <- get
               (fun, (param_types, return_type)) <- liftGen $ oneof (map pure (Map.toList $ functions env))
               -- Then.. make a fresh variable
               temp_var <- freshName Var
               -- add to local environment a binding of temp_var with the return type
               modify (\env -> env { localVariables = Map.insert temp_var return_type (localVariables env) })
               -- Generate something for the goal using this
               t1 <- genTypedExpression typeSpec
               -- Generate the arguments
               argument_exprs <- mapM genTypedExpression param_types
               -- Performance a syntactic substitution of temp_var for an function application of
               -- `fun` with the `arguments`
               let arguments = fromList () (map (Argument () nullSpan Nothing . ArgExpr) argument_exprs)
               let fun_call = ExpFunctionCall () nullSpan (ExpValue () nullSpan (ValVariable fun)) arguments
               return $ substitute fun_call temp_var t1

          {-
               Same sequent calculus style rule as 'expression', but for a binary
               operator standing in for the "function" being applied:

               G, x : B |- C => t2
               G |- A1 => t1     G |- A2 => t3
               ------------------------------------------
               G, op : A1 -> A2 -> B |- C => [(t1 `op` t3) / x] t2
          -}
          binaryOpExpr :: GenM (Expression A0)
          binaryOpExpr =
               case [ e | e@(_, _, _, result) <- binaryOpTable, result == goalBaseType ] of
                    [] -> genTypedValue typeSpec
                    candidates -> do
                         (op, lhsBase, rhsBase, _) <- liftGen $ elements candidates
                         temp_var <- freshName Var
                         modify (\env -> env { localVariables = Map.insert temp_var typeSpec (localVariables env) })
                         t1 <- genTypedExpression typeSpec
                         lhs <- genTypedExpressionOfBase lhsBase
                         rhs <- genTypedExpressionOfBase rhsBase
                         let opExpr = ExpBinary () nullSpan op lhs rhs
                         return $ substitute opExpr temp_var t1

          -- As 'binaryOpExpr', but for unary operators.
          unaryOpExpr :: GenM (Expression A0)
          unaryOpExpr =
               case [ e | e@(_, _, result) <- unaryOpTable, result == goalBaseType ] of
                    [] -> genTypedValue typeSpec
                    candidates -> do
                         (op, argBase, _) <- liftGen $ elements candidates
                         temp_var <- freshName Var
                         modify (\env -> env { localVariables = Map.insert temp_var typeSpec (localVariables env) })
                         t1 <- genTypedExpression typeSpec
                         arg <- genTypedExpressionOfBase argBase
                         let opExpr = ExpUnary () nullSpan op arg
                         return $ substitute opExpr temp_var t1

          variable :: GenM (Expression A0)
          variable = do
               env <- get
               -- See if a variable can fill the hole
               let candidates = [name | (name, t) <- Map.toList (localVariables env), t == typeSpec]
               -- If not...
               if null candidates
                 -- No variables of the correct type, fall back to arbitrary expression
                 then genTypedValue typeSpec
                 else do
                   -- Otherwise generate expressions from the variables
                   name <- liftGen $ elements candidates
                   annotation <- liftGen $ arbitrary
                   oneofCtxt [ pure (ExpValue annotation nullSpan $ ValVariable name)
                                     , genTypedValue typeSpec ]  -- In a full implementation, we would generate more complex expressions

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
     _ -> error "Cannot generate"


instance ArbitraryInCtxt a => ArbitraryInCtxt [a] where
  arbitraryInCtxt = do
    sz <- liftGen getSize
    n <- liftGen $ choose (0, sz)
    replicateM n arbitraryInCtxt

oneofCtxt :: [GenM a] -> GenM a
oneofCtxt gens = do
  gen <- liftGen $ elements gens
  gen

pickVar :: GenM (Name, TypeSpec A0)
pickVar = do
  env <- get
  liftGen $ elements (Map.toList $ localVariables env)

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
  -- Grow the size with the program index so programs get progressively bigger
  pus <- generate $ mapM (\i -> resize i (arbitrary :: Gen (ProgramUnit A0))) [1 .. n]
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


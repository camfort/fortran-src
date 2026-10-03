{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Fortran.Generate where

import Prelude hiding (EQ, GT, LT)

import Language.Fortran.AST
import qualified Language.Fortran.AST.AList as AList
import Language.Fortran.AST.Literal.Real
import Test.QuickCheck hiding (Fun)

import Language.Fortran.Util.Position
import Language.Fortran.PrettyPrint
import Language.Fortran.Version

import Text.PrettyPrint

import Control.Monad (forM_, replicateM)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath ((</>))
import Data.Generics.Uniplate.Data (universeBi)

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
    floatLit <- arbitrary :: Gen Double
    -- Note: This is a very rough approximation. It generates a valid REAL
    -- literal, but it may not be exactly the same as the original float
    --  due to formatting differences.
    -- (Haskell's 'show' may use scientific notation- issue?)
    let (floatString, _) = break (== 'e') $ show floatLit
    return $ RealLit floatString (Exponent ExpLetterE (show (floor (logBase 10 (abs floatLit))) :: String))

instance Arbitrary BaseType where
  arbitrary = arbitraryBaseType False

arbitraryBaseType :: Bool -> Gen BaseType
arbitraryBaseType incReals = oneof $
    [ pure TypeInteger
    , pure TypeLogical
    , pure TypeCharacter
    ]
    ++ [pure TypeReal | incReals]

instance Arbitrary a => Arbitrary (TypeSpec a) where
  arbitrary = arbitrary >>= genTypeSpecOfBase

-- | Generate a 'TypeSpec' with a specific 'BaseType' (e.g. so that an
--   operator's operand type can be pinned to what it requires), filling in
--   the rest (annotation, kind selector) arbitrarily.
genTypeSpecOfBase :: Arbitrary a => BaseType -> Gen (TypeSpec a)
genTypeSpecOfBase base = do
  annotation <- arbitrary
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

instance Arbitrary Intent where
  arbitrary = elements [In, Out, InOut]

--------------------------------------------------------------------------------
-- Stateful generation
--------------------------------------------------------------------------------

-- | Typing Environment for the code generator
data Env = Env
  { localVariables :: Map Name (TypeSpec A0, Maybe Intent)
  , functions      :: Map Name ([(TypeSpec A0, Intent)], TypeSpec A0)
  , subroutines    :: Map Name [(TypeSpec A0, Intent)]
  , includeReals   :: Bool
  }

-- | Local variables (Nothing intent) and In/InOut arguments are readable in r-expressions.
readableLocalVariables :: Env -> Map Name (TypeSpec A0)
readableLocalVariables env = Map.map fst $ Map.filter (isReadable . snd) (localVariables env)
  where isReadable Nothing         = True
        isReadable (Just In)       = True
        isReadable (Just InOut)    = True
        isReadable _               = False

-- | Local variables (Nothing intent) and Out/InOut arguments are writable as l-values.
writableLocalVariables :: Env -> Map Name (TypeSpec A0)
writableLocalVariables env = Map.map fst $ Map.filter (isWritable . snd) (localVariables env)
  where isWritable Nothing         = True
        isWritable (Just Out)      = True
        isWritable (Just InOut)    = True
        isWritable _               = False

data VarType = Var | Sub | Fun

emptyEnv :: Env
emptyEnv = Env { localVariables = Map.empty
               , functions      = Map.empty
               , subroutines    = Map.empty
               , includeReals   = True
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

instance ArbitraryInCtxt BaseType where
  arbitraryInCtxt = do
    env <- get
    liftGen $ arbitraryBaseType (includeReals env)

instance ArbitraryInCtxt (TypeSpec A0) where
  arbitraryInCtxt = arbitraryInCtxt >>= liftGen . genTypeSpecOfBase

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
--   @initialise@ controls whether the declaration carries an initializer
--   (dummy arguments must not be initialised).
--   @isArg@ controls whether a random 'Intent' attribute is generated
--   (only dummy arguments carry intent; local variables do not).
-- | Also returns the declared variable's 'Name': callers that need to
--   preserve declaration order (e.g. building a dummy-argument list) must
--   not recover names via 'Map.keys' on 'localVariables', since that sorts
--   lexicographically ("var10" < "var2") rather than by declaration order.
genDecl :: Bool -> Bool -> GenM (Name, TypeSpec A0, Maybe Intent, Statement A0)
genDecl initialise isArg = do
  name      <- freshName Var
  typeSpec  <- arbitraryInCtxt
  initialExpr <- if initialise && not isArg
                   then Just <$> genTypedValue typeSpec
                   else pure Nothing
  mintent   <- if isArg
                 then Just <$> liftGen arbitrary
                 else pure Nothing
  let attrs = fmap (\i -> AList.fromList () [AttrIntent () nullSpan i]) mintent
  let
      varExpr   = ExpValue () nullSpan (ValVariable name)
      decl      = Declarator () nullSpan varExpr ScalarDecl Nothing initialExpr
      declList  = AList () nullSpan [decl]
  modify (\st -> st { localVariables = Map.insert name (typeSpec, mintent) (localVariables st) } )
  pure (name, typeSpec, mintent, StDeclaration () nullSpan typeSpec attrs declList)

-- | Generate @n@ declarations, building up the environment as we go.
genDecls :: Bool -> Bool -> Int -> GenM [(Name, TypeSpec A0, Maybe Intent, Statement A0)]
genDecls initialise isArg n = replicateM n (genDecl initialise isArg)

-- | Generate a program unit with a growing set of declarations.
instance Arbitrary (ProgramUnit A0) where
  arbitrary = genProgramUnit True

-- | Generate a 'ProgramUnit', with 'incReals' controlling whether 'TypeReal'
--   may appear in generated declarations and expressions.
genProgramUnit :: Bool -> Gen (ProgramUnit A0)
genProgramUnit incReals = sized $ \sz -> do
    let startEnv = emptyEnv { includeReals = incReals }
    -- Generate some other subroutines and functions
    (procs, env) <- runStateT genProcedures startEnv

    -- Generate some top-level declarations for the main program
    -- Uses QuickCheck's 'sized' so the number of declarations scales with test size.
    let numDecls = max 1 (sz `div` 5)
    (decls, env') <- runStateT (genDecls True False numDecls) env
    let declBlocks  = map (\(_, _, _, s) -> BlStatement () nullSpan Nothing s) decls
    -- Generate main program unit's statements
    topLevelBlocks <- evalStateT genBodyBlocks env'
    let blocks = declBlocks ++ topLevelBlocks ++ printAllEnd env'
    pure $ PUMain () nullSpan (Just "generated") blocks (Just procs)
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
      env <- get
      if null (writableLocalVariables env)
        then arbitraryInCtxt
        else do
          (lvar, typ) <- pickVar writableLocalVariables
          expr <- genTypedExpression typ
          pure $ StExpressionAssign () nullSpan (ExpValue () nullSpan (ValVariable lvar)) expr

     subroutine :: GenM (Statement A0)
     subroutine = do
          -- Pick a subroutine whose Out/InOut params can all be satisfied by
          -- a writable local variable; fall back to assignment if none exist.
          env <- get
          let viableSubs = [ sig | sig@(_, argTys) <- Map.toList (subroutines env)
                                  , all (hasSuitableActualArg env) argTys ]
          if null viableSubs
            then assignment
            else do
              (subName, subArgTypes) <- liftGen $ elements viableSubs
              -- Generate expressions for each argument
              argExprs <- mapM genTypedExpressionForIntent subArgTypes
              let subExpr = ExpValue () nullSpan (ValVariable subName)
                  argList = AList () nullSpan (map (Argument () nullSpan Nothing . ArgExpr) argExprs)
              pure $ StCall () nullSpan subExpr argList

     printer :: GenM (Statement A0)
     printer = do
       env <- get
       if Map.null (readableLocalVariables env)
         then assignment
         else do
           (name, _) <- pickVar readableLocalVariables
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
     argDecls <- genDecls False True numArgs
     -- Every argument declaration carries a Just intent (isArg = True above).
     let argTypes = [ (ts, intent) | (_, ts, Just intent, _) <- argDecls ]

     -- Generate parameters and declaration statements for the parameter.
     -- Names must stay in declaration order (matching argTypes above) since
     -- actual arguments are matched positionally; Map.keys would instead
     -- sort lexicographically ("var10" before "var2"), misaligning the
     -- dummy-argument list against the types used to generate call sites.
     let argNames   = [ n | (n, _, _, _) <- argDecls ]
         args       = fromList' () (map (ExpValue () nullSpan . ValVariable) argNames)
         declBlocks = map (\(_, _, _, s) -> BlStatement () nullSpan Nothing s) argDecls

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
numericTypes :: Bool -> [BaseType]
numericTypes incReals = TypeInteger : [TypeReal | incReals]

binaryOpTable :: Bool -> [(BinaryOp, BaseType, BaseType, BaseType)]
binaryOpTable incReals =
     -- Arithmetic: operands and result share a numeric type
     [ (op, ty, ty, ty)
     | op <- [Addition, Subtraction, Multiplication, Division, Exponentiation]
     , ty <- numericTypes incReals
     ] ++
     -- Relational: numeric operands, logical result
     [ (op, ty, ty, TypeLogical)
     | op <- [GT, GTE, LT, LTE, EQ, NE]
     , ty <- numericTypes incReals
     ] ++
     -- Logical: logical operands and result
     [ (op, TypeLogical, TypeLogical, TypeLogical)
     | op <- [Or, XOr, And, Equivalent, NotEquivalent]
     ]

-- | Signatures of unary operators, restricted the same way as 'binaryOpTable'.
unaryOpTable :: Bool -> [(UnaryOp, BaseType, BaseType)]
unaryOpTable incReals =
     [ (op, ty, ty) | op <- [Minus], ty <- numericTypes incReals ]
     ++ [ (Not, TypeLogical, TypeLogical) ]

-- Synthesise an expression of a given base type (any kind/selector).
genTypedExpressionOfBase :: BaseType -> GenM (Expression A0)
genTypedExpressionOfBase base = liftGen (genTypeSpecOfBase base) >>= genTypedExpression

-- | Generate the right-hand operand of a binary operator, avoiding programs
-- that might commonly be rejected by compilers when doing constant propoagation
-- i.e., division by zero, and avoiding overflow due to large exponents
genRhsOperand :: BinaryOp -> BaseType -> GenM (Expression A0)
genRhsOperand Division       base = liftGen (genTypeSpecOfBase base) >>= genNonZeroValue
genRhsOperand Exponentiation base = liftGen (genTypeSpecOfBase base) >>= genSmallNonNegValue
genRhsOperand _               base = genTypedExpressionOfBase base

-- | Generate the left-hand operand of a binary operator. Only
--   'Exponentiation' needs special treatment: its base must also be a
--   small, atomic literal (not a recursively-generated expression), because
--   otherwise two exponentiations can nest (e.g. @(base ** e1) ** e2@) and
--   compound their magnitudes past INTEGER(4) range even when each
--   individual exponent is small. Keeping the base a bounded literal (like
--   the exponent) also keeps it immune to 'withBoundVar''s substitution.
genLhsOperand :: BinaryOp -> BaseType -> GenM (Expression A0)
genLhsOperand Exponentiation base = liftGen (genTypeSpecOfBase base) >>= genSmallBaseValue
genLhsOperand _              base = genTypedExpressionOfBase base

-- | A small-magnitude literal (for an 'Exponentiation' base): combined with
--   the bounded exponent from 'genSmallNonNegValue', the largest possible
--   magnitude (10^4) stays far clear of the INTEGER(4) range.
genSmallBaseValue :: TypeSpec A0 -> GenM (Expression A0)
genSmallBaseValue (TypeSpec _ _ TypeInteger _) = do
  x <- liftGen $ choose (-10, 10 :: Integer)
  pure $ ExpValue () nullSpan (ValInteger (show x) Nothing)
genSmallBaseValue ts = genTypedValue ts

-- | A literal guaranteed not to be zero (for a 'Division' divisor).
genNonZeroValue :: TypeSpec A0 -> GenM (Expression A0)
genNonZeroValue (TypeSpec _ _ TypeInteger _) = do
  x <- liftGen $ (arbitrary :: Gen Integer) `suchThat` (/= 0)
  pure $ ExpValue () nullSpan (ValInteger (show x) Nothing)
genNonZeroValue ts = genTypedValue ts

-- | A small, non-negative literal (for an 'Exponentiation' exponent): keeps
--   fully-literal exponentiations within INTEGER(4) range, and (since the
--   base can independently be a literal 0) avoids "0 ** negative", which is
--   itself a division by zero.
genSmallNonNegValue :: TypeSpec A0 -> GenM (Expression A0)
genSmallNonNegValue (TypeSpec _ _ TypeInteger _) = do
  x <- liftGen $ choose (0, 4 :: Integer)
  pure $ ExpValue () nullSpan (ValInteger (show x) Nothing)
genSmallNonNegValue ts = genTypedValue ts

-- | Fortran requires the actual argument for an Out/InOut dummy parameter to
--   be a definable variable reference (the callee writes back into it), not
--   an arbitrary expression or literal. So for those intents we must pick a
--   variable from the *writable* locals, never fall back to a literal.
--   'In' parameters can take any r-expression, as usual.
genTypedExpressionForIntent :: (TypeSpec A0, Intent) -> GenM (Expression A0)
genTypedExpressionForIntent (t, In) = genTypedExpression t
genTypedExpressionForIntent (t, _)  = writableVariable t

-- | Pick a definable (writable) local variable of the given type. The caller
--   must already have ensured at least one such variable exists (see
--   'hasSuitableActualArg'); used for Out/InOut actual arguments.
writableVariable :: TypeSpec A0 -> GenM (Expression A0)
writableVariable typeSpec = do
  env <- get
  let candidates = [ name | (name, ts) <- Map.toList (writableLocalVariables env), ts == typeSpec ]
  name <- liftGen $ elements candidates
  pure (ExpValue () nullSpan $ ValVariable name)

-- | Whether the current scope has what it needs to supply an actual argument
--   for this dummy-parameter signature: any expression for 'In', but a
--   writable variable of the matching type for 'Out'/'InOut'.
hasSuitableActualArg :: Env -> (TypeSpec A0, Intent) -> Bool
hasSuitableActualArg _   (_,  In) = True
hasSuitableActualArg env (ts, _)  = ts `elem` Map.elems (writableLocalVariables env)

-- Synthesise an expression of the given type
genTypedExpression :: TypeSpec A0 -> GenM (Expression A0)
genTypedExpression typeSpec = do
     sz <- liftGen getSize
     -- Choose a strategy: variable, value, function application, or operator.
     -- The recursive strategies are only available while there is size left,
     -- otherwise generation is an unbounded branching process and may not terminate.
     if sz <= 0
       then oneofCtxt [variable typeSpec, genTypedValue typeSpec]
       else oneofCtxt [variable typeSpec, expression, binaryOpExpr, unaryOpExpr, genTypedValue typeSpec]

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
               -- Pick a function whose Out/InOut params can all be satisfied
               -- by a writable local variable.
               env <- get
               let viableFuns = [ sig | sig@(_, (param_types, _)) <- Map.toList (functions env)
                                       , all (hasSuitableActualArg env) param_types ]
               if null viableFuns
                then
                  -- try something else as we have no viable functions
                    genTypedValue typeSpec
                else do
                  (fun, (param_types, return_type)) <- liftGen $ elements viableFuns
                  withBoundVar typeSpec return_type $ do
                    -- Generate the arguments
                    argument_exprs <- mapM (smaller . genTypedExpressionForIntent) param_types
                    let arguments = fromList () (map (Argument () nullSpan Nothing . ArgExpr) argument_exprs)
                    pure $ ExpFunctionCall () nullSpan (ExpValue () nullSpan (ValVariable fun)) arguments

          {-
               Same sequent calculus style rule as 'expression', but for a binary
               operator standing in for the "function" being applied:

               G, x : B |- C => t2
               G |- A1 => t1     G |- A2 => t3
               ------------------------------------------
               G, op : A1 -> A2 -> B |- C => [(t1 `op` t3) / x] t2
          -}
          binaryOpExpr :: GenM (Expression A0)
          binaryOpExpr = do
               env <- get
               case [ e | e@(_, _, _, result) <- binaryOpTable (includeReals env), result == goalBaseType ] of
                    [] -> genTypedValue typeSpec
                    candidates -> do
                         (op, lhsBase, rhsBase, _) <- liftGen $ elements candidates
                         withBoundVar typeSpec typeSpec $ do
                           lhs <- smaller $ genLhsOperand op lhsBase
                           rhs <- smaller $ genRhsOperand op rhsBase
                           pure $ ExpBinary () nullSpan op lhs rhs

          -- As 'binaryOpExpr', but for unary operators.
          unaryOpExpr :: GenM (Expression A0)
          unaryOpExpr = do
               env <- get
               case [ e | e@(_, _, result) <- unaryOpTable (includeReals env), result == goalBaseType ] of
                    [] -> genTypedValue typeSpec
                    candidates -> do
                         (op, argBase, _) <- liftGen $ elements candidates
                         withBoundVar typeSpec typeSpec $ do
                           arg <- smaller $ genTypedExpressionOfBase argBase
                           pure $ ExpUnary () nullSpan op arg

variable :: TypeSpec A0 -> GenM (Expression A0)
variable typeSpec = do
      env <- get
      -- Only variables with In or InOut intent are valid in an r-expression
      let candidates = [ name
                        | (name, ts) <- Map.toList (readableLocalVariables env)
                        , ts == typeSpec]
      if null candidates
        then genTypedValue typeSpec
        else do
          name <- liftGen $ elements candidates
          annotation <- liftGen $ arbitrary
          oneofCtxt [ pure (ExpValue annotation nullSpan $ ValVariable name)
                    , genTypedValue typeSpec ]

-- | Run a generator on a smaller size (used for sub-terms so that
--   expression generation terminates).
smaller :: GenM a -> GenM a
smaller = mapStateT (scale (`div` 2))

-- | The shared "cut" of the sequent calculus style rules: bind a fresh
--   variable @x : tempType@, synthesise @t2@ for the goal under that binding,
--   generate @e@ (without @x@ in scope), and return @[e / x] t2@.
--
--   The binding is scoped: @x@ is removed from the environment afterwards so it
--   can never leak out as an undeclared variable. If @t2@ happens not to use
--   @x@ then the rule would be vacuous, so when @e@ itself has the goal type we
--   return @e@ (i.e., take @t2 = x@) rather than discarding it.
withBoundVar :: TypeSpec A0 -> TypeSpec A0 -> GenM (Expression A0) -> GenM (Expression A0)
withBoundVar goal tempType genE = do
     temp_var <- freshName Var
     before <- gets localVariables
     -- temp_var stands in for the result of 'genE', an arbitrary expression
     -- (e.g. a function call), not necessarily a variable. It must be marked
     -- read-only (In): if it were writable it could be picked as the actual
     -- argument for an Out/InOut dummy parameter while generating t2, and
     -- substituting 'e' in afterwards would then plug a non-variable
     -- expression into a slot that Fortran requires to be a definable
     -- variable reference.
     modify (\env -> env { localVariables = Map.insert temp_var (tempType, Just In) before })
     t2 <- smaller $ genTypedExpression goal
     modify (\env -> env { localVariables = before })
     e <- genE
     let used = not $ null [ () | ExpValue _ _ (ValVariable v) <- universeBi t2 :: [Expression A0], v == temp_var ]
     pure $ if used then substitute e temp_var t2
            else if tempType == goal then e
            else t2

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
          -- OLD STUFF for strings
          -- n <- liftGen $ choose (0, 20)
          --TODO: consider utf-8 because maybe this is somewhere things break in compilers
          --s <- liftGen $ vectorOf n (choose (' ', '~'))
          s <- liftGen $ choose (' ', '~')
          let s' = concat (map (\c -> if c == '\'' then "" else if c == '\"' then "\\\"" else [c]) [s])
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

pickVar :: (Env -> Map Name (TypeSpec A0)) -> GenM (Name, TypeSpec A0)
pickVar ability = do
  env <- get
  liftGen $ elements (Map.toList $ ability env)

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
--   When @incReals@ is 'False', 'TypeReal' is excluded from all generated types.
generatePrograms :: Bool -> Int -> FilePath -> IO ()
generatePrograms incReals n dir = do
  -- Grow the size with the program index so programs get progressively bigger
  pus <- generate $ mapM (\i -> resize i (genProgramUnit incReals)) [1 .. n]
  let meta = MetaInfo { miVersion = Fortran90, miFilename = "<generated>" }
  forM_ (zip [1 :: Int ..] pus) $ \(i, pu) -> do
    let name = "example" ++ show i
        pu'   = updateName name pu
        pf   = ProgramFile (meta { miFilename = name ++ ".f90" }) [pu']
        -- Deeply-nested generated expressions can produce lines longer than
        -- Fortran's fixed line-length limit; split them with continuations
        -- so gfortran doesn't reject them as truncated/malformed.
        src  = reformatMixedFormInsertContinuations $ pprintAndRender Fortran90 pf (Just 2)
        path = dir </> name ++ ".f90"
    writeFile path src
    putStrLn $ "Written: " ++ path
  where
    updateName name (PUMain a src (Just _) blocks subprog) = PUMain a src (Just name) blocks subprog
    -- TODO: maybe want to expand this.
    updateName name p = p


{-| Fortran expression evaluation.

Operators are evaluated by evaluating each individual expression, then running
the operator on them.

We support evaluating concrete values, and concrete types.

These evaluation modules are intended for intrinsics and such pure functions, so
the environment stays static inside evaluation.

Most interesting intrinsic procedures are pure functions. A more powerful
evaluation strategy would emulate memory via monadic state, and support
subroutines (non-returning functions) explicitly. There are a handful of
intrinsic subroutines, such as @SRAND@ in gfortran, and some may be called as
either a function or a subroutine e.g. @SIGNAL@, but these aren't "interesting".

Some intrinsics are integral to a basic syntax analysis: for example, the
numeric binary operators permitted in constant PARAMETER definitions. For other
intrinsics, we may only want to record the types.
-}

module Language.Fortran.Repr.Eval where

import           Language.Fortran.AST
import           Language.Fortran.AST.Literal.Real
import           Language.Fortran.AST.Literal.Complex
import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.Repr.Value
import           Language.Fortran.Repr.Value.Scalar
import           Language.Fortran.Repr.Type

import qualified Data.Data as Data
import           Data.Data ( Data )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import qualified Data.Text as Text

-- | Immutable expression evaluation environment, polymorphic over the
--   expression representation (values, or types).
data Env a = Env
  { envVars :: Map Name a
  , envOps  :: Map Name (Op a)
  }

newtype Op a = Op { unOp :: Env a -> [a] -> Either Error a }

data Error
  = ErrorNoSuchVar Name
  -- ^ No variable with that name in the environment.

  | ErrorNoSuchOp  Name
  -- ^ No operator with that name in the environment.

  | ErrorNoSuchKindForType String String
  -- ^ The kind provided is invalid for the given kinded scalar type.

  | ErrorNonIntegerKind String
  -- ^ The kind provided is invalid for the given kinded scalar type.

  | ErrorUnsupported String String
  -- ^ Some part of the AST wasn't supported (type, constructor name).

  | ErrorStr String
    deriving (Eq, Show)

-- | Helper to grab a value's constructor and wrap it into an error string.
errUnsupported :: Data x => String -> x -> Either Error a
errUnsupported ty = Left . ErrorUnsupported ty . show . Data.toConstr

-- | Evaluate a Fortran expression in the given environment.
evalExpr :: Data a => Env FVal -> Expression a -> Either Error FVal
evalExpr env = \case
  ExpValue _a _ss v -> evalValue env v
  e -> errUnsupported "expression" e

-- | Evaluate a Fortran value in the given environment.
--
-- Note that constants may store information that needs resolving via the
-- environment e.g. kind parameters, so we must retain the environment.
evalValue :: Data a => Env FVal -> Value a -> Either Error FVal
evalValue env = \case
  ValVariable v -> evalLookup env v
  ValString   s -> return $ FValScalar $ FValScalarString $ Text.pack s
  v -> errUnsupported "value" v

evalLookup :: Env FVal -> Name -> Either Error FVal
evalLookup env v =
    case Map.lookup v (envVars env) of
      Nothing  -> Left $ ErrorNoSuchVar v
      Just val -> Right val

evalNumeric :: Env FVal -> Value a -> Either Error FValScalar
evalNumeric env = \case
  -- regular kinded numeric literals: resolve kind, possibly bundle with value
  ValInteger i mkp -> do
    k <- resolveKindParamTo env "INTEGER" FTypeInt4  parseKindInt  mkp
    return $ FValScalarInt  $ FValInt  k (read i)
  ValReal    r mkp -> do
    k <- resolveKindParamTo env "REAL"    FTypeReal4 parseKindReal mkp
    return $ FValScalarReal $ FValReal k (readRealLit r)
  ValLogical b mkp -> do
    -- resolve kind, but throw it away (we store a true 'Bool' for LOGICALs)
    _ <- resolveKindParamTo env "LOGICAL" FTypeInt4  parseKindInt  mkp
    return $ FValScalarLogical b

  -- complex lits need more thought
  ValComplex c -> FValScalarComplex <$> evalComplex env c

  -- BOZ constants are untyped, so you usually don't want to evaluate them
  -- outside some context - but @INTEGER(4)@ is a useful, safe default
  ValBoz b -> undefined

  _ -> error "Language.Fortran.Repr.Eval.evalNumeric: argument was not numeric value"

evalComplex :: Env FVal -> ComplexLit a -> Either Error FValComplex
evalComplex env = undefined

-- | Resolve a 'KindParam' to a typed kind tag using the given function.
--
-- You must also provide a default kind tag to use in the case that there is no
-- kind parameter.
resolveKindParamTo
    :: Env FVal -> String
    -> kindTag -> (String -> Maybe kindTag) -> Maybe (KindParam a)
    -> Either Error kindTag
resolveKindParamTo env prettyTypeName defKind parseKind = \case
  Nothing                                   -> return defKind
  Just (KindParamInt _ _ kpInt) -> parseKind' kpInt
  Just (KindParamVar _ _ kpVar) -> do
    -- the 'show' here is sensible: kinds are primarily tags => strings, we have
    -- to put them into integers just to do arithmetic. the alternatives are to
    -- 'read' on the other case instead, or provide both parsers
    show <$> evalLookupKind env kpVar >>= parseKind'
  where
    parseKind' kpInt =
        case parseKind kpInt of
          Nothing -> Left $ ErrorNoSuchKindForType kpInt prettyTypeName
          Just k -> return k

-- Note that we do not permit BOZs here, because the syntax you'd have to
-- use is forbidden. You can't assign a BOZ to a variable (it's an untyped
-- compile-time construct), and the kind parameter syntax is limited, so
-- you can't write e.g. @123_x'10'@. (This goes for gfortran, at least.)
--
-- There shouldn't be a problem if you did want to permit them, though.
evalLookupKind :: Env FVal -> Name -> Either Error Integer
evalLookupKind env kpV =
    evalLookup env kpV >>= \case
      FValScalar (FValScalarInt (FValInt _ kpI)) -> return kpI
      val -> Left $ ErrorNonIntegerKind (show val)

{-
  ExpUnary _a _ss uop e -> do
    v <- eval env e
    evalUnaryOp uop v

  ExpBinary _a _ss bop valExpr1 valExpr2 -> do
    res1 <- eval env valExpr1
    res2 <- eval env valExpr2
    evalBinaryOp env bop res1 res2

  ExpFunctionCall _a _ss funcNameExpr args -> do
    let ExpValue _ _ (ValVariable funcName) = funcNameExpr
    evalOp env funcName (map argExtractExpr . aStrip $ fromMaybe (AList undefined undefined []) args)

  e -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr e

evalOp :: Env a -> Name -> [Expression a] -> Either Error FVal
evalOp _env _op _args = undefined
-}

{-
evalUnaryOp :: UnaryOp -> FVal -> Either Error FVal
evalUnaryOp _uop _v = undefined

evalBinaryOp :: Env FVal -> BinaryOp -> FVal -> FVal -> Either Error FVal
evalBinaryOp env bop v1 v2 =
    case bop of
      Addition    -> evalAs "+"
      Subtraction -> evalAs "-"
      _ -> Left $ ErrorNoSuchOp $ show bop
  where
    evalAs opName =
        case Map.lookup opName (envOps env) of
          Nothing -> Left $ ErrorNoSuchOp opName
          Just op ->
            case Op.op op [v1, v2] of
              Left  err -> Left $ ErrorOp err
              Right res -> return res

evalBinaryOp bop (FValScalarInt v1@(FValInt ty1 val1)) (FValScalarInt v2@(FValInt ty2 val2)) =
  case joinType ty1 ty2 of
    Nothing -> Left  $ ErrorUnsupportedCommonType (show ty1) (show ty2)
    Just jt ->
      (FValScalarInt . FValInt jt) <$>
        case bop of
          Addition       -> Right $ val1 + val2
          Multiplication -> Right $ val1 * val2
          Subtraction    -> Right $ val1 - val2
          -- Division is not a homomorphism wrt. representation, so must convert first
          Division       -> Right $ fvalInt (toRuntimeRepr v1) `div`  fvalInt (toRuntimeRepr v2)
          _              -> Left $ ErrorUnsupportedExpression ""

evalBinaryOp _ _ _ = Left $ ErrorUnsupportedValue ""
-}

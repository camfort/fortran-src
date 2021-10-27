{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Types
  ( analyseTypes
  , analyseTypesWithEnv
  , analyseAndCheckTypesWithEnv
  , extractTypeEnv
  , TypeEnv
  , TypeError
  , deriveScalarTyFromDeclaration
  , deriveScalarTyFromTypeSpec
  , deriveIntrinsicTyFromBaseType
  , runInfer
  , inferState0
  ) where

import Language.Fortran.AST

import Prelude hiding ( EQ, LT, GT )

import           Language.Fortran.Analysis
import           Language.Fortran.Repr
import           Language.Fortran.Analysis.Parameters
import           Language.Fortran.Intrinsics
import           Language.Fortran.Util.Position
import           Language.Fortran.Version
import           Language.Fortran.Parser.Utils

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map as Map
import           Data.Map ( Map )
import           Data.Maybe (maybeToList)
import           Data.List (find, foldl')

--------------------------------------------------

-- | Mapping of names to type information.
type TypeEnv = Map Name IDType

-- | Information about a detected type error.
type TypeError = (String, SrcSpan)

-- | Mapping of structures to field types
type StructTypeEnv = Map Name StructMemberTypeEnv
type StructMemberTypeEnv = Map Name IDType

--------------------------------------------------

-- Monad for type inference work
type Infer a = StateT InferState (Reader InferConfig) a

data InferState = InferState
  { langVersion :: FortranVersion
  , intrinsics  :: IntrinsicsTable
  , environ     :: TypeEnv
  , structs     :: StructTypeEnv
  , entryPoints :: Map Name (Name, Maybe Name)
  , typeErrors  :: [TypeError]
  , constMap    :: ConstMap
  } deriving (Show)

data InferConfig = InferConfig
  { inferConfigAcceptNonCharLengthAsKind :: Bool
  -- ^ How to handle declarations like @INTEGER x*8@. If true, providing a
  --   character length for a non-character data type will treat it as a kind
  --   parameter. In both cases, a warning is logged (nonstandard syntax).
  } deriving (Eq, Show)

type InferFunc t = t -> Infer ()

--------------------------------------------------

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information.
analyseTypes :: Data a => ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypes = analyseTypesWithEnv Map.empty

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information; provided with a
-- starting type environment.
analyseTypesWithEnv :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypesWithEnv env pf = (pf', tenv)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState

-- | Annotate AST nodes with type information, return a type
-- environment mapping names to type information and return any type
-- errors found; provided with a starting type environment.
analyseAndCheckTypesWithEnv
  :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv, [TypeError])
analyseAndCheckTypesWithEnv env pf = (pf', tenv, terrs)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState
    terrs           = typeErrors endState

analyseTypesWithEnv' :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), InferState)
analyseTypesWithEnv' env pf@(ProgramFile mi _) = runInfer (miVersion mi) env $ do
  -- Do const work
  modify $ \s -> s { constMap = gatherConsts pf }

  -- Gather information.
  mapM_ intrinsicsExp (allExpressions pf)
  mapM_ programUnit (allProgramUnits pf)
  mapM_ recordArrayDeclarator (allDeclarators pf)
  mapM_ statement (allStatements pf)

  -- Gather types for known entry points.
  eps <- gets (Map.toList . entryPoints)
  forM_ eps $ \ (eName, (fName, mRetName)) -> do
    mFType <- getRecordedType fName
    case mFType of
      Just (IDType fVType fCType) -> do
        recordMType fVType fCType eName
        -- FIXME: what about functions that return arrays?
        maybe (return ()) (error "Entry points with result variables unsupported" >> recordMType fVType Nothing) mRetName
      _                           -> return ()

  annotateTypes pf              -- Annotate AST nodes with their types.

extractTypeEnv :: forall a. Data a => ProgramFile (Analysis a) -> TypeEnv
extractTypeEnv pf = Map.union puEnv expEnv
  where
    puEnv = Map.fromList [ (n, ty) | pu <- universeBi pf :: [ProgramUnit (Analysis a)]
                                 , Named n <- [puName pu]
                                 , ty <- maybeToList (idType (getAnnotation pu)) ]
    expEnv = Map.fromList [ (n, ty) | e@(ExpValue _ _ ValVariable{}) <- universeBi pf :: [Expression (Analysis a)]
                                  , let n = varName e
                                  , ty <- maybeToList (idType (getAnnotation e)) ]

type TransType f g a = (f (Analysis a) -> Infer (f (Analysis a))) -> g (Analysis a) -> Infer (g (Analysis a))
annotateTypes :: Data a => ProgramFile (Analysis a) -> Infer (ProgramFile (Analysis a))
annotateTypes pf = (transformBiM :: Data a => TransType Expression ProgramFile a) annotateExpression pf >>=
                   (transformBiM :: Data a => TransType ProgramUnit ProgramFile a) annotateProgramUnit

intrinsicsExp :: Data a => InferFunc (Expression (Analysis a))
intrinsicsExp (ExpSubscript _ _ nexp _)    = intrinsicsHelper nexp
intrinsicsExp (ExpFunctionCall _ _ nexp _) = intrinsicsHelper nexp
intrinsicsExp _                            = return ()

intrinsicsHelper :: MonadState InferState m => Expression (Analysis a) -> m ()
intrinsicsHelper nexp | isNamedExpression nexp = do
  itab <- gets intrinsics
  case getIntrinsicReturnType (srcName nexp) itab of
    Just _ -> do
      let n = varName nexp
      recordCType CTIntrinsic n
      -- recordBaseType _  n -- FIXME: going to skip base types for the moment
    _             -> return ()
intrinsicsHelper _ = return ()

programUnit :: Data a => InferFunc (ProgramUnit (Analysis a))
programUnit pu@(PUFunction _ _ mRetType _ _ _ mRetVar blocks _)
  | Named n <- puName pu   = do
    -- record some type information that we can glean
    recordCType CTFunction n
    case (mRetType, mRetVar) of
      (Just ts@(TypeSpec _ _ _ _), Just v) -> do
        semType' <- deriveScalarTyFromTypeSpec ts
        let semType = TyScalarTy semType'
        recordSemType semType n >> recordSemType semType (varName v)
      (Just ts@(TypeSpec _ _ _ _), _)      -> do
        semType' <- deriveScalarTyFromTypeSpec ts
        let semType = TyScalarTy semType'
        recordSemType semType n
      _                                        -> return ()
    -- record entry points for later annotation
    forM_ blocks $ \ block ->
      sequence_ [ recordEntryPoint n (varName v) (fmap varName mRetVar') | (StEntry _ _ v _ mRetVar') <- allStatements block ]
programUnit pu@(PUSubroutine _ _ _ _ _ blocks _) | Named n <- puName pu = do
  -- record the fact that this is a subroutine
  recordCType CTSubroutine n
  -- record entry points for later annotation
  forM_ blocks $ \ block ->
    sequence_ [ recordEntryPoint n (varName v) Nothing | (StEntry _ _ v _ _) <- allStatements block ]
programUnit _                                           = return ()

-- | Records array type information from a 'Declarator'. (Scalar type info is
--   processed elsewhere.)
--
--   Note that 'ConstructType' is rewritten for 'Declarator's in
--   'handleDeclaration' later. TODO how does this assist exactly? disabling
--   apparently doesn't impact tests
recordArrayDeclarator :: Data a => InferFunc (Declarator (Analysis a))
recordArrayDeclarator (Declarator _ _ v (Just ddAList) _ _) =
    recordCType (CTArray $ dimDeclarator ddAList) (varName v)
recordArrayDeclarator _ = return ()

dimDeclarator :: AList DimensionDeclarator a -> [(Maybe Int, Maybe Int)]
dimDeclarator ddAList = [ (lb, ub) | DimensionDeclarator _ _ lbExp ubExp <- aStrip ddAList
                                   , let lb = do ExpValue _ _ (ValInteger i) <- lbExp
                                                 return $ read i
                                   , let ub = do ExpValue _ _ (ValInteger i) <- ubExp
                                                 return $ read i ]

-- | Auxiliary function for getting semantic and construct type of a declaration.
-- Used in standard declarations and structures
handleDeclaration :: Data a => TypeEnv -> SrcSpan -> TypeSpec (Analysis a)
  -> Maybe (AList Attribute (Analysis a))
  -> AList Declarator (Analysis a)
  -> Infer [(Name, Ty, ConstructType)]
handleDeclaration env stmtSs ts mAttrAList declAList
  | mAttrs  <- maybe [] aStrip mAttrAList
  , attrDim <- find isAttrDimension mAttrs
  , isParam <- any isAttrParameter mAttrs
  , isExtrn <- any isAttrExternal mAttrs
  , decls   <- aStrip declAList =
    let cType n | isExtrn                                     = CTExternal
                | Just (AttrDimension _ _ ddAList) <- attrDim = CTArray (dimDeclarator ddAList)
                | isParam                                     = CTParameter
                | Just (IDType _ (Just ct)) <- Map.lookup n env
                , ct /= CTIntrinsic                           = ct
                | otherwise                                   = CTVariable
        handler rs = \case
          Declarator _ declSs v mDdAList mLenExpr _ -> do
            st <- deriveScalarTyFromDeclaration stmtSs declSs ts mLenExpr
            let n = varName v
                ct = maybe (cType n) (CTArray . dimDeclarator) mDdAList
            pure $ (n, TyScalarTy st, ct) : rs
    in foldM handler [] decls

{-
handleParameterStmt :: Data a => AList Declarator (Analysis a) -> Infer ()
handleParameterStmt declAList =
    let decls = aStrip declAList
     in mapM_ f decls
  where
    f :: Declarator a -> Infer ()
    f = undefined
-}
{-
    f = \case
      DeclVariable _ _ varE _ (Just valE) ->
        let sym = srcName varExp
            val = case tryAstValueToScalarVal valE of
              Just val -> val
              Nothing  -> error "fucko"
            kind' = getKindOfExpVal val'   -- infer kind from value
            pd'   = SParameter (setTypeKind (typeOfExpVal val') (Just kind')) val'
            entry = case Map.lookup symbol symt of
                    -- Entry found implies there is a preceding declaration
                    -- of the name. 
                    -- If that is variable declaration, keep the accurate type
                    -- and kind informatio from the declaration.
                    -- Else if it is dummy variable, keep the accurate type 
                    -- and update kind
                    -- Else raise error for conflicting parameter attribute
                    -- Parameter name does not necessarily have a type
                    -- declaration or a kind is assumed. In that case type
                    -- and kind are inferred from the value of parameter.
            Nothing               -> pd'
            Just (SVariable ty _) -> case ty of
              -- TODO previously TCharacter Nothing
              TCharacter CharLenStar _ -> pd'
              _                        -> SParameter ty val'
            Just SDummy{} | isStr val' -> pd'
            Just _ ->
              let errStr t =
                      "Invalid PARAMETER statement for symbol \'" ++ t ++ "\'"
               in error $ errStr symbol
        in  Map.insert symbol entry symt
      _ -> error "parser probably can't make this"
-}

handleStructureItem :: Data a => StructMemberTypeEnv -> StructureItem (Analysis a) -> Infer StructMemberTypeEnv
handleStructureItem mt (StructFields _ src ts mAttrAList declAList) = do
  env <- gets environ
  ds <- handleDeclaration env src ts mAttrAList declAList
  pure $ foldl' (\m (n, s, c) -> Map.insert n (IDType (Just s) (Just c)) m) mt ds
-- TODO: These should eventually be implemented
handleStructureItem mt StructUnion{} = pure mt
handleStructureItem mt StructStructure{} = pure mt

-- | Create a structure env from the list of fields and add it to the InferState
handleStructure :: Data a => Maybe String -> AList StructureItem (Analysis a) -> Infer ()
handleStructure mName itemAList = do
  case mName of
    Just n -> do
      structEnv <- foldM handleStructureItem Map.empty (aStrip itemAList)
      recordStruct structEnv n
    Nothing -> pure ()

statement :: Data a => InferFunc (Statement (Analysis a))

{-
statement (StParameter _ _ declAList) = do
  env <- gets environ
  decls <- handleParameterStmt declAList
  -- TODO: place into parammap
-}

statement (StDeclaration _ stmtSs ts mAttrAList declAList) = do
  env <- gets environ
  decls <- handleDeclaration env stmtSs ts mAttrAList declAList
  forM_ decls $ \(n, b, c) -> recordType b c n
statement (StExternal _ _ varAList) = do
  let vars = aStrip varAList
  mapM_ (recordCType CTExternal . varName) vars
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  --  | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    mIDType <- getExprRecordedType v
    case mIDType of
      Just (IDType _ (Just CTArray{})) -> return ()                -- do nothing, it's already known to be an array
      _                                -> recordCType CTFunction (varName v) -- assume it's a function statement

-- FIXME: if StFunctions can only be identified after types analysis
-- is complete and disambiguation is performed, then how do we get
-- them in the first place? (iterate until fixed point?)
statement (StFunction _ _ v _ _) = recordCType CTFunction (varName v)
-- (part of answer to above is) nullary function statement: foo() = ...
statement (StExpressionAssign _ _ (ExpFunctionCall _ _ v Nothing) _) = recordCType CTFunction (varName v)

statement (StDimension _ _ declAList) = do
  let decls = aStrip declAList
  forM_ decls $ \ decl -> case decl of
    Declarator _ _ v (Just ddAList) _ _ ->
      recordCType (CTArray $ dimDeclarator ddAList) (varName v)
    _ -> return ()

statement (StStructure _ _ mName itemAList) = handleStructure mName itemAList

statement _ = return ()

annotateExpression :: Data a => Expression (Analysis a) -> Infer (Expression (Analysis a))

-- handle the various literals
annotateExpression e@(ExpValue _ _ ValVariable{})    = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ _ ValIntrinsic{})   = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ ss (ValReal r))        = do
    k <- deriveRealLiteralKind ss r
    return $ setSemType (TyScalarTy $ ScalarTyIntrinsic $ IntrinsicTy BTyReal k) e
annotateExpression e@(ExpValue _ ss (ValComplex e1 e2)) = do
    st <- complexLiteralType ss e1 e2
    return $ setSemType st e
annotateExpression e@(ExpValue _ _ ValInteger{})     =
    -- FIXME: in >F90, int lits can have kind info on end @_8@, same as real
    -- lits. We do parse this into the lit string, it is available to us.
    return $ setSemType (TyScalarTy $ ScalarTyIntrinsic $ deriveIntrinsicTyFromBaseType TypeInteger) e

annotateExpression e@(ExpValue _ _ (ValLogical _ mKp))     = do
    sTy <- deriveScalarTyFromBaseTypeAndKindParam TypeLogical mKp
    let e' = setSemType (TyScalarTy sTy) e
    return e'

annotateExpression e@(ExpBinary _ _ op e1 e2)          = flip setIDType e `fmap` binaryOpType (getSpan e) op e1 e2
annotateExpression e@(ExpUnary _ _ op e1)              = flip setIDType e `fmap` unaryOpType (getSpan e1) op e1
annotateExpression e@(ExpSubscript _ _ e1 idxAList)    = flip setIDType e `fmap` subscriptType (getSpan e) e1 idxAList
annotateExpression e@(ExpFunctionCall _ _ e1 parAList) = flip setIDType e `fmap` functionCallType (getSpan e) e1 parAList
annotateExpression e                                   = return e

annotateProgramUnit :: Data a => ProgramUnit (Analysis a) -> Infer (ProgramUnit (Analysis a))
annotateProgramUnit pu | Named n <- puName pu = maybe pu (`setIDType` pu) `fmap` getRecordedType n
annotateProgramUnit pu                        = return pu

-- | Derive the kind of a REAL literal constant.
--
-- Logic taken from HP's F90 reference pg.33, written to gfortran's behaviour.
-- Stays in the 'Infer' monad so it can report type errors
deriveRealLiteralKind :: SrcSpan -> String -> Infer Kind
deriveRealLiteralKind ss r =
    case realLitKindParam realLit of
      Nothing -> return kindFromExpOrDefault
      Just k  ->
        case realLitExponent realLit of
          Nothing  -> return k  -- no exponent, use kind param
          Just expo ->
            -- can only use kind param with 'e' or no exponent
            case expLetter expo of
              ExpLetterE -> return k
              _          -> do
                -- badly formed literal, but we'll allow and use the provided
                -- kind param (with no doubling or anything)
                typeError "only real literals with exponent letter 'e' can specify explicit kind parameter" ss
                return k
  where
    realLit = parseRealLiteral r
    kindFromExpOrDefault =
        case realLitExponent realLit of
          -- no exponent: select default real kind
          Nothing             -> 4
          Just expo           ->
            case expLetter expo of
              ExpLetterE -> 4
              ExpLetterD -> 8

-- | Get the type of a COMPLEX literal constant.
--
-- The kind is derived only from the first expression, the second is ignored.
complexLiteralType :: SrcSpan -> Expression a -> Expression a -> Infer Ty
complexLiteralType ss (ExpValue _ _ (ValReal r)) _ = do
    k <- deriveRealLiteralKind ss r
    return $ TyScalarTy $ ScalarTyIntrinsic $ IntrinsicTy BTyComplex k
complexLiteralType _ _ _ = return $ TyScalarTy $ ScalarTyIntrinsic $ deriveIntrinsicTyFromBaseType TypeComplex

binaryOpType :: Data a => SrcSpan -> BinaryOp -> Expression (Analysis a) -> Expression (Analysis a) -> Infer IDType
binaryOpType ss op e1 e2 = do
  mst1 <- case getIDType e1 of
            Just (IDType (Just st) _) -> return $ Just st
            _ -> typeError "Unable to obtain type for first operand" (getSpan e1) >> return Nothing
  mst2 <- case getIDType e2 of
            Just (IDType (Just st) _) -> return $ Just st
            _ -> typeError "Unable to obtain type for second operand" (getSpan e2) >> return Nothing
  case (mst1, mst2) of
    (_, Nothing) -> return emptyType
    (Nothing, _) -> return emptyType
    (Just st1, Just st2) -> do
      mst  <- binopSimpleCombineSemTypes ss op st1 st2
      mst' <- case mst of
        Just st
          | op `elem` [ Addition, Subtraction, Multiplication, Division
                      , Exponentiation, Concatenation, Or, XOr, And ]       -> return $ Just st
          | op `elem` [GT, GTE, LT, LTE, EQ, NE, Equivalent, NotEquivalent] -> return $ Just . TyScalarTy $ ScalarTyIntrinsic $ deriveIntrinsicTyFromBaseType TypeLogical
          | BinCustom{} <- op -> typeError "custom binary ops not supported" ss >> return Nothing
        _ -> return Nothing

      return $ IDType mst' Nothing -- FIXME: might have to check kinds of each operand

-- | Combine two 'SemType's with a 'BinaryOp'.
--
-- No real work done here, no kind combining, just selection.
binopSimpleCombineSemTypes :: SrcSpan -> BinaryOp -> Ty -> Ty -> Infer (Maybe Ty)
binopSimpleCombineSemTypes ss op tyL tyR =
    case (tyL, tyR) of
      (TyScalarTy sTyL, TyScalarTy sTyR) ->
        case (sTyL, sTyR) of
          (ScalarTyIntrinsic iTyL, ScalarTyIntrinsic iTyR) ->
            case (iTyBase iTyL, iTyBase iTyR) of
              (_, BTyComplex) -> ret $ iTy BTyComplex (iTyKind iTyR)
              (BTyComplex, _) -> ret $ iTy BTyComplex (iTyKind iTyL)
              (_, BTyReal) -> ret $ iTy BTyReal (iTyKind iTyR)
              (BTyReal, _) -> ret $ iTy BTyReal (iTyKind iTyL)
              (_, BTyInteger) -> ret $ iTy BTyInteger (iTyKind iTyR)
              (BTyInteger, _) -> ret $ iTy BTyInteger (iTyKind iTyL)
              (BTyLogical, BTyLogical) -> ret $ iTy BTyLogical (iTyKind iTyL)
              (BTyCharacter lenL, BTyCharacter lenR)
                | iTyKind iTyL /= iTyKind iTyR -> do
                    typeError "operation on character strings of different kinds" ss
                    return Nothing
                | op == Concatenation -> ret $ iTy (BTyCharacter (lenL + lenR)) (iTyKind iTyL)
                | op `elem` [EQ, NE]  -> ret $ TyScalarTy $ ScalarTyIntrinsic $ deriveIntrinsicTyFromBaseType TypeLogical
                | otherwise -> do typeError "Invalid op on character strings" ss
                                  return Nothing
              _ -> do typeError "type error in scalar binary expression" ss
                      return Nothing
          (ScalarTyCustom{}, ScalarTyCustom{}) -> do
            typeError "custom types / binary op not supported" ss
            return Nothing
          _ -> do
            typeError "Type error between operands of binary operator" ss
            return Nothing
      _ -> do
        typeError "Type error between operands of binary operator" ss
        return Nothing
  where
    ret = return . Just
    iTy bt k = TyScalarTy $ ScalarTyIntrinsic $ IntrinsicTy bt k

unaryOpType :: Data a => SrcSpan -> UnaryOp -> Expression (Analysis a) -> Infer IDType
unaryOpType ss op e = do
  mst <- case getIDType e of
           Just (IDType (Just st) _) -> return $ Just st
           _ -> typeError "Unable to obtain type for" (getSpan e) >> return Nothing
  mst' <- case (mst, op) of
    (Nothing, _)               -> return Nothing
    -- (Just TCustom{}, _)        -> typeError "custom types / unary ops not supported" ss >> return Nothing
    (_, UnCustom{})            -> typeError "custom unary ops not supported" ss >> return Nothing
    -- (Just st@(TLogical _), Not)    -> return $ Just st
    (Just st, _)
      | op `elem` [Plus, Minus] &&
        isNumericType st -> return $ Just st
    _ -> typeError "Type error for unary operator" ss >> return Nothing
  return $ IDType mst' Nothing -- FIXME: might have to check kind of operand

subscriptType :: Data a => SrcSpan -> Expression (Analysis a) -> AList Index (Analysis a) -> Infer IDType
subscriptType ss e1 (AList _ _ idxs) = do
    {-
  let isInteger ie | Just (IDType (Just (TInteger _)) _) <- getIDType ie = True
                   | otherwise = False
    -}
  let isInteger = const False
  forM_ idxs $ \ idx -> case idx of
    IxSingle _ _ _ ie
      | not (isInteger ie) -> typeError "Invalid or unknown type for index" (getSpan ie)
    IxRange _ _ mie1 mie2 mie3
      | Just ie1 <- mie1, not (isInteger ie1) -> typeError "Invalid or unknown type for index" (getSpan ie1)
      | Just ie2 <- mie2, not (isInteger ie2) -> typeError "Invalid or unknown type for index" (getSpan ie2)
      | Just ie3 <- mie3, not (isInteger ie3) -> typeError "Invalid or unknown type for index" (getSpan ie3)
    _ -> return ()
  case getIDType e1 of
    Just ty@(IDType mst (Just (CTArray dds))) -> do
      when (length idxs /= length dds) $ typeError "Length of indices does not match rank of array." ss
      let isSingle (IxSingle{}) = True; isSingle _ = False
      if all isSingle idxs
        then return $ IDType mst Nothing
        else return ty
    _ -> return emptyType

functionCallType :: Data a => SrcSpan -> Expression (Analysis a) -> Maybe (AList Argument (Analysis a)) -> Infer IDType
functionCallType ss (ExpValue _ _ (ValIntrinsic n)) (Just (AList _ _ params)) = do
  itab <- gets intrinsics
  let mRetType = getIntrinsicReturnType n itab
  case mRetType of
    Nothing -> return emptyType
    Just retType -> do
      mst <- case retType of
            ITReal      -> wrapBaseType TypeReal
            ITInteger   -> wrapBaseType TypeInteger
            ITComplex   -> wrapBaseType TypeComplex
            ITDouble    -> wrapBaseType TypeDoublePrecision
            ITLogical   -> wrapBaseType TypeLogical
            ITCharacter -> wrapBaseType TypeCharacter
            ITParam i
              | length params >= i, Argument _ _ _ e <- params !! (i-1)
                -> return $ idVType =<< getIDType e
              | otherwise -> typeError ("Invalid parameter list to intrinsic '" ++ n ++ "'") ss >> return Nothing
      case mst of
        Nothing -> return emptyType
        Just _ -> return $ IDType mst Nothing
  where
    wrapBaseType bt = do
        sTy <- deriveScalarTyFromBaseTypeAndKindParam bt Nothing
        return $ Just $ TyScalarTy sTy

functionCallType ss e1 _ = case getIDType e1 of
  Just (IDType (Just st) (Just CTFunction)) -> return $ IDType (Just st) Nothing
  Just (IDType (Just st) (Just CTExternal)) -> return $ IDType (Just st) Nothing
  _ -> typeError "non-function invoked by call" ss >> return emptyType

isNumericType :: Ty -> Bool
isNumericType = \case
  TyScalarTy sTy -> case sTy of
    ScalarTyIntrinsic iTy -> case iTyBase iTy of
      BTyInteger -> True
      BTyReal    -> True
      BTyComplex -> True
      -- TODO byte? logical?
      _ -> False
    _ -> False
  _  -> False

--------------------------------------------------
-- Monadic helper combinators.

inferState0 :: FortranVersion -> InferState
inferState0 v = InferState
  { environ     = Map.empty
  , structs     = Map.empty
  , entryPoints = Map.empty
  , langVersion = v
  , intrinsics  = getVersionIntrinsics v
  , typeErrors  = []
  , constMap    = Map.empty
  }

inferConfig0 :: InferConfig
inferConfig0 = InferConfig
  { inferConfigAcceptNonCharLengthAsKind = True
  }

runInfer :: FortranVersion -> TypeEnv -> Infer a -> (a, InferState)
runInfer v env f = flip runReader inferConfig0 $ flip runStateT ((inferState0 v) { environ = env }) f

typeError :: MonadState InferState m => String -> SrcSpan -> m ()
typeError msg ss = modify $ \ s -> s { typeErrors = (msg, ss):typeErrors s }

emptyType :: IDType
emptyType = IDType Nothing Nothing

-- Record the type of the given name.
recordType :: Ty -> ConstructType -> Name -> Infer ()
recordType st ct n = modify $ \ s -> s { environ = Map.insert n (IDType (Just st) (Just ct)) (environ s) }

recordStruct :: StructMemberTypeEnv -> Name -> Infer ()
recordStruct mt n = modify $ \s -> s { structs = Map.insert n mt (structs s) }

-- Record the type (maybe) of the given name.
recordMType :: Maybe Ty -> Maybe ConstructType -> Name -> Infer ()
recordMType st ct n = modify $ \ s -> s { environ = Map.insert n (IDType st ct) (environ s) }

-- Record the CType of the given name.
recordCType :: MonadState InferState m => ConstructType -> Name -> m ()
recordCType ct n = modify $ \ s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (mIDType >>= idVType) (Just ct))

-- Record the SemType of the given name.
recordSemType :: Ty -> Name -> Infer ()
recordSemType st n = modify $ \ s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just st) (mIDType >>= idCType))

recordEntryPoint :: Name -> Name -> Maybe Name -> Infer ()
recordEntryPoint fn en mRetName = modify $ \ s -> s { entryPoints = Map.insert en (fn, mRetName) (entryPoints s) }

getRecordedType :: Name -> Infer (Maybe IDType)
getRecordedType n = gets (Map.lookup n . environ)

getExprRecordedType :: Data a => Expression (Analysis a) -> Infer (Maybe IDType)
getExprRecordedType e@(ExpValue _ _ (ValVariable _)) = getRecordedType $ varName e
getExprRecordedType (ExpSubscript _ _ base _) = do
  mTy <- getExprRecordedType base
  case mTy of
    Just (IDType semTy (Just CTArray{})) -> pure . Just $ IDType semTy (Just CTVariable)
    _ -> pure Nothing
getExprRecordedType (ExpDataRef _ _ base ref) = do
  mTy <- getExprRecordedType base
  case mTy of
    Just (IDType (Just (TyScalarTy (ScalarTyCustom n))) _) -> do
      mStructEnv <- gets (Map.lookup n . structs)
      case mStructEnv of
        Nothing -> pure Nothing
        Just env -> pure $ Map.lookup (varName ref) env
    x -> pure x
getExprRecordedType _ = pure Nothing

-- Set the idType annotation
setIDType :: Annotated f => IDType -> f (Analysis a) -> f (Analysis a)
setIDType ty x =
    let a = getAnnotation x
     in setAnnotation (a { idType = Just ty }) x

-- Get the idType annotation
getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
getIDType x = idType (getAnnotation x)

-- | For all types holding an 'IDType' (in an 'Analysis'), set the 'SemType'
--   field of the 'IDType'.
setSemType :: (Annotated f, Data a) => Ty -> f (Analysis a) -> f (Analysis a)
setSemType st x =
    let anno  = getAnnotation x
        idt   = idType anno
        anno' = anno { idType = Just (setIDTypeSemType idt) }
     in setAnnotation anno' x
  where
    setIDTypeSemType :: Maybe IDType -> IDType
    setIDTypeSemType (Just (IDType _ mCt)) = IDType (Just st) mCt
    setIDTypeSemType Nothing               = IDType (Just st) Nothing

-- Set the CType part of idType annotation
--setCType :: (Annotated f, Data a) => ConstructType -> f (Analysis a) -> f (Analysis a)
--setCType ct x
--  | a@(Analysis { idType = Nothing }) <- getAnnotation x = setAnnotation (a { idType = Just (IDType Nothing (Just ct)) }) x
--  | a@(Analysis { idType = Just it }) <- getAnnotation x = setAnnotation (a { idType = Just (it { idCType = Just ct }) }) x

type UniFunc f g a = f (Analysis a) -> [g (Analysis a)]

allProgramUnits :: Data a => UniFunc ProgramFile ProgramUnit a
allProgramUnits = universeBi

allDeclarators :: Data a => UniFunc ProgramFile Declarator a
allDeclarators = universeBi

allStatements :: (Data a, Data (f (Analysis a))) => UniFunc f Statement a
allStatements = universeBi

allExpressions :: (Data a, Data (f (Analysis a))) => UniFunc f Expression a
allExpressions = universeBi

isAttrDimension :: Attribute a -> Bool
isAttrDimension AttrDimension {} = True
isAttrDimension _                = False

isAttrParameter :: Attribute a -> Bool
isAttrParameter AttrParameter {} = True
isAttrParameter _                = False

isAttrExternal :: Attribute a -> Bool
isAttrExternal AttrExternal {} = True
isAttrExternal _               = False

isIxSingle :: Index a -> Bool
isIxSingle IxSingle {} = True
isIxSingle _           = False

--------------------------------------------------

-- Most, but not all deriving functions can report type errors. So most of these
-- functions are in the Infer monad.

-- | Attempt to derive a variable's 'ScalarTy' from the relevant parts of its
--   surrounding 'StDeclaration'.
--
-- A declaration looks as follows:
--
--     INTEGER(8) :: var_name
--
-- In the AST, this is split into a LHS 'TypeSpec', and a RHS list of
-- 'Declarator's. Note that @CHARACTER@ variables are allowed to specify their
-- length via special syntax on the RHS:
--
--     CHARACTER :: string*10
--
-- so to handle that, this function takes that length as a Maybe Expression (as
-- provided in 'StDeclaration'). (Note that @CHARACTER :: string(10)@ is array
-- dimension declarator syntax, not the same. This function does not handle
-- array type information.)
--
-- If a length was defined on both sides, the declaration length (RHS) is used.
-- This matches gfortran's behaviour, though even with -Wall they don't warn on
-- this rather confusing syntax usage. We report a (soft) type error.
--
-- The internal functions this uses aren't user-facing. Some are non-total, and
-- will runtime error on unexpected parameters (which in normal operation are
-- handled before their call).
deriveScalarTyFromDeclaration
    :: forall a m
    . (MonadState InferState m, MonadReader InferConfig m)
    => SrcSpan -> SrcSpan -> TypeSpec a -> Maybe (Expression a) -> m ScalarTy
deriveScalarTyFromDeclaration stmtSs declSs ts@(TypeSpec tsA tsSS bt mSel) mLenExpr =
    case mLenExpr of
      -- no RHS length, can continue with regular deriving
      Nothing -> deriveScalarTyFromTypeSpec ts

      -- we got a RHS length; only CHARACTERs permit this
      Just lenExpr ->
        case bt of
          TypeCharacter -> deriveCharWithLen lenExpr

          _ -> do
            -- oh dear! probably the nonstandard kind param syntax @INTEGER x*2@
            asks inferConfigAcceptNonCharLengthAsKind >>= \case
              False -> do
                flip typeError stmtSs $
                    "non-CHARACTER variable given a length @ "
                 <> show (getSpan lenExpr)
                 <> ": ignoring"
                deriveScalarTyFromTypeSpec ts
              True -> do
                flip typeError stmtSs $
                    "non-CHARACTER variable given a length @ "
                 <> show (getSpan lenExpr)
                 <> ": treating as nonstandard kind parameter syntax"

                -- silly check to give an in-depth type error
                case mSel of
                  Just (Selector sA sSS sLen sMKpExpr) -> do
                    _ <- case sMKpExpr of
                           Nothing     -> return ()
                           Just kpExpr -> do
                             -- also got a LHS kind param, inform that we are
                             -- overriding
                             flip typeError stmtSs $
                                 "non-CHARACTER variable"
                              <> " given both"
                              <> " LHS kind @ " <> show (getSpan kpExpr) <> " and"
                              <> " nonstandard RHS kind @ " <> show (getSpan lenExpr)
                              <> ": specific RHS declarator overrides"
                             return ()
                    let sel = Selector sA sSS sLen (Just lenExpr)
                        ts' = TypeSpec tsA tsSS bt (Just sel)
                     in deriveScalarTyFromTypeSpec ts'
                  Nothing ->
                    let sel = Selector undefined undefined Nothing (Just lenExpr)
                        ts' = TypeSpec tsA tsSS bt (Just sel)
                     in deriveScalarTyFromTypeSpec ts'

  where
    -- Function called when we have a TypeCharacter and a RHS declarator length.
    deriveCharWithLen lenExpr =
        case mSel of
          Just (Selector selA selSs mSelLenExpr mKindExpr) -> do
            _ <- case mSelLenExpr of
                   Just _ -> do
                      -- both LHS & RHS lengths: surprising syntax, notify user
                      -- Ben has seen this IRL: a high-ranking Fortran
                      -- tutorial site uses it (2021-04-30):
                      -- http://web.archive.org/web/20210118202503/https://www.tutorialspoint.com/fortran/fortran_strings.htm
                     flip typeError stmtSs $
                         "warning: CHARACTER variable @ " <> show declSs
                      <> " has length in LHS type spec and RHS declarator"
                      <> " -- specific RHS declarator overrides"
                   _ -> return ()
            -- overwrite the Selector with RHS length expr & continue
            let sel' = Selector selA selSs (Just lenExpr) mKindExpr
            deriveScalarTyFromBaseTypeAndSelector TypeCharacter sel'
          Nothing -> do
            -- got RHS len, no Selector (e.g. @CHARACTER :: x*3 = "sup"@)
            -- create fake Selector with RHS length expr & continue
            let sel' = Selector undefined undefined (Just lenExpr) Nothing
            deriveScalarTyFromBaseTypeAndSelector TypeCharacter sel'
                {-
            -- naughty let binding so we don't have to redefine logic
            let ScalarTyIntrinsic iTy = deriveScalarTyFromBaseType TypeCharacter
                charLen = evalCharLengthInt undefined lenExpr
             in return $ ScalarTyIntrinsic $ iTy { iTyBase = BTyCharacter charLen }
             -}

-- | Attempt to derive a 'ScalarTy' from a 'TypeSpec'.
--
-- 'TypeSpec' stores only scalar type information: 'BaseType', with optional
-- kind information (and a special case for @CHARACTER@s). Array type
-- information is stored in 'DimensionDeclarator's, and handled elsewhere.
deriveScalarTyFromTypeSpec
    :: MonadState InferState m => TypeSpec a -> m ScalarTy
deriveScalarTyFromTypeSpec (TypeSpec _ _ bt mSel) =
    case mSel of
      -- Selector present: we might have kind/other info provided
      Just sel -> deriveScalarTyFromBaseTypeAndSelector bt sel
      -- no Selector: derive using default kinds etc.
      Nothing  -> deriveScalarTyFromBaseTypeAndKindParam bt Nothing

-- | Attempt to derive a 'ScalarTy' from a 'BaseType' and a 'Selector'.
--
-- TODO cleanup (I think one uses guards here)
deriveScalarTyFromBaseTypeAndSelector
    :: MonadState InferState m => BaseType -> Selector a -> m ScalarTy
deriveScalarTyFromBaseTypeAndSelector bt (Selector _ ss mLenExpr mKp) = do
    sTy <- deriveScalarTyFromBaseTypeAndKindParam bt mKp
    case mLenExpr of
      Nothing      -> return sTy
      Just lenExpr -> do
        case sTy of
          ScalarTyIntrinsic iTy ->
            case iTyBase iTy of
              BTyCharacter{} -> do
                len <- case lenExpr of
                         ExpValue _ _ ValStar -> error "TODO can't yet encode star in BTyCharacter"
                         _ -> runWithConstMap $ evalCharLengthInt lenExpr
                return $ ScalarTyIntrinsic $ iTy { iTyBase = BTyCharacter len }
              _ -> do
                -- (unreachable code path in correct parser operation)
                typeError "only CHARACTER types can specify length (separate to kind)" ss
                return sTy
          _ -> do
            -- (unreachable code path in correct parser operation)
            typeError "only CHARACTER types can specify length (separate to kind)" ss
            return sTy

deriveScalarTyFromBaseTypeAndKindParam
    :: MonadState InferState m => BaseType -> Maybe (Expression a) -> m ScalarTy
deriveScalarTyFromBaseTypeAndKindParam bt mKp =
    case bt of
      TypeCustom ty' ->
        case mKp of
          Nothing -> return $ ScalarTyCustom ty'
          Just{}  -> error "impossible parse: kind in TYPE(x)"
      _ ->
          let iTy = deriveIntrinsicTyFromBaseType bt
           in case mKp of
                Nothing -> return $ ScalarTyIntrinsic iTy
                Just kp -> do
                  k <- runWithConstMap $ evalKindParam kp
                  return $ ScalarTyIntrinsic $ iTy { iTyKind = k }

runWithConstMap :: MonadState InferState m => (ConstMap -> a) -> m a
runWithConstMap = flip fmap (gets constMap)

-- | Derive a default 'IntrinsicTy' from a plain 'BaseType'.
--
-- Default kinds attempt to match gfortran's behaviour.
--
-- Some 'BaseType' constructors are only used for special cases and can't be
-- matched to a 'ScalarTy'. These will cause a runtime error, and must be
-- handled higher up the stack!
--
-- If used with a 'TypeCharacter', it is the caller's responbility to handle
-- length. This function is intended to be used after the parent 'Selector' is
-- confirmed to have no length provided, so the default length can be used.
--
-- Similarly, non-intrinsics like F90 DDTs, F77 commons stored in 'TypeCustom'
-- should must be handled prior.
deriveIntrinsicTyFromBaseType :: BaseType -> IntrinsicTy
deriveIntrinsicTyFromBaseType = uncurry IntrinsicTy . \case
  TypeInteger         -> (BTyInteger, 4)
  TypeReal            -> (BTyReal,    4)
  TypeComplex         -> (BTyComplex, 4)
  TypeLogical         -> (BTyLogical, 4)

  -- Fortran specs & compilers seem to agree on equating these intrinsic types
  -- to others with a larger kind, so we drop the extra syntactic info here.
  TypeDoublePrecision -> (BTyReal,    8)
  TypeDoubleComplex   -> (BTyComplex, 8)

  -- HP's Fortran 90 reference says that BYTE is an HP extension, equates BYTEs
  -- are apparently a less agreed-on extension. gfortran sets it to @INTEGER(1)@
  -- (yes, signed) and some other resources agree.
  TypeByte            -> (BTyInteger, 1)

  -- CHARACTERs default to len=1, kind=1 (non-1 is rare)
  TypeCharacter       -> (BTyCharacter 1, 1)

  bt -> error $ "not an intrinsic type: " <> show bt

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

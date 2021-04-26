{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module Language.Fortran.Analysis.Types
  ( analyseTypes, analyseTypesWithEnv, analyseAndCheckTypesWithEnv, extractTypeEnv, TypeEnv, TypeError )
where

import Language.Fortran.AST

import Prelude hiding (lookup, EQ, LT, GT)
import Data.Map (insert)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.List (find)
import Control.Monad.State.Strict
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Functor.Identity (Identity ())
import Control.Applicative (liftA2)
import Language.Fortran.Analysis
import Language.Fortran.Intrinsics
import Language.Fortran.Util.Position
import Language.Fortran.Version (FortranVersion(..))
import Language.Fortran.Parser.Utils

--------------------------------------------------

-- | Mapping of names to type information.
type TypeEnv = M.Map Name IDType

-- | Information about a detected type error.
type TypeError = (String, SrcSpan)

--------------------------------------------------

-- Monad for type inference work
type Infer a = State InferState a
data InferState = InferState { langVersion :: FortranVersion
                             , intrinsics  :: IntrinsicsTable
                             , environ     :: TypeEnv
                             , entryPoints :: M.Map Name (Name, Maybe Name)
                             , typeErrors  :: [TypeError] }
  deriving Show
type InferFunc t = t -> Infer ()

--------------------------------------------------

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information.
analyseTypes :: Data a => ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypes = analyseTypesWithEnv M.empty

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
  -- Gather information.
  mapM_ intrinsicsExp (allExpressions pf)
  mapM_ programUnit (allProgramUnits pf)
  mapM_ declarator (allDeclarators pf)
  mapM_ statement (allStatements pf)

  -- Gather types for known entry points.
  eps <- gets (M.toList . entryPoints)
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
extractTypeEnv pf = M.union puEnv expEnv
  where
    puEnv = M.fromList [ (n, ty) | pu <- universeBi pf :: [ProgramUnit (Analysis a)]
                                 , Named n <- [puName pu]
                                 , ty <- maybeToList (idType (getAnnotation pu)) ]
    expEnv = M.fromList [ (n, ty) | e@(ExpValue _ _ ValVariable{}) <- universeBi pf :: [Expression (Analysis a)]
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

intrinsicsHelper :: Expression (Analysis a) -> StateT InferState Identity ()
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
      -- TODO: what exactly is going on here (accessing uniqueName/sourceName)
      (Just ts@(TypeSpec _ _ baseType _), Just v) -> do
        semType <- deriveSemTypeFromTypeSpec ts
        recordSemType semType n >> recordSemType semType (varName v)
      (Just ts@(TypeSpec _ _ baseType _), _)      -> do
        semType <- deriveSemTypeFromTypeSpec ts
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

declarator :: Data a => InferFunc (Declarator (Analysis a))
declarator (DeclArray _ _ v ddAList _ _) = recordCType (CTArray $ dimDeclarator ddAList) (varName v)
declarator _ = return ()

dimDeclarator :: AList DimensionDeclarator a -> [(Maybe Int, Maybe Int)]
dimDeclarator ddAList = [ (lb, ub) | DimensionDeclarator _ _ lbExp ubExp <- aStrip ddAList
                                   , let lb = do ExpValue _ _ (ValInteger i) <- lbExp
                                                 return $ read i
                                   , let ub = do ExpValue _ _ (ValInteger i) <- ubExp
                                                 return $ read i ]

statement :: Data a => InferFunc (Statement (Analysis a))
statement (StDeclaration _ _ ts@(TypeSpec _ _ baseType _) mAttrAList declAList)
  | mAttrs  <- maybe [] aStrip mAttrAList
  , attrDim <- find isAttrDimension mAttrs
  , isParam <- any isAttrParameter mAttrs
  , isExtrn <- any isAttrExternal mAttrs
  , decls   <- aStrip declAList = do
    env <- gets environ
    let cType n | isExtrn                                     = CTExternal
                | Just (AttrDimension _ _ ddAList) <- attrDim = CTArray (dimDeclarator ddAList)
                | isParam                                     = CTParameter
                | Just (IDType _ (Just ct)) <- M.lookup n env
                , ct /= CTIntrinsic                           = ct
                | otherwise                                   = CTVariable
    semType <- deriveSemTypeFromTypeSpec ts
    forM_ decls $ \ decl -> case decl of
        -- TODO: we're throwing away the (Maybe Expr) in the Declarator -- is
        -- that bad? the original usage was confusing
        DeclArray _ _ v ddAList _ _ -> recordType semType (CTArray $ dimDeclarator ddAList) (varName v)
        DeclVariable _ _ v _ _      -> recordType semType (cType n) n where n = varName v

statement (StExternal _ _ varAList) = do
  let vars = aStrip varAList
  mapM_ (recordCType CTExternal . varName) vars
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  --  | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    let n = varName v
    mIDType <- getRecordedType n
    case mIDType of
      Just (IDType _ (Just CTArray{})) -> return ()                -- do nothing, it's already known to be an array
      _                                -> recordCType CTFunction n -- assume it's a function statement

-- FIXME: if StFunctions can only be identified after types analysis
-- is complete and disambiguation is performed, then how do we get
-- them in the first place? (iterate until fixed point?)
statement (StFunction _ _ v _ _) = recordCType CTFunction (varName v)
-- (part of answer to above is) nullary function statement: foo() = ...
statement (StExpressionAssign _ _ (ExpFunctionCall _ _ v Nothing) _) = recordCType CTFunction (varName v)

statement (StDimension _ _ declAList) = do
  let decls = aStrip declAList
  forM_ decls $ \ decl -> case decl of
    DeclArray _ _ v ddAList _ _ -> recordCType (CTArray $ dimDeclarator ddAList) (varName v)
    _                           -> return ()

statement _ = return ()

annotateExpression :: Data a => Expression (Analysis a) -> Infer (Expression (Analysis a))

-- handle the various literals
annotateExpression e@(ExpValue _ _ (ValVariable _))    = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ _ (ValIntrinsic _))   = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ ss (ValReal r))        = do
    k <- deriveRealLiteralKind ss r
    return $ setSemType (STyReal k) e
annotateExpression e@(ExpValue _ ss (ValComplex e1 e2)) = do
    st <- complexLiteralType ss e1 e2
    return $ setSemType st e
annotateExpression e@(ExpValue _ _ (ValInteger _))     =
    -- TODO: in >F90, int lits can have kind info on end, same as real lits. We
    -- do parse this into the lit string.
    return $ setSemType (deriveSemTypeFromBaseType TypeInteger) e
annotateExpression e@(ExpValue _ _ (ValLogical _))     =
    return $ setSemType (deriveSemTypeFromBaseType TypeLogical) e

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
complexLiteralType :: SrcSpan -> Expression a -> Expression a -> Infer SemType
complexLiteralType ss (ExpValue _ _ (ValReal r)) _ = do
    k1 <- deriveRealLiteralKind ss r
    return $ STyComplex k1
complexLiteralType _ _ _ = return $ deriveSemTypeFromBaseType TypeComplex

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
          | op `elem` [GT, GTE, LT, LTE, EQ, NE, Equivalent, NotEquivalent] -> return $ Just (deriveSemTypeFromBaseType TypeLogical)
          | BinCustom{} <- op -> typeError "custom binary ops not supported" ss >> return Nothing
        _ -> return Nothing

      return $ IDType mst' Nothing -- FIXME: might have to check kinds of each operand

-- | Combine two 'SemType's with a 'BinaryOp'.
--
-- No real work done here, no kind combining, just selection.
binopSimpleCombineSemTypes :: SrcSpan -> BinaryOp -> SemType -> SemType -> Infer (Maybe SemType)
binopSimpleCombineSemTypes ss op st1 st2 = do
    case (st1, st2) of
      (_           , STyComplex k2) -> ret $ STyComplex k2
      (STyComplex k1, _           ) -> ret $ STyComplex k1
      (_           , STyReal    k2) -> ret $ STyReal k2
      (STyReal    k1, _           ) -> ret $ STyReal k1
      (_           , STyInteger k2) -> ret $ STyInteger k2
      (STyInteger k1, _           ) -> ret $ STyInteger k1
      (STyByte    k1, STyByte    _ ) -> ret $ STyByte k1
      (STyLogical k1, STyLogical _ ) -> ret $ STyLogical k1
      (STyCustom  n1, STyCustom  n2) -> do
        typeError "custom types / binary op not supported" ss
        return Nothing
      (STyCharacter k1, STyCharacter k2)
        | op == Concatenation -> ret $ STyCharacter (liftA2 (+) k1 k2)
        | op `elem` [EQ, NE]  -> ret $ deriveSemTypeFromBaseType TypeLogical
        | otherwise -> do typeError "Invalid op on character strings" ss
                          return Nothing
      _ -> do typeError "Type error between operands of binary operator" ss
              return Nothing
  where
    ret = return . Just

unaryOpType :: Data a => SrcSpan -> UnaryOp -> Expression (Analysis a) -> Infer IDType
unaryOpType ss op e = do
  mst <- case getIDType e of
           Just (IDType (Just st) _) -> return $ Just st
           _ -> typeError "Unable to obtain type for" (getSpan e) >> return Nothing
  mst' <- case (mst, op) of
    (Nothing, _)               -> return Nothing
    (Just STyCustom{}, _)      -> typeError "custom types / unary ops not supported" ss >> return Nothing
    (_, UnCustom{})            -> typeError "custom unary ops not supported" ss >> return Nothing
    (Just st@(STyLogical _), Not)    -> return $ Just st
    (Just st, _)
      | op `elem` [Plus, Minus] &&
        isNumericType st -> return $ Just st
    _ -> typeError "Type error for unary operator" ss >> return Nothing
  return $ IDType mst' Nothing -- FIXME: might have to check kind of operand

subscriptType :: Data a => SrcSpan -> Expression (Analysis a) -> AList Index (Analysis a) -> Infer IDType
subscriptType ss e1 (AList _ _ idxs) = do
  let isInteger ie | Just (IDType (Just (STyInteger _)) _) <- getIDType ie = True
                   | otherwise = False
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
            ITParam i
              | length params >= i, Argument _ _ _ e <- params !! (i-1)
                -> return $ idVType =<< getIDType e
              | otherwise -> typeError ("Invalid parameter list to intrinsic '" ++ n ++ "'") ss >> return Nothing
            _ -> (return . intrinsicToMaybeSemType) retType
      case mst of
        Nothing -> return emptyType
        Just _ -> return $ IDType mst Nothing
functionCallType ss e1 _ = case getIDType e1 of
  Just (IDType (Just st) (Just CTFunction)) -> return $ IDType (Just st) Nothing
  Just (IDType (Just st) (Just CTExternal)) -> return $ IDType (Just st) Nothing
  _ -> typeError "non-function invoked by call" ss >> return emptyType

-- TODO: do this (what is 'IntrinsicType'?)
intrinsicToMaybeSemType :: IntrinsicType -> Maybe SemType
intrinsicToMaybeSemType = const Nothing

charLenConcat :: CharacterLen -> CharacterLen -> CharacterLen
charLenConcat l1 l2 = case (l1, l2) of
  (CharLenExp    , _             ) -> CharLenExp
  (_             , CharLenExp    ) -> CharLenExp
  (CharLenStar   , _             ) -> CharLenStar
  (_             , CharLenStar   ) -> CharLenStar
  (CharLenColon  , _             ) -> CharLenColon
  (_             , CharLenColon  ) -> CharLenColon
  (CharLenInt i1 , CharLenInt i2 ) -> CharLenInt (i1 + i2)

isNumericType :: SemType -> Bool
isNumericType = \case
  STyComplex{} -> True
  STyReal{}    -> True
  STyInteger{} -> True
  STyByte{}    -> True
  _            -> False

--------------------------------------------------
-- Monadic helper combinators.

inferState0 :: FortranVersion -> InferState
inferState0 v = InferState { environ = M.empty, entryPoints = M.empty, langVersion = v
                           , intrinsics = getVersionIntrinsics v, typeErrors = [] }
runInfer :: FortranVersion -> TypeEnv -> State InferState a -> (a, InferState)
runInfer v env = flip runState ((inferState0 v) { environ = env })

typeError :: String -> SrcSpan -> Infer ()
typeError msg ss = modify $ \ s -> s { typeErrors = (msg, ss):typeErrors s }

emptyType :: IDType
emptyType = IDType Nothing Nothing

-- Record the type of the given name.
recordType :: SemType -> ConstructType -> Name -> Infer ()
recordType st ct n = modify $ \ s -> s { environ = insert n (IDType (Just st) (Just ct)) (environ s) }

-- Record the type (maybe) of the given name.
recordMType :: Maybe SemType -> Maybe ConstructType -> Name -> Infer ()
recordMType st ct n = modify $ \ s -> s { environ = insert n (IDType st ct) (environ s) }

-- Record the CType of the given name.
recordCType :: ConstructType -> Name -> Infer ()
recordCType ct n = modify $ \ s -> s { environ = M.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (mIDType >>= idVType) (Just ct))

-- Record the SemType of the given name.
recordSemType :: SemType -> Name -> Infer ()
recordSemType st n = modify $ \ s -> s { environ = M.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just st) (mIDType >>= idCType))

recordEntryPoint :: Name -> Name -> Maybe Name -> Infer ()
recordEntryPoint fn en mRetName = modify $ \ s -> s { entryPoints = M.insert en (fn, mRetName) (entryPoints s) }

getRecordedType :: Name -> Infer (Maybe IDType)
getRecordedType n = gets (M.lookup n . environ)

-- Set the idType annotation
setIDType :: Annotated f => IDType -> f (Analysis a) -> f (Analysis a)
setIDType ty x
  | a@Analysis {} <- getAnnotation x = setAnnotation (a { idType = Just ty }) x
  | otherwise                        = x

-- Get the idType annotation
getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
getIDType x = idType (getAnnotation x)

-- | For all types holding an 'IDType' (in an 'Analysis'), set the 'SemType'
--   field of the 'IDType'.
setSemType :: (Annotated f, Data a) => SemType -> f (Analysis a) -> f (Analysis a)
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

-- | Attempt to derive a 'SemType' from a 'TypeSpec' (and the 'BaseType' etc.
--   inside).
--
-- This is an 'Infer' action so that it can report type errors.
deriveSemTypeFromTypeSpec :: TypeSpec a -> Infer SemType
deriveSemTypeFromTypeSpec (TypeSpec _ _ bt mSel) =
    case mSel of
      -- Selector present: we might have kind/other info provided
      Just sel -> deriveSemTypeFromBaseTypeAndSelector bt sel

      -- no Selector: derive using default kinds
      Nothing  -> return $ deriveSemTypeFromBaseType bt

-- | Attempt to derive a SemType from a 'BaseType' and a 'Selector' (e.g.
--   extracted from a 'TypeSpec').
deriveSemTypeFromBaseTypeAndSelector :: BaseType -> Selector a -> Infer SemType
deriveSemTypeFromBaseTypeAndSelector bt (Selector _ ss mLen mKindExpr) =
    case mLen of
      Nothing  ->
        case mKindExpr of
          Nothing       -> defaultSemType
          Just kindExpr ->
            -- only support integer kind selectors for now, no params
            case kindExpr of
              ExpValue _ _ (ValInteger k) ->
                deriveSemTypeFromBaseTypeAndKind bt (read k)
              _ -> do
                typeError "unsupported or invalid kind selector" (getSpan kindExpr)
                defaultSemType
      Just len ->
        -- length only makes sense with a CHARACTER (AST does not enforce)
        case bt of
          TypeCharacter mCharLenExpr mKindStr ->
            -- soft todo: OK, here we have:
            --   * the Selector (a Maybe lenexpr and Maybe kindexp)
            --   * the Selector parsed into a CharacterLen and a stricter kind
            -- and we need to stuff those into a single Maybe Kind (Int). I
            -- can't see how fortran-vars does it! TypeCheck.typeOfValue is for
            -- constants, and sets Just (length str). The stuff in Kind sets
            -- Just 1??? As a placeholder, I assume.
            -- So if we got a CharLenInt, we'll use that. Otherwise we'll leave
            -- a placeholder saying it's invalid for now.
            case mCharLenExpr of
              Just (CharLenInt chLen) -> return $ STyCharacter (Just chLen)
              Just charLenExpr -> do
                typeError "non-int char length spec currently unsupported, invalidating" ss
                return $ STyCharacter (Just invalidKind)
              _ -> return $ STyCharacter (Just invalidKind)
          _ -> do
            -- (upon tests, likely unreachable due to parser behaviour)
            typeError "only CHARACTER types can specify length (separate to kind)" ss
            defaultSemType
  where
    -- Same behaviour as for no Selector, intended to handle type error cases or
    -- empty Selectors (though latter should not occur).
    defaultSemType = return $ deriveSemTypeFromBaseType bt

deriveSemTypeFromBaseType :: BaseType -> SemType
deriveSemTypeFromBaseType = \case
  TypeInteger         -> STyInteger 4
  TypeReal            -> STyReal    4
  TypeComplex         -> STyComplex 4
  TypeLogical         -> STyLogical 4

  -- Fortran specs & compilers seem to agree on equating these intrinsic types
  -- to others with a larger kind, so we drop the extra syntactic info here.
  TypeDoublePrecision -> STyReal    8
  TypeDoubleComplex   -> STyComplex 8

  -- BYTE: HP's Fortran 90 reference says that BYTE is an HP extension, equates
  -- it to INTEGER(1), and indicates that it doesn't take a kind selector.
  -- Don't know how BYTEs are used in the wild. I wonder if we could safely
  -- equate BYTE to (STyInteger 1)?
  TypeByte            -> STyByte    noKind

  -- (see deriveSemTypeFromBaseTypeAndSelector)
  TypeCharacter mLen mKindExpr ->
    -- If we're here, mLen and mKindExpr are almost certainly Nothing. So we'll
    -- ignore them and say "dynamic length" (unsure if that's true)
    STyCharacter Nothing

  -- TODO: no clue what to do here. SemType maybe doesn't support F90 properly.
  -- (I think one or two of these map to error in FV)
  ClassStar           -> STyCustom "ClassStar"
  TypeCustom    str   -> STyCustom ("TypeCustom "  <> str)
  ClassCustom   str   -> STyCustom ("ClassCustom " <> str)

noKind, invalidKind :: Kind
noKind      = -1
invalidKind = -2

-- in the Infer monad because some intrinsic types may not take a kind, so it'd
-- be best to type error here to inform
-- but it's small fry stuff so we just write the given kind regardless
deriveSemTypeFromBaseTypeAndKind :: BaseType -> Kind -> Infer SemType
deriveSemTypeFromBaseTypeAndKind bt k =
    return $ setTypeKind (deriveSemTypeFromBaseType bt) k

setTypeKind :: SemType -> Kind -> SemType
setTypeKind st k = case st of
  STyInteger   _ -> STyInteger   k
  STyReal      _ -> STyReal      k
  STyComplex   _ -> STyComplex   k
  STyLogical   _ -> STyLogical   k
  STyByte      _ -> STyByte      k
  STyCharacter _ -> STyCharacter (Just k)
  STyCustom    s -> STyCustom s
  STyArray st' mDims ->
    -- TODO: appears invalid -- however none of my code creates 'STyArray's
    STyArray st' mDims

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

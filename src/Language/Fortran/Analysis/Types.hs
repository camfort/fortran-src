{-# LANGUAGE ScopedTypeVariables #-}
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
import Language.Fortran.Analysis
import Language.Fortran.Intrinsics
import Language.Fortran.Util.Position
import Language.Fortran.ParserMonad (FortranVersion(..))

import qualified Language.Fortran.Vars.Types as FV

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
  _ <- forM eps $ \ (eName, (fName, mRetName)) -> do
    mFType <- getRecordedType fName
    case mFType of
      Just (IDType fVType fCType kind fvextType) -> do
        recordMType fVType fCType kind eName
        -- FIXME: what about functions that return arrays?
        maybe (return ()) (error "Entry points with result variables unsupported" >> recordMType fVType Nothing Nothing) mRetName
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
      (Just (TypeSpec _ _ baseType _), Just v) -> recordBaseType baseType n >> recordBaseType baseType (varName v)
      (Just (TypeSpec _ _ baseType _), _)      -> recordBaseType baseType n
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
-- maybe FIXME: should Kind Selectors be part of types?
statement (StDeclaration _ _ (TypeSpec _ _ baseType sel) mAttrAList declAList)
  | mAttrs  <- maybe [] aStrip mAttrAList
  , attrDim <- find isAttrDimension mAttrs
  , isParam <- any isAttrParameter mAttrs
  , isExtrn <- any isAttrExternal mAttrs
  , decls   <- aStrip declAList = do
    env <- gets environ
    let cType n | isExtrn                                     = CTExternal
                | Just (AttrDimension _ _ ddAList) <- attrDim = CTArray (dimDeclarator ddAList)
                | isParam                                     = CTParameter
                | Just (IDType _ (Just ct) _) <- M.lookup n env
                , ct /= CTIntrinsic                           = ct
                | otherwise                                   = CTVariable
    let charLen (ExpValue _ _ (ValInteger i)) = CharLenInt (read i)
        charLen (ExpValue _ _ ValStar)        = CharLenStar
        charLen _                             = CharLenExp
    let bType (Just e)
          | TypeCharacter _ kind <- baseType = TypeCharacter (Just $ charLen e) kind
          | otherwise                        = TypeCharacter (Just $ charLen e) Nothing
        bType Nothing  = baseType
    let kind (Just (Selector _ _ _ (Just (ExpValue _ _ (ValInteger k))))) = Just k
        kind _ = Nothing
    let fvType = FV.TInteger fvKind
        fvKind = 0
    forM_ decls $ \ decl -> case decl of
      DeclArray _ _ v ddAList e _ -> recordType (bType e) (CTArray $ dimDeclarator ddAList) (kind sel) (Just fvType) (varName v)
      DeclVariable _ _ v e _      -> recordType (bType e) (cType n) (kind sel) (Just fvType) n where n = varName v

statement (StExternal _ _ varAList) = do
  let vars = aStrip varAList
  mapM_ (recordCType CTExternal . varName) vars
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  --  | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    let n = varName v
    mIDType <- getRecordedType n
    case mIDType of
      Just (IDType _ (Just CTArray{}) _) -> return ()                -- do nothing, it's already known to be an array
      _                                  -> recordCType CTFunction n -- assume it's a function statement

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
annotateExpression e@(ExpValue _ _ (ValVariable _))    = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ _ (ValIntrinsic _))   = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ _ (ValReal r))        = return $ realLiteralType r `setIDType` e
annotateExpression e@(ExpValue _ _ (ValComplex e1 e2)) = return $ complexLiteralType e1 e2 `setIDType` e
annotateExpression e@(ExpValue _ _ (ValInteger _))     = return $ IDType (Just TypeInteger) Nothing Nothing Nothing `setIDType` e
annotateExpression e@(ExpValue _ _ (ValLogical _))     = return $ IDType (Just TypeLogical) Nothing Nothing Nothing `setIDType` e
annotateExpression e@(ExpBinary _ _ op e1 e2)          = flip setIDType e `fmap` binaryOpType (getSpan e) op e1 e2
annotateExpression e@(ExpUnary _ _ op e1)              = flip setIDType e `fmap` unaryOpType (getSpan e1) op e1
annotateExpression e@(ExpSubscript _ _ e1 idxAList)    = flip setIDType e `fmap` subscriptType (getSpan e) e1 idxAList
annotateExpression e@(ExpFunctionCall _ _ e1 parAList) = flip setIDType e `fmap` functionCallType (getSpan e) e1 parAList
annotateExpression e                                   = return e

annotateProgramUnit :: Data a => ProgramUnit (Analysis a) -> Infer (ProgramUnit (Analysis a))
annotateProgramUnit pu | Named n <- puName pu = maybe pu (`setIDType` pu) `fmap` getRecordedType n
annotateProgramUnit pu                        = return pu

-- FIXME: parse any kind info out of literals, real or complex
realLiteralType :: String -> IDType
realLiteralType r | 'd' `elem` r = IDType (Just TypeDoublePrecision) Nothing Nothing Nothing
                  | otherwise    = IDType (Just TypeReal) Nothing Nothing Nothing

complexLiteralType :: Expression a -> Expression a -> IDType
complexLiteralType (ExpValue _ _ (ValReal r)) _
 | IDType (Just TypeDoublePrecision) _ _ <- realLiteralType r = IDType (Just TypeDoubleComplex) Nothing Nothing
 | otherwise                                                  = IDType (Just TypeComplex) Nothing Nothing
complexLiteralType _ _ = IDType (Just TypeComplex) Nothing Nothing

binaryOpType :: Data a => SrcSpan -> BinaryOp -> Expression (Analysis a) -> Expression (Analysis a) -> Infer IDType
binaryOpType ss op e1 e2 = do
  mbt1 <- case getIDType e1 of
            Just (IDType (Just bt) _ _) -> return $ Just bt
            _ -> typeError "Unable to obtain type for first operand" (getSpan e1) >> return Nothing
  mbt2 <- case getIDType e2 of
            Just (IDType (Just bt) _ _) -> return $ Just bt
            _ -> typeError "Unable to obtain type for second operand" (getSpan e2) >> return Nothing
  case (mbt1, mbt2) of
    (_, Nothing) -> return emptyType
    (Nothing, _) -> return emptyType
    (Just bt1, Just bt2) -> do
      mbt <- case (bt1, bt2) of
        (_                   , TypeDoubleComplex   ) -> return . Just $ TypeDoubleComplex
        (TypeDoubleComplex   , _                   ) -> return . Just $ TypeDoubleComplex
        (_                   , TypeComplex         ) -> return . Just $ TypeComplex
        (TypeComplex         , _                   ) -> return . Just $ TypeComplex
        (_                   , TypeDoublePrecision ) -> return . Just $ TypeDoublePrecision
        (TypeDoublePrecision , _                   ) -> return . Just $ TypeDoublePrecision
        (_                   , TypeReal            ) -> return . Just $ TypeReal
        (TypeReal            , _                   ) -> return . Just $ TypeReal
        (_                   , TypeInteger         ) -> return . Just $ TypeInteger
        (TypeInteger         , _                   ) -> return . Just $ TypeInteger
        (TypeByte            , TypeByte            ) -> return . Just $ TypeByte
        (TypeLogical         , TypeLogical         ) -> return . Just $ TypeLogical
        (TypeCustom _        , TypeCustom _        ) -> do
          typeError "custom types / binary op not supported" ss
          return Nothing
        (TypeCharacter l1 k1 , TypeCharacter l2 _ )
          | op == Concatenation -> return . Just $ TypeCharacter (liftM2 charLenConcat l1 l2) k1
          | op `elem` [EQ, NE]  -> return $ Just TypeLogical
          | otherwise -> do typeError "Invalid op on character strings" ss
                            return Nothing
        _ -> do typeError "Type error between operands of binary operator" ss
                return Nothing
      mbt' <- case mbt of
        Just bt
          | op `elem` [ Addition, Subtraction, Multiplication, Division
                      , Exponentiation, Concatenation, Or, XOr, And ]       -> return $ Just bt
          | op `elem` [GT, GTE, LT, LTE, EQ, NE, Equivalent, NotEquivalent] -> return $ Just TypeLogical
          | BinCustom{} <- op -> typeError "custom binary ops not supported" ss >> return Nothing
        _ -> return Nothing

      return $ IDType mbt' Nothing Nothing -- FIXME: might have to check kinds of each operand

unaryOpType :: Data a => SrcSpan -> UnaryOp -> Expression (Analysis a) -> Infer IDType
unaryOpType ss op e = do
  mbt <- case getIDType e of
           Just (IDType (Just bt) _ _) -> return $ Just bt
           _ -> typeError "Unable to obtain type for" (getSpan e) >> return Nothing
  mbt' <- case (mbt, op) of
    (Nothing, _)               -> return Nothing
    (Just TypeCustom{}, _)     -> typeError "custom types / unary ops not supported" ss >> return Nothing
    (_, UnCustom{})            -> typeError "custom unary ops not supported" ss >> return Nothing
    (Just TypeLogical, Not)    -> return $ Just TypeLogical
    (Just bt, _)
      | op `elem` [Plus, Minus] &&
        bt `elem` numericTypes -> return $ Just bt
    _ -> typeError "Type error for unary operator" ss >> return Nothing
  return $ IDType mbt' Nothing Nothing -- FIXME: might have to check kind of operand

subscriptType :: Data a => SrcSpan -> Expression (Analysis a) -> AList Index (Analysis a) -> Infer IDType
subscriptType ss e1 (AList _ _ idxs) = do
  let isInteger ie | Just (IDType (Just TypeInteger) _ _) <- getIDType ie = True | otherwise = False
  forM_ idxs $ \ idx -> case idx of
    IxSingle _ _ _ ie
      | not (isInteger ie) -> typeError "Invalid or unknown type for index" (getSpan ie)
    IxRange _ _ mie1 mie2 mie3
      | Just ie1 <- mie1, not (isInteger ie1) -> typeError "Invalid or unknown type for index" (getSpan ie1)
      | Just ie2 <- mie2, not (isInteger ie2) -> typeError "Invalid or unknown type for index" (getSpan ie2)
      | Just ie3 <- mie3, not (isInteger ie3) -> typeError "Invalid or unknown type for index" (getSpan ie3)
    _ -> return ()
  case getIDType e1 of
    Just ty@(IDType mbt (Just (CTArray dds)) kind) -> do
      when (length idxs /= length dds) $ typeError "Length of indices does not match rank of array." ss
      let isSingle (IxSingle{}) = True; isSingle _ = False
      if all isSingle idxs
        then return $ IDType mbt Nothing kind
        else return ty
    _ -> return emptyType

functionCallType :: Data a => SrcSpan -> Expression (Analysis a) -> Maybe (AList Argument (Analysis a)) -> Infer IDType
functionCallType ss (ExpValue _ _ (ValIntrinsic n)) (Just (AList _ _ params)) = do
  itab <- gets intrinsics
  let mRetType = getIntrinsicReturnType n itab
  case mRetType of
    Nothing -> return emptyType
    Just retType -> do
      mbt <- case retType of
            ITReal      -> return $ Just TypeReal
            ITInteger   -> return $ Just TypeInteger
            ITComplex   -> return $ Just TypeComplex
            ITDouble    -> return $ Just TypeDoublePrecision
            ITLogical   -> return $ Just TypeLogical
            ITCharacter -> return . Just $ TypeCharacter Nothing Nothing
            ITParam i
              | length params >= i, Argument _ _ _ e <- params !! (i-1)
                -> return $ idVType =<< getIDType e
              | otherwise -> typeError ("Invalid parameter list to intrinsic '" ++ n ++ "'") ss >> return Nothing
      case mbt of
        Nothing -> return emptyType
        Just _ -> return $ IDType mbt Nothing Nothing
functionCallType ss e1 _ = case getIDType e1 of
  Just (IDType (Just bt) (Just CTFunction) kind) -> return $ IDType (Just bt) Nothing kind
  Just (IDType (Just bt) (Just CTExternal) kind) -> return $ IDType (Just bt) Nothing kind
  _ -> typeError "non-function invoked by call" ss >> return emptyType

charLenConcat :: CharacterLen -> CharacterLen -> CharacterLen
charLenConcat l1 l2 = case (l1, l2) of
  (CharLenExp    , _             ) -> CharLenExp
  (_             , CharLenExp    ) -> CharLenExp
  (CharLenStar   , _             ) -> CharLenStar
  (_             , CharLenStar   ) -> CharLenStar
  (CharLenColon  , _             ) -> CharLenColon
  (_             , CharLenColon  ) -> CharLenColon
  (CharLenInt i1 , CharLenInt i2 ) -> CharLenInt (i1 + i2)

numericTypes :: [BaseType]
numericTypes = [TypeDoubleComplex, TypeComplex, TypeDoublePrecision, TypeReal, TypeInteger, TypeByte]

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
emptyType = IDType Nothing Nothing Nothing

-- Record the type of the given name.
recordType :: BaseType -> ConstructType -> Maybe Kind -> Maybe FV.Type -> Name -> Infer ()
recordType bt ct k n = modify $ \ s -> s { environ = insert n (IDType (Just bt) (Just ct) k) (environ s) }

-- Record the type (maybe) of the given name.
recordMType :: Maybe BaseType -> Maybe ConstructType -> Maybe Kind -> Name -> Infer ()
recordMType bt ct k n = modify $ \ s -> s { environ = insert n (IDType bt ct k) (environ s) }

-- Record the CType of the given name.
recordCType :: ConstructType -> Name -> Infer ()
recordCType ct n = modify $ \ s -> s { environ = M.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (mIDType >>= idVType) (Just ct) (mIDType >>= idKind))

-- Record the BaseType of the given name.
recordBaseType :: BaseType -> Name -> Infer ()
recordBaseType bt n = modify $ \ s -> s { environ = M.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just bt) (mIDType >>= idCType) (mIDType >>= idKind))

recordEntryPoint :: Name -> Name -> Maybe Name -> Infer ()
recordEntryPoint fn en mRetName = modify $ \ s -> s { entryPoints = M.insert en (fn, mRetName) (entryPoints s) }

getRecordedType :: Name -> Infer (Maybe IDType)
getRecordedType n = gets (M.lookup n . environ)

-- Set the idType annotation
setIDType :: Annotated f => IDType -> f (Analysis a) -> f (Analysis a)
setIDType ty x
  | a@Analysis {} <- getAnnotation x = setAnnotation (a { idType = Just ty }) x
  | otherwise                          = x

-- Get the idType annotation
getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
getIDType x = idType (getAnnotation x)

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
isAttrParameter _                  = False

isAttrExternal :: Attribute a -> Bool
isAttrExternal AttrExternal {} = True
isAttrExternal _                 = False

isIxSingle :: Index a -> Bool
isIxSingle IxSingle {} = True
isIxSingle _             = False

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

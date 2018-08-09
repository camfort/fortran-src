{-# LANGUAGE ScopedTypeVariables #-}
module Language.Fortran.Analysis.Types ( analyseTypes, analyseTypesWithEnv, extractTypeEnv, TypeEnv ) where

import Language.Fortran.AST

import Prelude hiding (lookup)
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
import Language.Fortran.ParserMonad (FortranVersion(..))


--------------------------------------------------

-- | Mapping of names to type information.
type TypeEnv = M.Map Name IDType

--------------------------------------------------

-- Monad for type inference work
type Infer a = State InferState a
data InferState = InferState { langVersion :: FortranVersion
                             , intrinsics  :: IntrinsicsTable
                             , environ     :: TypeEnv
                             , entryPoints :: M.Map Name (Name, Maybe Name) }
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
analyseTypesWithEnv env pf@(ProgramFile mi _) = fmap environ . runInfer (miVersion mi) env $ do
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
    expEnv = M.fromList [ (n, ty) | e <- universeBi pf :: [Expression (Analysis a)]
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
statement (StDeclaration _ _ (TypeSpec _ _ baseType _) mAttrAList declAList)
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
    let charLen (ExpValue _ _ (ValInteger i)) = CharLenInt (read i)
        charLen (ExpValue _ _ ValStar)        = CharLenStar
        charLen _                             = CharLenExp
    let bType (Just e)
          | TypeCharacter _ kind <- baseType = TypeCharacter (Just $ charLen e) kind
          | otherwise                        = TypeCharacter (Just $ charLen e) Nothing
        bType Nothing  = baseType
    forM_ decls $ \ decl -> case decl of
      DeclArray _ _ v ddAList e _ -> recordType (bType e) (CTArray $ dimDeclarator ddAList) (varName v)
      DeclVariable _ _ v e _      -> recordType (bType e) (cType n) n where n = varName v

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
annotateExpression e@(ExpValue _ _ (ValVariable _))  = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e@(ExpValue _ _ (ValIntrinsic _)) = maybe e (`setIDType` e) `fmap` getRecordedType (varName e)
annotateExpression e                                 = return e

annotateProgramUnit :: Data a => ProgramUnit (Analysis a) -> Infer (ProgramUnit (Analysis a))
annotateProgramUnit pu | Named n <- puName pu = maybe pu (`setIDType` pu) `fmap` getRecordedType n
annotateProgramUnit pu                        = return pu

--------------------------------------------------
-- Monadic helper combinators.

inferState0 :: FortranVersion -> InferState
inferState0 v = InferState { environ = M.empty, entryPoints = M.empty, langVersion = v, intrinsics = getVersionIntrinsics v }
runInfer :: FortranVersion -> TypeEnv -> State InferState a -> (a, InferState)
runInfer v env = flip runState ((inferState0 v) { environ = env })

-- Record the type of the given name.
recordType :: BaseType -> ConstructType -> Name -> Infer ()
recordType bt ct n = modify $ \ s -> s { environ = insert n (IDType (Just bt) (Just ct)) (environ s) }

-- Record the type (maybe) of the given name.
recordMType :: Maybe BaseType -> Maybe ConstructType -> Name -> Infer ()
recordMType bt ct n = modify $ \ s -> s { environ = insert n (IDType bt ct) (environ s) }

-- Record the CType of the given name.
recordCType :: ConstructType -> Name -> Infer ()
recordCType ct n = modify $ \ s -> s { environ = M.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (mIDType >>= idVType) (Just ct))

-- Record the BaseType of the given name.
recordBaseType :: BaseType -> Name -> Infer ()
recordBaseType bt n = modify $ \ s -> s { environ = M.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just bt) (mIDType >>= idCType))

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
--getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
--getIDType x = idType (getAnnotation x)

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

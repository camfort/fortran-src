{-# LANGUAGE ScopedTypeVariables #-}
module Language.Fortran.Analysis.Types ( inferTypes, analyseTypes, TypeScope(..), TypeEnv ) where

import Language.Fortran.AST

import Prelude hiding (lookup)
import Data.Map (findWithDefault, insert, empty, lookup, Map)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Language.Fortran.Analysis

import Debug.Trace

--------------------------------------------------

type TypeEnv = M.Map Name IDType
data InferState = InferState { environ :: TypeEnv, entryPoints :: M.Map Name (Name, Maybe Name) }
  deriving Show
type Infer a = State InferState a
type InferFunc t = t -> Infer ()

--------------------------------------------------

analyseTypes :: Data a => ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypes pf = fmap environ . runInfer $ do
  -- Gather information.
  mapM_ programUnit (allProgramUnits pf)
  mapM_ declarator (allDeclarators pf)
  mapM_ statement (allStatements pf)

  -- Gather types for known entry points.
  eps <- gets (M.toList . entryPoints)
  forM eps $ \ (eName, (fName, mRetName)) -> do
    mFType <- getRecordedType fName
    case mFType of
      Just (IDType fVType fCType) -> do
        recordMType fVType fCType eName
        -- FIXME: what about functions that return arrays?
        maybe (return ()) (error "Entry points with result variables unsupported" >> recordMType fVType Nothing) mRetName
      _                           -> return ()

  annotateTypes pf              -- Annotate AST nodes with their types.

type TransType f g a = (f (Analysis a) -> Infer (f (Analysis a))) -> g (Analysis a) -> Infer (g (Analysis a))
annotateTypes :: Data a => ProgramFile (Analysis a) -> Infer (ProgramFile (Analysis a))
annotateTypes pf = (transformBiM :: Data a => TransType Expression ProgramFile a) annotateExpression pf >>=
                   (transformBiM :: Data a => TransType ProgramUnit ProgramFile a) annotateProgramUnit

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
      sequence_ [ recordEntryPoint n (varName v) (fmap varName mRetVar) | (StEntry _ _ v _ mRetVar) <- allStatements block ]
programUnit pu@(PUSubroutine _ _ _ _ _ blocks _) | Named n <- puName pu = do
  -- record the fact that this is a subroutine
  recordCType CTSubroutine n
  -- record entry points for later annotation
  forM_ blocks $ \ block ->
    sequence_ [ recordEntryPoint n (varName v) Nothing | (StEntry _ _ v _ _) <- allStatements block ]
programUnit _                                           = return ()

declarator :: Data a => InferFunc (Declarator (Analysis a))
declarator (DeclArray _ _ v _ _ _) = recordCType CTArray (varName v)
declarator _                       = return ()

statement :: Data a => InferFunc (Statement (Analysis a))
-- maybe FIXME: should Kind Selectors be part of types?
statement (StDeclaration _ _ (TypeSpec _ _ baseType _) mAttrAList declAList)
  | isArray <- any isAttrDimension (maybe [] aStrip mAttrAList)
  , decls   <- aStrip declAList = do
    forM_ decls $ \ decl -> case decl of
      DeclVariable _ _ v (Just _) _ -> recordType baseType CTVariable (varName v)
      DeclVariable _ _ v Nothing _  -> recordBaseType baseType (varName v) >> when isArray (recordCType CTArray (varName v))
      DeclArray _ _ v _ _ _         -> recordType baseType CTArray (varName v)
    return ()
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  -- | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    let n = varName v
    mIDType <- getRecordedType n
    case mIDType of
      Just (IDType mBT (Just CTArray)) -> return ()                -- do nothing, it's already known to be an array
      _                                -> recordCType CTFunction n -- assume it's a function statement

-- FIXME: if StFunctions can only be identified after types analysis
-- is complete and disambiguation is performed, then how do we get
-- them in the first place? (iterate until fixed point?)
statement (StFunction _ _ v _ _) = recordCType CTFunction (varName v)

statement _ = return ()

annotateExpression :: Data a => Expression (Analysis a) -> Infer (Expression (Analysis a))
annotateExpression e@(ExpValue _ _ (ValVariable _)) = maybe e (flip setIDType e) `fmap` getRecordedType (varName e)
annotateExpression e                                = return e

annotateProgramUnit :: Data a => ProgramUnit (Analysis a) -> Infer (ProgramUnit (Analysis a))
annotateProgramUnit pu | Named n <- puName pu = maybe pu (flip setIDType pu) `fmap` getRecordedType n
annotateProgramUnit pu                        = return pu

--------------------------------------------------
-- Monadic helper combinators.

inferState0 = InferState { environ = M.empty, entryPoints = M.empty }
runInfer = flip runState inferState0

-- Record the type of the given name.
recordType :: BaseType -> ConstructType -> Name -> Infer ()
recordType bt ct n = modify $ \ s -> s { environ = insert n (IDType (Just bt) (Just ct)) (environ s) }

-- Record the type (maybe) of the given name.
recordMType :: Maybe BaseType -> Maybe ConstructType -> Name -> Infer ()
recordMType bt ct n = modify $ \ s -> s { environ = insert n (IDType (bt) (ct)) (environ s) }

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
  | a@(Analysis {}) <- getAnnotation x = setAnnotation (a { idType = Just ty }) x
  | otherwise                          = x

-- Get the idType annotation
getIDType :: (Annotated f, Data a) => f (Analysis a) -> Maybe IDType
getIDType x = idType (getAnnotation x)

-- Set the CType part of idType annotation
setCType :: (Annotated f, Data a) => ConstructType -> f (Analysis a) -> f (Analysis a)
setCType ct x
  | a@(Analysis { idType = Nothing }) <- getAnnotation x = setAnnotation (a { idType = Just (IDType Nothing (Just ct)) }) x
  | a@(Analysis { idType = Just it }) <- getAnnotation x = setAnnotation (a { idType = Just (it { idCType = Just ct }) }) x

type UniFunc f g a = f (Analysis a) -> [g (Analysis a)]

allProgramUnits :: Data a => UniFunc ProgramFile ProgramUnit a
allProgramUnits = universeBi

allDeclarators :: Data a => UniFunc ProgramFile Declarator a
allDeclarators = universeBi

allStatements :: (Data a, Data (f (Analysis a))) => UniFunc f Statement a
allStatements = universeBi

isAttrDimension (AttrDimension {}) = True
isAttrDimension _                  = False

isAttrParameter (AttrDimension {}) = True
isAttrParameter _                  = False

isIxSingle (IxSingle {}) = True
isIxSingle _             = False

--------------------------------------------------


--------------------------------------------------------------------------------
-- Type mapping definitions
--------------------------------------------------------------------------------

data TypeScope = Global | Local ProgramUnitName deriving (Ord,Eq,Show)

data TypeState a = TypeState
  { tsProgramFile :: ProgramFile a
  , tsMapping :: Map TypeScope (Map String IDType) }

type TypeMapping a = State (TypeState a)

--------------------------------------------------------------------------------
-- Monadic helpers
--------------------------------------------------------------------------------

getProgramFile :: Data a => TypeMapping a (ProgramFile a)
getProgramFile = fmap tsProgramFile get

putProgramFile :: Data a => ProgramFile a -> TypeMapping a ()
putProgramFile pf = get >>= (\ts -> put $ ts { tsProgramFile = pf })

getMapping :: Data a => TypeMapping a (Map TypeScope (Map String IDType))
getMapping = fmap tsMapping get

queryIDType :: Data a => TypeScope -> String -> TypeMapping a (Maybe IDType)
queryIDType ts s = do
  mapping <- getMapping
  return $ do
    inner <- lookup ts mapping
    lookup s inner

putMapping :: Data a => Map TypeScope (Map String IDType) -> TypeMapping a ()
putMapping mapping = get >>= (\ts -> put $ ts { tsMapping = mapping })

addToMapping :: Data a => TypeScope -> String -> BaseType -> ConstructType -> TypeMapping a ()
addToMapping ts s vt ct = addToMappingViaFunc ts s typeFunction
  where
    typeFunction idt = idt { idVType = Just vt, idCType = Just ct }

addValueToMapping :: Data a => TypeScope -> String -> BaseType -> TypeMapping a ()
addValueToMapping ts s vt = addToMappingViaFunc ts s typeFunction
  where
    typeFunction idt = idt { idVType = Just vt }

addConstructToMapping :: Data a => TypeScope -> String -> ConstructType -> TypeMapping a ()
addConstructToMapping ts s ct = addToMappingViaFunc ts s typeFunction
  where
    typeFunction idt = idt { idCType = Just ct }

addToMappingViaFunc :: Data a => TypeScope -> String -> (IDType -> IDType) -> TypeMapping a ()
addToMappingViaFunc ts s tf = do
  mapping <- getMapping
  let innerMapping = findWithDefault empty ts mapping
  let idType = findWithDefault (IDType Nothing Nothing) s innerMapping
  let newIdType = tf idType
  let newInnerMapping = insert s newIdType innerMapping
  let newMapping = insert ts newInnerMapping mapping
  putMapping newMapping

--------------------------------------------------------------------------------
-- Inference mechanism
--------------------------------------------------------------------------------

inferTypes :: Data a => ProgramFile a -> Map TypeScope (Map String IDType)
inferTypes pf = tsMapping . execState (inferGlobal >> inferLocal) $ state
  where
    state = TypeState pf empty

inferGlobal :: Data a => TypeMapping a ()
inferGlobal = do
  (ProgramFile comAndPus _) <- getProgramFile
  let pus = map snd comAndPus
  mapM_ inferSubprograms pus

inferSubprograms :: Data a => ProgramUnit a -> TypeMapping a ()
inferSubprograms pu =
  case pu of
    (PUFunction _ _ mts _ n _ _ _ _) -> do
      addToMappingViaFunc Global n $ updateForFunction mts
      addEntries (updateForFunction mts)
    (PUSubroutine _ _ _ n _ _ _) -> do
      addConstructToMapping Global n CTSubroutine
      addEntries (\idt -> idt { idCType = Just CTSubroutine })
    _ -> return ()
  where
    addEntries :: Data a => (IDType -> IDType) -> TypeMapping a ()
    addEntries func = do
      let statements = universeBi :: Data a => ProgramUnit a -> [Statement a]
      let entryExps = [ e | (StEntry _ _ e _ _) <- statements pu ]
      let entryNames = [ n | n :: String <- universeBi entryExps :: [String] ]
      mapM_ (\n' -> addToMappingViaFunc Global n' func) entryNames
    updateForFunction Nothing it =
      it { idCType = Just CTFunction }
    updateForFunction (Just (TypeSpec _ _ bt _)) it =
      it { idCType = Just CTFunction, idVType = Just bt }

inferLocal :: Data a => TypeMapping a ()
inferLocal = do
  (ProgramFile comAndPus _) <- getProgramFile
  let pus = map snd comAndPus
  mapM_ inferInProgramFile pus

inferInProgramFile :: Data a => ProgramUnit a -> TypeMapping a ()
inferInProgramFile pu = do
  let declPairs = [ (bt, vars) | (StDeclaration _ _ bt _ (AList _ _ vars)) <- universeBi pu ]
  inferFromDeclarations puName declPairs
  let paramSts = [ paramSt | paramSt@StParameter{} <- universeBi pu]
  inferFromParameters puName paramSts
  let dimSts = [ dim | dim@StDimension{} <- universeBi pu]
  inferFromDimensions puName dimSts
  inferFromFuncStatements pu
  where
    puName = Local $ getName pu

inferFromFuncStatements :: Data a => ProgramUnit a -> TypeMapping a ()
inferFromFuncStatements pu = do
  let statements = universeBi :: Data a => ProgramUnit a -> [Statement a]
  let lhsNames = [ s | StExpressionAssign _ _ (ExpSubscript _ _ (ExpValue _ _ (ValVariable s)) _) _ <- statements pu ]
  idts <- mapM (queryIDType puName) lhsNames
  let filteredNames = map fst $ filter p $ zip lhsNames idts
  mapM_ (\n -> addConstructToMapping puName n CTFunction) filteredNames
  let lhsNames = [ s | StFunction _ _ (ExpValue _ _ (ValVariable s)) _ _ <- statements pu ]
  mapM_ (\n -> addConstructToMapping puName n CTFunction) lhsNames
  where
    puName = Local $ getName pu
    -- Predicate makes sure Dimension or Type statements did not register
    -- this Array seeming LHS value as an Array. In which case it can only
    -- be a Function Statement.
    p (_, Nothing) = True
    p (_, (Just (IDType _ Nothing))) = True
    p _ = False

inferFromDimensions :: Data a => TypeScope -> [ Statement a ] -> TypeMapping a ()
inferFromDimensions ts dimSts = do
  let decls = universeBi :: Data a => [Statement a] -> [Declarator a]
  let arrayExps = [ exp | DeclArray _ _ exp _ _ _ <- decls dimSts]
  let arrayNames = [ s | ExpValue _ _ (ValVariable s) <- arrayExps ]
  mapM_ (\n -> addConstructToMapping ts n CTArray) arrayNames

inferFromParameters :: Data a => TypeScope -> [ Statement a ] -> TypeMapping a ()
inferFromParameters ts paramSts = do
  let values = universeBi :: Data a => [ Statement a ] -> [ Declarator a ]
  let paramNames = [ n | DeclVariable _ _ (ExpValue _ _ (ValVariable n)) _ _ <- values paramSts ]
  mapM_ (\n -> addConstructToMapping ts n CTParameter) paramNames

inferFromDeclarations :: Data a => TypeScope -> [ (TypeSpec a, [ Declarator a ]) ] -> TypeMapping a ()
inferFromDeclarations _ [ ] = return ()
inferFromDeclarations ts (((TypeSpec _ _ bt _), decls):ds) = do
  addDecls decls
  inferFromDeclarations ts ds
  where
    addDecls [ ] = return ()
    addDecls (d':ds') = do
      case d' of
        DeclArray _ _ e _ _ _ -> addToMapping ts (expToId e) bt CTArray
        -- Decl variables might also be functions or arrays qualified by
        -- later specifications.
        DeclVariable _ _ e _ _ -> addValueToMapping ts (expToId e) bt
      addDecls ds'
    expToId (ExpValue _ _ (ValVariable s)) = s

--------------------------------------------------------------------------------
-- Utility methods
--------------------------------------------------------------------------------

genImplicitMapping :: Data a => ProgramUnit a -> (Char -> Maybe BaseType)
genImplicitMapping pu
  | null impSts = globalImplicitMapping -- Apply default type mappings
  | containsNone impSts = const Nothing -- If IMPLICIT NONE is in
  | otherwise = -- Try all possible mappings.
    tryImps $ map impElToFun impPairs
  where
    statements = universeBi :: Data a => ProgramUnit a -> [Statement a]
    impSts = [ imp | imp@(StImplicit _ _ _) <- statements pu ]
    implists = universeBi :: Data a => ProgramUnit a -> [ImpList a]
    impPairs = join . join . map couple $ ([ (bt,xs) | ((ImpList _ _ (TypeSpec _ _ bt _) (AList _ _ xs))) <- implists pu ])
    couple (a, []) = []
    couple (a, x:xs) = [ (a,x) ] : couple (a, xs)
    containsNone imps = length [ x | x@(StImplicit _ _ Nothing) <- impSts ] == 1
    impElToFun (vt, ImpCharacter _ _ c) c' = if c' == head c then Just vt else Nothing
    impElToFun (vt, ImpRange _ _ c1 c2) c' = if c' >= head c1 && c' <= head c2 then Just vt else Nothing
    tryImps [] c = Nothing
    tryImps (x:xs) c =
      case x c of
        Just bt -> Just bt
        Nothing -> tryImps xs c

globalImplicitMapping :: Char -> Maybe BaseType
globalImplicitMapping c
  | c `elem` "ijklmn" = Just TypeInteger
  | otherwise = Just TypeReal

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

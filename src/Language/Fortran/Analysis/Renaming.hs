{-# LANGUAGE ScopedTypeVariables, PatternGuards, TupleSections #-}

-- |
-- Analyse variables/function names and produce unique names that can
-- be used to replace the original names while maintaining program
-- equivalence (a.k.a. alpha-conversion). The advantage of the unique
-- names is that scoping issues can be ignored when doing further
-- analysis.

module Language.Fortran.Analysis.Renaming
  ( analyseRenames, analyseRenamesWithModuleMap, rename, unrename, ModuleMap )
where

import Language.Fortran.AST hiding (fromList)
import Language.Fortran.Intrinsics
import Language.Fortran.Analysis
import Language.Fortran.ParserMonad (FortranVersion(..))

import Prelude hiding (lookup)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.List as L
import Data.Map (insert, empty, lookup, Map)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Data.Generics.Uniplate.DataOnly
import Data.Data
import Data.Functor.Identity (Identity)

--------------------------------------------------

type ModuleMap     = Map ProgramUnitName ModEnv

type Renamer a     = State RenameState a -- the monad.
data RenameState   = RenameState { langVersion :: FortranVersion
                                 , intrinsics  :: IntrinsicsTable
                                 , scopeStack  :: [String]
                                 , uniqNums    :: [Int]
                                 , environ     :: [ModEnv]
                                 , moduleMap   :: ModuleMap }
  deriving (Show, Eq)
type RenamerFunc t = t -> Renamer t

--------------------------------------------------
-- Main interface functions.

-- | Annotate unique names for variable and function declarations and uses.
analyseRenames :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseRenames (ProgramFile mi pus) = cleanupUseRenames $ ProgramFile mi pus'
  where
    (Just pus', _) = runRenamer (renameSubPUs (Just pus)) (renameState0 (miVersion mi))

-- | Annotate unique names for variable and function declarations and uses. With external module map.
analyseRenamesWithModuleMap :: Data a => ModuleMap -> ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseRenamesWithModuleMap mmap (ProgramFile mi pus) = cleanupUseRenames $ ProgramFile mi pus'
  where
    (Just pus', _) = runRenamer (renameSubPUs (Just pus)) (renameState0 (miVersion mi)) { moduleMap = mmap }

-- | Take the unique name annotations and substitute them into the actual AST.
rename :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
rename pf = trPU fPU (trE fE pf)
  where
    trE :: Data a => (Expression a -> Expression a) -> ProgramFile a -> ProgramFile a
    trE = transformBi
    fE :: Data a => Expression (Analysis a) -> Expression (Analysis a)
    fE (ExpValue a s (ValVariable v))  = ExpValue a s . ValVariable $ fromMaybe v (uniqueName a)
    fE (ExpValue a s (ValIntrinsic v)) = ExpValue a s . ValIntrinsic $ fromMaybe v (uniqueName a)
    fE x                               = x

    trPU :: Data a => (ProgramUnit a -> ProgramUnit a) -> ProgramFile a -> ProgramFile a
    trPU = transformBi
    fPU :: Data a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
    fPU (PUFunction a s ty r n args res b subs) =
      PUFunction a s ty r (fromMaybe n (uniqueName a)) args res b subs
    fPU (PUSubroutine a s r n args b subs) =
      PUSubroutine a s r (fromMaybe n (uniqueName a)) args b subs
    fPU x = x

-- | Take a renamed program and undo the renames.
unrename :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
unrename = trPU fPU . trE fE
  where
    trE :: Data a => (Expression (Analysis a) -> Expression (Analysis a)) -> ProgramFile (Analysis a) -> ProgramFile (Analysis a)
    trE = transformBi
    fE :: Data a => Expression (Analysis a) -> Expression (Analysis a)
    fE e@(ExpValue a s (ValVariable _))  = ExpValue a s (ValVariable (srcName e))
    fE e@(ExpValue a s (ValIntrinsic _)) = ExpValue a s (ValIntrinsic (srcName e))
    fE e                                 = e

    trPU :: Data a => (ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)) -> ProgramFile (Analysis a) -> ProgramFile (Analysis a)
    trPU = transformBi
    fPU :: Data a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
    fPU (PUFunction a s ty r _ args res b subs)
      | Just srcN <- sourceName a = PUFunction a s ty r srcN args res b subs
    fPU (PUSubroutine a s r _ args b subs)
      | Just srcN <- sourceName a = PUSubroutine a s r srcN args b subs
    fPU           pu              = pu

--------------------------------------------------
-- Renaming transformations for pieces of the AST. Uses a language of
-- monadic combinators defined below.

programUnit :: Data a => RenamerFunc (ProgramUnit (Analysis a))
programUnit (PUModule a s name blocks m_contains) = do
  env0        <- initialEnv blocks
  pushScope name env0
  blocks1     <- mapM renameModDecls blocks  -- handle declarations
  blocks2     <- mapM renameUseSt blocks1    -- handle use statements
  m_contains' <- renameSubPUs m_contains     -- handle contained program units
  blocks3     <- mapM renameBlock blocks2    -- process all uses of functions/subroutine names
  env         <- getEnv
  addModEnv name env                         -- save the module environment
  let a'      = a { moduleEnv = Just env }   -- also annotate it on the module
  popScope
  return (PUModule a' s name blocks3 m_contains')

programUnit (PUFunction a s ty rec name args res blocks m_contains) = do
  ~(Just name') <- getFromEnv name                  -- get renamed function name
  (blocks1, _)  <- returnBlocksEnv blocks name
  blocks2     <- mapM renameEntryPointResultDecl blocks1 -- rename the result
  res'        <- mapM renameGenericDecls res             -- variable(s) if needed
  args'       <- mapM renameGenericDecls args -- rename arguments
  blocks3     <- mapM renameDeclDecls blocks2 -- handle declarations
  m_contains' <- renameSubPUs m_contains      -- handle contained program units
  blocks4     <- mapM renameBlock blocks3     -- process all uses of variables
  let env     = M.singleton name (name', NTSubprogram)
  let a'      = a { moduleEnv = Just env }    -- also annotate it on the program unit
  popScope
  let pu' = PUFunction a' s ty rec name args' res' blocks4 m_contains'
  return . setSourceName name . setUniqueName name' $ pu'

programUnit (PUSubroutine a s rec name args blocks m_contains) = do
  ~(Just name') <- getFromEnv name                  -- get renamed subroutine name
  (blocks1, _)  <- returnBlocksEnv blocks name
  args'       <- mapM renameGenericDecls args -- rename arguments
  blocks2     <- mapM renameDeclDecls blocks1 -- handle declarations
  m_contains' <- renameSubPUs m_contains      -- handle contained program units
  blocks3     <- mapM renameBlock blocks2     -- process all uses of variables
  let env     = M.singleton name (name', NTSubprogram)
  let a'      = a { moduleEnv = Just env }    -- also annotate it on the program unit
  popScope
  let pu' = PUSubroutine a' s rec name args' blocks3 m_contains'
  return . setSourceName name . setUniqueName name' $ pu'

programUnit (PUMain a s n blocks m_contains) = do
  env0        <- initialEnv blocks
  pushScope (fromMaybe "_main" n) env0        -- assume default program name is "_main"
  blocks'     <- mapM renameDeclDecls blocks  -- handle declarations
  m_contains' <- renameSubPUs m_contains      -- handle contained program units
  blocks''    <- mapM renameBlock blocks'     -- process all uses of variables
  popScope
  return (PUMain a s n blocks'' m_contains')

programUnit pu = return pu

returnBlocksEnv :: Data a => [Block (Analysis a)]
                          -> String
                          -> StateT RenameState Identity ([Block (Analysis a)], ModEnv)
returnBlocksEnv bs n = do
  bs1 <- mapM renameEntryPointDecl bs
  e0 <- initialEnv bs1
  pushScope n e0
  return (bs1, e0)

declarator :: forall a. Data a => RenamerFunc (Declarator (Analysis a))
declarator (DeclVariable a s e1 me2 me3) = do
  e1' <- renameExpDecl e1
  me2' <- transformBiM (renameExp :: RenamerFunc (Expression (Analysis a))) me2
  me3' <- transformBiM (renameExp :: RenamerFunc (Expression (Analysis a))) me3
  return $ DeclVariable a s e1' me2' me3'
declarator (DeclArray a s e1 ddAList me2 me3) = do
  e1' <- renameExpDecl e1
  ddAList' <- transformBiM (renameExp :: RenamerFunc (Expression (Analysis a))) ddAList
  me2' <- transformBiM (renameExp :: RenamerFunc (Expression (Analysis a))) me2
  me3' <- transformBiM (renameExp :: RenamerFunc (Expression (Analysis a))) me3
  return $ DeclArray a s e1' ddAList' me2' me3'

expression :: Data a => RenamerFunc (Expression (Analysis a))
expression = renameExp

--------------------------------------------------
-- Helper monadic combinators for composing into renaming
-- transformations.

-- Initial monad state.
renameState0 :: FortranVersion -> RenameState
renameState0 v = RenameState { langVersion = v
                             , intrinsics  = getVersionIntrinsics v
                             , scopeStack  = []
                             , uniqNums    = [1..]
                             , environ     = [empty]
                             , moduleMap   = empty }

-- Run the monad.
runRenamer :: State a b -> a -> (b, a)
runRenamer = runState

-- Get a freshly generated number.
getUniqNum :: Renamer Int
getUniqNum = do
  uniqNum <- gets (head . uniqNums)
  modify $ \ s -> s { uniqNums = drop 1 (uniqNums s) }
  return uniqNum

-- Concat a scope, a variable, and a freshly generated number together
-- to generate a "unique name".
uniquify :: String -> String -> Renamer String
uniquify scope var = do
  n <- getUniqNum
  return $ scope ++ "_" ++ var ++ show n

--isModule :: ProgramUnit a -> Bool
--isModule (PUModule {}) = True; isModule _             = False

isUseStatement :: Block a -> Bool
isUseStatement (BlStatement _ _ _ (StUse _ _ (ExpValue _ _ (ValVariable _)) _ _ _)) = True
isUseStatement _                                                                    = False

-- Generate an initial environment for a scope based upon any Use
-- statements in the blocks.
initialEnv :: forall a. Data a => [Block (Analysis a)] -> Renamer ModEnv
initialEnv blocks = do
  let uses = filter isUseStatement blocks
  mMap <- gets moduleMap
  modEnv <- fmap M.unions . forM uses $ \ use -> case use of
    (BlStatement _ _ _ (StUse _ _ (ExpValue _ _ (ValVariable m)) _ _ Nothing)) ->
      return $ fromMaybe empty (Named m `lookup` mMap)
    (BlStatement _ _ _ (StUse _ _ (ExpValue _ _ (ValVariable m)) _ _ (Just onlyAList)))
      | only <- aStrip onlyAList -> do
      let env = fromMaybe empty (Named m `lookup` mMap)
      -- list of (local name, original name) from USE declaration:
      let localNamePairs = flip mapMaybe only $ \ r -> case r of
            UseID _ _ v@(ExpValue _ _ ValVariable{}) -> Just (varName v, varName v)
            UseRename _ _ u v                        -> Just (varName u, varName v)
            _                                        -> Nothing
      -- create environment based on local name written in ONLY list
      -- (if applicable) and variable information found in imported
      -- mod env.
      let re = M.fromList [ (local, info) | (local, orig) <- localNamePairs
                                          , Just info     <- [M.lookup orig env] ]
      return re
    _ -> return empty

  -- Include any global names from program units defined outside of
  -- modules as well.
  let global = fromMaybe M.empty $ M.lookup NamelessMain mMap

  -- Include any mappings defined by COMMON blocks: use variable
  -- source name prefixed by name of COMMON block.
  let common = M.fromList [ (v, (v', NTVariable))
                          | CommonGroup _ _ me1 alist <- universeBi blocks :: [CommonGroup (Analysis a)]
                          , let prefix = case me1 of Just e1 -> srcName e1; _ -> ""
                          , e@(ExpValue _ _ ValVariable{}) <- universeBi (aStrip alist) :: [Expression (Analysis a)]
                          , let v = srcName e
                          , let v' = prefix ++ "_" ++ v ++ "_common" ]

  return $ M.unions [modEnv,  global, common]

-- Get the current scope name.
--getScope :: Renamer String
--getScope = gets (head . scopeStack)

-- Get the concatenated scopes.
getScopes :: Renamer String
getScopes = gets (L.intercalate "_" . reverse . scopeStack)

-- Push a scope onto the lexical stack.
pushScope :: String -> ModEnv -> Renamer ()
pushScope name env0 = modify $ \ s -> s { scopeStack = name : scopeStack s
                                        , environ    = env0 : environ s }

-- Pop a scope from the lexical stack.
popScope :: Renamer ()
popScope = modify $ \ s -> s { scopeStack = drop 1 $ scopeStack s
                             , environ    = drop 1 $ environ s }


-- Add an environment for a module to the table that keeps track of
-- modules.
addModEnv :: String -> ModEnv -> Renamer ()
addModEnv name env = modify $ \ s -> s { moduleMap = insert (Named name) env (moduleMap s) }

-- Get the current environment.
getEnv :: Renamer ModEnv
getEnv = gets (head . environ)

-- Gets an environment composed of all nested environments.
getEnvs :: Renamer ModEnv
getEnvs = M.unionsWith (curry fst) `fmap` gets environ

-- Get a mapping from the current environment if it exists.
getFromEnv :: String -> Renamer (Maybe String)
getFromEnv v = ((fst `fmap`) . lookup v) `fmap` getEnv

-- Get a mapping from the combined nested environment, if it exists.
-- If not, check if it is an intrinsic name.
getFromEnvs :: String -> Renamer (Maybe String)
getFromEnvs = fmap (fmap fst) . getFromEnvsWithType

-- Get a mapping, plus name type, from the combined nested
-- environment, if it exists.
-- If not, check if it is an intrinsic name.
getFromEnvsWithType :: String -> Renamer (Maybe (String, NameType))
getFromEnvsWithType v = do
  envs <- getEnvs
  case lookup v envs of
    Just (v', nt) -> return $ Just (v', nt)
    Nothing       -> do
      itab <- gets intrinsics
      case getIntrinsicReturnType v itab of
        Nothing -> return Nothing
        Just _  -> (Just . (,NTIntrinsic)) `fmap` addUnique v NTIntrinsic


-- To conform with Fortran specification about subprogram names:
-- search for subprogram names in all containing scopes first, then
-- search for variables in the current scope.
getFromEnvsIfSubprogram :: String -> Renamer (Maybe String)
getFromEnvsIfSubprogram v = do
  mEntry <- getFromEnvsWithType v
  case mEntry of
    Just (v', NTSubprogram) -> return $ Just v'
    Just (_, NTVariable)    -> getFromEnv v
    _                       -> return Nothing

-- Add a renaming mapping to the environment.
addToEnv :: String -> String -> NameType -> Renamer ()
addToEnv v v' nt = modify $ \ s -> s { environ = insert v (v', nt) (head (environ s)) : drop 1 (environ s) }

-- Add a unique renaming to the environment.
addUnique :: String -> NameType -> Renamer String
addUnique v nt = do
  v' <- flip uniquify v =<< getScopes
  addToEnv v v' nt
  return v'

addUnique_ :: String -> NameType -> Renamer ()
addUnique_ v nt = void (addUnique v nt)

-- This function will be invoked by occurrences of
-- declarations. First, search to see if v is a subprogram name that
-- exists in any containing scope; if so, use it. Then, search to see
-- if v is a variable in the current scope; if so, use it. Otherwise,
-- assume that it is either a new name or that it is shadowing a
-- variable, so generate a new unique name and add it to the current
-- environment.
maybeAddUnique :: String -> NameType -> Renamer String
maybeAddUnique v nt = maybe (addUnique v nt) return =<< getFromEnvsIfSubprogram v

-- If uniqueName/sourceName property is not set, then set it.
setUniqueName, setSourceName :: (Annotated f, Data a) => String -> f (Analysis a) -> f (Analysis a)
setUniqueName un x
  | a@Analysis { uniqueName = Nothing } <- getAnnotation x = setAnnotation (a { uniqueName = Just un }) x
  | otherwise                                              = x

setSourceName sn x
  | a@Analysis { sourceName = Nothing } <- getAnnotation x = setAnnotation (a { sourceName = Just sn }) x
  | otherwise                                              = x

-- Work recursively into sub-program units.
renameSubPUs :: Data a => RenamerFunc (Maybe [ProgramUnit (Analysis a)])
renameSubPUs Nothing = return Nothing
renameSubPUs (Just pus) = skimProgramUnits pus >> Just <$> mapM programUnit pus

-- Go through all program units at the same level and add their names
-- to the environment.
skimProgramUnits :: Data a => [ProgramUnit (Analysis a)] -> Renamer ()
skimProgramUnits pus = forM_ pus $ \ pu -> case pu of
  PUModule _ _ name _ _           -> addToEnv name name NTSubprogram
  PUFunction _ _ _ _ name _ _ _ _ -> addUnique_ name NTSubprogram
  PUSubroutine _ _ _ name _ _ _   -> addUnique_ name NTSubprogram
  PUMain _ _ (Just name) _ _      -> addToEnv name name NTSubprogram
  _                               -> return ()

----------
-- rename*Decl[s] functions: possibly generate new unique mappings:

-- Rename any ExpValue variables within a given value by assuming that
-- they are declarations and that they possibly require the creation
-- of new unique mappings.
renameGenericDecls :: (Data a, Data (f (Analysis a))) => RenamerFunc (f (Analysis a))
renameGenericDecls = trans renameExpDecl
  where
    trans :: (Data a, Data (f (Analysis a))) => RenamerFunc (Expression (Analysis a)) -> RenamerFunc (f (Analysis a))
    trans = transformBiM

-- Rename an ExpValue variable assuming that it is to be treated as a
-- declaration that possibly requires the creation of a new unique
-- mapping.
renameExpDecl :: Data a => RenamerFunc (Expression (Analysis a))
renameExpDecl e@(ExpValue _ _ (ValVariable v))  = flip setUniqueName (setSourceName v e) `fmap` maybeAddUnique v NTVariable
-- Intrinsics get unique names for each use.
renameExpDecl e@(ExpValue _ _ (ValIntrinsic v)) = flip setUniqueName (setSourceName v e) `fmap` addUnique v NTIntrinsic
renameExpDecl e                                 = return e

renameInterfaces :: (Data a, Data (f (Analysis a))) => RenamerFunc (f (Analysis a))
renameInterfaces = trans interface
  where
    trans :: (Data a, Data (f (Analysis a))) => RenamerFunc (Block (Analysis a)) -> RenamerFunc (f (Analysis a))
    trans = transformBiM

interface :: Data a => RenamerFunc (Block (Analysis a))
interface (BlInterface a s (Just e@(ExpValue _ _ (ValVariable v))) abst pus bs) = do
  e' <- flip setUniqueName (setSourceName v e) `fmap` maybeAddUnique v NTSubprogram
  pure $ BlInterface a s (Just e') abst pus bs
interface b = pure b

-- Handle generic-interfaces as if they were subprograms, then handle
-- other declarations, assuming they might possibly need the creation
-- of new unique mappings.
renameModDecls :: (Data a, Data (f (Analysis a))) => RenamerFunc (f (Analysis a))
renameModDecls = renameDeclDecls <=< renameInterfaces

-- Find all declarators within a value and then dive within those
-- declarators to rename any ExpValue variables, assuming they might
-- possibly need the creation of new unique mappings.
renameDeclDecls :: (Data a, Data (f (Analysis a))) => RenamerFunc (f (Analysis a))
renameDeclDecls = trans declarator
  where
    trans :: (Data a, Data (f (Analysis a))) => RenamerFunc (Declarator (Analysis a)) -> RenamerFunc (f (Analysis a))
    trans = transformBiM

-- Find all entry points within a block and then rename them, assuming
-- they might possibly need the creation of new unique mappings.
renameEntryPointDecl :: Data a => RenamerFunc (Block (Analysis a))
renameEntryPointDecl (BlStatement a s l (StEntry a' s' v mArgs mRes)) = do
  v' <- renameExpDecl v
  return (BlStatement a s l (StEntry a' s' v' mArgs mRes))
renameEntryPointDecl b = return b

-- Find all entry points within a block and then rename their result
-- variables, if applicable, assuming they might possibly need the
-- creation of new unique mappings.
renameEntryPointResultDecl :: Data a => RenamerFunc (Block (Analysis a))
renameEntryPointResultDecl (BlStatement a s l (StEntry a' s' v mArgs (Just res))) = do
  res' <- renameExpDecl res
  return (BlStatement a s l (StEntry a' s' v mArgs (Just res')))
renameEntryPointResultDecl b = return b

----------
-- Do not generate new unique mappings, instead look in outer scopes:

-- Rename an ExpValue variable, assuming that it is to be treated as a
-- reference to a previous declaration, possibly in an outer scope.
renameExp :: Data a => RenamerFunc (Expression (Analysis a))
renameExp e@(ExpValue _ _ (ValVariable v))  = maybe e (`setUniqueName` setSourceName v e) `fmap` getFromEnvs v
-- Intrinsics get unique names for each use.
renameExp e@(ExpValue _ _ (ValIntrinsic v)) = flip setUniqueName (setSourceName v e) `fmap` addUnique v NTIntrinsic
renameExp e                                 = return e

-- Rename all ExpValue variables found within the block, assuming that
-- they are to be treated as references to previous declarations,
-- possibly in an outer scope.
renameBlock :: Data a => RenamerFunc (Block (Analysis a))
renameBlock = trans expression
  where
    trans :: Data a => RenamerFunc (Expression a) -> RenamerFunc (Block a)
    trans = transformBiM -- search all expressions, bottom-up

-- Rename the components of a Use statement contained in the block.
renameUseSt :: Data a => RenamerFunc (Block (Analysis a))
renameUseSt (BlStatement a s l st@StUse{}) = BlStatement a s l <$> trans expression st
  where
    trans :: Data a => RenamerFunc (Expression a) -> RenamerFunc (Statement a)
    trans = transformBiM -- search all expressions, bottom-up
renameUseSt b = return b

--------------------------------------------------

-- Ensure second part of UseRename has the right uniqueName &
-- sourceName, since that name does not appear in our mod env, because
-- it has been given a different local name by the programmer.
cleanupUseRenames :: forall a. Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
cleanupUseRenames = transformBi (\ u -> case u :: Use (Analysis a) of
  UseRename a s e1 e2@(ExpValue _ _ (ValVariable v)) -> UseRename a s e1 $ setUniqueName (varName e1) (setSourceName v e2)
  _                                                  -> u)




-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

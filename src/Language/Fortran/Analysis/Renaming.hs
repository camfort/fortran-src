{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- |
-- Analyse variables/function names and produce unique names that can
-- be used to replace the original names while maintaining program
-- equivalence (a.k.a. alpha-conversion). The advantage of the unique
-- names is that scoping issues can be ignored when doing further
-- analysis.

module Language.Fortran.Analysis.Renaming
  ( analyseRenames, rename, extractNameMap, renameAndStrip, unrename, underRenaming, NameMap )
where

import Language.Fortran.AST hiding (fromList)
import Language.Fortran.Util.Position
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Types

import Prelude hiding (lookup)
import Data.Maybe (maybe, fromMaybe)
import qualified Data.List as L
import Data.Map (findWithDefault, insert, union, empty, lookup, member, Map, fromList)
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Tuple

import Text.PrettyPrint.GenericPretty

--------------------------------------------------

type NameMap       = Map String String
type Env           = Map String String
type ModuleMap     = Map ProgramUnitName Env

type Renamer a     = State RenameState a -- the monad.
data RenameState   = RenameState { scopeStack :: [String]
                                 , uniqNums   :: [Int]
                                 , environ    :: [Env]
                                 , nameMap    :: NameMap
                                 , moduleMap  :: ModuleMap }
  deriving (Show, Eq)
type RenamerFunc t = t -> Renamer t

--------------------------------------------------
-- Main interface functions.

-- | Annotate unique names for variable and function declarations and uses.
analyseRenames :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseRenames pf = pf'
  where
    (pf', _) = runRenamer (descPU programUnit pf) renameState0

    descPU :: Data a => RenamerFunc (ProgramUnit a) -> RenamerFunc (ProgramFile a)
    descPU = descendBiM -- work from the top down

-- | Take the unique name annotations and substitute them into the actual AST.
rename :: Data a => ProgramFile (Analysis a) -> (NameMap, ProgramFile (Analysis a))
rename pf = (extractNameMap pf, trPU fPU (trE fE pf))
  where
    trE :: Data a => (Expression a -> Expression a) -> ProgramFile a -> ProgramFile a
    trE = transformBi
    fE :: Data a => Expression (Analysis a) -> Expression (Analysis a)
    fE (ExpValue a s (ValVariable a' v)) = ExpValue a s . ValVariable a' $ fromMaybe v (uniqueName a)
    fE x                 = x

    trPU :: Data a => (ProgramUnit a -> ProgramUnit a) -> ProgramFile a -> ProgramFile a
    trPU = transformBi
    fPU :: Data a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
    fPU (PUFunction a s ty r n args res b subs) =
      PUFunction a s ty r (fromMaybe n (uniqueName a)) args res b subs
    fPU x                            = x

-- | Create a map of unique name => original name for each variable
-- and function in the program.
extractNameMap :: Data a => ProgramFile (Analysis a) -> NameMap
extractNameMap pf = eMap `union` puMap
  where
    eMap  = fromList [ (un, n) | ExpValue (Analysis { uniqueName = Just un }) _ (ValVariable _ n) <- uniE pf ]
    puMap = fromList [ (un, n) | PUFunction (Analysis { uniqueName = Just un }) _ _ _ n _ _ _ _   <- uniPU pf ]

    uniE :: Data a => ProgramFile a -> [Expression a]
    uniE = universeBi
    uniPU :: Data a => ProgramFile a -> [ProgramUnit a]
    uniPU = universeBi

-- | Perform the rename, stripAnalysis, and extractNameMap functions.
renameAndStrip :: Data a => ProgramFile (Analysis a) -> (NameMap, ProgramFile a)
renameAndStrip pf = fmap stripAnalysis (rename pf)

-- | Take a renamed program and its corresponding NameMap, and undo the renames.
unrename :: Data a => (NameMap, ProgramFile a) -> ProgramFile a
unrename (nm, pf) = trPU fPU . trV fV $ pf
  where
    trV :: Data a => (Value a -> Value a) -> ProgramFile a -> ProgramFile a
    trV = transformBi
    fV :: Data a => Value a -> Value a
    fV (ValVariable a v) = ValVariable a $ fromMaybe v (v `lookup` nm)
    fV x                 = x

    trPU :: Data a => (ProgramUnit a -> ProgramUnit a) -> ProgramFile a -> ProgramFile a
    trPU = transformBi
    fPU :: Data a => ProgramUnit a -> ProgramUnit a
    fPU (PUFunction a s ty r n args res b subs) = PUFunction a s ty r (fromMaybe n (n `lookup` nm)) args res b subs
    fPU x               = x

-- | Run a function with the program file placed under renaming
-- analysis, then undo the renaming in the result of the function.
underRenaming :: (Data a, Data b) => (ProgramFile (Analysis a) -> b) -> ProgramFile a -> b
underRenaming f pf = tryUnrename `descendBi` f pf'
  where
    (renameMap, pf') = rename . analyseRenames . initAnalysis $ pf
    tryUnrename n = n `fromMaybe` lookup n renameMap

--------------------------------------------------
-- Renaming transformations for pieces of the AST. Uses a language of
-- monadic combinators defined below.

programUnit :: Data a => RenamerFunc (ProgramUnit (Analysis a))
programUnit (PUModule a s name blocks m_contains) = do
  env0        <- initialEnv blocks
  pushScope name env0
  blocks'     <- mapM renameDeclDecls blocks -- handle declarations
  m_contains' <- renameSubPUs m_contains     -- handle contained program units
  getEnv >>= addModEnv name                  -- create the module environment
  popScope
  return (PUModule a s name blocks' m_contains')

programUnit (PUFunction a s ty rec name args res blocks m_contains) = do
  res'        <- mapM renameGenericDecls res  -- rename the result variable if needed
  name'       <- addUnique name               -- add unique function name to outer environment
  env0        <- initialEnv blocks
  pushScope name env0
  args'       <- mapM renameGenericDecls args -- rename arguments
  blocks'     <- mapM renameDeclDecls blocks  -- handle declarations
  m_contains' <- renameSubPUs m_contains      -- handle contained program units
  blocks''    <- renameBlocks blocks'         -- process all uses of variables
  popScope
  return . setUniqueName name' $ PUFunction a s ty rec name args' res' blocks'' m_contains'

programUnit (PUSubroutine a s rec name args blocks m_contains) = do
  name'       <- addUnique name               -- add unique subroutine name to outer environment
  env0        <- initialEnv blocks
  pushScope name env0
  args'       <- mapM renameGenericDecls args -- rename arguments
  blocks'     <- mapM renameDeclDecls blocks  -- handle declarations
  m_contains' <- renameSubPUs m_contains      -- handle contained program units
  blocks''    <- renameBlocks blocks'         -- process all uses of variables
  popScope
  return . setUniqueName name' $ PUSubroutine a s rec name args' blocks'' m_contains'

programUnit (PUMain a s n blocks m_contains) = do
  env0        <- initialEnv blocks
  pushScope (fromMaybe "_main" n) env0        -- assume default program name is "_main"
  blocks'     <- mapM renameDeclDecls blocks  -- handle declarations
  m_contains' <- renameSubPUs m_contains      -- handle contained program units
  blocks''    <- renameBlocks blocks'         -- process all uses of variables
  popScope
  return (PUMain a s n blocks'' m_contains')

programUnit pu = return pu

declarator :: Data a => RenamerFunc (Declarator (Analysis a))
declarator = renameGenericDecls

expression :: Data a => RenamerFunc (Expression (Analysis a))
expression = renameExp

--------------------------------------------------
-- Helper monadic combinators for composing into renaming
-- transformations.

-- Initial monad state.
renameState0 = RenameState { scopeStack = []
                           , uniqNums = [1..]
                           , environ = [empty]
                           , nameMap = empty
                           , moduleMap = empty }
-- Run the monad.
runRenamer m = runState m

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

isModule (PUModule {}) = True
isModule _             = False

isUseStatement (BlStatement _ _ _ (StUse _ _ (ExpValue _ _ (ValVariable _ _)) _)) = True
isUseStatement _                                                                  = False

-- Generate an initial environment for a scope based upon any Use
-- statements in the blocks.
initialEnv :: Data a => [Block a] -> Renamer Env
initialEnv blocks = do
  -- FIXME: add "use only / renaming" declarations (requires change in
  -- NameMap because it would be possible for the same program object
  -- to have two different names used by different parts of the
  -- program).
  let uses = takeWhile isUseStatement blocks
  fmap M.unions . forM uses $ \ use -> case use of
    (BlStatement _ _ _ (StUse _ _ (ExpValue _ _ (ValVariable _ m)) Nothing)) -> do
      mMap <- gets moduleMap
      return $ fromMaybe empty (Named m `lookup` mMap)

-- Get the current scope name.
getScope :: Renamer String
getScope = gets (head . scopeStack)

-- Get the concatenated scopes.
getScopes :: Renamer String
getScopes = gets (L.intercalate "_" . reverse . scopeStack)

-- Push a scope onto the lexical stack.
pushScope :: String -> Env -> Renamer ()
pushScope name env0 = modify $ \ s -> s { scopeStack = name : scopeStack s
                                        , environ    = env0 : environ s }

-- Pop a scope from the lexical stack.
popScope :: Renamer ()
popScope = modify $ \ s -> s { scopeStack = drop 1 $ scopeStack s
                             , environ    = drop 1 $ environ s }


-- Add an environment for a module to the table that keeps track of modules.
addModEnv :: String -> Env -> Renamer ()
addModEnv name env = modify $ \ s -> s { moduleMap = insert (Named name) env (moduleMap s) }

-- Get the current environment.
getEnv :: Renamer Env
getEnv = gets (head . environ)

-- Gets an environment composed of all nested environments.
getEnvs :: Renamer Env
getEnvs = M.unionsWith (curry fst) `fmap` gets environ

-- Get a mapping from the current environment if it exists.
getFromEnv :: String -> Renamer (Maybe String)
getFromEnv v = lookup v `fmap` getEnv

-- Get a mapping from the combined nested environment, if it exists.
getFromEnvs :: String -> Renamer (Maybe String)
getFromEnvs v = lookup v `fmap` getEnvs

-- Add a renaming mapping to the environment.
addToEnv :: String -> String -> Renamer ()
addToEnv v v' = modify $ \ s -> s { environ = insert v v' (head (environ s)) : drop 1 (environ s) }

-- Add a unique renaming to the environment.
addUnique :: RenamerFunc String
addUnique v = do
  v' <- flip uniquify v =<< getScopes
  addToEnv v v'
  return v'

-- If an existing renaming in the current environment exists, use it;
-- otherwise generate a new unique one and add it to the environment.
maybeAddUnique :: RenamerFunc String
maybeAddUnique v = maybe (addUnique v) return =<< getFromEnv v

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
renameExpDecl e@(ExpValue _ _ (ValVariable _ v)) = flip setUniqueName e `fmap` maybeAddUnique v
renameExpDecl e                                  = return e

-- Rename an ExpValue variable assuming that it is to be treated as a
-- references to a previous declaration, possibly in an outer scope.
renameExp :: Data a => RenamerFunc (Expression (Analysis a))
renameExp e@(ExpValue _ _ (ValVariable _ v)) = maybe e (flip setUniqueName e) `fmap` getFromEnvs v
renameExp e                                  = return e

-- Rename all the declarations found inside of the block list, then
-- using those mappings, rename the non-declaration variables found in
-- expressions.
renameBlocks :: Data a => RenamerFunc [Block (Analysis a)]
renameBlocks = mapM ((transExpr expression =<<) . transDecl declarator)
  where
    transDecl :: Data a => RenamerFunc (Declarator a) -> RenamerFunc (Block a)
    transDecl = transformBiM
    transExpr :: Data a => RenamerFunc (Expression a) -> RenamerFunc (Block a)
    transExpr = transformBiM

-- Find all declarators within a value and then dive within those
-- declarators to rename any ExpValue variables, assuming they might
-- possibly need the creation of new unique mappings.
renameDeclDecls :: (Data a, Data (f (Analysis a))) => RenamerFunc (f (Analysis a))
renameDeclDecls = trans declarator
  where
    trans :: (Data a, Data (f (Analysis a))) => RenamerFunc (Declarator (Analysis a)) -> RenamerFunc (f (Analysis a))
    trans = transformBiM

-- Work recursively into sub-program units.
renameSubPUs :: Data a => RenamerFunc (Maybe [ProgramUnit (Analysis a)])
renameSubPUs = maybe (return Nothing) ((Just `fmap`) . mapM programUnit)

-- If uniqueName property is not set, then set it.
setUniqueName :: (Annotated f, Data a) => String -> f (Analysis a) -> f (Analysis a)
setUniqueName un x
  | a@(Analysis { uniqueName = Nothing }) <- getAnnotation x = setAnnotation (a { uniqueName = Just un }) x
  | otherwise                                              = x

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

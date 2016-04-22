{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- |
-- Analyse variables/function names and produce unique names that can
-- be used to replace the original names while maintaining program
-- equivalence (a.k.a. alpha-conversion). The advantage of the unique
-- names is that scoping issues can be ignored when doing further
-- analysis.

module Forpar.Analysis.Renaming
  ( analyseRenames, rename, extractNameMap, renameAndStrip, unrename, underRenaming, NameMap )
where

import Forpar.AST
import Forpar.Util.Position
import Forpar.Analysis
import Forpar.Analysis.Types

import Prelude hiding (lookup)
import Data.Maybe (maybe, fromMaybe)
import qualified Data.List as L
import Data.Map (findWithDefault, insert, union, empty, lookup, member, Map, fromList)
import Control.Monad.State.Lazy
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Tuple

import Text.PrettyPrint.GenericPretty

--------------------------------------------------

type Renamer a = State RenameState a
type NameMap = Map String String
data RenameState = RenameState { scopeStack :: [String]
                               , uniqNums :: [Int]
                               , environ :: [Map String String]
                               , nameMap :: NameMap }
  deriving (Show, Eq)

getUniqNum :: Renamer Int
getUniqNum = do
  uniqNum <- gets (head . uniqNums)
  modify $ \ s -> s { uniqNums = drop 1 (uniqNums s) }
  return uniqNum

mungeName (Named s) = s
mungeName n = show n

renameState0 = RenameState { scopeStack = ["_"]
                           , uniqNums = [1..]
                           , environ = [empty]
                           , nameMap = empty }
runRenamer m = runState m

type RenamerFunc t = t -> Renamer t

--------------------------------------------------

-- | Annotate unique names for variable and function declarations and uses.
analyseRenames :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseRenames pf = globaliseFuncsAndSubs pf'
  where (pf', _) = runRenamer (transPU programUnit pf) renameState0
        transPU :: Data a => RenamerFunc (ProgramUnit a) -> RenamerFunc (ProgramFile a)
        transPU = transformBiM -- work from the bottom up

-- | Take the unique name annotations and substitute them into the actual AST.
rename :: Data a => ProgramFile (Analysis a) -> (NameMap, ProgramFile (Analysis a))
rename pf = (extractNameMap pf, trPU fPU (trV fV pf))
  where
    trV :: Data a => (Value a -> Value a) -> ProgramFile a -> ProgramFile a
    trV = transformBi
    fV :: Data a => Value (Analysis a) -> Value (Analysis a)
    fV (ValVariable a v) = ValVariable a $ fromMaybe v (uniqueName a)
    fV (ValArray a v)    = ValArray a $ fromMaybe v (uniqueName a)
    fV x                 = x

    trPU :: Data a => (ProgramUnit a -> ProgramUnit a) -> ProgramFile a -> ProgramFile a
    trPU = transformBi
    fPU :: Data a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
    fPU (PUFunction a s ty n args b) = PUFunction a s ty (fromMaybe n (uniqueName a)) args b
    fPU x                            = x

-- | Create a map of unique name => original name for each variable
-- and function in the program.
extractNameMap :: Data a => ProgramFile (Analysis a) -> NameMap
extractNameMap pf = vMap `union` aMap `union` puMap
  where
    vMap  = fromList [ (un, n) | ValVariable (Analysis { uniqueName = Just un }) n <- uniV pf ]
    aMap  = fromList [ (un, n) | ValArray (Analysis { uniqueName = Just un }) n <- uniV pf ]
    puMap = fromList [ (un, n) | PUFunction (Analysis { uniqueName = Just un }) _ _ n _ _ <- uniPU pf ]

    uniV :: Data a => ProgramFile a -> [Value a]
    uniV = universeBi
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
    fV (ValArray a v)    = ValArray a $ fromMaybe v (v `lookup` nm)
    fV x                 = x

    trPU :: Data a => (ProgramUnit a -> ProgramUnit a) -> ProgramFile a -> ProgramFile a
    trPU = transformBi
    fPU :: Data a => ProgramUnit a -> ProgramUnit a
    fPU (PUFunction a s ty n args b) = PUFunction a s ty (fromMaybe n (n `lookup` nm)) args b
    fPU x               = x

-- | Run a function with the program file placed under renaming
-- analysis, then undo the renaming in the result of the function.
underRenaming :: (Data a, Data b) => (ProgramFile (Analysis a) -> b) -> ProgramFile a -> b
underRenaming f pf = tryUnrename `descendBi` f pf'
  where
    (renameMap, pf') = rename . analyseRenames . initAnalysis $ pf
    tryUnrename n = n `fromMaybe` lookup n renameMap

--------------------------------------------------

-- extract name from declaration
declName (DeclVariable _ _ (ExpValue _ _ (ValVariable _ v))) = v
declName (DeclArray _ _ (ExpValue _ _ (ValArray _ v)) _) = v
declName (DeclCharVariable _ _ (ExpValue _ _ (ValVariable _ v)) _) = v
declName (DeclCharArray _ _ (ExpValue _ _ (ValVariable _ v)) _ _) = v

programUnit :: Data a => RenamerFunc (ProgramUnit (Analysis a))
programUnit pu = do
  uniqNum <- getUniqNum
  scope <- gets (head . scopeStack)

  -- uniquely name the new program unit scope
  let name = scope ++ "_" ++ mungeName (getName pu) ++ show uniqNum
  -- push the new scope onto stack
  modify $ \ s -> s { scopeStack = name:scopeStack s }

  -- the function name is declared as a variable in this scope, needs
  -- a unique name.  also get a list of parameters if applicable:
  -- parameters are declared using block statements but are also
  -- mentioned in the function declaration; these two declaration
  -- places need to match.

  -- e.g.
  --    FUNCTION f(x)
  --      INTEGER x, f
  --      f = x + 1
  --    END

  (pu', m_params) <- case pu of
    PUFunction _ _ _ n params _ -> do
      -- put function name on the renaming environment
      let fenv = [(n, name)]
      -- push the renaming environment on the stack and rename the
      -- uses of the function name.  also keep track of the renames in
      -- the nameMap.
      modify $ \ s -> s { environ = fromList fenv:environ s
                        , nameMap = fromList (map swap fenv) `union` nameMap s }

      let transV_PU :: Data a => RenamerFunc (Value a) -> RenamerFunc (ProgramUnit a)
          transV_PU = descendBiM

      pu' <- transV_PU value pu

      -- pop env stack and go back to where we were
      modify $ \ s -> s { environ = drop 1 (environ s) }
      return (pu', Just params)
    PUSubroutine _ _ _ params _ -> return (pu, Just params)
    _ -> return (pu, Nothing)

  -- search for block statement declarations within the program unit and process them
  let transBS_PU :: Data a => RenamerFunc [Block a] -> RenamerFunc (ProgramUnit a)
      transBS_PU = transformBiM

  pu'' <- transBS_PU blstmtList pu'

  -- now rename the parameters, if applicable, using the unique names
  -- determined while processing the block statements

  let uniS_PU :: Data a => ProgramUnit a -> [Statement a]
      uniS_PU = universeBi

  let f (DeclVariable _ _ (ExpValue _ _ (ValVariable (Analysis { uniqueName = Just un }) n))) = [(n, un)]
      f (DeclArray _ _ (ExpValue _ _ (ValArray (Analysis { uniqueName = Just un }) n)) _)     = [(n, un)]
      f d = []

  pu''' <- case m_params of
    Just params -> do
      -- get a list of the renames that were performed in this program unit body
      let renames = concat [ f =<< aStrip dalist | StDeclaration _ _ _ dalist <- uniS_PU pu'' ]
      -- for each parameter, apply the first rename found
      let params' = flip aMap params $ \ p -> case p of
                      ValVariable a n -> ValVariable (a { uniqueName = L.lookup n renames }) n
                      ValArray a n    -> ValArray (a { uniqueName = L.lookup n renames }) n
      -- and put it back into its place
      return $ case pu'' of
        PUFunction a s t n params b -> PUFunction a s t n params' b
        PUSubroutine a s n params b -> PUSubroutine a s n params' b
    _ -> return pu''

  -- pop the scope
  modify $ \ s -> s { scopeStack = drop 1 (scopeStack s) }

  -- set the unique name annotation of the program unit
  return $ setAnnotation ((getAnnotation pu''') { uniqueName = Just name }) pu'''

blstmtList :: Data a => RenamerFunc [Block (Analysis a)]
blstmtList st@(BlStatement _ _ _ (StDeclaration a s ty valist):_) = do
  scope <- gets (head . scopeStack)
  -- D.trace ("\n\nvalist: "++show (fmap uniqueName valist)) $ return ()
  let vs = aStrip valist
  -- create a renaming environment for the variables
  env <- flip mapM vs $ \ decl -> do
      let v = declName decl
      uniqNum <- getUniqNum
      return (v, scope ++ "_" ++ v ++ show uniqNum)
  -- push the renaming environment on the stack and rename the vars
  -- also keep track of the renames in the nameMap
  modify $ \ s -> s { environ = fromList env:environ s
                    , nameMap = fromList (map swap env) `union` nameMap s }

  let transV_BS :: Data a => RenamerFunc (Value a) -> RenamerFunc [Block a]
      transV_BS = descendBiM

  st' <- transV_BS value st
  -- pop the renaming environment
  modify $ \ s -> s { environ = drop 1 (environ s) }
  return st'
blstmtList bs = return bs

value :: Data a => RenamerFunc (Value (Analysis a))
value v@(ValVariable (Analysis { uniqueName = Just _ }) _) = return v
value v@(ValArray (Analysis { uniqueName = Just _ }) _) = return v
value (ValVariable a v) = do
  env <- gets (head . environ)
  return $ ValVariable (a { uniqueName = v `lookup` env }) v
value (ValArray a v) = do
  env <- gets (head . environ)
  return $ ValArray (a { uniqueName = v `lookup` env }) v
value v = return v

--------------------------------------------------

-- list of functions/subroutines and their uniquenames
funcsAndSubs :: Data a => ProgramFile (Analysis a) -> NameMap
funcsAndSubs pf =
  fromList $
    [ (n, un) | PUFunction (Analysis { uniqueName = Just un }) _ _ n _ _ <- uniPU_PF pf ] ++
    [ (n, un) | PUSubroutine (Analysis { uniqueName = Just un }) _ n _ _ <- uniPU_PF pf ]
  where
    uniPU_PF :: Data a => ProgramFile a -> [ProgramUnit a]
    uniPU_PF = universeBi

-- ensure that function and subroutine references use the same name globally
globaliseFuncsAndSubs :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
globaliseFuncsAndSubs pf = fst $ runRenamer (transV fV pf) renameState0
  where
    nm = funcsAndSubs pf

    transV :: Data a => RenamerFunc (Value a) -> RenamerFunc (ProgramFile a)
    transV = transformBiM -- work from the bottom up

    fV :: Data a => RenamerFunc (Value (Analysis a))
    fV (ValVariable a v) | v `member` nm = return $ ValVariable (a { uniqueName = v `lookup` nm }) v
    fV v = return v

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:

{-# LANGUAGE ScopedTypeVariables #-}
module Forpar.Analysis.Renaming where

import Forpar.AST
import Forpar.Analysis.Types

import Prelude hiding (lookup)
import Data.Maybe (maybe, fromMaybe)
import Data.Map (findWithDefault, insert, union, empty, lookup, Map, fromList)
import Control.Monad.State.Lazy
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Tuple

import Text.PrettyPrint.GenericPretty

--------------------------------------------------

import qualified Debug.Trace as D
import qualified Forpar.Parser.Fortran77 as F77 -- temp

-- testparse "test1.f"
testparse f = do
  inp <- readFile f
  return $ forparse inp f
  where
    forparse :: String -> String -> ProgramFile ()
    forparse contents f = F77.fortran77Parser contents f

--------------------------------------------------

variables f = [ v | v@ValVariable {} <- universeBi f ]

declVariables f = [ v | v@DeclVariable {} <- universeBi f ]

--------------------------------------------------

type Renamer a = State RenameState a
type NameMap = Map String String
data RenameState = RenameState { scopeStack :: [String]
                               , uniqNums :: [Int]
                               , environ :: [Map String String]
                               , nameMap :: NameMap }
  deriving (Show, Eq)

getUniqNum = do
  uniqNum <- gets (head . uniqNums)
  modify $ \ s -> s { uniqNums = drop 1 (uniqNums s) }
  return uniqNum

renameState0 = RenameState { scopeStack = ["_"]
                           , uniqNums = [1..]
                           , environ = [empty]
                           , nameMap = empty }
runRenamer m = runState m

type RenamerFunc t = t -> Renamer t

--------------------------------------------------

rename :: Data a => ProgramFile a -> (ProgramFile a, NameMap)
rename pf = (pf', nameMap s')
  where (pf', s') = runRenamer (transPU programUnit pf) renameState0

programUnit :: Data a => RenamerFunc (ProgramUnit a)
programUnit pu = do
  uniqNum <- getUniqNum
  scope <- gets (head . scopeStack)

  -- uniquely name the new program unit scope
  let name = scope ++ "_" ++ mungeName (getName pu) ++ show uniqNum
  -- push the new scope onto stack
  modify $ \ s -> s { scopeStack = name:scopeStack s }

  -- if there are parameters, find them
  let vars = case pu of PUFunction _ _ _ _ vs _ -> aStrip vs
                        PUSubroutine _ _ _ vs _ -> aStrip vs
                        _ -> []
  -- if there are parameters, create a renaming environment for them
  pu' <- if null vars then return pu else do
    env <- flip mapM vars $ \ (ValVariable _ v) -> do
             uniqNum <- getUniqNum
             return (v, name ++ "_" ++ v ++ show uniqNum)
    -- also put function name, if applicable, on the renaming environment
    let fenv = (case pu of PUFunction _ _ _ n _ _ -> [(n, name)]; _ -> []) ++ env
    -- push the renaming environment on the stack and rename the vars
    -- also keep track of the renames in the nameMap
    modify $ \ s -> s { environ = fromList fenv:environ s
                      , nameMap = fromList (map swap fenv) `union` nameMap s }
    pu' <- transV_PU value pu
    -- pop and go back to where we were
    modify $ \ s -> s { environ = drop 1 (environ s) }
    return pu'

  -- search for block statement declarations within the program unit
  pu'' <- transBS_PU blstmtList pu'

  -- pop the scope
  modify $ \ s -> s { scopeStack = drop 1 (scopeStack s) }

  -- if there's a name, make it unique
  return $ setName (Named name) pu''

block :: Data a => RenamerFunc (Block a)
block b = transPU_B programUnit b

-- declList :: Data a => RenamerFunc [Declarator a]
-- declList dl = undefined

blstmtList :: Data a => RenamerFunc [Block a]
blstmtList st@(BlStatement _ _ _ (StDeclaration a s ty valist):_) = do
  scope <- gets (head . scopeStack)
  let vs = aStrip valist
  -- create a renaming environment for the variables
  env <- flip mapM vs $ \ (DeclVariable _ _ (ExpValue _ _ (ValVariable _ v))) -> do -- FIXME: arrays
    uniqNum <- getUniqNum
    return (v, scope ++ "_" ++ v ++ show uniqNum)
  -- push the renaming environment on the stack and rename the vars
  -- also keep track of the renames in the nameMap
  modify $ \ s -> s { environ = fromList env:environ s
                    , nameMap = fromList (map swap env) `union` nameMap s }
  st' <- transV_BS value st
  -- pop the renaming environment
  modify $ \ s -> s { environ = drop 1 (environ s) }
  return st'
blstmtList bs = return bs

value :: Data a => RenamerFunc (Value a)
value v@(ValVariable _ ('_':_)) = return v -- FIXME: hack (already renamed)
value (ValVariable a v) = do
  env <- gets (head . environ)
  return $ ValVariable a (fromMaybe v (v `lookup` env))
value v = return v

--------------------------------------------------

unrename :: Data a => (ProgramFile a, NameMap) -> ProgramFile a
unrename (pf, nm) = trPU fPU (trV fV pf)
  where
    trV :: Data a => (Value a -> Value a) -> ProgramFile a -> ProgramFile a
    trV = transformBi
    fV :: Data a => Value a -> Value a
    fV (ValVariable a v) = ValVariable a $ fromMaybe v (v `lookup` nm)
    fV x               = x

    trPU :: Data a => (ProgramUnit a -> ProgramUnit a) -> ProgramFile a -> ProgramFile a
    trPU = transformBi
    fPU :: Data a => ProgramUnit a -> ProgramUnit a
    fPU (PUFunction a s ty n args b) = PUFunction a s ty (fromMaybe n (n `lookup` nm)) args b
    fPU x               = x

--------------------------------------------------

mungeName (Named s) = s
mungeName n = show n

transPU :: Data a => RenamerFunc (ProgramUnit a) -> RenamerFunc (ProgramFile a)
transPU = transformBiM

transSS :: Data a => RenamerFunc [Statement a] -> RenamerFunc (ProgramUnit a)
transSS = descendBiM

transSS_PF :: Data a => RenamerFunc [Statement a] -> RenamerFunc (ProgramFile a)
transSS_PF = descendBiM

transPU_B :: Data a => RenamerFunc (ProgramUnit a) -> RenamerFunc (Block a)
transPU_B = descendBiM

transBS_PU :: Data a => RenamerFunc [Block a] -> RenamerFunc (ProgramUnit a)
transBS_PU = transformBiM

transV_PF :: Data a => RenamerFunc (Value a) -> RenamerFunc (ProgramFile a)
transV_PF = transformBiM

transV_PU :: Data a => RenamerFunc (Value a) -> RenamerFunc (ProgramUnit a)
transV_PU = descendBiM

transV_SS :: Data a => RenamerFunc (Value a) -> RenamerFunc [Statement a]
transV_SS = descendBiM

transV_BS :: Data a => RenamerFunc (Value a) -> RenamerFunc [Block a]
transV_BS = descendBiM

transB :: Data a => RenamerFunc (Block a) -> RenamerFunc (ProgramUnit a)
transB = descendBiM

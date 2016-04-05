{-# LANGUAGE ScopedTypeVariables #-}
module Forpar.Analysis.Types ( inferTypes
                             , ConstructType(..)
                             , BaseType(..)
                             , TypeScope(..)
                             , IDType(..)) where

import Forpar.AST

import Prelude hiding (lookup)
import Data.Map (findWithDefault, insert, empty, lookup, Map)
import Control.Monad.State.Lazy
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data

import Debug.Trace

--------------------------------------------------------------------------------
-- Type mapping definitions
--------------------------------------------------------------------------------

data ConstructType =
    CTFunction
  | CTSubroutine
  | CTVariable
  | CTArray
  | CTParameter
  deriving (Show, Eq)

data IDType = IDType
  { idVType :: Maybe BaseType
  , idCType :: Maybe ConstructType }
  deriving (Show, Eq)

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
    (PUFunction _ _ mts n _ _) -> do
      addToMappingViaFunc Global n $ updateForFunction mts
      addEntries (updateForFunction mts)
    (PUSubroutine _ _ n _ _) -> do
      addConstructToMapping Global n CTSubroutine
      addEntries (\idt -> idt { idCType = Just CTSubroutine })
    _ -> return ()
  where
    addEntries :: Data a => (IDType -> IDType) -> TypeMapping a ()
    addEntries func = do
      let statements = universeBi :: Data a => ProgramUnit a -> [Statement a]
      let entryExps = [ e | (StEntry _ _ e _) <- statements pu ]
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
  let lhsNames = [ s | StExpressionAssign _ _ (ExpSubscript _ _ (ExpValue _ _ (ValArray _ s)) _) _ <- statements pu ]
  idts <- mapM (queryIDType puName) lhsNames
  let filteredNames = map fst $ filter p $ zip lhsNames idts
  mapM_ (\n -> addConstructToMapping puName n CTFunction) filteredNames
  let lhsNames = [ s | StFunction _ _ (ExpValue _ _ (ValFunctionName s)) _ _ <- statements pu ]
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
  let arrayNames = [ s | ExpValue _ _ (ValArray _ s) <- arrayExps ]
  mapM_ (\n -> addConstructToMapping ts n CTArray) arrayNames

inferFromParameters :: Data a => TypeScope -> [ Statement a ] -> TypeMapping a ()
inferFromParameters ts paramSts = do
  let values = universeBi :: Data a => [Statement a] -> [Value a]
  let paramNames = [ s | (ValParameter s :: Value a) <- values paramSts ]
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
    expToId (ExpValue _ _ (ValVariable _ s)) = s
    expToId (ExpValue _ _ (ValArray _ s)) = s

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

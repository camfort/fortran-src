{-# LANGUAGE ScopedTypeVariables #-}
module Forpar.Analysis.Types ( inferTypes
                             , ConstructType(..)
                             , ValueType(..)
                             , TypeScope(..)
                             , IDType(..)) where

import Forpar.AST

import Prelude hiding (lookup)
import Data.Map (findWithDefault, insert, empty, lookup, Map)
import Control.Monad.State.Lazy
import Data.Generics.Uniplate.Data

import Debug.Trace

--------------------------------------------------------------------------------
-- Type mapping definitions
--------------------------------------------------------------------------------

data ValueType =
    VTInteger
  | VTReal
  | VTDoublePrecision
  | VTComplex
  | VTLogical
  | VTCharacter
  deriving (Show, Eq)

toValueType :: BaseType a -> ValueType
toValueType TypeInteger{} = VTInteger
toValueType TypeReal{} = VTReal
toValueType TypeDoublePrecision{} = VTDoublePrecision
toValueType TypeLogical{} = VTLogical
toValueType TypeComplex{} = VTComplex
toValueType TypeCharacter{} = VTCharacter

data ConstructType =
    CTFunction
  | CTSubroutine
  | CTVariable
  | CTArray
  | CTParameter
  deriving (Show, Eq)

data IDType = IDType 
  { idVType :: Maybe ValueType
  , idCType :: Maybe ConstructType }
  deriving (Show, Eq)

data TypeScope = Global | Local ProgramUnitName deriving (Ord,Eq,Show)

data TypeState = TypeState
  { tsProgramFile :: ProgramFile ()
  , tsMapping :: Map TypeScope (Map String IDType) }

type TypeMapping = State TypeState

--------------------------------------------------------------------------------
-- Monadic helpers
--------------------------------------------------------------------------------

getProgramFile :: TypeMapping (ProgramFile ())
getProgramFile = fmap tsProgramFile get

putProgramFile :: ProgramFile () -> TypeMapping ()
putProgramFile pf = get >>= (\ts -> put $ ts { tsProgramFile = pf })

getMapping :: TypeMapping (Map TypeScope (Map String IDType))
getMapping = fmap tsMapping get

queryIDType :: TypeScope -> String -> TypeMapping (Maybe IDType)
queryIDType ts s = do
  mapping <- getMapping
  return $ do
    inner <- lookup ts mapping
    lookup s inner

putMapping :: Map TypeScope (Map String IDType) -> TypeMapping ()
putMapping mapping = get >>= (\ts -> put $ ts { tsMapping = mapping })

addToMapping :: TypeScope -> String -> ValueType -> ConstructType -> TypeMapping ()
addToMapping ts s vt ct = addToMappingViaFunc ts s typeFunction
  where
    typeFunction idt = idt { idVType = Just vt, idCType = Just ct }

addValueToMapping :: TypeScope -> String -> ValueType -> TypeMapping ()
addValueToMapping ts s vt = addToMappingViaFunc ts s typeFunction
  where
    typeFunction idt = idt { idVType = Just vt }

addConstructToMapping :: TypeScope -> String -> ConstructType -> TypeMapping ()
addConstructToMapping ts s ct = addToMappingViaFunc ts s typeFunction
  where
    typeFunction idt = idt { idCType = Just ct }

addToMappingViaFunc :: TypeScope -> String -> (IDType -> IDType) -> TypeMapping ()
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

inferTypes :: ProgramFile () -> Map TypeScope (Map String IDType)
inferTypes pf = tsMapping . execState (inferGlobal >> inferLocal) $ state
  where
    state = TypeState pf empty

inferGlobal :: TypeMapping ()
inferGlobal = do
  (ProgramFile comAndPus _) <- getProgramFile
  let pus = map snd comAndPus
  mapM_ inferSubprograms pus

inferSubprograms :: ProgramUnit () -> TypeMapping ()
inferSubprograms pu =
  case pu of
    (PUFunction _ _ mbt n _ _) -> do
      addToMappingViaFunc Global n $ updateForFunction $ fmap toValueType mbt
      addEntries (updateForFunction $ fmap toValueType mbt)
    (PUSubroutine _ _ n _ _) -> do
      addConstructToMapping Global n CTSubroutine
      addEntries (\idt -> idt { idCType = Just CTSubroutine })
    _ -> return ()
  where
    addEntries func = do
      let entryExps = [ e | StEntry () _ e _<- universeBi pu ]
      let entryNames = [ n | n :: String <- universeBi entryExps ]
      mapM_ (\n' -> addToMappingViaFunc Global n' func) entryNames
    updateForFunction mvt it = it { idCType = Just CTFunction
                                  , idVType = mvt }

inferLocal :: TypeMapping ()
inferLocal = do
  (ProgramFile comAndPus _) <- getProgramFile
  let pus = map snd comAndPus
  mapM_ inferInProgramFile pus

inferInProgramFile :: ProgramUnit () -> TypeMapping ()
inferInProgramFile pu = do
  let declPairs = [ (bt, vars) | (StDeclaration _ _ bt (AList () _ vars)) <- universeBi pu ] 
  inferFromDeclarations puName declPairs
  let paramSts = [ paramSt | paramSt@StParameter{} <- universeBi pu]
  inferFromParameters puName paramSts
  let dimSts = [ dim | dim@StDimension{} <- universeBi pu]
  inferFromDimensions puName dimSts
  inferFromFuncStatements pu
  where
    puName = Local $ getName pu

inferFromFuncStatements :: ProgramUnit () -> TypeMapping ()
inferFromFuncStatements pu = do
  let lhsNames = [ s | StExpressionAssign () _ (ExpSubscript () _ (ExpValue () _ (ValArray s)) _) _ <- universeBi pu ]
  idts <- mapM (queryIDType puName) lhsNames
  let filteredNames = map fst $ filter p $ zip lhsNames idts
  mapM_ (\n -> addConstructToMapping puName n CTFunction) filteredNames
  let lhsNames = [ s | StFunction () _ (ExpValue () _ (ValFunctionName s)) _ _ <- universeBi pu ]
  mapM_ (\n -> addConstructToMapping puName n CTFunction) lhsNames
  where
    puName = Local $ getName pu
    -- Predicate makes sure Dimension or Type statements did not register
    -- this Array seeming LHS value as an Array. In which case it can only
    -- be a Function Statement.
    p (_, Nothing) = True
    p (_, (Just (IDType _ Nothing))) = True
    p _ = False

inferFromDimensions :: TypeScope -> [ Statement () ] -> TypeMapping ()
inferFromDimensions ts dimSts = do
  let arrayExps = [ exp | DeclArray () _ exp _ <- universeBi dimSts]
  let arrayNames = [ s | ExpValue () _ (ValArray s) <- arrayExps ]
  mapM_ (\n -> addConstructToMapping ts n CTArray) arrayNames

inferFromParameters :: TypeScope -> [ Statement () ] -> TypeMapping ()
inferFromParameters ts paramSts = do
  let paramNames = [ s | (ValParameter s :: Value ()) <- universeBi paramSts ]
  mapM_ (\n -> addConstructToMapping ts n CTParameter) paramNames

inferFromDeclarations :: TypeScope -> [ (BaseType (), [ Declarator () ]) ] -> TypeMapping ()
inferFromDeclarations _ [ ] = return ()
inferFromDeclarations ts ((bt, decls):ds) = do
  addDecls decls
  inferFromDeclarations ts ds
  where
    vt = toValueType bt
    addDecls [ ] = return ()
    addDecls (d':ds') = do
      case d' of
        DeclArray _ _ e _ -> addToMapping ts (expToId e) vt CTArray
        DeclCharArray _ _ e _ _ -> addToMapping ts (expToId e) vt CTArray
        -- Decl variables might also be functions or arrays qualified by
        -- later specifications.
        DeclVariable _ _ e -> addValueToMapping ts (expToId e) vt
        DeclCharVariable _ _ e _ -> addValueToMapping ts (expToId e) vt
      addDecls ds'
    expToId (ExpValue _ _ (ValVariable s)) = s
    expToId (ExpValue _ _ (ValArray s)) = s

--------------------------------------------------------------------------------
-- Utility methods
--------------------------------------------------------------------------------

genImplicitMapping :: ProgramUnit () -> (Char -> Maybe ValueType)
genImplicitMapping pu
  | null impSts = globalImplicitMapping -- Apply default type mappings
  | containsNone impSts = const Nothing -- If IMPLICIT NONE is in
  | otherwise = -- Try all possible mappings.
    tryImps $ map impElToFun impPairs
  where
    impSts = [ imp | imp@(StImplicit () _ _) <- universeBi pu ]
    impPairs = join . join . map couple $ [ (toValueType bt,xs) | (ImpList () _ bt (AList () _ xs)) <- universeBi pu ]
    couple (a, []) = []
    couple (a, x:xs) = [ (a,x) ] : couple (a, xs)
    containsNone imps = length [ x | x@(StImplicit _ _ Nothing) <- impSts ] == 1
    impElToFun (vt, ImpCharacter _ _ c) c' = if c' == head c then Just vt else Nothing
    impElToFun (vt, ImpRange _ _ c1 c2) c' = if c' >= head c1 && c' <= head c2 then Just vt else Nothing
    tryImps [] c = Nothing
    tryImps (x:xs) c =
      case x c of
        Just vt -> Just vt
        Nothing -> tryImps xs c

globalImplicitMapping :: Char -> Maybe ValueType
globalImplicitMapping c
  | c `elem` "ijklmn" = Just VTInteger
  | otherwise = Just VTReal

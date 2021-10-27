{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Analysis.Types.FortranVars where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Value

import           GHC.Generics
import           Data.Data
import           Control.DeepSeq

import qualified Data.Map                       as Map
import           Data.Map                       ( Map )

data FVType
  = TInteger Int
  | TReal Int
  | TComplex Int
  | TLogical Int
  | TByte Int
  | TCharacter () Int -- TODO
  | TArray FVType (Maybe ()) -- TODO
  | TCustom String
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ExpVal
  = Int     Int
  | Real    Double
  | Str     String
  | Logical Bool
  | Boz     String
  deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
  deriving anyclass (NFData)

type SymbolTable = Map Name SymbolTableEntry

data SymbolTableEntry
  = SParameter { parType :: FVType , parVal :: ExpVal }
  | SVariable { varType :: FVType , varLoc :: Location }
  | SDummy { dumType :: FVType }
  | SExternal {extType :: FVType }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

type Location = (MemoryBlockName, Offset)
type MemoryBlockName = Name
type Offset = Int

st :: Map Name FValScalar -> Map Name IDType -> Map Name SymbolTableEntry
st mc = Map.mapWithKey (st1 mc)

st1 :: Map Name FValScalar -> Name -> IDType -> SymbolTableEntry
st1 mc v idt =
    case idCType idt of
      Just CTParameter ->
        case Map.lookup v mc of
          Nothing -> error "constant without an entry in constant value env"
          Just sv ->
              let fvv = scalarValueToFortranVarsValue sv
               in SParameter fvt fvv
      _ -> SVariable fvt (v, 0) -- TODO loc???
  where
    fvt =
        case idVType idt of
          Nothing -> error "typeenv stored a var with no scalar type"
          Just st -> scalarTypeToFortranVarsType st

scalarTypeToFortranVarsType :: FTypeScalar -> FVType
scalarTypeToFortranVarsType = \case
  FTypeScalarInt     k -> TInteger $ prettyKindInt k
  FTypeScalarReal    k -> TReal undefined
  FTypeScalarComplex k -> TComplex undefined
  FTypeScalarLogical k -> TLogical $ prettyKindInt k
  FTypeScalarChar    k -> TCharacter () undefined
  FTypeScalarCustom  s -> TCustom s

scalarValueToFortranVarsValue :: FValScalar -> ExpVal
scalarValueToFortranVarsValue = \case
  FValScalarInt (FValInt _ i) -> Int $ fromIntegral i

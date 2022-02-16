{-# LANGUAGE ConstraintKinds #-}

module Language.Fortran.Analysis.Types.Internal where

import           Language.Fortran.AST
import           Language.Fortran.Repr.Value.Scalar
import           Language.Fortran.Repr.Eval ( Op )
import           Language.Fortran.Repr.Value ( FVal )
import           Language.Fortran.Analysis
import           Language.Fortran.Version
import           Language.Fortran.Util.Position
import           Language.Fortran.Intrinsics

import           Control.Monad.State.Strict
import           Control.Monad.Reader

import           Data.Map ( Map )

-- | Concrete monad for type analysis work
type Infer a = StateT InferState (Reader InferConfig) a

-- | Abstract monad for type analysis work
type MonadInfer m = (MonadState InferState m, MonadReader InferConfig m)

-- | Type analysis environment.
data InferConfig = InferConfig
  { inferConfigAcceptNonCharLengthAsKind :: Bool
  -- ^ How to handle declarations like @INTEGER x*8@. If true, providing a
  --   character length for a non-character data type will treat it as a kind
  --   parameter. In both cases, a warning is logged (nonstandard syntax).

  , inferConfigConstantOps :: Map Name (Op FVal)
  -- ^ Existence of and implementation for intrinsic functions to use during
  --   PARAMETER evaluation.

  , inferConfigLangVersion :: FortranVersion
  , inferConfigIntrinsics  :: IntrinsicsTable
  }

data InferState = InferState
  { environ     :: TypeEnv
  , structs     :: StructTypeEnv
  , entryPoints :: Map Name (Name, Maybe Name)
  , typeErrors  :: [TypeError]
  , constMap    :: Map Name FValScalar
  } deriving (Show)

-- | Mapping of names to type information.
type TypeEnv = Map Name IDType

-- | Information about a detected type error.
type TypeError = (String, SrcSpan)

-- | Mapping of structures to field types
type StructTypeEnv = Map Name StructMemberTypeEnv
type StructMemberTypeEnv = Map Name IDType

type InferFunc t = t -> Infer ()

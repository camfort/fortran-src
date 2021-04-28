-- | TODO: copied from fortran-vars (then edited)

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Analysis.SemanticTypes where

import           Data.Data                      ( Data )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Language.Fortran.AST           ( Kind, CharacterLen(..) )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

-- | Term type representation.
data SemType
  = STyInteger Kind
  | STyReal Kind
  | STyComplex Kind
  | STyLogical Kind
  | STyByte Kind
  | STyCharacter CharacterLen
  | STyArray SemType (Maybe Dimensions) -- ^ Nothing denotes dynamic dimensions
  | STyCustom String          -- use for F77 structures, F90 DDTs
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Binary SemType
instance Out    SemType

type Dimensions = [(Int, Int)]

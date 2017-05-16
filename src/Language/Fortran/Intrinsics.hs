{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Fortran.Intrinsics
  ( getVersionIntrinsics, getIntrinsicReturnType, getIntrinsicNames, IntrinsicType(..), IntrinsicsTable )
where

import qualified Data.Map.Strict as M
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import Data.List
import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty
import Language.Fortran.ParserMonad (FortranVersion(..))

import Language.Fortran.Analysis
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

import Debug.Trace

data IntrinsicType = ITReal | ITInteger | ITComplex | ITDouble | ITLogical | ITParam Int
  deriving (Show, Eq, Ord, Typeable, Generic)

type IntrinsicsTable = M.Map String IntrinsicType

-- | Obtain set of intrinsics that are most closely aligned with given version.
getVersionIntrinsics :: FortranVersion -> IntrinsicsTable
getVersionIntrinsics v = snd . last . filter ((<= v) . fst) . sort $ fortranVersionIntrinsics

getIntrinsicReturnType :: String -> IntrinsicsTable -> Maybe IntrinsicType
getIntrinsicReturnType = M.lookup

getIntrinsicNames :: IntrinsicsTable -> [String]
getIntrinsicNames = M.keys

fortranVersionIntrinsics =
  [ (Fortran66, fortran77intrinsics) -- FIXME: find list of original '66 intrinsics
  , (Fortran77, fortran77intrinsics)
  , (Fortran90, fortran90intrinisics) ]

-- | name => (return-unit, parameter-units)
fortran77intrinsics :: IntrinsicsTable
fortran77intrinsics = M.fromList
  [ ("abs"     , ITParam 1)
  , ("aimag"   , ITReal)
  , ("aint"    , ITReal)
  , ("anint"   , ITReal)
  , ("cmplx"   , ITComplex)
  , ("conjg"   , ITComplex)
  , ("dble"    , ITDouble)
  , ("dim"     , ITReal)
  , ("dprod"   , ITDouble)
  , ("int"     , ITInteger)
  , ("max"     , ITParam 1)
  , ("min"     , ITParam 1)
  , ("mod"     , ITParam 1)
  , ("nint"    , ITInteger)
  , ("real"    , ITReal)
  , ("sign"    , ITParam 1) ]

fortran90intrinisics :: IntrinsicsTable
fortran90intrinisics = fortran77intrinsics `M.union` M.fromList
  [ ("iabs"    , ITInteger)
  , ("dabs"    , ITDouble)
  , ("cabs"    , ITComplex)
  , ("dint"    , ITDouble)
  , ("dnint"   , ITDouble)
  , ("idnint"  , ITInteger)
  , ("ifix"    , ITInteger)
  , ("idint"   , ITInteger)
  , ("min0"    , ITInteger)
  , ("amin1"   , ITReal)
  , ("dmin1"   , ITDouble)
  , ("amin0"   , ITReal)
  , ("min1"    , ITInteger)
  , ("amod"    , ITReal)
  , ("dmod"    , ITDouble)
  , ("float"   , ITReal)
  , ("sngl"    , ITReal)
  , ("isign"   , ITInteger)
  , ("dsign"   , ITDouble)
  , ("present" , ITLogical)
  , ("sqrt"    , ITParam 1)
  , ("dsqrt"   , ITDouble)
  , ("csqrt"   , ITComplex)
  , ("exp"     , ITParam 1)
  , ("dexp"    , ITDouble)
  , ("cexp"    , ITComplex)
  , ("log"     , ITParam 1)
  , ("alog"    , ITReal)
  , ("dlog"    , ITDouble)
  , ("clog"    , ITComplex)
  , ("log10"   , ITParam 1)
  , ("alog10"  , ITReal)
  , ("dlog10"  , ITDouble)
  , ("idim"    , ITInteger)
  , ("ddim"    , ITDouble)
  , ("sin"     , ITReal)
  , ("dsin"    , ITDouble)
  , ("csin"    , ITComplex)
  , ("cos"     , ITReal)
  , ("dcos"    , ITDouble)
  , ("ccos"    , ITComplex)
  , ("tan"     , ITReal)
  , ("dtan"    , ITDouble)
  , ("asin"    , ITReal)
  , ("dasin"   , ITDouble)
  , ("acos"    , ITReal)
  , ("dacos"   , ITDouble)
  , ("atan"    , ITReal)
  , ("datan"   , ITDouble)
  , ("atan2"   , ITReal)
  , ("datan2"  , ITDouble)
  , ("sinh"    , ITReal)
  , ("dsinh"   , ITDouble)
  , ("cosh"    , ITReal)
  , ("dcosh"   , ITDouble)
  , ("tanh"    , ITReal)
  , ("dtanh"   , ITDouble)
  ]

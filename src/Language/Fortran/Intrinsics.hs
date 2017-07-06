{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Fortran.Intrinsics
  ( getVersionIntrinsics, getIntrinsicReturnType, getIntrinsicNames, getIntrinsicDefsUses, isIntrinsic
  , IntrinsicType(..), IntrinsicsTable, allIntrinsics )
where

import qualified Data.Map.Strict as M
import Data.Data
import Data.List
import GHC.Generics (Generic)
import Language.Fortran.ParserMonad (FortranVersion(..))


data IntrinsicType = ITReal | ITInteger | ITComplex | ITDouble | ITLogical | ITParam Int
  deriving (Show, Eq, Ord, Typeable, Generic)

data IntrinsicsEntry = IEntry { iType :: IntrinsicType, iDefsUses :: ([Int], [Int]) }
  deriving (Show, Eq, Ord, Typeable, Generic)

mkIEntry ty du = IEntry ty du

type IntrinsicsTable = M.Map String IntrinsicsEntry

-- Main table of Fortran intrinsics by version
fortranVersionIntrinsics =
  [ (Fortran66, fortran77intrinsics) -- FIXME: find list of original '66 intrinsics
  , (Fortran77, fortran77intrinsics)
  , (Fortran90, fortran90intrinisics) ]

-- | Obtain set of intrinsics that are most closely aligned with given version.
getVersionIntrinsics :: FortranVersion -> IntrinsicsTable
getVersionIntrinsics v = snd . last . filter ((<= v) . fst) . sort $ fortranVersionIntrinsics

getIntrinsicReturnType :: String -> IntrinsicsTable -> Maybe IntrinsicType
getIntrinsicReturnType i = fmap iType . M.lookup i

getIntrinsicDefsUses :: String -> IntrinsicsTable -> Maybe ([Int], [Int])
getIntrinsicDefsUses i = fmap iDefsUses . M.lookup i

getIntrinsicNames :: IntrinsicsTable -> [String]
getIntrinsicNames = M.keys

isIntrinsic :: String -> IntrinsicsTable -> Bool
isIntrinsic = M.member

allIntrinsics :: IntrinsicsTable
allIntrinsics = M.unions (map snd fortranVersionIntrinsics)

func1 = ([0],[1])
func2 = ([0],[1,2])
func3 = ([0],[1,2,3])
funcN = func2 -- FIXME: implement arbitrary-# parameter functions

-- | name => (return-unit, parameter-units)
fortran77intrinsics :: IntrinsicsTable
fortran77intrinsics = M.fromList
  [ ("abs"     , mkIEntry (ITParam 1)   func1)
  , ("aimag"   , mkIEntry (ITReal)      func1)
  , ("aint"    , mkIEntry (ITReal)      func1)
  , ("anint"   , mkIEntry (ITReal)      func1)
  , ("cmplx"   , mkIEntry (ITComplex)   func1)
  , ("conjg"   , mkIEntry (ITComplex)   func1)
  , ("dble"    , mkIEntry (ITDouble)    func1)
  , ("dim"     , mkIEntry (ITReal)      func1)
  , ("dprod"   , mkIEntry (ITDouble)    func1)
  , ("int"     , mkIEntry (ITInteger)   func1)
  , ("max"     , mkIEntry (ITParam 1)   funcN)
  , ("min"     , mkIEntry (ITParam 1)   funcN)
  , ("mod"     , mkIEntry (ITParam 1)   func2)
  , ("nint"    , mkIEntry (ITInteger)   func1)
  , ("real"    , mkIEntry (ITReal)      func1)
  , ("sign"    , mkIEntry (ITParam 1)   func2)
  ]

fortran90intrinisics :: IntrinsicsTable
fortran90intrinisics = fortran77intrinsics `M.union` M.fromList
  [ ("iabs"    , mkIEntry (ITInteger)   func1)
  , ("dabs"    , mkIEntry (ITDouble)    func1)
  , ("cabs"    , mkIEntry (ITComplex)   func1)
  , ("dint"    , mkIEntry (ITDouble)    func1)
  , ("dnint"   , mkIEntry (ITDouble)    func1)
  , ("idnint"  , mkIEntry (ITInteger)   func1)
  , ("ifix"    , mkIEntry (ITInteger)   func1)
  , ("idint"   , mkIEntry (ITInteger)   func1)
  , ("min0"    , mkIEntry (ITInteger)   funcN)
  , ("amin1"   , mkIEntry (ITReal)      funcN)
  , ("dmin1"   , mkIEntry (ITDouble)    funcN)
  , ("amin0"   , mkIEntry (ITReal)      funcN)
  , ("min1"    , mkIEntry (ITInteger)   funcN)
  , ("amod"    , mkIEntry (ITReal)      func2)
  , ("dmod"    , mkIEntry (ITDouble)    func2)
  , ("float"   , mkIEntry (ITReal)      func1)
  , ("sngl"    , mkIEntry (ITReal)      func1)
  , ("isign"   , mkIEntry (ITInteger)   func2)
  , ("dsign"   , mkIEntry (ITDouble)    func2)
  , ("present" , mkIEntry (ITLogical)   func1)
  , ("sqrt"    , mkIEntry (ITParam 1)   func1)
  , ("dsqrt"   , mkIEntry (ITDouble)    func1)
  , ("csqrt"   , mkIEntry (ITComplex)   func1)
  , ("exp"     , mkIEntry (ITParam 1)   func1)
  , ("dexp"    , mkIEntry (ITDouble)    func1)
  , ("cexp"    , mkIEntry (ITComplex)   func1)
  , ("log"     , mkIEntry (ITParam 1)   func1)
  , ("alog"    , mkIEntry (ITReal)      func1)
  , ("dlog"    , mkIEntry (ITDouble)    func1)
  , ("clog"    , mkIEntry (ITComplex)   func1)
  , ("log10"   , mkIEntry (ITParam 1)   func1)
  , ("alog10"  , mkIEntry (ITReal)      func1)
  , ("dlog10"  , mkIEntry (ITDouble)    func1)
  , ("idim"    , mkIEntry (ITInteger)   func2)
  , ("ddim"    , mkIEntry (ITDouble)    func2)
  , ("sin"     , mkIEntry (ITReal)      func1)
  , ("dsin"    , mkIEntry (ITDouble)    func1)
  , ("csin"    , mkIEntry (ITComplex)   func1)
  , ("cos"     , mkIEntry (ITReal)      func1)
  , ("dcos"    , mkIEntry (ITDouble)    func1)
  , ("ccos"    , mkIEntry (ITComplex)   func1)
  , ("tan"     , mkIEntry (ITReal)      func1)
  , ("dtan"    , mkIEntry (ITDouble)    func1)
  , ("asin"    , mkIEntry (ITReal)      func1)
  , ("dasin"   , mkIEntry (ITDouble)    func1)
  , ("acos"    , mkIEntry (ITReal)      func1)
  , ("dacos"   , mkIEntry (ITDouble)    func1)
  , ("atan"    , mkIEntry (ITReal)      func1)
  , ("datan"   , mkIEntry (ITDouble)    func1)
  , ("atan2"   , mkIEntry (ITReal)      func2)
  , ("datan2"  , mkIEntry (ITDouble)    func2)
  , ("sinh"    , mkIEntry (ITReal)      func1)
  , ("dsinh"   , mkIEntry (ITDouble)    func1)
  , ("cosh"    , mkIEntry (ITReal)      func1)
  , ("dcosh"   , mkIEntry (ITDouble)    func1)
  , ("tanh"    , mkIEntry (ITReal)      func1)
  , ("dtanh"   , mkIEntry (ITDouble)    func1)
  , ("modulo"  , mkIEntry (ITParam 1)   func2)
  , ("ceiling" , mkIEntry (ITParam 1)   func1)
  , ("floor"   , mkIEntry (ITParam 1)   func1)
  , ("iand"    , mkIEntry (ITInteger)   func2)
  , ("ior"     , mkIEntry (ITInteger)   func2)
  , ("ieor"    , mkIEntry (ITInteger)   func2)
  , ("iany"    , mkIEntry (ITInteger)   func2)
  , ("ibclr"   , mkIEntry (ITInteger)   func2)
  , ("ibits"   , mkIEntry (ITInteger)   func3)
  , ("ibset"   , mkIEntry (ITInteger)   func2)
  , ("ishftc"  , mkIEntry (ITInteger)   func3)
  , ("btest"   , mkIEntry (ITInteger)   func2)
  , ("not"     , mkIEntry (ITInteger)   func1)
  ]

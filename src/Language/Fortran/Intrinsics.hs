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


data IntrinsicType = ITReal | ITInteger | ITComplex | ITDouble | ITLogical | ITCharacter | ITParam Int
  deriving (Show, Eq, Ord, Typeable, Generic)

data IntrinsicsEntry = IEntry { iType :: IntrinsicType, iDefsUses :: ([Int], [Int]) }
  deriving (Show, Eq, Ord, Typeable, Generic)

mkIEntry :: IntrinsicType -> ([Int], [Int]) -> IntrinsicsEntry
mkIEntry = IEntry

type IntrinsicsTable = M.Map String IntrinsicsEntry

-- Main table of Fortran intrinsics by version
fortranVersionIntrinsics :: [(FortranVersion, IntrinsicsTable)]
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

func1 :: ([Int], [Int])
func1 = ([0],[1])
func2 :: ([Int], [Int])
func2 = ([0],[1,2])
func3 :: ([Int], [Int])
func3 = ([0],[1,2,3])
func4 :: ([Int], [Int])
func4 = ([0],[1,2,3,4])
funcN :: ([Int], [Int])
funcN = func2 -- FIXME: implement arbitrary-# parameter functions

-- | name => (return-unit, parameter-units)
-- This is an exhaustive list of intrinsics listed in 15.10 of X3.9-1978
fortran77intrinsics :: IntrinsicsTable
fortran77intrinsics = M.fromList
  [ ("int"     , mkIEntry ITInteger func1)
  , ("ifix"    , mkIEntry ITInteger func1)
  , ("idint"   , mkIEntry ITInteger func1)
  , ("real"    , mkIEntry ITReal func1)
  , ("float"   , mkIEntry ITReal func1)
  , ("sngl"    , mkIEntry ITReal func1)
  , ("dble"    , mkIEntry ITDouble func1)
  , ("cmplx"   , mkIEntry ITComplex func1)
  , ("ichar"   , mkIEntry ITInteger func1)
  , ("char"    , mkIEntry ITCharacter func1)
  , ("achar"   , mkIEntry ITCharacter func1)
  , ("aint"    , mkIEntry (ITParam 1) func1)
  , ("dint"    , mkIEntry ITDouble func1)
  , ("anint"   , mkIEntry (ITParam 1) func1)
  , ("dnint"   , mkIEntry ITDouble func1)
  , ("nint"    , mkIEntry (ITParam 1) func1)
  , ("idnint"  , mkIEntry ITDouble func1)
  , ("abs"     , mkIEntry (ITParam 1) func1)
  , ("iabs"    , mkIEntry ITInteger func1)
  , ("dabs"    , mkIEntry ITDouble func1)
  , ("cabs"    , mkIEntry ITComplex func1)
  , ("mod"     , mkIEntry (ITParam 1) func2)
  , ("amod"    , mkIEntry ITReal func2)
  , ("dmod"    , mkIEntry ITDouble func2)
  , ("sign"    , mkIEntry (ITParam 1) func2)
  , ("isign"   , mkIEntry ITInteger func2)
  , ("dsign"   , mkIEntry ITDouble func2)
  , ("dim"     , mkIEntry (ITParam 1) func2)
  , ("idim"    , mkIEntry ITInteger func2)
  , ("ddim"    , mkIEntry ITDouble func2)
  , ("dprod"   , mkIEntry ITDouble func2)
  , ("max"     , mkIEntry (ITParam 1) funcN)
  , ("max0"    , mkIEntry ITInteger funcN)
  , ("amax1"   , mkIEntry ITReal funcN)
  , ("dmax1"   , mkIEntry ITDouble funcN)
  , ("amax0"   , mkIEntry ITReal funcN)
  , ("max1"    , mkIEntry ITInteger funcN)
  , ("min"     , mkIEntry (ITParam 1) funcN)
  , ("min0"    , mkIEntry ITInteger funcN)
  , ("amin1"   , mkIEntry ITReal funcN)
  , ("dmin1"   , mkIEntry ITDouble funcN)
  , ("amin0"   , mkIEntry ITReal funcN)
  , ("min1"    , mkIEntry ITInteger funcN)
  , ("len"     , mkIEntry ITInteger func1)
  , ("index"   , mkIEntry ITInteger func2)
  , ("aimag"   , mkIEntry ITReal func1)
  , ("conjg"   , mkIEntry ITComplex func1)
  , ("sqrt"    , mkIEntry (ITParam 1) func1)
  , ("dsqrt"   , mkIEntry ITDouble func1)
  , ("csqrt"   , mkIEntry ITComplex func1)
  , ("exp"     , mkIEntry (ITParam 1) func1)
  , ("dexp"    , mkIEntry ITDouble func1)
  , ("cexp"    , mkIEntry ITComplex func1)
  , ("log"     , mkIEntry (ITParam 1) func1)
  , ("alog"    , mkIEntry ITReal func1)
  , ("dlog"    , mkIEntry ITDouble func1)
  , ("clog"    , mkIEntry ITComplex func1)
  , ("log10"   , mkIEntry (ITParam 1) func1)
  , ("alog10"  , mkIEntry ITReal func1)
  , ("dlog10"  , mkIEntry ITDouble func1)
  , ("sin"     , mkIEntry (ITParam 1) func1)
  , ("dsin"    , mkIEntry ITDouble func1)
  , ("csin"    , mkIEntry ITComplex func1)
  , ("cos"     , mkIEntry (ITParam 1) func1)
  , ("dcos"    , mkIEntry ITDouble func1)
  , ("ccos"    , mkIEntry ITComplex func1)
  , ("tan"     , mkIEntry (ITParam 1) func1)
  , ("dtan"    , mkIEntry ITDouble func1)
  , ("asin"    , mkIEntry (ITParam 1) func1)
  , ("dasin"   , mkIEntry ITDouble func1)
  , ("acos"    , mkIEntry (ITParam 1) func1)
  , ("dacos"   , mkIEntry ITDouble func1)
  , ("atan"    , mkIEntry (ITParam 1) func1)
  , ("datan"   , mkIEntry ITDouble func1)
  , ("atan2"   , mkIEntry (ITParam 1) func2)
  , ("datan2"  , mkIEntry ITDouble func2)
  , ("sinh"    , mkIEntry (ITParam 1) func1)
  , ("dsinh"   , mkIEntry ITDouble func1)
  , ("cosh"    , mkIEntry (ITParam 1) func1)
  , ("dcosh"   , mkIEntry ITDouble func1)
  , ("tanh"    , mkIEntry (ITParam 1) func1)
  , ("dtanh"   , mkIEntry ITDouble func1)
  , ("lge"     , mkIEntry ITLogical func2)
  , ("lgt"     , mkIEntry ITLogical func2)
  , ("lle"     , mkIEntry ITLogical func2)
  , ("llt"     , mkIEntry ITLogical func2)
  -- https://gcc.gnu.org/onlinedocs/gfortran/Argument-list-functions.html
  , ("%loc", mkIEntry (ITParam 1) func1)
  , ("%ref", mkIEntry (ITParam 1) func1)
  , ("%val", mkIEntry (ITParam 1) func1)
  ]

fortran90intrinisics :: IntrinsicsTable
fortran90intrinisics = fortran77intrinsics `M.union` M.fromList
  [ ("present" , mkIEntry ITLogical     func1)
  , ("modulo"  , mkIEntry (ITParam 1)   func2)
  , ("ceiling" , mkIEntry (ITParam 1)   func1)
  , ("iand"    , mkIEntry ITInteger     func2)
  , ("ior"     , mkIEntry ITInteger     func2)
  , ("ieor"    , mkIEntry ITInteger     func2)
  , ("iany"    , mkIEntry ITInteger     func2)
  , ("ibclr"   , mkIEntry ITInteger     func2)
  , ("ibits"   , mkIEntry ITInteger     func3)
  , ("ibset"   , mkIEntry ITInteger     func2)
  , ("ishftc"  , mkIEntry ITInteger     func3)
  , ("btest"   , mkIEntry ITInteger     func2)
  , ("not"     , mkIEntry ITInteger     func1)
  , ("dot_product"  , mkIEntry (ITParam 1)   func2)
  , ("matmul"       , mkIEntry (ITParam 1)   func2)
  , ("all"          , mkIEntry ITLogical     func2)
  , ("any"          , mkIEntry ITLogical     func2)
  , ("count"        , mkIEntry ITInteger     func2)
  , ("maxval"       , mkIEntry (ITParam 1)   func3)
  , ("minval"       , mkIEntry (ITParam 1)   func3)
  , ("product"      , mkIEntry (ITParam 1)   func3)
  , ("sum"          , mkIEntry (ITParam 1)   func3)
  , ("allocated"    , mkIEntry ITLogical     func1)
  , ("lbound"       , mkIEntry ITInteger     func2)
  , ("ubound"       , mkIEntry ITInteger     func2)
  , ("shape"        , mkIEntry ITInteger     func1)
  , ("size"         , mkIEntry ITInteger     func2)
  , ("merge"        , mkIEntry ITInteger     func3)
  , ("pack"         , mkIEntry (ITParam 3)   func3)
  , ("spread"       , mkIEntry (ITParam 1)   func3)
  , ("unpack"       , mkIEntry (ITParam 3)   func3)
  , ("reshape"      , mkIEntry (ITParam 1)   func4)
  , ("eoshift"      , mkIEntry (ITParam 1)   func4)
  , ("transpose"    , mkIEntry (ITParam 1)   func1)
  , ("maxloc"       , mkIEntry (ITParam 1)   func2)
  , ("minloc"       , mkIEntry (ITParam 1)   func2)
  ]

-- | Fortran version enum and tools for selecting version for a given file.

module Language.Fortran.Version
  ( FortranVersion(..)
  , QualifiedFortranVersion(..)
  , CompilerOption(..)
  , fortranVersionAliases
  , selectFortranVersion
  , deduceFortranVersion
  , hasDecStructure
  , getLanguageRevision
  , addCompilerOption
  , makeQualifiedVersion
  , getCompilerOptions
  ) where

import           Data.Char (toLower)
import           Data.List (isInfixOf, isSuffixOf, find)

import Data.Data    (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Text.PrettyPrint.GenericPretty (Out)

-- | The Fortran specification version used (or relevant to its context).
--
-- The constructor ordering is important, since it's used for the Ord instance
-- (which is used extensively for pretty printing).
data FortranVersion = Fortran66
                    | Fortran77         -- ^ fairly close to FORTRAN 77 standard
                    | Fortran77Extended -- ^ F77 with some extensions
                    | Fortran77Legacy   -- ^ F77 with most extensions
                    | Fortran90
                    | Fortran95
                    | Fortran2003
                    | Fortran2008
                    deriving (Ord, Eq, Data, Typeable, Generic)

data CompilerOption = DecStructure -- | represents -fdec-structure (gfortran, DEC extensions), also supported in intel fortran
                    deriving (Show, Ord, Eq, Data, Typeable, Generic)

data QualifiedFortranVersion = VanillaVersion FortranVersion
                             | QualifiedVersion FortranVersion [CompilerOption]
                             deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- Extract the base Fortran version
getLanguageRevision :: QualifiedFortranVersion -> FortranVersion
getLanguageRevision (VanillaVersion v) = v
getLanguageRevision (QualifiedVersion v _) = v

getCompilerOptions :: QualifiedFortranVersion -> [CompilerOption]
getCompilerOptions (VanillaVersion _) = []
getCompilerOptions (QualifiedVersion _ opts) = opts
-- Check if a specific compiler option is enabled
hasCompilerOption :: CompilerOption -> QualifiedFortranVersion -> Bool
hasCompilerOption _ (VanillaVersion _) = False
hasCompilerOption opt (QualifiedVersion _ opts) = opt `elem` opts

-- Add a compiler option to a QualifiedFortranVersion
addCompilerOption :: CompilerOption -> QualifiedFortranVersion -> QualifiedFortranVersion
addCompilerOption opt (VanillaVersion v) = QualifiedVersion v [opt]
addCompilerOption opt (QualifiedVersion v opts)
  | opt `elem` opts = QualifiedVersion v opts  -- Avoid duplicates?
  | otherwise = QualifiedVersion v (opt : opts)

-- | Check for Fortran77Legacy language revision or DecStructure compiler option
hasDecStructure :: QualifiedFortranVersion -> Bool
hasDecStructure qfv = getLanguageRevision  qfv == Fortran77Legacy || hasCompilerOption DecStructure qfv

makeQualifiedVersion :: FortranVersion -> [CompilerOption] -> QualifiedFortranVersion
makeQualifiedVersion version [] = VanillaVersion version
makeQualifiedVersion version opts = QualifiedVersion version opts

instance Show FortranVersion where
  show Fortran66         = "Fortran 66"
  show Fortran77         = "Fortran 77"
  show Fortran77Extended = "Fortran 77 Extended"
  show Fortran77Legacy   = "Fortran 77 Legacy"
  show Fortran90         = "Fortran 90"
  show Fortran95         = "Fortran 95"
  show Fortran2003       = "Fortran 2003"
  show Fortran2008       = "Fortran 2008"

instance Out    FortranVersion
instance NFData FortranVersion

fortranVersionAliases :: [(String, FortranVersion)]
fortranVersionAliases = [ ("66" , Fortran66)
                        , ("77e", Fortran77Extended)
                        , ("77l", Fortran77Legacy)
                        , ("77" , Fortran77)
                        , ("90" , Fortran90)
                        , ("95" , Fortran95)
                        , ("03" , Fortran2003)
                        , ("08" , Fortran2008) ]

selectFortranVersion :: String -> Maybe FortranVersion
selectFortranVersion alias = snd <$> find (\ entry -> fst entry `isInfixOf` map toLower alias) fortranVersionAliases

-- | Deduce the 'FortranVersion' from a 'FilePath' using extension.
--
-- Defaults to Fortran 90 if suffix is unrecognized.
deduceFortranVersion :: FilePath -> FortranVersion
deduceFortranVersion path
  | isExtensionOf ".f"      = Fortran77Legacy
  | isExtensionOf ".for"    = Fortran77Legacy
  | isExtensionOf ".fpp"    = Fortran77Legacy
  | isExtensionOf ".ftn"    = Fortran77Legacy
  | isExtensionOf ".f90"    = Fortran90
  | isExtensionOf ".f95"    = Fortran95
  | isExtensionOf ".f03"    = Fortran2003
  | isExtensionOf ".f2003"  = Fortran2003
  | isExtensionOf ".f08"    = Fortran2008
  | isExtensionOf ".f2008"  = Fortran2008
  | otherwise               = Fortran90         -- unrecognized, default to F90
  where
    isExtensionOf = flip isSuffixOf $ map toLower path

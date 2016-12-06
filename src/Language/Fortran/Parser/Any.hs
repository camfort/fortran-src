module Language.Fortran.Parser.Any where

import Language.Fortran.AST
import Language.Fortran.ParserMonad (FortranVersion(..))
import Language.Fortran.Parser.Fortran66 (fortran66Parser)
import Language.Fortran.Parser.Fortran77 (fortran77Parser, extended77Parser)
import Language.Fortran.Parser.Fortran90 (fortran90Parser)
import Language.Fortran.Parser.Fortran95Experimental (fortran95Parser)

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (isSuffixOf)

deduceVersion :: String -> FortranVersion
deduceVersion path
  | isExtensionOf ".f"      = Fortran77Extended
  | isExtensionOf ".for"    = Fortran77
  | isExtensionOf ".fpp"    = Fortran77
  | isExtensionOf ".ftn"    = Fortran77
  | isExtensionOf ".f90"    = Fortran90
  | isExtensionOf ".f95"    = Fortran95
  | isExtensionOf ".f03"    = Fortran2003
  | isExtensionOf ".f2003"  = Fortran2003
  | isExtensionOf ".f08"    = Fortran2008
  | isExtensionOf ".f2008"  = Fortran2008
  | otherwise               = Fortran90 -- default
  where
    isExtensionOf = flip isSuffixOf $ map toLower path

type Parser = B.ByteString -> String -> ProgramFile A0
parserVersions :: [(FortranVersion, Parser)]
parserVersions =
  [ (Fortran66, fortran66Parser)
  , (Fortran77, fortran77Parser)
  , (Fortran77Extended, extended77Parser)
  , (Fortran90, fortran90Parser)
  , (Fortran95, fortran95Parser) ]

fortranParser :: B.ByteString -> String -> ProgramFile ()
fortranParser contents filename = do
   let Just parserF = lookup (deduceVersion filename) parserVersions
   parserF contents filename

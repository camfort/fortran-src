{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Parser.Any where

import Language.Fortran.AST
import Language.Fortran.Util.ModFile
import Language.Fortran.Version (FortranVersion(..), deduceFortranVersion)
import Language.Fortran.ParserMonad (ParseErrorSimple(..), fromParseResult)

import Language.Fortran.Parser.Fortran66 ( fortran66Parser, fortran66ParserWithModFiles )
import Language.Fortran.Parser.Fortran77 ( fortran77Parser, fortran77ParserWithModFiles
                                         , extended77Parser, extended77ParserWithModFiles
                                         , legacy77Parser, legacy77ParserWithModFiles )
import Language.Fortran.Parser.Fortran90 ( fortran90Parser, fortran90ParserWithModFiles )
import Language.Fortran.Parser.Fortran95 ( fortran95Parser, fortran95ParserWithModFiles )
import Language.Fortran.Parser.Fortran2003 ( fortran2003Parser, fortran2003ParserWithModFiles )

import qualified Data.ByteString.Char8 as B

type Parser = B.ByteString -> String -> Either ParseErrorSimple (ProgramFile A0)
parserVersions :: FortranVersion -> Parser
parserVersions = \case
  Fortran66         -> fromParseResult `after` fortran66Parser
  Fortran77         -> fromParseResult `after` fortran77Parser
  Fortran77Extended -> fromParseResult `after` extended77Parser
  Fortran77Legacy   -> fromParseResult `after` legacy77Parser
  Fortran90         -> fromParseResult `after` fortran90Parser
  Fortran95         -> fromParseResult `after` fortran95Parser
  Fortran2003       -> fromParseResult `after` fortran2003Parser
  _                 -> error "no parser available for requested Fortran version"

type ParserWithModFiles = ModFiles -> B.ByteString -> String -> Either ParseErrorSimple (ProgramFile A0)
parserWithModFilesVersions :: FortranVersion -> ParserWithModFiles
parserWithModFilesVersions = \case
  Fortran66         -> helper fortran66ParserWithModFiles
  Fortran77         -> helper fortran77ParserWithModFiles
  Fortran77Extended -> helper extended77ParserWithModFiles
  Fortran77Legacy   -> helper legacy77ParserWithModFiles
  Fortran90         -> helper fortran90ParserWithModFiles
  Fortran95         -> helper fortran95ParserWithModFiles
  Fortran2003       -> helper fortran2003ParserWithModFiles
  _                 -> error "no parser available for requested Fortran version"
  where
    helper parser m s = fromParseResult . parser m s

after :: (b -> c) -> (t -> a -> b) -> t -> a -> c
after g f x = g . f x

-- | Deduce the type of parser from the filename and parse the
-- contents of the file.
fortranParser :: Parser
fortranParser contents filename =
   let parserF = parserVersions (deduceFortranVersion filename)
    in parserF contents filename

-- | Deduce the type of parser from the filename and parse the
-- contents of the file, within the context of given "mod files".
fortranParserWithModFiles :: ParserWithModFiles
fortranParserWithModFiles mods contents filename =
   let parserF = parserWithModFilesVersions (deduceFortranVersion filename)
    in parserF mods contents filename

-- | Given a FortranVersion, parse the contents of the file.
fortranParserWithVersion :: FortranVersion -> Parser
fortranParserWithVersion v contents filename =
   let parserF = parserVersions v
    in parserF contents filename

-- | Given a FortranVersion, parse the contents of the file, within
-- the context of given "mod files".
fortranParserWithModFilesAndVersion :: FortranVersion -> ParserWithModFiles
fortranParserWithModFilesAndVersion v mods contents filename =
   let parserF = parserWithModFilesVersions v
    in parserF mods contents filename

-- | TODO. Base helper function.
--fortranParserWithModFilesAndVersionAndTrans
--    :: FortranVersion -> ModFiles

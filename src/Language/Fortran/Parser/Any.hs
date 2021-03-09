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

-- TODO: rewrite in lambda case style?
type Parser = B.ByteString -> String -> Either ParseErrorSimple (ProgramFile A0)
parserVersions :: [(FortranVersion, Parser)]
parserVersions =
  [ (Fortran66, fromParseResult `after` fortran66Parser)
  , (Fortran77, fromParseResult `after` fortran77Parser)
  , (Fortran77Extended, fromParseResult `after` extended77Parser)
  , (Fortran77Legacy, fromParseResult `after` legacy77Parser)
  , (Fortran90, fromParseResult `after` fortran90Parser)
  , (Fortran95, fromParseResult `after` fortran95Parser)
  , (Fortran2003, fromParseResult `after` fortran2003Parser) ]

-- TODO: rewrite in lambda case style?
type ParserWithModFiles = ModFiles -> B.ByteString -> String -> Either ParseErrorSimple (ProgramFile A0)
parserWithModFilesVersions :: [(FortranVersion, ParserWithModFiles)]
parserWithModFilesVersions =
  [ (Fortran66, \m s -> fromParseResult . fortran66ParserWithModFiles m s)
  , (Fortran77, \m s -> fromParseResult . fortran77ParserWithModFiles m s)
  , (Fortran77Extended, \m s -> fromParseResult . extended77ParserWithModFiles m s)
  , (Fortran77Legacy, \m s -> fromParseResult . legacy77ParserWithModFiles m s)
  , (Fortran90, \m s -> fromParseResult . fortran90ParserWithModFiles m s)
  , (Fortran95, \m s -> fromParseResult . fortran95ParserWithModFiles m s)
  , (Fortran2003, \m s -> fromParseResult . fortran2003ParserWithModFiles m s) ]

after :: (b -> c) -> (t -> a -> b) -> t -> a -> c
after g f x = g . f x

-- | Deduce the type of parser from the filename and parse the
-- contents of the file.
fortranParser :: Parser
fortranParser contents filename = do
   let Just parserF = lookup (deduceFortranVersion filename) parserVersions
   parserF contents filename

-- | Deduce the type of parser from the filename and parse the
-- contents of the file, within the context of given "mod files".
fortranParserWithModFiles :: ParserWithModFiles
fortranParserWithModFiles mods contents filename = do
   let Just parserF = lookup (deduceFortranVersion filename) parserWithModFilesVersions
   parserF mods contents filename

-- | Given a FortranVersion, parse the contents of the file.
fortranParserWithVersion :: FortranVersion -> Parser
fortranParserWithVersion v contents filename = do
   let Just parserF = lookup v parserVersions
   parserF contents filename

-- | Given a FortranVersion, parse the contents of the file, within
-- the context of given "mod files".
fortranParserWithModFilesAndVersion :: FortranVersion -> ParserWithModFiles
fortranParserWithModFilesAndVersion v mods contents filename = do
   let Just parserF = lookup v parserWithModFilesVersions
   parserF mods contents filename

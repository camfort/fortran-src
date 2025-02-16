{-| Common interface to various Fortran parsers.

Each parser exports various Happy-generated functions. All export a top-level
'ProgramFile' parser. Most also export intermediate parsers e.g. for
'Statement's and 'Expression's. Fixed form and free form parsers use different
lexing schemes. And, due to headaches with Fortran's syntax, we usually want to
enforce some post-parse transformations.

This module provides a common wrapper over all that functionality. Internal
combinators are exposed to assist in manually configuring parsers.
-}

module Language.Fortran.Parser
  (
  -- * Main parsers (ProgramFile, with transformation)
    byVer, byVerWithMods
  , f66, f77, f77e, f77l, f90, f95, f2003

  -- * Main parsers without post-parse transformation
  , byVerNoTransform
  , f66NoTransform, f77NoTransform, f77eNoTransform, f77lNoTransform
    , f90NoTransform, f95NoTransform, f2003NoTransform

  -- * Other parsers
  , f90Expr
  , byVerFromFilename

  -- ** Statement
  , byVerStmt
  , f66StmtNoTransform, f77StmtNoTransform, f77eStmtNoTransform
    , f77lStmtNoTransform, f90StmtNoTransform, f95StmtNoTransform
    , f2003StmtNoTransform
  , byVerInclude
  , f66IncludesNoTransform, f77IncludesNoTransform, f77eIncludesNoTransform
  , f77lIncludesNoTransform, f90IncludesNoTransform, f95IncludesNoTransform
  , f2003IncludesNoTransform

  -- * Various combinators
  , transformAs, defaultTransformation
  , Parser, ParseErrorSimple(..)
  , StateInit, ParserMaker, makeParser, makeParserFixed, makeParserFree
  , initParseStateFixed, initParseStateFree
  , initParseStateFixedExpr, initParseStateFreeExpr
  , parseUnsafe
  , collectTokensSafe, collectTokens
  , throwIOLeft

  -- * F77 with inlined includes
  -- $f77includes
  , byVerInlineIncludes
  , f66InlineIncludes, f77InlineIncludes, f77eInlineIncludes
  , f77lInlineIncludes, f90InlineIncludes , f95InlineIncludes
  , f2003InlineIncludes
  ) where

import Language.Fortran.AST
import Language.Fortran.Parser.Monad

import qualified Language.Fortran.Parser.Fixed.Fortran66  as F66
import qualified Language.Fortran.Parser.Fixed.Fortran77  as F77
import qualified Language.Fortran.Parser.Free.Fortran90   as F90
import qualified Language.Fortran.Parser.Free.Fortran95   as F95
import qualified Language.Fortran.Parser.Free.Fortran2003 as F2003
import qualified Language.Fortran.Parser.Fixed.Lexer as Fixed
import qualified Language.Fortran.Parser.Free.Lexer  as Free
import Language.Fortran.Version
import Language.Fortran.Util.Position
import Language.Fortran.Util.ModFile
import Language.Fortran.Transformation.Monad
import qualified Language.Fortran.Transformation.Grouping                 as Trans
import qualified Language.Fortran.Transformation.Disambiguation.Function  as Trans
import qualified Language.Fortran.Transformation.Disambiguation.Intrinsic as Trans

import qualified Data.ByteString.Char8 as B
import Data.Data

import Control.Monad.State
import qualified Data.Map as Map
import           Data.Map ( Map )
import Data.Generics.Uniplate.Operations ( descendBiM )
import Control.Exception ( throwIO, Exception )
import System.FilePath ( (</>) )
import System.Directory ( doesFileExist )

-- | Our common Fortran parser type takes a filename and input, and returns
--   either a normalized error (tokens are printed) or an untransformed
--   'ProgramFile'.
type Parser a = String -> B.ByteString -> Either ParseErrorSimple a

-- Provides a way to aggregate errors that come
-- from parses with different token types
data ParseErrorSimple = ParseErrorSimple
  { errorPos      :: Position
  , errorFilename :: String
  , errorMsg      :: String
  } deriving anyclass (Exception)

instance Show ParseErrorSimple where
  show err = errorFilename err ++ ", " ++ show (errorPos err) ++ ": " ++ errorMsg err

-- | May be used to lift parse results into IO and force unwrap.
throwIOLeft :: (Exception e, MonadIO m) => Either e a -> m a
throwIOLeft = \case Right a -> pure a
                    Left  e -> liftIO $ throwIO e

--------------------------------------------------------------------------------

failUnknownVersion :: String -> FortranVersion -> a
failUnknownVersion who v = error $ who <> ": no parser available for requested version: " <> show v

byVer :: FortranVersion -> Parser (ProgramFile A0)
byVer = \case
  Fortran66         -> f66
  Fortran77         -> f77
  Fortran77Extended -> f77e
  Fortran77Legacy   -> f77l
  Fortran90         -> f90
  Fortran95         -> f95
  Fortran2003       -> f2003
  v                 -> failUnknownVersion "Language.Fortran.Parser.byVer" v

modsByVersion :: String -> ModFiles -> FortranVersion -> Parser (ProgramFile A0)
modsByVersion who mods = \case
  Fortran66         -> f66Mods mods
  Fortran77         -> f77Mods mods
  Fortran77Extended -> f77eMods mods
  Fortran77Legacy   -> f77lMods mods
  Fortran90         -> f90Mods mods
  Fortran95         -> f95Mods mods
  Fortran2003       -> f2003Mods mods
  v                 -> failUnknownVersion who v

-- parserForVersion Fortran66         input = F66.programParser input
-- parserForVersion Fortran77         input = F77.programParser input 
-- parserForVersion Fortran77Extended input = F77.programParser input 
-- parserForVersion Fortran77Legacy   input = F77.programParser input 
-- parserForVersion Fortran90         input = F90.programParser input 
-- parserForVersion Fortran95         input = F95.programParser input 
-- parserForVersion Fortran2003       input = Fortran2003.programParser input 

byVerWithMods :: ModFiles -> QualifiedFortranVersion -> Parser (ProgramFile A0)
byVerWithMods mods (VanillaVersion version)           = modsByVersion "Language.Fortran.Parser.byVerWithMods" mods version
-- todo: something special to use the options in the parser
byVerWithMods mods (QualifiedVersion version options) = 
  modsByVersion "Language.Fortran.Parser.byVerWithMods" mods version
  
  

f66, f77, f77e, f77l, f90, f95, f2003 :: Parser (ProgramFile A0)
f66   = f66Mods   []
f77   = f77Mods   []
f77e  = f77eMods  []
f77l  = f77lMods  []
f90   = f90Mods   []
f95   = f95Mods   []
f2003 = f2003Mods []

f66Mods, f77Mods, f77eMods, f77lMods, f90Mods, f95Mods, f2003Mods
    :: ModFiles -> Parser (ProgramFile A0)
f66Mods   = transformAs Fortran66         f66NoTransform
f77Mods   = transformAs Fortran77         f77NoTransform
f77eMods  = transformAs Fortran77Extended f77eNoTransform
f77lMods  = transformAs Fortran77Legacy   f77lNoTransform
f90Mods   = transformAs Fortran90         f90NoTransform
f95Mods   = transformAs Fortran95         f95NoTransform
f2003Mods = transformAs Fortran2003       f2003NoTransform

-- todo: generated parser isn't type checking with FortranVersion anymore
f66NoTransform, f77NoTransform, f77eNoTransform, f77lNoTransform,
  f90NoTransform, f95NoTransform, f2003NoTransform
    :: Parser (ProgramFile A0)
f66NoTransform   = makeParserFixed F66.programParser   (makeQualifiedVersion Fortran66         [])
f77NoTransform   = makeParserFixed F77.programParser   (makeQualifiedVersion Fortran77         [])
f77eNoTransform  = makeParserFixed F77.programParser   (makeQualifiedVersion Fortran77Extended [])
f77lNoTransform  = makeParserFixed F77.programParser   (makeQualifiedVersion Fortran77Legacy   [])
f90NoTransform   = makeParserFree  F90.programParser   (makeQualifiedVersion Fortran90         [])
f95NoTransform   = makeParserFree  F95.programParser   (makeQualifiedVersion Fortran95         [])
f2003NoTransform = makeParserFree  F2003.programParser (makeQualifiedVersion Fortran2003       [])

-- todo: generated parser isn't type checking with FortranVersion anymore
f66StmtNoTransform, f77StmtNoTransform, f77eStmtNoTransform, f77lStmtNoTransform,
  f90StmtNoTransform, f95StmtNoTransform, f2003StmtNoTransform
    :: Parser (Statement A0)
f66StmtNoTransform   = makeParserFixed F66.statementParser   (makeQualifiedVersion Fortran66         [])
f77StmtNoTransform   = makeParserFixed F77.statementParser   (makeQualifiedVersion Fortran77         [])
f77eStmtNoTransform  = makeParserFixed F77.statementParser   (makeQualifiedVersion Fortran77Extended [])
f77lStmtNoTransform  = makeParserFixed F77.statementParser   (makeQualifiedVersion Fortran77Legacy   [])
f90StmtNoTransform   = makeParserFree  F90.statementParser   (makeQualifiedVersion Fortran90         [])
f95StmtNoTransform   = makeParserFree  F95.statementParser   (makeQualifiedVersion Fortran95         [])
f2003StmtNoTransform = makeParserFree  F2003.statementParser (makeQualifiedVersion Fortran2003       [])

byVerStmt :: FortranVersion -> Parser (Statement A0)
byVerStmt = \case
  Fortran66         -> f66StmtNoTransform
  Fortran77         -> f77StmtNoTransform
  Fortran77Extended -> f77eStmtNoTransform
  Fortran77Legacy   -> f77lStmtNoTransform
  Fortran90         -> f90StmtNoTransform
  Fortran95         -> f95StmtNoTransform
  Fortran2003       -> f2003StmtNoTransform
  v                 -> failUnknownVersion "Language.Fortran.Parser.byVerStmt" v

byVerNoTransform :: FortranVersion -> Parser (ProgramFile A0)
byVerNoTransform = \case
  Fortran66         -> f66NoTransform
  Fortran77         -> f77NoTransform
  Fortran77Legacy   -> f77lNoTransform
  Fortran77Extended -> f77eNoTransform
  Fortran90         -> f90NoTransform
  Fortran95         -> f90NoTransform
  Fortran2003       -> f2003NoTransform
  v                 -> failUnknownVersion "Language.Fortran.Parser.byVerNoTransform" v

-- todo: generated parser isn't type checking with FortranVersion anymore
f90Expr :: Parser (Expression A0)
f90Expr = makeParser initParseStateFreeExpr F90.expressionParser (makeQualifiedVersion Fortran90 [])

-- | Obtain a Fortran parser by assuming the version from the filename provided.
byVerFromFilename :: Parser (ProgramFile A0)
byVerFromFilename fn = byVer v fn
  where v = deduceFortranVersion fn

--------------------------------------------------------------------------------

transformAs
    :: Data a
    => FortranVersion -> Parser (ProgramFile a) -> ModFiles
    -> Parser (ProgramFile a)
transformAs fv p mods fn bs = do
    pf <- p fn bs
    let pf' = pfSetFilename fn pf
    return $ transform pf'
  where transform = runTransform (combinedTypeEnv mods)
                                 (combinedModuleMap mods)
                                 (defaultTransformation fv)

-- | The default post-parse AST transformation for each Fortran version.
--
-- Formed by composing transformations end-to-end.
--
-- Note that some transformations are noncommutative e.g. labeled DO grouping
-- must be done before block DO grouping.
defaultTransformation :: Data a => FortranVersion -> Transform a ()
defaultTransformation = \case
  Fortran66         -> sequence_ [ Trans.groupLabeledDo
                                 , Trans.disambiguateIntrinsic
                                 , Trans.disambiguateFunction ]
  Fortran77         -> defaultTransformation Fortran66
  Fortran77Legacy   -> sequence_ [ Trans.groupLabeledDo
                                 , Trans.groupDo
                                 , Trans.disambiguateIntrinsic
                                 , Trans.disambiguateFunction ]
  _ -> defaultTransformation Fortran77Legacy

--------------------------------------------------------------------------------

type StateInit s = String -> QualifiedFortranVersion -> B.ByteString -> ParseState s
type ParserMaker ai tok a = Parse ai tok a -> QualifiedFortranVersion -> Parser a

makeParser
    :: (Loc ai, LastToken ai tok, Show tok)
    => StateInit ai -> ParserMaker ai tok a
makeParser fInitState p qfv fn = fromParseResult . runParse p . fInitState fn qfv

makeParserFixed :: ParserMaker Fixed.AlexInput Fixed.Token a
makeParserFixed = makeParser initParseStateFixed

makeParserFree :: ParserMaker Free.AlexInput Free.Token a
makeParserFree = makeParser initParseStateFree

initParseStateFixed :: StateInit Fixed.AlexInput
initParseStateFixed fn qfv bs = initParseState fn qfv ai
  where ai = Fixed.vanillaAlexInput qfv fn bs

initParseStateFree :: StateInit Free.AlexInput
initParseStateFree fn qfv bs = initParseState fn qfv ai
  where ai = Free.vanillaAlexInput qfv fn bs

-- | Initialize free-form parser state with the lexer configured for standalone
--   expression parsing.
--
-- The free-form lexer needs a non-default start code for lexing standaloe
-- expressions.
initParseStateFreeExpr :: StateInit Free.AlexInput
initParseStateFreeExpr fn qfv bs = st
  { psAlexInput = ai { Free.aiStartCode = Free.StartCode Free.scN Free.Return } }
  where
    ai = Free.vanillaAlexInput qfv fn bs
    st = initParseStateFree fn qfv bs

-- checked in generated file: 1=assn, 4=iif, 6=st
-- 6, 1, 4 seem best in order. Looks like 6 is correct.
-- TODO guesswork, relies on internal behaviour :/
initParseStateFixedExpr :: StateInit Fixed.AlexInput
initParseStateFixedExpr fn qfv bs = st
  { psAlexInput = ai { Fixed.aiStartCode = 6
                     , Fixed.aiWhiteSensitiveCharCount = 0 } }
  where
    ai = Fixed.vanillaAlexInput qfv fn bs
    st = initParseStateFixed fn qfv bs

-- | Convenience wrapper to easily use a parser unsafely.
--
-- This throws a catchable runtime IO exception, which is used in the tests.
parseUnsafe :: Parser a -> B.ByteString -> a
parseUnsafe p bs =
    case p "<unknown>" bs of
      Left err -> throwIOError $  "Language.Fortran.Parser.parseUnsafe: "
                               <> "parse error: " <> show err
      Right a -> a

-- | Helper for preparing initial parser state for the different lexers.
initParseState :: FilePath -> QualifiedFortranVersion -> ai -> ParseState ai
initParseState fn qfv ai = ParseState
  { psAlexInput = ai
  , psVersion = qfv
  , psFilename = fn
  , psParanthesesCount = ParanthesesCount 0 False
  , psContext = [ ConStart ] }

--------------------------------------------------------------------------------

{- $f77includes
The Fortran 77 parser can parse and inline includes at parse time. Parse errors
are thrown as IO exceptions.

Can be cleaned up and generalized to use for other parsers.
-}

f66InlineIncludes, f77InlineIncludes, f77eInlineIncludes, f77lInlineIncludes,
  f90InlineIncludes, f95InlineIncludes, f2003InlineIncludes
  :: [FilePath] -> ModFiles -> String -> B.ByteString -> IO (ProgramFile A0)
f66InlineIncludes   = byVerInlineIncludes (makeQualifiedVersion Fortran66         [])
f77lInlineIncludes  = byVerInlineIncludes (makeQualifiedVersion Fortran77Legacy   [])
f77eInlineIncludes  = byVerInlineIncludes (makeQualifiedVersion Fortran77Extended [])
f77InlineIncludes   = byVerInlineIncludes (makeQualifiedVersion Fortran77         [])
f90InlineIncludes   = byVerInlineIncludes (makeQualifiedVersion Fortran90         [])
f95InlineIncludes   = byVerInlineIncludes (makeQualifiedVersion Fortran95         [])
f2003InlineIncludes = byVerInlineIncludes (makeQualifiedVersion Fortran2003       [])

byVerInlineIncludes
    :: QualifiedFortranVersion -> [FilePath] -> ModFiles -> String -> B.ByteString
    -> IO (ProgramFile A0)
byVerInlineIncludes version incs mods fn bs = do
  case byVerNoTransform languageRevisison fn bs of
    Left e -> liftIO $ throwIO e
    Right pf -> do
      let pf' = pfSetFilename fn pf
      pf'' <- evalStateT (descendBiM (parserInlineIncludes version incs []) pf') Map.empty
      let pf''' = runTransform (combinedTypeEnv mods)
                                (combinedModuleMap mods)
                                (defaultTransformation languageRevisison)
                                pf''
      return pf'''
  where
    languageRevisison = getLanguageRevision version

-- Internal function to go through the includes and inline them
parserInlineIncludes
    :: QualifiedFortranVersion -> [FilePath] -> [FilePath] -> Statement A0
    -> StateT (Map String [Block A0]) IO (Statement A0)
parserInlineIncludes version dirs = go
  where
    go seen st = case st of
      StInclude a s e@(ExpValue _ _ (ValString path)) Nothing -> do
        if path `notElem` seen then do
          incMap <- get
          case Map.lookup path incMap of
            Just blocks' -> pure $ StInclude a s e (Just blocks')
            Nothing -> do
              (fullPath, incBs) <- liftIO $ readInDirs dirs path
              case byVerInclude version fullPath incBs of
                Right blocks -> do
                  blocks' <- descendBiM (go (path:seen)) blocks
                  modify (Map.insert path blocks')
                  pure $ StInclude a s e (Just blocks')
                Left err -> liftIO $ throwIO err
        else pure st
      _ -> pure st

f66IncludesNoTransform, f77IncludesNoTransform, f77eIncludesNoTransform,
  f77lIncludesNoTransform, f90IncludesNoTransform, f95IncludesNoTransform,
  f2003IncludesNoTransform
  :: Parser [Block A0]
f66IncludesNoTransform   = makeParserFixed F66.includesParser   (makeQualifiedVersion Fortran66         [])
f77IncludesNoTransform   = makeParserFixed F77.includesParser   (makeQualifiedVersion Fortran77         [])
f77eIncludesNoTransform  = makeParserFixed F77.includesParser   (makeQualifiedVersion Fortran77Extended [])
f77lIncludesNoTransform  = makeParserFixed F77.includesParser   (makeQualifiedVersion Fortran77Legacy   [])
f90IncludesNoTransform   = makeParserFree  F90.includesParser   (makeQualifiedVersion Fortran90         [])
f95IncludesNoTransform   = makeParserFree  F95.includesParser   (makeQualifiedVersion Fortran95         [])
f2003IncludesNoTransform = makeParserFree  F2003.includesParser (makeQualifiedVersion Fortran2003       [])

byVerInclude :: QualifiedFortranVersion -> Parser [Block A0]
byVerInclude qfv = 
  case getLanguageRevision qfv of
    Fortran66         -> f66IncludesNoTransform
    Fortran77         -> f77IncludesNoTransform
    Fortran77Extended -> f77eIncludesNoTransform
    Fortran77Legacy   -> f77lIncludesNoTransform
    Fortran90         -> f90IncludesNoTransform
    Fortran95         -> f95IncludesNoTransform
    Fortran2003       -> f2003IncludesNoTransform
    v                 -> failUnknownVersion "Language.Fortran.Parser.byVerInclude" v

readInDirs :: [String] -> String -> IO (String, B.ByteString)
readInDirs [] f = fail $ "cannot find file: " ++ f
readInDirs (d:ds) f = do
  let path = d</>f
  b <- doesFileExist path
  if b then
    (path,) <$> B.readFile path
  else
    readInDirs ds f

--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Generic token collection and functions (inherited from ParserMonad)
-------------------------------------------------------------------------------

collectTokens
    :: forall a b
    .  (Loc b, Tok a, LastToken b a, Show a)
    => Parse b a a -> ParseState b -> [a]
collectTokens lexer initState =
    evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: ParseState b -> Parse b a [a]
    _collectTokens st = do
      let (_token, _st) = runParseUnsafe lexer st
      if eofToken _token
      then return [_token]
      else do
        _tokens <- _collectTokens _st
        return $ _token:_tokens

collectTokensSafe
    :: forall a b
    .  (Loc b, Tok a, LastToken b a, Show a)
    => Parse b a a -> ParseState b -> Maybe [a]
collectTokensSafe lexer initState =
    evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: ParseState b -> Parse b a (Maybe [a])
    _collectTokens st =
      case unParse lexer st of
        ParseOk _token _st ->
          if eofToken _token
          then return $ Just [_token]
          else do
            _mTokens <- _collectTokens _st
            case _mTokens of
              Just _tokens -> return $ Just $ _token:_tokens
              _ -> return Nothing
        _ -> return Nothing

fromParseResult :: (Show c) => ParseResult b c a -> Either ParseErrorSimple a
fromParseResult (ParseOk a _)     = Right a
fromParseResult (ParseFailed err) =
    Left ParseErrorSimple
      { errorPos = errPos err
      , errorFilename = errFilename err
      , errorMsg = errMsg err ++ "\n" ++ tokenMsg (errLastToken err)  }

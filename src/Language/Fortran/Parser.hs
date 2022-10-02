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
  , f66NoTransform, f77NoTransform, f77eNoTransform, f77lNoTransform
    , f90NoTransform, f95NoTransform, f2003NoTransform

  -- * Other parsers
  , f90Expr

  -- ** Statement
  , byVerStmt
  , f66StmtNoTransform, f77StmtNoTransform, f77eStmtNoTransform
    , f77lStmtNoTransform, f90StmtNoTransform, f95StmtNoTransform
    , f2003StmtNoTransform

  -- * Various combinators
  , transformAs, defaultTransformation
  , Parser, ParseErrorSimple(..)
  , StateInit, ParserMaker, makeParser, makeParserFixed, makeParserFree
  , initParseStateFixed, initParseStateFree
  , initParseStateFixedExpr, initParseStateFreeExpr
  , parseUnsafe
  , collectTokensSafe, collectTokens

  -- * F77 with inlined includes
  -- $f77includes
  , f77lIncludes
  , f77lIncIncludes
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
  } deriving (Exception)

instance Show ParseErrorSimple where
  show err = errorFilename err ++ ", " ++ show (errorPos err) ++ ": " ++ errorMsg err

--------------------------------------------------------------------------------

byVer :: FortranVersion -> Parser (ProgramFile A0)
byVer = \case
  Fortran66         -> f66
  Fortran77         -> f77
  Fortran77Extended -> f77e
  Fortran77Legacy   -> f77l
  Fortran90         -> f90
  Fortran95         -> f95
  Fortran2003       -> f2003
  v                 -> error $  "Language.Fortran.Parser.byVer: "
                             <> "no parser available for requested version: "
                             <> show v

byVerWithMods :: ModFiles -> FortranVersion -> Parser (ProgramFile A0)
byVerWithMods mods = \case
  Fortran66         -> f66Mods mods
  Fortran77         -> f77Mods mods
  Fortran77Extended -> f77eMods mods
  Fortran77Legacy   -> f77lMods mods
  Fortran90         -> f90Mods mods
  Fortran95         -> f95Mods mods
  Fortran2003       -> f2003Mods mods
  v                 -> error $ "Language.Fortran.Parser.byVerWithMods: no parser available for requested version: " <> show v

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

f66NoTransform, f77NoTransform, f77eNoTransform, f77lNoTransform,
  f90NoTransform, f95NoTransform, f2003NoTransform
    :: Parser (ProgramFile A0)
f66NoTransform   = makeParserFixed F66.programParser   Fortran66
f77NoTransform   = makeParserFixed F77.programParser   Fortran77
f77eNoTransform  = makeParserFixed F77.programParser   Fortran77Extended
f77lNoTransform  = makeParserFixed F77.programParser   Fortran77Legacy
f90NoTransform   = makeParserFree  F90.programParser   Fortran90
f95NoTransform   = makeParserFree  F95.programParser   Fortran95
f2003NoTransform = makeParserFree  F2003.programParser Fortran2003

f66StmtNoTransform, f77StmtNoTransform, f77eStmtNoTransform, f77lStmtNoTransform,
  f90StmtNoTransform, f95StmtNoTransform, f2003StmtNoTransform
    :: Parser (Statement A0)
f66StmtNoTransform   = makeParserFixed F66.statementParser   Fortran66
f77StmtNoTransform   = makeParserFixed F77.statementParser   Fortran77
f77eStmtNoTransform  = makeParserFixed F77.statementParser   Fortran77Extended
f77lStmtNoTransform  = makeParserFixed F77.statementParser   Fortran77Legacy
f90StmtNoTransform   = makeParserFree  F90.statementParser   Fortran90
f95StmtNoTransform   = makeParserFree  F95.statementParser   Fortran95
f2003StmtNoTransform = makeParserFree  F2003.statementParser Fortran2003

byVerStmt :: FortranVersion -> Parser (Statement A0)
byVerStmt = \case
  Fortran66         -> f66StmtNoTransform
  Fortran77         -> f77StmtNoTransform
  Fortran77Extended -> f77eStmtNoTransform
  Fortran77Legacy   -> f77lStmtNoTransform
  Fortran90         -> f90StmtNoTransform
  Fortran95         -> f95StmtNoTransform
  Fortran2003       -> f2003StmtNoTransform
  v                 -> error $  "Language.Fortran.Parser.byVerStmt: "
                             <> "no parser available for requested version: "
                             <> show v

f90Expr :: Parser (Expression A0)
f90Expr = makeParser initParseStateFreeExpr F90.expressionParser Fortran90

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

type StateInit s = String -> FortranVersion -> B.ByteString -> ParseState s
type ParserMaker ai tok a = Parse ai tok a -> FortranVersion -> Parser a

makeParser
    :: (Loc ai, LastToken ai tok, Show tok)
    => StateInit ai -> ParserMaker ai tok a
makeParser fInitState p fv fn = fromParseResult . runParse p . fInitState fn fv

makeParserFixed :: ParserMaker Fixed.AlexInput Fixed.Token a
makeParserFixed = makeParser initParseStateFixed

makeParserFree :: ParserMaker Free.AlexInput Free.Token a
makeParserFree = makeParser initParseStateFree

initParseStateFixed :: StateInit Fixed.AlexInput
initParseStateFixed fn fv bs = initParseState fn fv ai
  where ai = Fixed.vanillaAlexInput fn fv bs

initParseStateFree :: StateInit Free.AlexInput
initParseStateFree fn fv bs = initParseState fn fv ai
  where ai = Free.vanillaAlexInput fn bs

-- | Initialize free-form parser state with the lexer configured for standalone
--   expression parsing.
--
-- The free-form lexer needs a non-default start code for lexing standaloe
-- expressions.
initParseStateFreeExpr :: StateInit Free.AlexInput
initParseStateFreeExpr fn fv bs = st
  { psAlexInput = ai { Free.aiStartCode = Free.StartCode Free.scN Free.Return } }
  where
    ai = Free.vanillaAlexInput fn bs
    st = initParseStateFree fn fv bs

-- checked in generated file: 1=assn, 4=iif, 6=st
-- 6, 1, 4 seem best in order. Looks like 6 is correct.
-- TODO guesswork, relies on internal behaviour :/
initParseStateFixedExpr :: StateInit Fixed.AlexInput
initParseStateFixedExpr fn fv bs = st
  { psAlexInput = ai { Fixed.aiStartCode = 6
                     , Fixed.aiWhiteSensitiveCharCount = 0 } }
  where
    ai = Fixed.vanillaAlexInput fn fv bs
    st = initParseStateFixed fn fv bs

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
initParseState :: FilePath -> FortranVersion -> ai -> ParseState ai
initParseState fn fv ai = ParseState
  { psAlexInput = ai
  , psVersion = fv
  , psFilename = fn
  , psParanthesesCount = ParanthesesCount 0 False
  , psContext = [ ConStart ] }

--------------------------------------------------------------------------------

{- $f77includes
The Fortran 77 parser can parse and inline includes at parse time. Parse errors
are thrown as IO exceptions.

Can be cleaned up and generalized to use for other parsers.
-}

f77lIncludes
    :: [FilePath] -> ModFiles -> String -> B.ByteString
    -> IO (ProgramFile A0)
f77lIncludes incs mods fn bs = do
    case f77lNoTransform fn bs of
      Left e -> liftIO $ throwIO e
      Right pf -> do
        let pf' = pfSetFilename fn pf
        pf'' <- evalStateT (descendBiM (f77lIncludesInline incs []) pf') Map.empty
        let pf''' = runTransform (combinedTypeEnv mods)
                                 (combinedModuleMap mods)
                                 (defaultTransformation Fortran77Legacy)
                                 pf''
        return pf'''

-- | Entry point for include files
-- 
-- We can't perform full analysis (though it might be possible to do in future)
-- but the AST is enough for certain types of analysis/refactoring
f77lIncIncludes
  :: [FilePath] -> String -> B.ByteString -> IO [Block A0]
f77lIncIncludes incs fn bs =
  case makeParserFixed F77.includesParser Fortran77Legacy fn bs of
    Left e -> liftIO $ throwIO e
    Right bls ->
      evalStateT (descendBiM (f77lIncludesInline incs []) bls) Map.empty

f77lIncludesInner :: Parser [Block A0]
f77lIncludesInner = makeParserFixed F77.includesParser Fortran77Legacy

f77lIncludesInline
    :: [FilePath] -> [FilePath] -> Statement A0
    -> StateT (Map String [Block A0]) IO (Statement A0)
f77lIncludesInline dirs seen st = case st of
  StInclude a s e@(ExpValue _ _ (ValString path)) Nothing -> do
    if notElem path seen then do
      incMap <- get
      case Map.lookup path incMap of
        Just blocks' -> pure $ StInclude a s e (Just blocks')
        Nothing -> do
          (fullPath, inc) <- liftIO $ readInDirs dirs path
          case f77lIncludesInner fullPath inc of
            Right blocks -> do
              blocks' <- descendBiM (f77lIncludesInline dirs (path:seen)) blocks
              modify (Map.insert path blocks')
              return $ StInclude a s e (Just blocks')
            Left err -> liftIO $ throwIO err
    else return st
  _ -> return st

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

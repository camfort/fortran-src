{-| Parser/lexer monad, plus common functionality and definitions. -}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Language.Fortran.Parser.Monad where

#if !MIN_VERSION_base(4,13,0)
-- Control.Monad.Fail import is redundant since GHC 8.8.1
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fail (MonadFail)
#endif

import Language.Fortran.Version
import Language.Fortran.Util.Position

import Control.Exception
import GHC.IO.Exception ( IOException(..), IOErrorType(..) )
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Typeable

-------------------------------------------------------------------------------
-- Helper datatype definitions
-------------------------------------------------------------------------------

data ParanthesesCount = ParanthesesCount
  { pcActual :: Integer
  , pcHasReached0 :: Bool }
  deriving (Show, Eq)

data Context =
    ConStart
  | ConData
  | ConImplicit
  | ConNamelist
  | ConCommon
  deriving (Show, Eq)

data ParseState a = ParseState
  { psAlexInput :: a
  , psParanthesesCount :: ParanthesesCount
  , psVersion :: FortranVersion  -- To differentiate lexing behaviour
  , psFilename :: String -- To save correct source location in AST
  , psContext :: [ Context ]
  }
  deriving (Show)

data ParseError a b = ParseError
  { errPos        :: Position
  , errLastToken  :: Maybe b
  , errFilename   :: String
  , errMsg        :: String }

instance Show b => Show (ParseError a b) where
  show err = show (errPos err) ++ ": " ++ errMsg err ++ lastTokenMsg
    where lastTokenMsg = tokenMsg (errLastToken err)

tokenMsg :: Show a => Maybe a -> String
tokenMsg (Just a) = "Last parsed token: " ++ show a ++ "."
tokenMsg Nothing = "No token had been lexed."

data ParseResult b c a = ParseOk a (ParseState b) | ParseFailed (ParseError b c)
    deriving stock (Functor)

instance (Typeable a, Typeable b, Show a, Show b) => Exception (ParseError a b)

-- Provides a way to aggregate errors that come
-- from parses with different token types
data ParseErrorSimple = ParseErrorSimple
  { errorPos      :: Position
  , errorFilename :: String
  , errorMsg      :: String
  } deriving (Exception)

instance Show ParseErrorSimple where
  show err = errorFilename err ++ ", " ++ show (errorPos err) ++ ": " ++ errorMsg err

class LastToken a b | a -> b where
  getLastToken :: (Show b) => a -> Maybe b

class Tok a where
  eofToken :: a -> Bool

-------------------------------------------------------------------------------
-- Parser Monad definition
-------------------------------------------------------------------------------

newtype Parse b c a = Parse { unParse :: ParseState b -> ParseResult b c a }

instance (Loc b, LastToken b c, Show c) => Monad (Parse b c) where
  return a = Parse $ \s -> ParseOk a s

  (Parse m) >>= f = Parse $ \s ->
    case m s of
      ParseOk a s' -> unParse (f a) s'
      ParseFailed e -> ParseFailed e

#if !MIN_VERSION_base(4,13,0)
  -- Monad(fail) was removed in GHC 8.8.1
  fail = Fail.fail
#endif

instance (Loc b, LastToken b c, Show c) => MonadFail (Parse b c) where
  fail msg = Parse $ \s -> ParseFailed ParseError
    { errPos        = (getPos . psAlexInput) s
    , errLastToken  = (getLastToken . psAlexInput) s
    , errFilename   = psFilename s
    , errMsg        = msg }

instance (Loc b, LastToken b c, Show c) => Functor (Parse b c) where
  fmap = liftM

instance (Loc b, LastToken b c, Show c) => Applicative (Parse b c) where
  pure  = return
  (<*>) = ap

instance (Loc b, LastToken b c, Show c) => MonadState (ParseState b) (Parse b c) where
  get = Parse $ \s -> ParseOk s s
  put s = Parse $ \_ -> ParseOk () s

instance (Loc b, LastToken b c, Show c) => MonadError (ParseError b c) (Parse b c) where
  throwError e = Parse $ \_ -> ParseFailed e

  (Parse m) `catchError` f = Parse $ \s ->
    case m s of
      ParseFailed e -> unParse (f e) s
      m' -> m'


runParse
    :: (Loc b, LastToken b c, Show c)
    => Parse b c a -> ParseState b -> ParseResult b c a
runParse = unParse

runParseUnsafe
    :: (Loc b, LastToken b c, Show c)
    => Parse b c a -> ParseState b -> (a, ParseState b)
runParseUnsafe lexer initState =
  case unParse lexer initState of
    ParseOk a s -> (a, s)
    ParseFailed e -> throwIOError $ show e

throwIOError :: String -> a
throwIOError s = throw
  IOError { ioe_handle      = Nothing
          , ioe_type        = UserError
          , ioe_location    = "fortran-src"
          , ioe_description = s
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing }

evalParse
    :: (Loc b, LastToken b c, Show c)
    => Parse b c a -> ParseState b -> a
evalParse m s = fst (runParseUnsafe m s)

execParse
    :: (Loc b, LastToken b c, Show c)
    => Parse b c a -> ParseState b -> ParseState b
execParse m s = snd (runParseUnsafe m s)

-------------------------------------------------------------------------------
-- Parser helper functions
-------------------------------------------------------------------------------

getVersion :: (Loc a, LastToken a b, Show b) => Parse a b FortranVersion
getVersion = do
  s <- get
  return (psVersion s)

putAlex :: (Loc a, LastToken a b, Show b) => a -> Parse a b ()
putAlex ai = do
  s <- get
  put (s { psAlexInput = ai })

getAlex :: (Loc a, LastToken a b, Show b) => Parse a b a
getAlex = do
  s <- get
  return (psAlexInput s)

topContext :: (Loc a, LastToken a b, Show b) => Parse a b Context
topContext = head . psContext <$> get

popContext :: (Loc a, LastToken a b, Show b) => Parse a b ()
popContext = modify $ \ps -> ps { psContext = tail $ psContext ps }

pushContext :: (Loc a, LastToken a b, Show b) => Context -> Parse a b ()
pushContext context = modify $ \ps -> ps { psContext = context : psContext ps }

getPosition :: (Loc a, LastToken a b, Show b) => Parse a b Position
getPosition = do
  parseState <- get
  return $ getPos $ psAlexInput parseState

getSrcSpan :: (Loc a, LastToken a b, Show b) => Position -> Parse a b SrcSpan
getSrcSpan loc1 = do
  loc2 <- getPosition
  return $ SrcSpan loc1 loc2

getParanthesesCount :: (Loc a, LastToken a b, Show b) => Parse a b ParanthesesCount
getParanthesesCount = psParanthesesCount <$> get

resetPar :: (Loc a, LastToken a b, Show b) => Parse a b ()
resetPar = do
  ps <- get
  put $ ps { psParanthesesCount = ParanthesesCount 0 False }

incPar :: (Loc a, LastToken a b, Show b) => Parse a b ()
incPar = do
  ps <- get
  let pc = psParanthesesCount ps
  let count = pcActual pc
  put $ ps { psParanthesesCount = pc { pcActual = count + 1 } }

decPar :: (Loc a, LastToken a b, Show b) => Parse a b ()
decPar = do
  ps <- get
  let pc = psParanthesesCount ps
  let newCount = pcActual pc - 1
  let reached0 = pcHasReached0 pc || newCount == 0
  put $ ps { psParanthesesCount = ParanthesesCount newCount reached0 }

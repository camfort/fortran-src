{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Fortran.ParserMonad where

import GHC.IO.Exception
import Control.Exception

import Control.Monad.State hiding (state)
import Control.Monad.Except

import Data.Typeable
import Data.Data
import GHC.Generics (Generic)
import Language.Fortran.Util.Position
import Data.Char (toLower)
import Data.List (isInfixOf, find)

-------------------------------------------------------------------------------
-- Helper datatype definitions
-------------------------------------------------------------------------------

data FortranVersion = Fortran66
                    | Fortran77
                    | Fortran77Extended
                    | Fortran77Legacy
                    | Fortran90
                    | Fortran95
                    | Fortran2003
                    | Fortran2008
                    deriving (Ord, Eq, Data, Typeable, Generic)

instance Show FortranVersion where
  show Fortran66 = "Fortran 66"
  show Fortran77 = "Fortran 77"
  show Fortran77Extended = "Fortran 77 Extended"
  show Fortran77Legacy = "Fortran 77 Legacy"
  show Fortran90 = "Fortran 90"
  show Fortran95 = "Fortran 95"
  show Fortran2003 = "Fortran 2003"
  show Fortran2008 = "Fortran 2008"

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
    where
      lastTokenMsg = tokenMsg (errLastToken err)

tokenMsg :: Show a => Maybe a -> String
tokenMsg (Just a) = "Last parsed token: " ++ show a ++ "."
tokenMsg Nothing = "No token had been lexed."

instance Functor (ParseResult b c) where
    fmap f (ParseOk a s) = ParseOk (f a) s
    fmap _ (ParseFailed err) = ParseFailed err

instance (Typeable a, Typeable b, Show a, Show b) => Exception (ParseError a b)

data ParseResult b c a = ParseOk a (ParseState b) | ParseFailed (ParseError b c)

-- Provides a way to aggregate errors that come
-- from parses with different token types
data ParseErrorSimple = ParseErrorSimple
  { errorPos      :: Position
  , errorFilename :: String
  , errorMsg      :: String }

fromParseResultUnsafe :: (Show c) => ParseResult b c a -> a
fromParseResultUnsafe (ParseOk a _) = a
fromParseResultUnsafe (ParseFailed err) = throwIOerror $ show err

fromRight :: Show a => Either a b -> b
fromRight (Left x)  = throwIOerror . show $ x
fromRight (Right x) = x

fromParseResult :: (Show c) => ParseResult b c a -> Either ParseErrorSimple a
fromParseResult (ParseOk a _)     = Right a
fromParseResult (ParseFailed err) =
    Left ParseErrorSimple
      { errorPos = errPos err
      , errorFilename = errFilename err
      , errorMsg = errMsg err ++ "\n" ++ tokenMsg (errLastToken err)  }

instance Show ParseErrorSimple where
  show err = errorFilename err ++ ", " ++ show (errorPos err) ++ ": " ++ errorMsg err

class LastToken a b | a -> b where
  getLastToken :: (Show b) => a -> Maybe b

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

-------------------------------------------------------------------------------
-- Generic token collection and functions
-------------------------------------------------------------------------------

throwIOerror :: String -> a
throwIOerror s = throw
  IOError { ioe_handle      = Nothing
          , ioe_type        = UserError
          , ioe_location    = "fortran-src"
          , ioe_description = s
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing }

runParse :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> ParseResult b c a
runParse = unParse

runParseUnsafe :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> (a, ParseState b)
runParseUnsafe lexer initState =
  case unParse lexer initState of
    ParseOk a s -> (a, s)
    ParseFailed e -> throwIOerror $ show e

evalParse :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> a
evalParse m s = fst (runParseUnsafe m s)

execParse :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> ParseState b
execParse m s = snd (runParseUnsafe m s)

class Tok a where
  eofToken :: a -> Bool

collectTokens :: forall a b . (Loc b, Tok a, LastToken b a, Show a) => Parse b a a -> ParseState b -> [a]
collectTokens lexer initState =
    evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: (Loc b, Tok a, LastToken b a, Show a) => ParseState b -> Parse b a [a]
    _collectTokens state = do
      let (_token, _state) = runParseUnsafe lexer state
      if eofToken _token
      then return [_token]
      else do
        _tokens <- _collectTokens _state
        return $ _token:_tokens

collectTokensSafe :: forall a b . (Loc b, Tok a, LastToken b a, Show a) => Parse b a a -> ParseState b -> Maybe [a]
collectTokensSafe lexer initState =
    evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: (Loc b, Tok a, LastToken b a, Show a) => ParseState b -> Parse b a (Maybe [a])
    _collectTokens state =
      case unParse lexer state of
        ParseOk _token _state ->
          if eofToken _token
          then return $ Just [_token]
          else do
            _mTokens <- _collectTokens _state
            case _mTokens of
              Just _tokens -> return $ Just $ _token:_tokens
              _ -> return Nothing
        _ -> return Nothing

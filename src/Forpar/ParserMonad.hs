{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Forpar.ParserMonad where

import Control.Exception

import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

import Data.Typeable

import Forpar.Util.Position

-------------------------------------------------------------------------------
-- Helper datatype definitions
-------------------------------------------------------------------------------

data FortranVersion = Fortran66
                    | Fortran77
                    | Fortran90
                    | Fortran95
                    | Fortran2003
                    | Fortran2008
                    deriving (Ord, Eq, Show)

data ParseState a = ParseState
  { psAlexInput :: a
  , psParanthesesCount :: Integer
  , psVersion :: FortranVersion  -- To differentiate lexing behaviour
  , psFilename :: String -- To save correct source location in AST
  }

data ParseError a b = ParseError
  { errPos        :: Position
  , errLastToken  :: Maybe b
  , errFilename   :: String
  , errMsg        :: String }

instance Show b => Show (ParseError a b) where
  show err =
    let lastTokenMsg =
          (case errLastToken err of
            Just a -> "Last parsed token: " ++ show a ++ "."
            Nothing -> "Not token had been lexed.") in
    show (errPos err) ++ ": " ++ errMsg err ++ lastTokenMsg

instance (Typeable a, Typeable b, Show a, Show b) => Exception (ParseError a b)

data ParseResult b c a = ParseOk a (ParseState b) | ParseFailed (ParseError b c)

class LastToken a b | a -> b where
  getLastToken :: (Show b) => a -> Maybe b

-------------------------------------------------------------------------------
-- Parser Monad definition
-------------------------------------------------------------------------------

data Parse b c a = Parse { unParse :: ParseState b -> ParseResult b c a }

instance (Loc b, LastToken b c, Show c) => Monad (Parse b c) where
  return a = Parse $ \s -> ParseOk a s

  (Parse m) >>= f = Parse $ \s ->
    case m s of
      ParseOk a s' -> unParse (f a) s'
      ParseFailed e -> ParseFailed e

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

getPosition :: (Loc a, LastToken a b, Show b) => Parse a b Position
getPosition = do
  parseState <- get
  return $ getPos $ psAlexInput parseState

getSrcSpan :: (Loc a, LastToken a b, Show b) => Position -> Parse a b SrcSpan
getSrcSpan loc1 = do
  loc2 <- getPosition
  return $ SrcSpan loc1 loc2

getParanthesesCount :: (Loc a, LastToken a b, Show b) => Parse a b Integer
getParanthesesCount = psParanthesesCount <$> get

incPar :: (Loc a, LastToken a b, Show b) => Parse a b ()
incPar = do
  ps <- get
  put $ ps { psParanthesesCount = psParanthesesCount ps + 1}

decPar :: (Loc a, LastToken a b, Show b) => Parse a b ()
decPar = do
  ps <- get
  put $ ps { psParanthesesCount = psParanthesesCount ps - 1}

-------------------------------------------------------------------------------
-- Generic token collection and functions
-------------------------------------------------------------------------------

runParse :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> (a, ParseState b)
runParse lexer initState =
  case unParse lexer initState of
    ParseOk a s -> (a, s)
    ParseFailed e -> error $ show e

evalParse :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> a
evalParse m s = fst (runParse m s)

execParse :: (Loc b, LastToken b c, Show c) => Parse b c a -> ParseState b -> ParseState b
execParse m s = snd (runParse m s)

class Tok a where
  eofToken :: a -> Bool

collectTokens :: forall a b . (Loc b, Tok a, LastToken b a, Show a) => Parse b a (Maybe a) -> ParseState b -> Maybe [a]
collectTokens lexer initState =
    evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: (Loc b, Tok a, LastToken b a, Show a) => ParseState b -> Parse b a (Maybe [a])
    _collectTokens state = do
      let (_token, _state) = runParse lexer state
      case _token of
        Just _token' ->
          if eofToken _token'
          then return $ Just [_token']
          else do
            tokens <- _collectTokens _state
            return $ do
              tokens' <- tokens
              return (_token':tokens')
        Nothing -> return Nothing

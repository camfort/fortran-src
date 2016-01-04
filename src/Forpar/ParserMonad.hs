{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forpar.ParserMonad where

import Control.Exception

import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

import Forpar.Util.Position

-------------------------------------------------------------------------------
-- Helper datatype definitions
-------------------------------------------------------------------------------

data FortranVersion = Fortran66 
                    | Fortran77 
                    | Fortran90 
                    | Fortran2003 
                    | Fortran2008
                    deriving (Ord, Eq, Show)

data ParseState a = ParseState 
  { psAlexInput :: a
  , psVersion :: FortranVersion  -- To differentiate lexing behaviour
  , psFilename :: String -- To save correct source location in AST
  }

data ParseError = ParseError
  { errLoc :: SrcLoc
  , errMsg :: String }

instance Show ParseError where
  show err = show (errLoc err) ++ ": " ++ errMsg err

instance Exception ParseError

data ParseResult b a = ParseOk a (ParseState b) 
                     | ParseFailed ParseError

-------------------------------------------------------------------------------
-- Parser Monad definition
-------------------------------------------------------------------------------

data Parse b a = Parse { unParse :: ParseState b -> ParseResult b a }

instance (Loc (ParseState b)) => Monad (Parse b) where
  return a = Parse $ \s -> ParseOk a s

  (Parse m) >>= f = Parse $ \s -> 
    case m s of 
      ParseOk a s' -> unParse (f a) s'
      ParseFailed e -> ParseFailed e

  fail msg = Parse $ \s -> ParseFailed $ ParseError 
    { errLoc = SrcLoc { locPosition = getPos s, locFilename = psFilename s }
    , errMsg = msg }

instance (Loc (ParseState b)) => Functor (Parse b) where
  fmap = liftM

instance (Loc (ParseState b)) => Applicative (Parse b) where
  pure  = return
  (<*>) = ap

instance (Loc (ParseState b)) => MonadState (ParseState b) (Parse b) where
  get = Parse $ \s -> ParseOk s s
  put s = Parse $ \_ -> ParseOk () s

instance (Loc (ParseState b)) => MonadError ParseError (Parse b) where
  throwError e = Parse $ \_ -> ParseFailed e

  (Parse m) `catchError` f = Parse $ \s ->
    case m s of
      ParseFailed e -> unParse (f e) s
      m' -> m'
  
-------------------------------------------------------------------------------
-- Parser helper functions
-------------------------------------------------------------------------------

getVersionP :: (Loc (ParseState a)) => Parse a FortranVersion
getVersionP = do
  s <- get
  return (psVersion s)

putAlexP :: (Loc (ParseState a)) => a -> Parse a ()
putAlexP ai = do
  s <- get
  put (s { psAlexInput = ai })

getAlexP :: (Loc (ParseState a)) => Parse a a
getAlexP = do
  s <- get
  return (psAlexInput s)

getSrcLoc :: (Loc (ParseState a), Loc a) => Parse a SrcLoc
getSrcLoc = do
  parseState <- get
  let pos = getPos . psAlexInput $ parseState
  let filename = psFilename parseState
  return $ SrcLoc { locPosition = pos, locFilename = filename }

getSrcSpan :: (Loc (ParseState a), Loc a) => SrcLoc -> Parse a SrcSpan
getSrcSpan loc1 = do
  loc2 <- getSrcLoc
  return $ SrcSpan loc1 loc2

-------------------------------------------------------------------------------
-- Generic token collection and functions
-------------------------------------------------------------------------------

runParse :: (Loc (ParseState b)) => Parse b a -> ParseState b -> (a, ParseState b)
runParse lexer initState = 
  case unParse lexer initState of
    ParseOk a s -> (a, s)
    ParseFailed e -> error $ show e

evalParse :: (Loc (ParseState b)) => Parse b a -> ParseState b -> a
evalParse m s = fst (runParse m s)

execParse :: (Loc (ParseState b)) => Parse b a -> ParseState b -> ParseState b
execParse m s = snd (runParse m s)

collectTokens :: forall a b . (Loc (ParseState b), Eq a) => a -> Parse b (Maybe a) -> ParseState b -> Maybe [a]
collectTokens finishingToken lexer initState = 
    evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: (Loc (ParseState b), Eq a) => ParseState b -> Parse b (Maybe [a])
    _collectTokens state = do
      let (_token, _state) = runParse lexer state
      case _token of
        Just _token' ->
          if _token' == finishingToken 
          then return $ Just [_token']
          else do
            tokens <- _collectTokens _state
            return $ do
              tokens' <- tokens
              return (_token':tokens')
        Nothing -> return Nothing

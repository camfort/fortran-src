module Forpar.ParserMonad where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont
import Control.Exception

-------------------------------------------------------------------------------
-- Helper datatype definitions
-------------------------------------------------------------------------------

data FortranVersion = Fortran66 
                    | Fortran77 
                    | Fortran90 
                    | Fortran2003 
                    | Fortran2008
                    deriving (Ord, Eq, Show)

data ParseState a = ParseState {
  rAlexInput :: a,
  rVersion :: FortranVersion 
}

-------------------------------------------------------------------------------
-- Lexer Monad definitions
-------------------------------------------------------------------------------

type Lex b r a = ContT r (Parse b) a

-------------------------------------------------------------------------------
-- Parser Monad definitions
-------------------------------------------------------------------------------

type Parse b = State (ParseState b)

-------------------------------------------------------------------------------
-- Lexer helper functions
-------------------------------------------------------------------------------

getAlexL :: Lex a r a
getAlexL = do
    s <- lift getAlexP
    return s

putAlexL :: a -> Lex a r ()
putAlexL ai = do
    lift (putAlexP ai)
    return ()

-------------------------------------------------------------------------------
-- Parser helper functions
-------------------------------------------------------------------------------

getVersionP :: Parse a FortranVersion
getVersionP = do
  s <- get
  return (rVersion s)

putAlexP :: a -> Parse a ()
putAlexP ai = do
  s <- get
  put (s { rAlexInput = ai })

getAlexP :: Parse a a
getAlexP = do
    s <- get
    return (rAlexInput s)

-------------------------------------------------------------------------------
-- Generic token collection and functions
-------------------------------------------------------------------------------

runLex :: Lex b a a -> ParseState b -> a
runLex lexer initState = evalState (runContT lexer return) initState

collectTokens :: Eq a => a -> Lex b (Maybe a) (Maybe a) -> ParseState b -> Maybe [a]
collectTokens finishingToken lexer initState = 
    evalState (_collectTokens initState) undefined
  where
    _collectTokens state = do
      let (_token, _state) = runState (runContT lexer return) state
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

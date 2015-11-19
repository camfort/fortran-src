module Forpar.ParserMonad where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont

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

putAlexL :: a -> Lex a r a
putAlexL ai = do
    lift (putAlexP ai)
    return ai

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

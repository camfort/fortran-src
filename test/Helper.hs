module Helper where

import Forpar.ParserMonad
import Control.Monad.Trans.Cont
import Control.Monad.State.Lazy
  
runLex :: Lex b a a -> ParseState b -> a
runLex lexer initState = evalState (runContT lexer return) initState

collectTokens :: Eq a => a -> Lex b a a -> ParseState b -> [a]
collectTokens finishingToken lexer initState = 
    evalState (_collectTokens initState) undefined
  where
    _collectTokens state = do
      let (_token, _state) = runState (runContT lexer return) state
      if _token == finishingToken 
      then return [_token]
      else do
        tokens <- _collectTokens _state
        return (_token:tokens)

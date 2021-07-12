{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Language.Fortran.Parser.Any
import           Language.Fortran.ParserMonad
import           Language.Fortran.Version
import           Language.Fortran.AST

import qualified Language.Fortran.Parser.Fortran90 as F90
import qualified Language.Fortran.Parser.Fortran77 as F77
import qualified Language.Fortran.Parser.Fortran66 as F66
import qualified Language.Fortran.Lexer.FreeForm   as LexFree
import qualified Language.Fortran.Lexer.FixedForm  as LexFixed

import           Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TSE
import           Data.List (intercalate)

main :: IO ()
main = defaultMain
  [ bgroup "parser"
      [ bgroup "Fortran 90"
          [ bench "statement (expr)" $ whnf pF90Stmt snippetFreeStmtExpr
          , bench "statement (assign)" $ whnf pF90Stmt snippetFreeStmtDeclAssign
          , bench "function" $ whnf pF90Func snippetFreeFunc
          , bench "ProgramFile (default transformations)" $ whnf pF90pfDefTs snippetFreePF
          , bench "ProgramFile (no transformations)" $ whnf pF90pfNoTs snippetFreePF
          ]
      , bgroup "Fortran 77"
          [ bench "statement (expr)" $ whnf pF77Stmt snippetFixedStmt
          --, bench "function" $ whnf pF77Func snippetFixedFunc
          , bench "ProgramFile (default transformations)" $ whnf pF77pfDefTs snippetFixedPF
          , bench "ProgramFile (no transformations)" $ whnf pF77pfNoTs snippetFixedPF
          ]
      , bgroup "Fortran 66"
          [ bench "statement (expr)" $ whnf pF66Stmt snippetFixedStmt
          -- , bench "function" $ whnf pF66Func snippetFixedFunc
          --, bench "ProgramFile (default transformations)" $ whnf pF66pfDefTs snippetFixedPF
          --, bench "ProgramFile (no transformations)" $ whnf pF66pfNoTs snippetFixedPF
          ]
      ]
  ]

--------------------------------------------------------------------------------

pF90Stmt :: ByteString -> Statement A0
pF90Stmt = parseFree Fortran90 F90.statementParser

pF90Func :: ByteString -> ProgramUnit A0
pF90Func = parseFree Fortran90 F90.functionParser

pF90pfDefTs :: ByteString -> ProgramFile A0
pF90pfDefTs = parseFull F90.fortran90Parser

pF90pfNoTs :: ByteString -> ProgramFile A0
pF90pfNoTs = parseFull (F90.fortran90ParserWithTransforms [])

pF77Stmt :: ByteString -> Statement A0
pF77Stmt = parseFixed Fortran77 F77.statementParser

--pF77Func :: ByteString -> ProgramUnit A0
--pF77Func = parseFixed Fortran77 F77.functionParser

pF77pfDefTs :: ByteString -> ProgramFile A0
pF77pfDefTs = parseFull F77.fortran77Parser

pF77pfNoTs :: ByteString -> ProgramFile A0
pF77pfNoTs = parseFull (F77.fortran77ParserWithTransforms [])

pF66Stmt :: ByteString -> Statement A0
pF66Stmt = parseFixed Fortran66 F66.statementParser

--pF66Func :: ByteString -> Statement A0
--pF66Func = parseFixed Fortran66 F66.functionParser

pF66pfDefTs :: ByteString -> ProgramFile A0
pF66pfDefTs = parseFull F66.fortran66Parser

pF66pfNoTs :: ByteString -> ProgramFile A0
pF66pfNoTs = parseFull (F66.fortran66ParserWithTransforms [])

-- polymorphic on lexer input+token in order to work with fixed & free form
parseFull
    :: (ByteString -> String -> ParseResult alexin token (ProgramFile A0))
    -> ByteString -> ProgramFile A0
parseFull parser bs =
    case parser bs "<unknown>" of
      ParseOk a _   -> a
      ParseFailed _ -> error "error"

parseFree :: FortranVersion
          -> Parse LexFree.AlexInput LexFree.Token a -> ByteString -> a
parseFree ver parser src = evalParse parser parserState
  where parserState = LexFree.initParseState src ver "<unknown>"

parseFixed :: FortranVersion
           -> Parse LexFixed.AlexInput LexFixed.Token a -> ByteString -> a
parseFixed ver parser src = evalParse parser parserState
  where parserState = LexFixed.initParseState src ver "<unknown>"

--------------------------------------------------------------------------------

snippetFreeStmtExpr :: ByteString
snippetFreeStmtExpr = programListing $
  [ "x = y*2 + z - 1"
  ]

snippetFreeStmtDeclAssign :: ByteString
snippetFreeStmtDeclAssign = programListing $
  [ "character(5) :: assign_in_decl*5 = \"test!\""
  ]

snippetFreeFunc :: ByteString
snippetFreeFunc = programListing $
  [ "integer function f(x, y, z) result(i)"
  , "  print *, i"
  , "  i = (i - 1)"
  , "end function f"
  ]

snippetFreePF :: ByteString
snippetFreePF = programListing $
  [ "integer function f(x, y, z) result(i)"
  , "  print *, i"
  , "  i = (i - 1)"
  , "end function f"
  , ""
  , "program main"
  , "  x = 1 + 2"
  , "end program main"
  ]

--------------------------------------------------------------------------------

snippetFixedStmt :: ByteString
snippetFixedStmt = programListing $
  [ "      x = y*2 + z - 1"
  ]

snippetFixedFunc :: ByteString
snippetFixedFunc = programListing $
  [ "      subroutine f(x, y, z)"
  , "      print *, i"
  , "      i = (i - 1)"
  , "      end"
  ]

snippetFixedPF :: ByteString
snippetFixedPF = programListing $
  [ "      subroutine f(x, y, z)"
  , "        print *, i"
  , "        i = (i - 1)"
  , "      end"
  , ""
  , "      program main"
  , "        x = 1 + 2"
  , "      end"
  ]

--------------------------------------------------------------------------------

-- | unlines but without the trailing newline
programListing :: [String] -> ByteString
programListing = strToByteString . intercalate "\n"

strToByteString :: String -> ByteString
strToByteString = TSE.encodeUtf8 . Text.pack

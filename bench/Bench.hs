{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Language.Fortran.Parser.Any
import           Language.Fortran.ParserMonad
import           Language.Fortran.Version
import           Language.Fortran.AST

import qualified Language.Fortran.Parser.Fortran90 as F90
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
          ]
      , bgroup "Fortran 66"
          [ bench "statement (expr)" $ whnf pF66Stmt snippetFixedStmt
          , bench "function" $ whnf pF90Func snippetFixedFunc
          ]
      ]
  ]

--------------------------------------------------------------------------------

pF90Stmt :: ByteString -> Statement A0
pF90Stmt = parseFree Fortran90 F90.statementParser

pF90Func :: ByteString -> ProgramUnit A0
pF90Func = parseFree Fortran90 F90.functionParser

pF66Stmt :: ByteString -> Statement A0
pF66Stmt = parseFixed Fortran66 F66.statementParser

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

--------------------------------------------------------------------------------

-- | unlines but without the trailing newline
programListing :: [String] -> ByteString
programListing = strToByteString . intercalate "\n"

strToByteString :: String -> ByteString
strToByteString = TSE.encodeUtf8 . Text.pack

--------------------------------------------------------------------------------

f90ProgramFile :: ByteString
f90ProgramFile = strToByteString $ intercalate "\n" $
  [ "program main"
  , "    character(5) :: assign_in_decl*5 = \"test!\""
  , "    assign_out_decl = \"test!\""
  , "end program main"
  ]

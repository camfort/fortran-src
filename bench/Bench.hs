{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Language.Fortran.Parser.Any
import           Language.Fortran.ParserMonad
import           Language.Fortran.Version
import           Language.Fortran.AST

import qualified Language.Fortran.Parser.Fortran90 as F90
import qualified Language.Fortran.Lexer.FreeForm   as LexFree

import           Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TSE
import           Data.List (intercalate)

main :: IO ()
main = defaultMain
  [ bgroup "parser"
      [ bgroup "Fortran 90"
          [ bench "statement (lone)" $ whnf pF90Stmt snippetFreeStmt
          , bench "function  (lone)" $ whnf pF90Func snippetFreeFuncComplex
          ]
      ]
  ]

pF90Stmt :: ByteString -> Statement A0
pF90Stmt = parseFree Fortran90 F90.statementParser

pF90Func :: ByteString -> ProgramUnit A0
pF90Func = parseFree Fortran90 F90.functionParser

parseFree :: FortranVersion
          -> Parse LexFree.AlexInput LexFree.Token a -> ByteString -> a
parseFree ver parser src = evalParse parser parserState
  where parserState = LexFree.initParseState src ver "<unknown>"

--------------------------------------------------------------------------------

snippetFreeStmt :: ByteString
snippetFreeStmt = programListing $
  [ "character(5) :: assign_in_decl*5 = \"test!\""
  ]

snippetFreeFuncComplex :: ByteString
snippetFreeFuncComplex = programListing $
  [ "integer function f(x, y, z) result(i)"
  , "  print *, i"
  , "  i = (i - 1)"
  , "end function f"
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

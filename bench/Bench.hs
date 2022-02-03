{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main

import qualified Language.Fortran.Parser                as Parser
import           Language.Fortran.Version
import           Language.Fortran.AST
import qualified Language.Fortran.Parser.Free.Fortran90 as F90

import           Data.ByteString (ByteString)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
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
pF90Stmt = Parser.parseUnsafe $ Parser.makeParserFree F90.statementParser Fortran90

pF90Func = undefined
pF90pfDefTs = undefined
pF90pfNoTs = undefined
pF77Stmt = undefined
pF77pfDefTs = undefined
pF77pfNoTs = undefined
pF66Stmt = undefined

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
strToByteString = Text.encodeUtf8 . Text.pack

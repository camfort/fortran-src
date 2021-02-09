{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Rewriter.InternalSpec
  ( spec
  )
where

import qualified Data.ByteString.Lazy.Char8    as BC
import           Test.Hspec
import           Control.Exception.Base         ( evaluate )
import           Language.Fortran.Rewriter.Internal

spec :: Spec
spec = do
  describe "SourceLocation" $ it "Initialize SourceLocation" $ do
    let sl = SourceLocation 0 0
    sl `shouldBe` SourceLocation 0 0
  describe "SourceRange" $ it "Initialize SourceRange" $ do
    let sr = SourceRange (SourceLocation 0 0) (SourceLocation 0 5)
    sr `shouldBe` SourceRange (SourceLocation 0 0) (SourceLocation 0 5)
  describe "RChar" $ do
    it "Initialize RChar" $ do
      let rch = RChar (Just 'a') False (SourceLocation 0 0) ""
      rch `shouldBe` RChar (Just 'a') False (SourceLocation 0 0) ""
    it "Convert line to [RChar]" $ do
      let line   = "Line"
          rcharl = toRCharList line
      rcharl
        `shouldBe` [ RChar (Just 'L') False (SourceLocation 0 0) ""
                   , RChar (Just 'i') False (SourceLocation 0 1) ""
                   , RChar (Just 'n') False (SourceLocation 0 2) ""
                   , RChar (Just 'e') False (SourceLocation 0 3) ""
                   , RChar Nothing    False (SourceLocation 0 4) ""
                   ]
    it "Convert multiline string to [RChar]" $ do
      let multiLine = "fi\nl\nes"
          rcharl    = toRCharList multiLine
      rcharl
        `shouldBe` [ RChar (Just 'f')  False (SourceLocation 0 0) ""
                   , RChar (Just 'i')  False (SourceLocation 0 1) ""
                   , RChar (Just '\n') False (SourceLocation 0 2) ""
                   , RChar (Just 'l')  False (SourceLocation 1 0) ""
                   , RChar (Just '\n') False (SourceLocation 1 1) ""
                   , RChar (Just 'e')  False (SourceLocation 2 0) ""
                   , RChar (Just 's')  False (SourceLocation 2 1) ""
                   , RChar Nothing     False (SourceLocation 2 2) ""
                   ]
    it "Mark range in line (in the middle)" $ do
      let line   = toRCharList "Long Line"
          range  = SourceRange (SourceLocation 0 1) (SourceLocation 0 5)
          rcharl = markRChars line range
      rcharl
        `shouldBe` [ RChar (Just 'L') False (SourceLocation 0 0) ""
                   , RChar (Just 'o') True  (SourceLocation 0 1) ""
                   , RChar (Just 'n') True  (SourceLocation 0 2) ""
                   , RChar (Just 'g') True  (SourceLocation 0 3) ""
                   , RChar (Just ' ') True  (SourceLocation 0 4) ""
                   , RChar (Just 'L') False (SourceLocation 0 5) ""
                   , RChar (Just 'i') False (SourceLocation 0 6) ""
                   , RChar (Just 'n') False (SourceLocation 0 7) ""
                   , RChar (Just 'e') False (SourceLocation 0 8) ""
                   , RChar Nothing    False (SourceLocation 0 9) ""
                   ]
    it "Mark range in line (the whole line)" $ do
      let line   = toRCharList "Long Line"
          range  = SourceRange (SourceLocation 0 0) (SourceLocation 0 9)
          rcharl = markRChars line range
      rcharl
        `shouldBe` [ RChar (Just 'L') True  (SourceLocation 0 0) ""
                   , RChar (Just 'o') True  (SourceLocation 0 1) ""
                   , RChar (Just 'n') True  (SourceLocation 0 2) ""
                   , RChar (Just 'g') True  (SourceLocation 0 3) ""
                   , RChar (Just ' ') True  (SourceLocation 0 4) ""
                   , RChar (Just 'L') True  (SourceLocation 0 5) ""
                   , RChar (Just 'i') True  (SourceLocation 0 6) ""
                   , RChar (Just 'n') True  (SourceLocation 0 7) ""
                   , RChar (Just 'e') True  (SourceLocation 0 8) ""
                   , RChar Nothing    False (SourceLocation 0 9) ""
                   ]
    it "Mark range in multiline string (in the middle)" $ do
      let line   = toRCharList "Li\nn\nes12"
          range  = SourceRange (SourceLocation 0 1) (SourceLocation 2 2)
          rcharl = markRChars line range
      rcharl
        `shouldBe` [ RChar (Just 'L')  False (SourceLocation 0 0) ""
                   , RChar (Just 'i')  True  (SourceLocation 0 1) ""
                   , RChar (Just '\n') True  (SourceLocation 0 2) ""
                   , RChar (Just 'n')  True  (SourceLocation 1 0) ""
                   , RChar (Just '\n') True  (SourceLocation 1 1) ""
                   , RChar (Just 'e')  True  (SourceLocation 2 0) ""
                   , RChar (Just 's')  True  (SourceLocation 2 1) ""
                   , RChar (Just '1')  False (SourceLocation 2 2) ""
                   , RChar (Just '2')  False (SourceLocation 2 3) ""
                   , RChar Nothing     False (SourceLocation 2 4) ""
                   ]
    it "Mark range in multiline string (the whole multiline)" $ do
      let multiline = toRCharList "Li\nn\nes12"
          range     = SourceRange (SourceLocation 0 0) (SourceLocation 2 4)
          rcharl    = markRChars multiline range
      rcharl
        `shouldBe` [ RChar (Just 'L')  True  (SourceLocation 0 0) ""
                   , RChar (Just 'i')  True  (SourceLocation 0 1) ""
                   , RChar (Just '\n') True  (SourceLocation 0 2) ""
                   , RChar (Just 'n')  True  (SourceLocation 1 0) ""
                   , RChar (Just '\n') True  (SourceLocation 1 1) ""
                   , RChar (Just 'e')  True  (SourceLocation 2 0) ""
                   , RChar (Just 's')  True  (SourceLocation 2 1) ""
                   , RChar (Just '1')  True  (SourceLocation 2 2) ""
                   , RChar (Just '2')  True  (SourceLocation 2 3) ""
                   , RChar Nothing     False (SourceLocation 2 4) ""
                   ]
    it "Set replacement string (SourceLocation)" $ do
      let line   = toRCharList "Line"
          sl     = SourceLocation 0 1
          repl   = "repl"
          rchars = setReplacementStringSL line sl repl False
      rchars
        `shouldBe` [ RChar (Just 'L') False (SourceLocation 0 0) ""
                   , RChar (Just 'i') False (SourceLocation 0 1) "repl"
                   , RChar (Just 'n') False (SourceLocation 0 2) ""
                   , RChar (Just 'e') False (SourceLocation 0 3) ""
                   , RChar Nothing    False (SourceLocation 0 4) ""
                   ]
    it "Set replacement string (SourceRange)" $ do
      let line   = toRCharList "Line"
          range  = SourceRange (SourceLocation 0 1) (SourceLocation 0 3)
          repl   = "repl"
          rchars = setReplacementStringSR line range repl False
      rchars
        `shouldBe` [ RChar (Just 'L') False (SourceLocation 0 0) ""
                   , RChar (Just 'i') False (SourceLocation 0 1) "repl"
                   , RChar (Just 'n') False (SourceLocation 0 2) ""
                   , RChar (Just 'e') False (SourceLocation 0 3) ""
                   , RChar Nothing    False (SourceLocation 0 4) ""
                   ]
    it "Evaluate single line (empty replacement)" $ do
      let rchars =
            [ RChar (Just 'L') False (SourceLocation 0 0) ""
            , RChar (Just 'i') False (SourceLocation 0 1) ""
            , RChar (Just 'n') False (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "Line"
    it "Evaluate single line (one-to-none replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) ""
            , RChar (Just 'i') False (SourceLocation 0 1) ""
            , RChar (Just 'n') False (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "ine"
    it "Evaluate single line (many-to-none replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) ""
            , RChar (Just 'i') True  (SourceLocation 0 1) ""
            , RChar (Just 'n') True  (SourceLocation 0 2) ""
            , RChar (Just 'q') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "q"
    it "Evaluate single line (many-to-none replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) ""
            , RChar (Just 'i') True  (SourceLocation 0 1) ""
            , RChar (Just 'n') True  (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "e"
    it "Evaluate single line (one-to-one replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) "T"
            , RChar (Just 'i') False (SourceLocation 0 1) ""
            , RChar (Just 'n') False (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "Tine"
    it "Evaluate single line (many-to-one replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) "T"
            , RChar (Just 'i') True  (SourceLocation 0 1) ""
            , RChar (Just 'n') True  (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "Te"
    it "Evaluate single line (one-to-many replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) "Argent"
            , RChar (Just 'i') False (SourceLocation 0 1) ""
            , RChar (Just 'n') False (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "Argentine"
    it "Evaluate single line (many-to-many replacement)" $ do
      let rchars =
            [ RChar (Just 'L') True  (SourceLocation 0 0) "Pineappl"
            , RChar (Just 'i') True  (SourceLocation 0 1) ""
            , RChar (Just 'n') True  (SourceLocation 0 2) ""
            , RChar (Just 'e') False (SourceLocation 0 3) ""
            , RChar Nothing    False (SourceLocation 0 4) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "Pineapple"
    it "Evaluate multiline" $ do
      let rchars =
            [ RChar (Just 'L')  True  (SourceLocation 0 0) "Formula"
            , RChar (Just 'i')  True  (SourceLocation 0 1) ""
            , RChar (Just '\n') True  (SourceLocation 0 2) ""
            , RChar (Just '\n') False (SourceLocation 0 3) ""
            , RChar (Just '1')  False (SourceLocation 0 4) ""
            , RChar Nothing     False (SourceLocation 0 5) ""
            ]
          result = evaluateRChars rchars
      result `shouldBe` "Formula\n1"
  describe "Chunk" $ do
    it "Take next chunk (no replacements)" $ do
      let line  = toRCharList "a very long text"
          chunk = nextChunk line
      chunk `shouldBe` (line, [])
    it "Take next chunk (replacement in the middle)" $ do
      let line =
            [ RChar (Just 'L') False (SourceLocation 0 0) ""
            , RChar (Just 'i') False (SourceLocation 0 1) ""
            , RChar (Just 'n') True  (SourceLocation 0 2) "repl"
            , RChar (Just 'e') True  (SourceLocation 0 3) ""
            , RChar (Just 'O') True  (SourceLocation 0 4) ""
            , RChar (Just 'f') False (SourceLocation 0 5) ""
            , RChar Nothing    False (SourceLocation 0 6) ""
            ]
          chunk = nextChunk line
      chunk `shouldBe` (take 2 line, drop 2 line)
    it "Take next chunk (replacement in the begining)" $ do
      let line =
            [ RChar (Just 'D') True  (SourceLocation 0 0) "repl"
            , RChar (Just 'o') True  (SourceLocation 0 1) ""
            , RChar (Just 'g') False (SourceLocation 0 2) ""
            , RChar Nothing    False (SourceLocation 0 3) ""
            ]
          chunk = nextChunk line
      chunk `shouldBe` (take 1 line, drop 1 line)
    it "Take next chunk (deletion in the begining)" $ do
      let line =
            [ RChar (Just 'D') True  (SourceLocation 0 0) ""
            , RChar (Just 'o') True  (SourceLocation 0 1) ""
            , RChar (Just 'g') False (SourceLocation 0 2) ""
            , RChar Nothing    False (SourceLocation 0 3) ""
            ]
          chunk = nextChunk line
      chunk `shouldBe` (take 1 line, drop 1 line)
    it "Take next chunk (with a new line)" $ do
      let multiline = toRCharList "some text with\na new line"
          chunk     = nextChunk multiline
      chunk `shouldBe` (take 15 multiline, drop 15 multiline)
    it "Take all chunks (single chunk)" $ do
      let line   = toRCharList "A single chunk"
          chunks = allChunks line
      chunks `shouldBe` [line]
    it "Take all chunks (chunks separated by a new line)" $ do
      let multiline = toRCharList "Two \n chunks"
          chunks    = allChunks multiline
      chunks `shouldBe` [take 5 multiline, drop 5 multiline]
    it "Take all chunks (chunks separated by a new line and deletion marks)"
      $ do
          let multiline =
                [ RChar (Just 'L')  False (SourceLocation 0 0) ""
                , RChar (Just 'i')  False (SourceLocation 0 1) ""
                , RChar (Just '\n') False (SourceLocation 0 2) ""
                , RChar (Just 'e')  True  (SourceLocation 1 0) "repl"
                , RChar (Just 't')  True  (SourceLocation 1 1) ""
                , RChar (Just 'f')  False (SourceLocation 2 0) ""
                , RChar Nothing     False (SourceLocation 2 1) ""
                ]
              chunks = allChunks multiline
          chunks
            `shouldBe` [ [ RChar (Just 'L')  False (SourceLocation 0 0) ""
                         , RChar (Just 'i')  False (SourceLocation 0 1) ""
                         , RChar (Just '\n') False (SourceLocation 0 2) ""
                         ]
                       , [RChar (Just 'e') True (SourceLocation 1 0) "repl"]
                       , [RChar (Just 't') True (SourceLocation 1 1) ""]
                       , [ RChar (Just 'f') False (SourceLocation 2 0) ""
                         , RChar Nothing    False (SourceLocation 2 1) ""
                         ]
                       ]
    it "Evaluate chunks (no replacements; short single line)" $ do
      let chunks = allChunks $ toRCharList "Line"
          str    = evaluateChunks chunks
      str `shouldBe` "Line"
    it "Evaluate chunks (no replacements; short multiline)" $ do
      let chunks = allChunks $ toRCharList "Multi\nline"
          str    = evaluateChunks chunks
      str `shouldBe` "Multi\nline"
    it "Evaluate chunks (no replacements; long single line)" $ do
      let rchars = toRCharList $ BC.replicate 90 'a'
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str `shouldBe` BC.replicate 90 'a'
    it "Evaluate chunks (no replacements; long multiline)" $ do
      let ostr =
            BC.replicate 20 'a'
              <> "\n"
              <> BC.replicate 40 'b'
              <> "\n"
              <> BC.replicate 90 'c'
              <> "\n"
              <> BC.replicate 70 'd'
          rchars = toRCharList ostr
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str `shouldBe` ostr
    it "Evaluate chunks (empty replacement; short line)" $ do
      let range  = SourceRange (SourceLocation 0 5) (SourceLocation 0 5)
          repl   = Replacement range ""
          rchars = setReplacement (toRCharList "A short one") repl
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str `shouldBe` "A short one"
    it "Evaluate chunks (short replacement; short line)" $ do
      let range  = SourceRange (SourceLocation 0 5) (SourceLocation 0 7)
          repl   = Replacement range "repl"
          rchars = setReplacement (toRCharList "A short line") repl
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str `shouldBe` "A shorepl line"
    it "Evaluate chunks (long replacement; short line)" $ do
      let
        range  = SourceRange (SourceLocation 0 34) (SourceLocation 0 35)
        replS  = replicate 60 'a'
        repl   = Replacement range replS
        rchars = setReplacement
          (toRCharList "A very long line with many words in it")
          repl
        chunks = allChunks rchars
        str    = evaluateChunks chunks
      str
        `shouldBe` "A very long line with many words i"
        <>         "\n     +"
        <>         BC.replicate 60 'a'
        <>         " it"
    it "Evaluate chunks (long replacement; long line)" $ do
      let range  = SourceRange (SourceLocation 0 3) (SourceLocation 0 4)
          replS  = replicate 70 'a'
          repl   = Replacement range replS
          rchars = setReplacement (toRCharList $ BC.replicate 70 'b') repl
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str
        `shouldBe` BC.replicate 3 'b'
        <>         "\n     +"
        <>         BC.replicate 70 'a'
        <>         "\n     +"
        <>         BC.replicate 66 'b'
    it "Evaluate chunks (multiline replacement; short line)" $ do
      let range  = SourceRange (SourceLocation 0 9) (SourceLocation 0 10)
          replS  = replicate 30 'a' <> "\n" <> replicate 30 'b'
          repl   = Replacement range replS
          rchars = setReplacement (toRCharList $ BC.replicate 20 'c') repl
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str
        `shouldBe` BC.replicate 9 'c'
        <>         BC.replicate 30 'a'
        <>         "\n"
        <>         BC.replicate 30 'b'
        <>         BC.replicate 10 'c'
    it "Evaluate chunks (multiline replacement; long line)" $ do
      let range  = SourceRange (SourceLocation 0 8) (SourceLocation 0 9)
          replS  = replicate 30 'a' <> "\n" <> replicate 30 'b'
          repl   = Replacement range replS
          rchars = setReplacement (toRCharList $ BC.replicate 60 'c') repl
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str
        `shouldBe` BC.replicate 8 'c'
        <>         BC.replicate 30 'a'
        <>         "\n"
        <>         BC.replicate 30 'b'
        <>         "\n     +"
        <>         BC.replicate 51 'c'
    it "Evaluate chunks (multiline replacement; multiline)" $ do
      let range  = SourceRange (SourceLocation 0 10) (SourceLocation 0 15)
          replS  = replicate 30 'a' <> "\n" <> replicate 30 'd'
          repl   = Replacement range replS
          rchars = setReplacement
            (toRCharList $ BC.replicate 30 'c' <> "\n" <> BC.replicate 30 'c')
            repl
          chunks = allChunks rchars
          str    = evaluateChunks chunks
      str
        `shouldBe` BC.replicate 10 'c'
        <>         BC.replicate 30 'a'
        <>         "\n"
        <>         BC.replicate 30 'd'
        <>         BC.replicate 15 'c'
        <>         "\n"
        <>         BC.replicate 30 'c'
  describe "Replacement" $ do
    it "Initialize Replacement" $ do
      let
        r =
          Replacement (SourceRange (SourceLocation 0 0) (SourceLocation 0 5)) ""
      r `shouldBe` Replacement
        (SourceRange (SourceLocation 0 0) (SourceLocation 0 5))
        ""
    it "Set replacement (regular)" $ do
      let line  = toRCharList "LineOf"
          range = SourceRange (SourceLocation 0 1) (SourceLocation 0 5)
          replS = "repl"
          r     = Replacement range replS
          res   = setReplacement line r
      res
        `shouldBe` [ RChar (Just 'L') False (SourceLocation 0 0) ""
                   , RChar (Just 'i') True  (SourceLocation 0 1) "repl"
                   , RChar (Just 'n') True  (SourceLocation 0 2) ""
                   , RChar (Just 'e') True  (SourceLocation 0 3) ""
                   , RChar (Just 'O') True  (SourceLocation 0 4) ""
                   , RChar (Just 'f') False (SourceLocation 0 5) ""
                   , RChar Nothing    False (SourceLocation 0 6) ""
                   ]
    it "Set replacement (insertion in the middle)" $ do
      let line  = toRCharList "shorty"
          range = SourceRange (SourceLocation 0 1) (SourceLocation 0 1)
          replS = "Va"
          r     = Replacement range replS
          res   = setReplacement line r
      res
        `shouldBe` [ RChar (Just 's') False (SourceLocation 0 0) ""
                   , RChar (Just 'h') True  (SourceLocation 0 1) "Vah"
                   , RChar (Just 'o') False (SourceLocation 0 2) ""
                   , RChar (Just 'r') False (SourceLocation 0 3) ""
                   , RChar (Just 't') False (SourceLocation 0 4) ""
                   , RChar (Just 'y') False (SourceLocation 0 5) ""
                   , RChar Nothing    False (SourceLocation 0 6) ""
                   ]
    it "Set replacement (insertion at the end)" $ do
      let line  = toRCharList "shorty"
          range = SourceRange (SourceLocation 0 6) (SourceLocation 0 6)
          replS = "Vb"
          r     = Replacement range replS
          res   = setReplacement line r
      res
        `shouldBe` [ RChar (Just 's') False (SourceLocation 0 0) ""
                   , RChar (Just 'h') False (SourceLocation 0 1) ""
                   , RChar (Just 'o') False (SourceLocation 0 2) ""
                   , RChar (Just 'r') False (SourceLocation 0 3) ""
                   , RChar (Just 't') False (SourceLocation 0 4) ""
                   , RChar (Just 'y') False (SourceLocation 0 5) ""
                   , RChar Nothing    True  (SourceLocation 0 6) "Vb"
                   ]
    it "Set replacements" $ do
      let code   = toRCharList $ BC.replicate 6 'a'
          range1 = SourceRange (SourceLocation 0 1) (SourceLocation 0 3)
          range2 = SourceRange (SourceLocation 0 3) (SourceLocation 0 5)
          replS1 = "repl1"
          replS2 = "repl2"
          r1     = Replacement range1 replS1
          r2     = Replacement range2 replS2
          res    = setReplacements code [r1, r2]
      res
        `shouldBe` [ RChar (Just 'a') False (SourceLocation 0 0) ""
                   , RChar (Just 'a') True (SourceLocation 0 1) (BC.pack replS1)
                   , RChar (Just 'a') True  (SourceLocation 0 2) ""
                   , RChar (Just 'a') True (SourceLocation 0 3) (BC.pack replS2)
                   , RChar (Just 'a') True  (SourceLocation 0 4) ""
                   , RChar (Just 'a') False (SourceLocation 0 5) ""
                   , RChar Nothing    False (SourceLocation 0 6) ""
                   ]
    it "Disjoint replacements (disjoint, different lines)" $ do
      let range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 14)
          range2 = SourceRange (SourceLocation 1 2) (SourceLocation 1 20)
          r1     = Replacement range1 "zz"
          r2     = Replacement range2 "xx"
      areDisjoint r1 r2 `shouldBe` True
      areDisjoint r2 r1 `shouldBe` True
    it "Disjoint replacements (disjoint, same line)" $ do
      let range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 4)
          range2 = SourceRange (SourceLocation 0 4) (SourceLocation 0 6)
          r1     = Replacement range1 "ee"
          r2     = Replacement range2 "ff"
      areDisjoint r1 r2 `shouldBe` True
      areDisjoint r2 r1 `shouldBe` True
    it "Disjoint replacements (overlapping, same line)" $ do
      let range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 14)
          range2 = SourceRange (SourceLocation 0 7) (SourceLocation 0 20)
          r1     = Replacement range1 "aa"
          r2     = Replacement range2 "bb"
      areDisjoint r1 r2 `shouldBe` False
      areDisjoint r2 r1 `shouldBe` False
    it "Disjoint replacements (overlapping, across lines)" $ do
      let range1 = SourceRange (SourceLocation 0 2) (SourceLocation 2 14)
          range2 = SourceRange (SourceLocation 1 7) (SourceLocation 5 20)
          r1     = Replacement range1 "cc"
          r2     = Replacement range2 "dd"
      areDisjoint r1 r2 `shouldBe` False
      areDisjoint r2 r1 `shouldBe` False
    it "Apply replacement (continuation line)" $ do
      let source = BC.replicate 30 'a' <> "\n" <> BC.replicate 30 'a'
          range  = SourceRange (SourceLocation 0 2) (SourceLocation 1 2)
          replS  = replicate 71 'b' <> "\n" <> replicate 71 'c'
          r      = Replacement range replS
          res    = applyReplacements source [r]
      res
        `shouldBe` (  BC.replicate 2 'a'
                   <> "\n     +"
                   <> BC.pack replS
                   <> "\n     +"
                   <> BC.replicate 28 'a'
                   )
    it "Apply replacement (insertion in the middle)" $ do
      let source = "abc"
          range  = SourceRange (SourceLocation 0 0) (SourceLocation 0 0)
          replS  = "DDD"
          r      = Replacement range replS
          res    = applyReplacements source [r]
      res `shouldBe` "DDDabc"
    it "Apply replacement (insertion at the end)" $ do
      let source = "abz"
          range  = SourceRange (SourceLocation 0 3) (SourceLocation 0 3)
          replS  = "DFG"
          r      = Replacement range replS
          res    = applyReplacements source [r]
      res `shouldBe` "abzDFG"
    it "Apply replacements (disjoint)" $ do
      let source = BC.replicate 30 'a'
          range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 4)
          range2 = SourceRange (SourceLocation 0 4) (SourceLocation 0 6)
          replS1 = "repl1"
          replS2 = "2lper"
          r1     = Replacement range1 replS1
          r2     = Replacement range2 replS2
          res    = applyReplacements source [r1, r2]
      res
        `shouldBe` BC.replicate 2 'a'
        <>         BC.pack replS1
        <>         BC.pack replS2
        <>         BC.replicate 24 'a'
    it "Apply replacements (overlapping)" $ do
      let source = BC.replicate 30 'a'
          range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 4)
          range2 = SourceRange (SourceLocation 0 3) (SourceLocation 0 6)
          replS  = "repl"
          r1     = Replacement range1 replS
          r2     = Replacement range2 replS
      evaluate (applyReplacements source [r1, r2])
        `shouldThrow` (== OverlappingError [(r1, r2)])
    it "Apply replacements (multiple overlapping)" $ do
      let source = BC.replicate 30 'a'
          range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 4)
          range2 = SourceRange (SourceLocation 0 3) (SourceLocation 0 6)
          range3 = SourceRange (SourceLocation 0 5) (SourceLocation 0 7)
          replS  = "repl"
          r1     = Replacement range1 replS
          r2     = Replacement range2 replS
          r3     = Replacement range3 replS
      evaluate (applyReplacements source [r1, r2, r3])
        `shouldThrow` (== OverlappingError [(r1, r2), (r2, r3)])
    it "Apply replacements (overlapping and out-of-order)" $ do
      let source = BC.replicate 30 'a'
          range1 = SourceRange (SourceLocation 0 2) (SourceLocation 0 4)
          range2 = SourceRange (SourceLocation 0 3) (SourceLocation 0 6)
          range3 = SourceRange (SourceLocation 0 5) (SourceLocation 0 7)
          replS  = "repl"
          r1     = Replacement range1 replS
          r2     = Replacement range2 replS
          r3     = Replacement range3 replS
      evaluate (applyReplacements source [r3, r2, r1])
        `shouldThrow` (== OverlappingError [(r1, r2), (r2, r3)])
    it "Apply replacements (invalid ranges; out of code)" $ do
      let source = BC.replicate 30 'a'
          range  = SourceRange (SourceLocation 0 2) (SourceLocation 0 31)
          repl   = Replacement range ""
      evaluate (applyReplacements source [repl])
        `shouldThrow` (== InvalidRangeError)
    it "Apply replacements (invalid ranges; invalid ordering of the parameters)"
      $ do
          let source = BC.replicate 30 'a'
              range  = SourceRange (SourceLocation 0 20) (SourceLocation 0 5)
              repl   = Replacement range ""
          evaluate (applyReplacements source [repl])
            `shouldThrow` (== InvalidRangeError)

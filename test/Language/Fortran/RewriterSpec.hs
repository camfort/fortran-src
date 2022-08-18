{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Language.Fortran.RewriterSpec
  ( spec
  )
where

import           Test.Hspec
import           Control.Exception.Base         ( try
                                                , throw
                                                )
import           Control.Monad                  ( foldM
                                                , unless
                                                )
import qualified Data.Map                      as M
import           System.Directory               ( copyFile
                                                , createDirectory
                                                , getCurrentDirectory
                                                , doesDirectoryExist
                                                )
import           Language.Fortran.Rewriter
import qualified Data.ByteString.Lazy.Char8    as BC

spec :: Spec
spec = do
  describe "ReplacementMap" $ do
    it "Initialize ReplacementMap" $ do
      let
        rm = M.fromList
          [ ( "foo.f" :: String
            , [ Replacement
                  (SourceRange (SourceLocation 0 0) (SourceLocation 1 1))
                  ""
              ]
            )
          , ("bar.f", [])
          ]
      rm `shouldBe` M.fromList
        [ ( "foo.f"
          , [ Replacement
                (SourceRange (SourceLocation 0 0) (SourceLocation 1 1))
                ""
            ]
          )
        , ("bar.f", [])
        ]
    it "Process ReplacementMap (empty; should not die)" $ do
      let rm = M.empty
      processReplacements rm
    it "Process ReplacementMap (simple replacements)" $ do
      base <- getCurrentDirectory
      let
        body workDir = processReplacements $ M.fromList
          [ ( workDir ++ "001_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 1 12) (SourceLocation 1 16))
                  "cccc"
              ]
            )
          , ( workDir ++ "002_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 1 7) (SourceLocation 1 16))
                  ""
              ]
            )
          , ( workDir ++ "003_foo.f"
            , [ Replacement
                (SourceRange (SourceLocation 1 12) (SourceLocation 1 17))
                "cccc1"
              , Replacement
                (SourceRange (SourceLocation 3 12) (SourceLocation 3 17))
                "cccc2"
              ]
            )
          , ( workDir ++ "004_unicode.f"
            , [ Replacement
                  (SourceRange (SourceLocation 3 17) (SourceLocation 3 28))
                  "int(z'deadbeef')"
              ]
            )
          , ( workDir ++ "005_unicode.f"
            , [ Replacement
                  (SourceRange (SourceLocation 3 21) (SourceLocation 3 21))
                  ""
              ]
            )
          ]
      wrapReplacementsMapInvocationTestHelper
        body
        base
        Nothing
        "replacementsmap-simple"
        [ "001_foo.f"
        , "002_foo.f"
        , "003_foo.f"
        , "004_unicode.f"
        , "005_unicode.f"
        ]
    it "Process ReplacementMap (insertions)" $ do
      base <- getCurrentDirectory
      let body workDir = processReplacements $ M.fromList
            [ ( workDir ++ "001_foo.f"
              , [ Replacement
                    (SourceRange (SourceLocation 0 24) (SourceLocation 0 24))
                    "\n       call bbbb"
                ]
              )
            ]
      wrapReplacementsMapInvocationTestHelper body
                                              base
                                              Nothing
                                              "replacementsmap-insertion"
                                              ["001_foo.f"]
    it "Process ReplacementMap (overlapping)" $ do
      base <- getCurrentDirectory
      let
        r1 = Replacement
          (SourceRange (SourceLocation 1 12) (SourceLocation 1 16))
          "cccc"
        r2 = Replacement
          (SourceRange (SourceLocation 1 7) (SourceLocation 1 16))
          "call dddd"
        body workDir =
          processReplacements $ M.fromList [(workDir ++ "001_foo.f", [r1, r2])]
      wrapReplacementsMapInvocationTestHelper
        body
        base
        (Just $ OverlappingError [(r2, r1)])
        "replacementsmap-overlapping"
        ["001_foo.f"]
    it "Process ReplacementMap (overlapping filtered)" $ do
      base <- getCurrentDirectory
      let r1 = Replacement
            (SourceRange (SourceLocation 1 12) (SourceLocation 1 16))
            "cccc"
          r2 = Replacement
            (SourceRange (SourceLocation 1 7) (SourceLocation 1 16))
            "call dddd"
          (repls, overlapping) = partitionOverlapping [r1, r2]
          body workDir =
            processReplacements $ M.fromList [(workDir ++ "001_foo.f", repls)]
      repls `shouldBe` [r1]
      wrapReplacementsMapInvocationTestHelper
        body
        base
        Nothing
        "replacementsmap-overlapping-filtered"
        ["001_foo.f"]
      overlapping `shouldBe` [r2]
    it "Process ReplacementMap (invalid range; start line)" $ do
      base <- getCurrentDirectory
      let
        body workDir = processReplacements $ M.fromList
          [ ( workDir ++ "001_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 9 12) (SourceLocation 9 15))
                  ""
              ]
            )
          ]
      wrapReplacementsMapInvocationTestHelper body
                                              base
                                              (Just InvalidRangeError)
                                              "replacementsmap-overlapping"
                                              ["001_foo.f"]
    it "Process ReplacementMap (invalid range; start column)" $ do
      base <- getCurrentDirectory
      let
        body workDir = processReplacements $ M.fromList
          [ ( workDir ++ "001_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 1 112) (SourceLocation 1 115))
                  ""
              ]
            )
          ]
      wrapReplacementsMapInvocationTestHelper body
                                              base
                                              (Just InvalidRangeError)
                                              "replacementsmap-overlapping"
                                              ["001_foo.f"]
    it "Process ReplacementMap (invalid range; end line)" $ do
      base <- getCurrentDirectory
      let
        body workDir = processReplacements $ M.fromList
          [ ( workDir ++ "001_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 1 13) (SourceLocation 19 115))
                  ""
              ]
            )
          ]
      wrapReplacementsMapInvocationTestHelper body
                                              base
                                              (Just InvalidRangeError)
                                              "replacementsmap-overlapping"
                                              ["001_foo.f"]
    it "Process ReplacementMap (invalid range; end column)" $ do
      base <- getCurrentDirectory
      let
        body workDir = processReplacements $ M.fromList
          [ ( workDir ++ "001_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 1 13) (SourceLocation 1 115))
                  ""
              ]
            )
          ]
      wrapReplacementsMapInvocationTestHelper body
                                              base
                                              (Just InvalidRangeError)
                                              "replacementsmap-overlapping"
                                              ["001_foo.f"]
    it "Process ReplacementMap (column limit)" $ do
      base <- getCurrentDirectory
      let
        body workDir = processReplacements $ M.fromList
          [ ( workDir ++ "001_foo.f"
            , [ Replacement
                  (SourceRange (SourceLocation 3 42) (SourceLocation 3 43))
                  "999999999999999999999"
              ]
            )
#ifndef FS_DISABLE_WIN_BROKEN_TESTS
          -- TODO fails on Windows due to some line ending/spacing bug
          , ( workDir ++ "002_other.f"
            , [ Replacement
                  (SourceRange (SourceLocation 4 61) (SourceLocation 4 62))
                  "999999999999"
              ]
            )
          -- TODO fails on Windows due to some line ending/spacing bug
          , ( workDir ++ "003_multiline.f"
            , [ Replacement
                  (SourceRange (SourceLocation 4 61) (SourceLocation 4 62))
                  "9 .and. \n     + 4 .lt. 4\n     + .or. .true."
              ]
            )
#endif
          , ( workDir ++ "004_comment.f"
            , [ Replacement
                (SourceRange (SourceLocation 2 18) (SourceLocation 2 19))
                "foobar"
              , Replacement
                (SourceRange (SourceLocation 8 16) (SourceLocation 8 27))
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              ]
            )
          , ( workDir ++ "005_removals.f"
            , [ Replacement
                (SourceRange (SourceLocation 2 50) (SourceLocation 3 24))
                ""
              , Replacement
                (SourceRange (SourceLocation 8 55) (SourceLocation 9 22))
                ""
              ]
            )
          , ( workDir ++ "006_linewrap_heuristic.f"
            , [ Replacement
                (SourceRange (SourceLocation 3 19) (SourceLocation 3 23))
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              , Replacement
                (SourceRange (SourceLocation 4 23) (SourceLocation 4 27))
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              , Replacement
                (SourceRange (SourceLocation 5 28) (SourceLocation 5 32))
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              , Replacement
                (SourceRange (SourceLocation 6 28) (SourceLocation 6 32))
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              , Replacement
                (SourceRange (SourceLocation 7 19) (SourceLocation 7 23))
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              , Replacement
                (SourceRange (SourceLocation 8 6) (SourceLocation 8 24))
                "call testcommons_logarray_settori_findex(logical_1_to_4(test), 1)"
              , Replacement
                (SourceRange (SourceLocation 11 12) (SourceLocation 11 28))
                "call testcommons_set_logical2var(logical_2_to_4(l2))"
              , Replacement
                (SourceRange (SourceLocation 12 12) (SourceLocation 12 12))
                "call testcommons_set_logical2var(logical_2_to_4(l2))"
              , Replacement
                (SourceRange (SourceLocation 13 12) (SourceLocation 13 13))
                "call testcommons_set_logical2var(logical_2_to_4(l2))"
              ]
            )
          ]
      wrapReplacementsMapInvocationTestHelper
        body
        base
        Nothing
        "replacementsmap-columnlimit"
        [ "001_foo.f"
#ifndef FS_DISABLE_WIN_BROKEN_TESTS
        , "002_other.f"
        , "003_multiline.f"
#endif
        , "004_comment.f"
        , "005_removals.f"
        , "006_linewrap_heuristic.f"
        ]

  describe "Filtering overlapping replacements" $ do
    it "Simple overlap" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 0) (SourceLocation 0 2))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "b"
        (disjoint, overlapping) = partitionOverlapping [r1, r2]
      disjoint `shouldBe` [r1]
      overlapping `shouldBe` [r2]
    it "Reversed order" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 0) (SourceLocation 0 2))
          "b"
        (disjoint, overlapping) = partitionOverlapping [r1, r2]
      disjoint `shouldBe` [r1]
      overlapping `shouldBe` [r2]
    it "Middle overlap" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 0) (SourceLocation 0 2))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "b"
        r3 = Replacement
          (SourceRange (SourceLocation 0 2) (SourceLocation 0 4))
          "c"
        (disjoint, overlapping) = partitionOverlapping [r1, r2, r3]
      disjoint `shouldBe` [r1, r3]
      overlapping `shouldBe` [r2]
    it "Middle overlap out of order" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 0) (SourceLocation 0 2))
          "b"
        r3 = Replacement
          (SourceRange (SourceLocation 0 2) (SourceLocation 0 4))
          "c"
        (disjoint, overlapping) = partitionOverlapping [r1, r2, r3]
      disjoint `shouldBe` [r1]
      overlapping `shouldBe` [r2, r3]
    it "Exact overlap" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "b"
        (disjoint, overlapping) = partitionOverlapping [r1, r2]
      disjoint `shouldBe` [r1]
      overlapping `shouldBe` [r2]
    it "Overlapping beginning" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 2))
          "b"
        (disjoint, overlapping) = partitionOverlapping [r1, r2]
      disjoint `shouldBe` [r1]
      overlapping `shouldBe` [r2]
    it "Some overlapping" $ do
      let
        r1 = Replacement
          (SourceRange (SourceLocation 0 1) (SourceLocation 0 3))
          "a"
        r2 = Replacement
          (SourceRange (SourceLocation 0 5) (SourceLocation 0 7))
          "b"
        r3 = Replacement
          (SourceRange (SourceLocation 0 3) (SourceLocation 0 5))
          "b"
        r4 = Replacement
          (SourceRange (SourceLocation 0 6) (SourceLocation 0 8))
          "b"
        (disjoint, overlapping) = partitionOverlapping [r1, r2, r3, r4]
      disjoint `shouldBe` [r1, r2, r3]
      overlapping `shouldBe` [r4]


wrapReplacementsMapInvocationTestHelper
  :: (String -> IO ())
  -> FilePath
  -> Maybe ReplacementError
  -> String
  -> [String]
  -> Expectation
wrapReplacementsMapInvocationTestHelper invocation baseDir Nothing ctx files =
  do
    (inputContextDir, outputContextDir, invokeResult) <-
      wrapReplacementsMapInvocationTestHelper_ invocation baseDir ctx files
    case invokeResult of
      Left  e -> error $ "Test failed unexpectedly; should pass: " <> show e
      Right _ -> do
        let res = foldM
              (\acc file -> do
                let expected = inputContextDir ++ "/" ++ file ++ ".expected"
                let output   = outputContextDir ++ "/" ++ file
                ok <- compareFile expected output
                return (acc && ok)
              )
              True
              files
        res `shouldReturn` True
wrapReplacementsMapInvocationTestHelper invocation baseDir (Just expExcp) ctx files
  = do
    (_, _, invokeResult) <- wrapReplacementsMapInvocationTestHelper_
      invocation
      baseDir
      ctx
      files
    case invokeResult of
      Left  ex -> throw ex `shouldThrow` (== expExcp)
      Right _  -> error "Test passed unexpectedly; should fail"

wrapReplacementsMapInvocationTestHelper_
  :: (String -> IO ())
  -> FilePath
  -> String
  -> [String]
  -> IO (String, String, Either ReplacementError ())
wrapReplacementsMapInvocationTestHelper_ invocation baseDir ctx files = do
  let inputContextDir  = baseDir ++ "/test-data/rewriter/" ++ ctx
      outputDir        = baseDir ++ "/test/rewriter-test-output"
      outputContextDir = outputDir ++ "/" ++ ctx
  doesDirectoryExist outputDir
    >>= (\x -> unless x $ createDirectory outputDir)
  doesDirectoryExist outputContextDir
    >>= (\x -> unless x $ createDirectory outputContextDir)
  mapM_
    (\file -> copyFile (inputContextDir ++ "/" ++ file)
                       (outputContextDir ++ "/" ++ file)
    )
    files
  result <-
    try (invocation $ outputContextDir ++ "/") :: IO
      (Either ReplacementError ())
  return (inputContextDir, outputContextDir, result)

--------------------------------------------------------------------------------

-- | Utility function to compare file content.
compareFile :: FilePath -> FilePath -> IO Bool
compareFile expected actual = do
  c1 <- BC.readFile expected
  c2 <- BC.readFile actual
  compareByteString c1 c2

-- XXX: It appears the rewriter has different behaviour on Windows and Linux --
--      specifically relating to newlines. So we use a custom equality check
--      that skips Windows newlines (@\r@ characters are skipped).
--
--      (This is morally grey: the issue is unlikely to rear its head in
--      practical usage, _but_ if you were to be comparing rewriter output from
--      different platforms, you may encounter it. Most cross-platform text
--      tooling recognises/ignores Windows newlines, however.)
compareByteString :: BC.ByteString -> BC.ByteString -> IO Bool
compareByteString expected actual = if expected `eqSkipWinNewlines` actual
  then return True
  else do
    BC.putStrLn ">>>>>>> EXPECTED"
    BC.putStrLn expected
    BC.putStrLn ">>>>>>> ACTUAL"
    BC.putStrLn actual
    return False

eqSkipWinNewlines :: BC.ByteString -> BC.ByteString -> Bool
eqSkipWinNewlines x1 x2 = go (BC.uncons x1) (BC.uncons x2)
  where
    go :: Maybe (Char, BC.ByteString) -> Maybe (Char, BC.ByteString) -> Bool

    -- both reached EOF simultaneously: identical
    go Nothing Nothing = True

    -- next character in either bytestring is @\r@: skip
    go (Just ('\r', bs1)) bs2 = go (BC.uncons bs1) bs2
    go bs1 (Just ('\r', bs2)) = go bs1 (BC.uncons bs2)

    -- only one bytestring is EOF: different
    go Nothing (Just _)       = False
    go (Just _) Nothing       = False

    -- main case: check equality of next words
    go (Just (b1, bs1)) (Just (b2, bs2)) =
        if   b1 == b2
        then go (BC.uncons bs1) (BC.uncons bs2)
        else False

-- | This module provides an interface for rewriting textual, unparsed Fortran
-- using a diff-like algorithm.
--
-- Original code from Bloomberg, used with permission.
--
-- Original authors:
--   * Daniel Beer
--   * Anthony Burzillo
--   * Raoul Hidalgo Charman
--   * Aiden Jeffrey
--   * Jason Xu
--   * Beleth Apophis
--   * Lukasz Kolodziejczyk

module Language.Fortran.Rewriter
  ( RI.SourceLocation(..)
  , RI.SourceRange(..)
  , RI.Replacement(..)
  , RI.ReplacementError(..)
  , RI.ReplacementMap
  , partitionOverlapping
  , processReplacements
  , spanToSourceRange
  , spanToSourceRange2
  , sourceRangeBetweenTwoSpans
  )
where

import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Language.Fortran.Rewriter.Internal
                                               as RI
import           Data.List                      ( partition )
import qualified Data.Map                      as M
import           Language.Fortran.Util.Position ( lineCol
                                                , SrcSpan(..)
                                                )
import           System.Directory               ( renameFile )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                , takeDirectory
                                                )
import           System.IO.Temp                 ( withTempDirectory )

-- | Remove overlapping items from a list of replacements and return a pair of
-- lists containing disjoint items and overlapping items, respectively.
--
-- __Important notes:__
--
-- Replacements that come first in the list will be given precedence over later
-- items.
partitionOverlapping :: [RI.Replacement] -> ([RI.Replacement], [RI.Replacement])
partitionOverlapping [] = ([], [])
partitionOverlapping (r:rs) =
  -- partition current list using front element, recurse on the disjoints
  -- (r is always treated as disjoint, which gives the precedence)
  let (disjoint,     overlapping)     = partition (RI.areDisjoint r) rs
      (disjointRest, overlappingRest) = partitionOverlapping disjoint
  in  (r : disjointRest, overlapping <> overlappingRest)

-- | Apply a list of 'Replacement's to the orginal source file.
--
-- __Important notes:__
--
-- Source locations specified in replacements are 0-indexed.
--
-- Rewriting applies continuation lines when lines are longer than 72 characters.
--
-- __Example replacements:__
--
-- Delete the first character in a file
--
-- @ Replacement (SourceRange (SourceLocation 0 0) (SourceLocation 0 1)) "" @
--
-- Prepend "a" to 1 line, 2 column character
--
-- @ Replacement (SourceRange (SourceLocation 0 1) (SourceLocation 0 1)) "a" @
--
-- Replace a character located in 2 line, 4 column with "a"
--
-- @ Replacement (SourceRange (SourceLocation 1 3) (SourceLocation 1 4)) "a" @
--
-- Replace string starting in 2 line, 4 column and ending in 2 line, 6 column (inclusive) with "a"
--
-- @ Replacement (SourceRange (SourceLocation 1 3) (SourceLocation 1 6)) "a" @
--
-- @since 0.1.0.0
processReplacements :: RI.ReplacementMap -> IO ()
processReplacements rm = processReplacements_ $ M.toList rm

processReplacements_ :: [(String, [RI.Replacement])] -> IO ()
processReplacements_ = mapM_ go
  where
    go :: (String, [RI.Replacement]) -> IO ()
    go (filePath, repls) = do
      contents <- BC.readFile filePath
      let newContents  = RI.applyReplacements contents repls
      withTempDirectory (takeDirectory filePath) ('.' : takeFileName filePath) $ \tmpDir ->
        let tmpFile = tmpDir </> "tmp.f"
         in do putStrLn tmpFile
               BC.writeFile tmpFile newContents
               renameFile tmpFile filePath

-- | Utility function to convert 'SrcSpan' to 'SourceRange'
--
-- @since 0.1.13.7
spanToSourceRange :: SrcSpan -> RI.SourceRange
spanToSourceRange (SrcSpan start end) =
  let (l1, c1) = lineCol start
      (l2, c2) = lineCol end
  in  RI.SourceRange (RI.SourceLocation (l1 - 1) (c1 - 1))
                     (RI.SourceLocation (l2 - 1) c2)

-- | Given two 'Span's, returns a 'SourceRange' that starts at the starting
-- location of the first span, and ends at the starting location of the second
-- span
--
-- @since 0.1.17.2
spanToSourceRange2 :: SrcSpan -> SrcSpan -> RI.SourceRange
spanToSourceRange2 (SrcSpan start1 _) (SrcSpan start2 _) =
  let (l1, c1) = lineCol start1
      (l2, c2) = lineCol start2
  in  RI.SourceRange (RI.SourceLocation (l1 - 1) (c1 - 1))
                     (RI.SourceLocation (l2 - 1) (c2 - 1))

-- | Given two 'Span's, returns a 'SourceRange' that starts at the ending
-- location of the first span, and ends at the starting location of the second
-- span
--
-- @since 0.1.17.2
sourceRangeBetweenTwoSpans :: SrcSpan -> SrcSpan -> RI.SourceRange
sourceRangeBetweenTwoSpans (SrcSpan _ end1) (SrcSpan start2 _) =
  let (l1, c1) = lineCol end1
      (l2, c2) = lineCol start2
  in  RI.SourceRange (RI.SourceLocation (l1 - 1) c1)
                     (RI.SourceLocation (l2 - 1) (c2 - 1))

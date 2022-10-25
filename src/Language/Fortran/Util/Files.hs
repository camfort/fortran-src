module Language.Fortran.Util.Files
  ( flexReadFile
  , runCPP
  , getDirContents
  , rGetDirContents
  ) where

import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.ByteString.Char8      as B
import           System.Directory (listDirectory, canonicalizePath,
                                   doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (callProcess)
import           Data.List        ((\\), foldl')
import           Data.Char        (isNumber)
-- | Obtain a UTF-8 safe 'B.ByteString' representation of a file's contents.
--
-- Invalid UTF-8 is replaced with the space character.
flexReadFile :: FilePath -> IO B.ByteString
flexReadFile = fmap (T.encodeUtf8 . T.decodeUtf8With (T.replace ' ')) . B.readFile

-- | List files in directory, with the directory prepended to each entry.
getDirContents :: FilePath -> IO [FilePath]
getDirContents d = do
  d' <- canonicalizePath d
  map (d' </>) `fmap` listDirectory d'

-- | List files in directory recursively.
rGetDirContents :: FilePath -> IO [FilePath]
rGetDirContents d = canonicalizePath d >>= \d' -> go [d'] d'
  where
    go seen d'' = do
      ds <- getDirectoryContents d''
      fmap concat . mapM f $ ds \\ [".", ".."] -- remove '.' and '..' entries
        where
          f x = do
            path <- canonicalizePath $ d ++ "/" ++ x
            g <- doesDirectoryExist path
            if g && notElem path seen then do
              x' <- go (path : seen) path
              return $ map (\ y -> x ++ "/" ++ y) x'
            else return [x]

-- | Run the C Pre Processor over the file before reading into a bytestring
runCPP :: Maybe String -> FilePath -> IO B.ByteString
runCPP Nothing path          = flexReadFile path -- Nothing = do not run CPP
runCPP (Just cppOpts) path   = do
  -- Fold over the lines, skipping CPP pragmas and inserting blank
  -- lines as needed to make the line numbers match up for the current
  -- file. CPP pragmas for other files are just ignored.
  let processCPPLine :: ([B.ByteString], Int) -> B.ByteString -> ([B.ByteString], Int)
      processCPPLine (revLs, curLineNo) curLine
        | B.null curLine || B.head curLine /= '#' = (curLine:revLs, curLineNo + 1)
        | linePath /= path                        = (revLs, curLineNo)
        | newLineNo <= curLineNo                  = (revLs, curLineNo)
        | otherwise                               = (replicate (newLineNo - curLineNo) B.empty ++ revLs,
                                                     newLineNo)
          where
            newLineNo = read . B.unpack . B.takeWhile isNumber . B.drop 2 $ curLine
            linePath = B.unpack . B.takeWhile (/='"') . B.drop 1 . B.dropWhile (/='"') $ curLine

  withSystemTempDirectory "fortran-src" $ \ tmpdir -> do
    let outfile = tmpdir </> "cpp.out"
    callProcess "cpp" $ words cppOpts ++ ["-CC", "-nostdinc", "-o", outfile, path]
    contents <- flexReadFile outfile
    let ls = B.lines contents
    let ls' = reverse . fst $ foldl' processCPPLine ([], 1) ls
    return $ B.unlines ls'

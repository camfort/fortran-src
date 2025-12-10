module Language.Fortran.Util.Files
  ( flexReadFile
  , runCPP
  , getDirContents
  , rGetDirContents
  , expandDirs
  , listFortranFiles
  , listDirectoryRecursively
  ) where

import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.ByteString.Char8      as B
import           System.Directory (listDirectory, canonicalizePath,
                                   doesDirectoryExist, getDirectoryContents, doesFileExist)
import           System.FilePath  ((</>), takeExtension)
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (callProcess)
import           Data.List        ((\\), foldl')
import           Data.Char        (isNumber, toLower)
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
            path <- canonicalizePath $ d </> x
            g <- doesDirectoryExist path
            if g && notElem path seen then do
              x' <- go (path : seen) path
              return $ map (\ y -> x </> y) x'
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

-- | Expand all paths that are directories into a list of Fortran
-- files from a recursive directory listing.
expandDirs :: [FilePath] -> IO [FilePath]
expandDirs = fmap concat . mapM each
  where
    each path = do
      isDir <- doesDirectoryExist path
      if isDir
        then listFortranFiles path
        else pure [path]

-- | Get a list of Fortran files under the given directory.
listFortranFiles :: FilePath -> IO [FilePath]
listFortranFiles dir = filter isFortran <$> listDirectoryRecursively dir
  where
    -- | True if the file has a valid fortran extension.
    isFortran :: FilePath -> Bool
    isFortran x = map toLower (takeExtension x) `elem` exts
      where exts = [".f", ".f90", ".f77", ".f03"]

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = listDirectoryRec dir ""
  where
    listDirectoryRec :: FilePath -> FilePath -> IO [FilePath]
    listDirectoryRec d f = do
      let fullPath = d </> f
      isDir <- doesDirectoryExist fullPath
      if isDir
      then do
        conts <- listDirectory fullPath
        concat <$> mapM (listDirectoryRec fullPath) conts
      else pure [fullPath]

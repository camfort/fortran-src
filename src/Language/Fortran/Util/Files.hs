module Language.Fortran.Util.Files
  ( flexReadFile
  , getDirContents
  , rGetDirContents
  ) where

import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.ByteString.Char8      as B
import           System.Directory (listDirectory, canonicalizePath,
                                   doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))
import           Data.List        ((\\))

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

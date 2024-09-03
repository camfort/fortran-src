module Language.Fortran.Analysis.ModFileSpec (spec) where

import Test.Hspec
import TestUtil

import Language.Fortran.Util.ModFile
import Language.Fortran.Util.Files (expandDirs, flexReadFile)
import Language.Fortran.Version
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Language.Fortran.Parser as Parser
import qualified Data.ByteString.Char8 as B
import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.DataFlow

spec :: Spec
spec =
  describe "Modfiles" $
    it "Test module maps for a small package" $
      testModuleMaps

pParser :: String -> IO (ProgramFile (Analysis A0))
pParser name = do
  contents <- flexReadFile name
  let pf = Parser.byVerWithMods [] Fortran90 name contents
  case pf of
    Right pf -> return $  rename . analyseBBlocks . analyseRenames . initAnalysis $ pf
    Left err -> error $ "Error parsing " ++ name ++ ": " ++ show err

-- A simple test that checks that we correctly localise the declaration
-- of the variable `constant` to the leaf module, whilst understanding
-- in the `mid1` and `mid2` modules that it is an imported declaration.
testModuleMaps = do
    paths <- expandDirs ["test-data" </> "module"]
    -- parse all files into mod files
    pfs <- mapM (\p -> pParser p) paths
    let modFiles = map genModFile pfs
    -- get unique name to filemap
    let mmap = genUniqNameToFilenameMap modFiles
    -- check that `constant` is declared in leaf.f90
    let Just leaf = M.lookup "leaf_constant_1" mmap
    leaf `shouldBe` "test-data/module/leaf.f90"


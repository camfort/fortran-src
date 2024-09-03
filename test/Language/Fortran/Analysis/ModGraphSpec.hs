module Language.Fortran.Analysis.ModGraphSpec (spec) where

import Test.Hspec
import TestUtil

import Language.Fortran.Analysis.ModGraph
import Language.Fortran.Util.Files (expandDirs)
import Language.Fortran.Version
import System.FilePath ((</>))

spec :: Spec
spec =
  describe "Modgraph" $
    it "Dependency graph and topological sort on small package" $
      testDependencyList

-- A simple test on a simple module structure to check that
-- we are understanding this correctly (via the dependency graph
-- and then its topological sort).
testDependencyList = do
    paths' <- expandDirs ["test-data" </> "module"]
    mg <- genModGraph (Just Fortran90) ["."] Nothing paths'
    let list = modGraphToList mg
    -- we should have two possible orderings
    let files1 = ["leaf.f90", "mid1.f90", "mid2.f90", "top.f90"]
    let filesWithPaths1 = map (("test-data" </> "module") </>) files1
    -- or in a different order
    let files2 = ["leaf.f90", "mid2.f90", "mid1.f90", "top.f90"]
    let filesWithPaths2 = map (("test-data" </> "module") </>) files2
    shouldSatisfy list (\x -> x == filesWithPaths1 || x == filesWithPaths2)

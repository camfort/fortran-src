{-| Core definitions for post-parse transformations.

Transformations are arbitrary pure operations done on an analyzed 'ProgramFile'.
They are useful for defining AST passes in a somewhat self-contained manner.
However, they interact with the analysis awkwardly: some transformations need an
initial analysis, then edit the AST enough to require a second analysis.
Obviously problematic for efficiency!
-}

module Language.Fortran.Transformation.Monad
  ( getProgramFile
  , putProgramFile
  , modifyProgramFile
  , runTransform
  , Transform
  ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy hiding (state)
import Data.Data

import Language.Fortran.Analysis
import Language.Fortran.Analysis.Types
import Language.Fortran.Analysis.Renaming
import Language.Fortran.AST (ProgramFile)

data TransformationState a = TransformationState
  { transProgramFile :: ProgramFile (Analysis a) }

type Transform a = State (TransformationState a)

-- | Run a given transformation on the given 'ProgramFile'.
--
-- Note that this runs the analysis before running the transformation,
-- then throws this analysis away afterwards. It would be nice to instead run a
-- smaller analysis, or perhaps do efficient analyses in the transformations
-- themselves.
runTransform
    :: Data a
    => TypeEnv -> ModuleMap -> Transform a () -> ProgramFile a -> ProgramFile a
runTransform env mmap trans pf =
    stripAnalysis . transProgramFile . execState trans $ initState
  where
    (pf', _, _) = analyseTypesWithEnv env . analyseRenamesWithModuleMap mmap . initAnalysis $ pf
    initState = TransformationState
      { transProgramFile = pf' }

getProgramFile :: Transform a (ProgramFile (Analysis a))
getProgramFile = gets transProgramFile

putProgramFile :: ProgramFile (Analysis a) -> Transform a ()
putProgramFile pf = do
  state <- get
  put $ state { transProgramFile = pf }

modifyProgramFile :: (ProgramFile (Analysis a) -> ProgramFile (Analysis a)) -> Transform a ()
modifyProgramFile f = modify $ \ s -> s { transProgramFile = f (transProgramFile s) }

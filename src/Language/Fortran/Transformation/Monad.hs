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

runTransform
    :: Data a
    => TypeEnv -> ModuleMap -> Transform a () -> ProgramFile a -> ProgramFile a
runTransform env mmap trans pf =
    stripAnalysis . transProgramFile . execState trans $ initState
  where
    (pf', _) = analyseTypesWithEnv env . analyseRenamesWithModuleMap mmap . initAnalysis $ pf
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

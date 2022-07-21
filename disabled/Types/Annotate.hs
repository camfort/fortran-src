{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

{-| Annotate AST nodes with extra information using analysis output.

Though it's "annotation", we do real work 
-}

module Language.Fortran.Analysis.Types.Annotate where

import           Language.Fortran.AST
import           Language.Fortran.Analysis ( Analysis, puName )
import           Language.Fortran.Analysis.Types.Internal ( TypeEnv )
import           Language.Fortran.Analysis.Types.Util     ( setIDType )

import qualified Data.Map as Map

import           Data.Data
import           Data.Generics.Uniplate.Data ( transformBi )

-- | Annotate every 'Expression' and 'ProgramUnit' in a 'ProgramFile' using a
--   'TypeEnv'.
annotateProgramFile
    :: forall a
    .  Data a
    => TypeEnv
    -> ProgramFile (Analysis a)
    -> ProgramFile (Analysis a)
annotateProgramFile tenv =
    -- transformBi :: Biplate from to => (to -> to) -> from -> from
    -- if we tell 'transformBi' about @to@, it can figure out the rest
      transformBi @_ @(Expression  (Analysis a)) (annotateExpression  tenv)
    . transformBi @_ @(ProgramUnit (Analysis a)) (annotateProgramUnit tenv)

annotateProgramUnit
    :: Data a
    => TypeEnv
    -> ProgramUnit (Analysis a)
    -> ProgramUnit (Analysis a)
annotateProgramUnit tenv pu
  | Named n <- puName pu =
      case Map.lookup n tenv of
        Nothing  -> pu
        Just idt -> setIDType idt pu
  | otherwise = pu

{-
-- TODO here, we should parse values into a Repr.Value type, allowing us to
-- check for initial safety. we can't put them into the parameter map, but we
-- should be able to store their validated/strong repr value in the annotation!
-}
annotateExpression
    :: Data a => TypeEnv -> Expression (Analysis a) -> Expression (Analysis a)
annotateExpression tenv = error "TODO"
  --ExpValue _ _ v -> trySetIDType handleValue v
  --maybe e (`setIDType` e) `fmap` getRecordedType (varName e)

--trySetIDType :: Annotated ast => ast (Analysis a) -> Maybe IDType -> 

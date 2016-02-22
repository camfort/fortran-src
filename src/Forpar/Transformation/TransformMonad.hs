module Forpar.Transformation.TransformMonad ( getTypes
                                            , queryIDType
                                            , getProgramFile
                                            , putProgramFile
                                            , runTransform
                                            , Transform(..) ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import Data.Map (lookup, Map)

import Forpar.Analysis.Types
import Forpar.AST (ProgramFile)

data TransformationState = TransformationState 
  { transProgramFile :: ProgramFile ()
  , transTypes :: Maybe (Map TypeScope (Map String IDType)) }

type Transform = State TransformationState

runTransform :: Transform () -> ProgramFile () -> ProgramFile ()
runTransform trans pf = transProgramFile . execState trans $ initState
  where
    initState = TransformationState 
      { transProgramFile = pf 
      , transTypes = Nothing }

getProgramFile :: Transform (ProgramFile ())
getProgramFile = fmap transProgramFile get

putProgramFile :: ProgramFile () -> Transform ()
putProgramFile pf = do
  state <- get
  put $ state { transProgramFile = pf }

-- If types are requested and are not available automatically infer them.
getTypes :: Transform (Map TypeScope (Map String IDType))
getTypes = do
  mTypes <- fmap transTypes get
  case mTypes of
    Just m -> return m
    Nothing -> do
      state <- get
      let pf = transProgramFile state
      let typeMapping = inferTypes pf
      put $ state { transTypes = Just typeMapping }
      return typeMapping

queryIDType :: TypeScope -> String -> Map TypeScope (Map String IDType) -> Maybe IDType
queryIDType ts s mapping = do
  innerMapping <- lookup ts mapping
  lookup s innerMapping

queryIDTypeM :: TypeScope -> String -> Transform (Maybe IDType)
queryIDTypeM ts s = do
  mapping <- getTypes
  return $ queryIDType ts s mapping

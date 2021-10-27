-- TODO dislike having to use String
-- TODO unlike fortran-src, we're not using each expression's unique label.
-- would be good for efficiency
--
-- SymValMap has to be built up via Statements (parameter declarations,
-- assignments). Then can be used in expressions. fortran-src currently does
-- these the other way round -- unclear if swapping them will impact anything
-- (appears unlikely?)
--
-- The overall approach here is rewriting the fortran-vars eval story (some
-- constructors in SymbolTable, an Eval module) into a classy interface that
-- provides the eval function, which SymbolTable can implement, and we can
-- improve fortran-src to also do similar work.
--
-- F90 ISO spec is great for this. See pg.38.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Constants where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Repr.Value
import qualified Language.Fortran.Repr.Eval.Scalar  as Eval

import           Data.Data
import qualified Data.Map                   as Map
import           Data.Map                   ( Map )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Function              ( on )
import qualified Data.List                          as List
import           Data.Maybe                         ( catMaybes )

type ConstMap          = Map Name FValScalar
type IntrinsicsEvalMap = Map Name ()

data Error
  = ErrorParameterReassigned Name
  | ErrorEval Eval.Error
    deriving (Eq, Show)

-- | Gather labeled constants (PARAMETERS).
--
-- This is ideal for a "full" type analysis, since PARAMETERs are permitted in
-- some type information e.g. variables can use PARAMETERs for their kind
-- parameters.
--
-- The AST must have gone through a variable renaming pass. (That's the only use
-- of 'Analysis a' here.)
--
-- TODO: F90 ISO pg.193 init expr. Their definition appears a little wider than
-- params, but it's pretty much what we're going for?
gatherConsts
    :: (MonadReader IntrinsicsEvalMap m, Data a)
    => ProgramFile (Analysis a) -> m (Either Error ConstMap)
gatherConsts pf = do
    let decls = extractParamDecls (allStatements pf)
    (result, cm) <- runStateT (traverseM handleParamDecl decls) Map.empty
    case result of
      Left err -> return $ Left err
      Right _  -> return $ Right cm

extractParamDecls :: Data a => [Statement (Analysis a)] -> [Declarator (Analysis a)]
extractParamDecls = concat . catMaybes . map tryExtractParamDecl

tryExtractParamDecl :: Data a => Statement (Analysis a) -> Maybe [Declarator (Analysis a)]
tryExtractParamDecl = \case
  StParameter _ _ decls -> ret decls
  StDeclaration _ _ _ (Just attrs) decls ->
    case List.find (sameConstructor fakeAttrParam) (aStrip attrs) of
      Just _  -> ret decls
      Nothing -> Nothing
  _ -> Nothing
  where
    ret = Just . aStrip
    fakeAttrParam = AttrParameter undefined undefined

sameConstructor :: Data a => a -> a -> Bool
sameConstructor = (==) `on` toConstr

handleParamDecl
    :: (MonadState ConstMap m, MonadReader IntrinsicsEvalMap m, Data a)
    => Declarator (Analysis a) -> m (Either Error FValScalar)
handleParamDecl (Declarator _ _ varExpr declType _ mInitExpr) =
    case declType of
      ArrayDecl{} -> error "impossible parse: array declarator in parameter declarator list"
      ScalarDecl ->
        case mInitExpr of
          Nothing -> error "impossible parse: no init expr in parameter declarator"
          Just initExpr -> go initExpr
  where
    go initExpr = do
        evalEnv <- makeEvalEnv
        case Eval.eval evalEnv initExpr of
          Left  err -> return $ Left $ ErrorEval err
          Right val -> assignConst (varName varExpr) val >> return (Right val)
    makeEvalEnv = do
        cm <- get
        im <- ask
        return $ Eval.Env cm im

assignConst :: MonadState ConstMap m => Name -> FValScalar -> m (Either Error ())
assignConst var val = do
    cm <- get
    case Map.member var cm of
      True  -> return $ Left $ ErrorParameterReassigned var
      False -> do
        modify $ Map.insert var val
        return $ Right ()

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs

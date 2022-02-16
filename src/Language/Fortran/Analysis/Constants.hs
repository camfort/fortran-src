{- | Explicit constant evaluation.

Fortran has the concept of constants, which are defined at the start of a
procedure and not redefined during. These are confusingly referred to as
"parameters". This module holds definitions for evaluating these, intended to
run before the main analysis to build up a list of constants to use during it.
This is required, because using constants in type information -- specifically,
kinds and array dimensions -- is permitted.

Note that this isn't constant folding. We are evaluating constant expressions
(expressions required by the language specification to be constant). This module
Is concerned with traversing the AST to obtain the statements containing
constant declarations. Evaluation is handled in 'Language.Fortran.Repr.Eval'.

Fortran specs (at least F90 and F2008) restrict constants to scalars, not
arrays, so we do the same.
-}

{-
SymValMap has to be built up via Statements (parameter declarations,
assignments). Then can be used in expressions. fortran-src currently does these
the other way round -- unclear if swapping them will impact anything (appears
unlikely?)

TODO
  * unlike fortran-src, we're not using each expression's unique label.
    would be good for efficiency
  * could be safer, more efficient if we separated initial non-exec
    statements from following exec statements (since this analysis is only
    interested in those initial declarations)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fortran.Analysis.Constants where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Repr.Value
import           Language.Fortran.Repr.Value.Scalar
import           Language.Fortran.Repr.Value.Array
import qualified Language.Fortran.Repr.Eval as Eval

import           Data.Data
import qualified Data.Map                   as Map
import           Data.Map                   ( Map )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Function              ( on )
import qualified Data.List                          as List
import           Data.Maybe                         ( catMaybes )

type ConstMap = Map Name FValScalar
type OpMap    = Map Name (Eval.Op FVal)

data Error
  = ErrorParameterReassigned Name
  | ErrorEval Eval.Error
  | ErrorEvalReturnedArray FValArray
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
    :: (MonadReader OpMap m, Data a)
    => ProgramFile (Analysis a) -> m (Either Error ConstMap)
gatherConsts pf = do
    let decls = extractParamDecls (allStatements pf)
    (result, cm) <- runStateT (traverseM handleParamDecl decls) Map.empty
    case result of
      Left err -> return $ Left err
      Right _  -> return $ Right cm

extractParamDecls :: Data a => [Statement (Analysis a)] -> [Declarator (Analysis a)]
extractParamDecls = concat . catMaybes . map tryExtractParamDecl

-- shortcut via 'sameConstructor' using 'Data' instance
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
    :: (MonadState ConstMap m, MonadReader OpMap m, Data a)
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
        case Eval.evalExpr evalEnv initExpr of
          Left  err -> return $ Left $ ErrorEval err
          Right val ->
            case val of
              FValScalar sval -> assignConst (varName varExpr) sval >> return (Right sval)
              FValArray' aval -> return $ Left $ ErrorEvalReturnedArray aval
    makeEvalEnv = do
        scm <- get
        let cm = Map.map FValScalar scm
        ops <- ask
        return $ Eval.Env cm ops

assignConst :: MonadState ConstMap m => Name -> FValScalar -> m (Either Error ())
assignConst var val = do
    cm <- get
    case Map.member var cm of
      True  -> return $ Left $ ErrorParameterReassigned var
      False -> do
        modify $ Map.insert var val
        return $ Right ()

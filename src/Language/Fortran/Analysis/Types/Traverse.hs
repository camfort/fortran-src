module Language.Fortran.Analysis.Types.Traverse where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Analysis.Types.Util
import           Language.Fortran.Analysis.Types.Internal
import qualified Language.Fortran.Analysis.Types.Resolve    as Resolve
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.Repr.Type.Array
import           Language.Fortran.Repr.Value
import           Language.Fortran.Repr.Value.Scalar
import qualified Language.Fortran.Repr.Eval                 as Eval
import           Language.Fortran.Intrinsics

import           Data.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map                                   as Map
import qualified Data.List                                  as List

programUnit
    :: (MonadInfer m, Data a)
    => ProgramUnit (Analysis a)
    -> m ()
programUnit pu@(PUFunction _ _ mRetType _ _ _ mRetVar blocks _)
  | Named n <- puName pu   = do
    -- record some type information that we can glean
    recordCType CTFunction n
    case (mRetType, mRetVar) of
      (Just ts@(TypeSpec _ _ _ _), Just v) -> do
        tryResolveTypeVia Resolve.fromTypeSpec ts $ \ft -> do
          recordScalarType n           ft
          recordScalarType (varName v) ft
      (Just ts@(TypeSpec _ _ _ _), _)      -> do
        tryResolveTypeVia Resolve.fromTypeSpec ts $ \ft -> do
          recordScalarType n ft
      _                                        -> return ()
    -- record entry points for later annotation
    forM_ blocks $ \ block ->
      sequence_ [ recordEntryPoint n (varName v) (fmap varName mRetVar') | (StEntry _ _ v _ mRetVar') <- allStatements block ]
programUnit pu@(PUSubroutine _ _ _ _ _ blocks _) | Named n <- puName pu = do
  -- record the fact that this is a subroutine
  recordCType CTSubroutine n
  -- record entry points for later annotation
  forM_ blocks $ \ block ->
    sequence_ [ recordEntryPoint n (varName v) Nothing | (StEntry _ _ v _ _) <- allStatements block ]
programUnit _                                           = return ()

-- TODO needed? rewritten from @recordArrayDecl@
declarator :: (MonadState InferState m, Data a) => Declarator (Analysis a) -> m ()
declarator (Declarator _ _ v (Just ddAList) _ _) =
    recordCType (CTArray $ dimDeclarator ddAList) (varName v)
declarator _ = return ()

-- TODO const eval them, don't limit to ints!!
dimDeclarator :: AList DimensionDeclarator a -> [(Maybe Int, Maybe Int)]
dimDeclarator ddAList = [ (lb, ub) | DimensionDeclarator _ _ lbExp ubExp <- aStrip ddAList
                                   , let lb = do ExpValue _ _ (ValInteger i _) <- lbExp
                                                 return $ read i
                                   , let ub = do ExpValue _ _ (ValInteger i _) <- ubExp
                                                 return $ read i ]

intrinsicsExp
    :: (MonadInfer m, Data a)
    => Expression (Analysis a) -> m ()
intrinsicsExp = \case
  ExpSubscript _ _ nexp _    -> go nexp
  ExpFunctionCall _ _ nexp _ -> go nexp
  _ -> return ()
  where
    go nexp | isNamedExpression nexp = do
      itab <- asks inferConfigIntrinsics
      case getIntrinsicReturnType (srcName nexp) itab of
        Just _ -> do
          let n = varName nexp
          recordCType CTIntrinsic n
          -- recordBaseType _  n -- FIXME: going to skip base types for the moment
        _             -> return ()
    go _ = return ()

statement
    :: (MonadInfer m, Data a)
    => Statement (Analysis a) -> m ()
statement (StDeclaration _ _ ts mAttrAList declAList) = do
  mapM_ (handleDeclarator ts (aStrip' mAttrAList)) (aStrip declAList)
statement (StExternal _ _ varAList) = do
  let vars = aStrip varAList
  mapM_ (recordCType CTExternal . varName) vars
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  --  | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    mIDType <- getExprRecordedType v
    case mIDType of
      Just (IDType _ Just{} _) ->
        -- do nothing, it's already known to be an array
        return ()
      _                        ->
        -- assume it's a function statement
        recordCType CTFunction (varName v)

-- FIXME: if StFunctions can only be identified after types analysis
-- is complete and disambiguation is performed, then how do we get
-- them in the first place? (iterate until fixed point?)
statement (StFunction _ _ v _ _) = recordCType CTFunction (varName v)
-- (part of answer to above is) nullary function statement: foo() = ...
statement (StExpressionAssign _ _ (ExpFunctionCall _ _ v Nothing) _) = recordCType CTFunction (varName v)

statement (StDimension _ _ declAList) =
  forM_ (aStrip declAList) $ \(Declarator _ _ v declType _ _) ->
      case declType of
        ScalarDecl     -> return ()
        ArrayDecl dims -> tryRecordArrayInfo (varName v) dims

-- TODO
--statement (StStructure _ _ mName itemAList) = handleStructure mName itemAList

statement _ = return ()

-- TODO
handleDeclarator
    :: (MonadInfer m, Data a)
    => TypeSpec (Analysis a)
    -> [Attribute (Analysis a)]
    -> Declarator (Analysis a)
    -> m ()
handleDeclarator ts attrs (Declarator _ _ v decl mLenExpr _) =
    case (decl, List.find isAttrDimension attrs) of
      (ArrayDecl _dims1, Just (AttrDimension _ _ _dims2)) -> do
        error "strange parse: dimension info in both attribute and main declaration"
      (ArrayDecl dims, _) -> do
        tryRecordArrayInfo v' dims
        tryRecordScalarType v' mLenExpr ts
      (ScalarDecl, Just (AttrDimension _ _ dims)) -> do
        tryRecordArrayInfo v' dims
        tryRecordScalarType v' mLenExpr ts
      (ScalarDecl, _) -> do
        tryRecordScalarType v' mLenExpr ts
  where v' = varName v

-- TODO needed? rewritten from @recordArrayDecl@
declarator
    :: (MonadInfer m)
    => Declarator (Analysis a) -> m ()
declarator (Declarator _ _ss v declType _ _) =
    case declType of
      ScalarDecl -> return ()
      ArrayDecl dims -> tryRecordArrayInfo (varName v) dims

-- | Try to resolve the scalar type information for a variable and record if
--   successful.
tryRecordScalarType
    :: (MonadInfer m, Data a)
    => Name -> Maybe (Expression (Analysis a)) -> TypeSpec (Analysis a)
    -> m ()
tryRecordScalarType v mDeclExpr ts = do
    tryResolveTypeVia (Resolve.fromDeclaration mDeclExpr) ts $ \sty -> recordType v (FType sty Nothing)

-- | Try to resolve an array variable's shape. Regardless of if successful or
--   not, record to explicit-shape.
--
-- Arrays are annoying and complex, and our analysis isn't suited to properly
-- identifying the different shapes. Also, we should only be finding minimal
-- "shape type" here, because all but 1 shape type aren't constant (and neither
-- are the dimensions). So we can't return any warnings on error, because the
-- array might just be (legally) sized using function arguments or something.
--
-- We take a AList for convenience because it wraps a SrcSpan.
tryRecordArrayInfo
    :: (MonadInfer m)
    => Name -> (AList DimensionDeclarator a)
    -> m ()
tryRecordArrayInfo v dims =
    Resolve.dimensions (aStrip dims) >>=
        \mds -> recordArrayInfo v (ExplicitShape mds)

--------------------------------------------------------------------------------

-- Wrapper around type resolution functions to use the 'Spanned' instance from
-- the 'TypeSpec' you pass. (Generalized to all 'Spanned' because why not.)
tryResolveTypeVia
    :: (MonadState InferState m, Spanned a)
    => (a -> m (Either Resolve.Error FTypeScalar))
    -> a
    -> (FTypeScalar -> m ())
    -> m ()
tryResolveTypeVia fResolve a useType =
    fResolve a >>= \case
      Left err -> typeError ("error while deriving a type: " <> show err) (getSpan a)
      Right ft -> useType ft

--------------------------------------------------------------------------------

{-
-- | Create a structure env from the list of fields and add it to the InferState
handleStructure
    :: (MonadInfer m, Data a)
    => Maybe String -> AList StructureItem (Analysis a) -> m ()
handleStructure mName itemAList = do
  case mName of
    Just n -> do
      structEnv <- foldM handleStructureItem Map.empty (aStrip itemAList)
      recordStruct structEnv n
    Nothing -> pure ()

handleStructureItem
    :: (MonadInfer m, Data a)
    => StructMemberTypeEnv -> StructureItem (Analysis a) -> m StructMemberTypeEnv
handleStructureItem mt (StructFields _ _ ts mAttrAList declAList) = do
  ds <- handleDeclaration ts (aStrip' mAttrAList) (aStrip declAList)
  pure $ List.foldl' (\m (n, idt) -> Map.insert n idt m) mt ds
-- TODO: These should eventually be implemented
handleStructureItem mt StructUnion{} = pure mt
handleStructureItem mt StructStructure{} = pure mt
-}

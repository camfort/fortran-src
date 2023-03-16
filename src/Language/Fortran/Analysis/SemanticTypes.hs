{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Analysis.SemanticTypes
  ( module Language.Fortran.Analysis.SemanticTypes
  , module Language.Fortran.Common.Array
  ) where

import Language.Fortran.Common.Array

import           Data.Data                      ( Data )
import           Control.DeepSeq                ( NFData )
import           GHC.Generics                   ( Generic )
import           Language.Fortran.AST           ( BaseType(..)
                                                , Expression(..)
                                                , Value(..)
                                                , TypeSpec(..)
                                                , Selector(..) )
import           Language.Fortran.Util.Position ( SrcSpan(..) )
import           Language.Fortran.Version       ( FortranVersion(..) )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )
import           Language.Fortran.PrettyPrint   ( Pretty(..) )
import qualified Text.PrettyPrint as Pretty

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

type Kind = Int

-- | Semantic type assigned to variables.
--
-- 'BaseType' stores the "type tag" given in syntax. 'SemType's add metadata
-- (kind and length), and resolve some "simple" types to a core type with a
-- preset kind (e.g. `DOUBLE PRECISION` -> `REAL(8)`).
--
-- Fortran 90 (and beyond) features may not be well supported.
data SemType
  = TInteger Kind
  | TReal Kind
  | TComplex Kind
  | TLogical Kind
  | TByte Kind
  | TCharacter CharacterLen Kind

  | TArray SemType Dimensions
  -- ^ A Fortran array type is represented by a type and a set of dimensions.

  | TCustom String
  -- ^ Constructor to use for F77 structures, F90 DDTs

    deriving stock    (Ord, Eq, Show, Data, Generic)
    deriving anyclass (NFData, Binary, Out)

type Dimensions = Dims NonEmpty Int

-- TODO placeholder, not final or tested
-- should really attempt to print with kind info, and change to DOUBLE PRECISION
-- etc. for <F90. Maybe cheat, use 'recoverSemTypeTypeSpec' and print resulting
-- TypeSpec?
instance Pretty SemType where
  pprint' v = \case
    TInteger k -> "integer"<>pd k
    TReal    k -> "real"<>pd k
    TComplex k -> "complex"<>pd k
    TLogical k -> "logical"<>pd k
    TByte    k -> "byte"<>pd k
    TCharacter _ _ -> "character"
    TArray st dims -> pprint' v st <> pprint' v dims
    TCustom str -> pprint' v (TypeCustom str)
    where
      pd = Pretty.parens . doc

-- | Convert 'Dimensions' data type to its previous type synonym
--   @(Maybe [(Int, Int)])@.
--
-- Will not return @Just []@.
dimensionsToTuples :: Dimensions -> Maybe [(Int, Int)]
dimensionsToTuples = \case
  DimsExplicitShape ds     ->
    Just $ NonEmpty.toList $ fmap (\(Dim lb ub) -> (lb, ub)) ds
  DimsAssumedSize   _ds _d -> Nothing
  DimsAssumedShape  _ss    -> Nothing

--------------------------------------------------------------------------------

data CharacterLen = CharLenStar    -- ^ specified with a *
                  | CharLenColon   -- ^ specified with a : (Fortran2003)
                    -- FIXME, possibly, with a more robust const-exp:
                  | CharLenExp     -- ^ specified with a non-trivial expression
                  | CharLenInt Int -- ^ specified with a constant integer
    deriving stock    (Ord, Eq, Show, Data, Generic)
    deriving anyclass (NFData, Binary, Out)

charLenSelector :: Maybe (Selector a) -> (Maybe CharacterLen, Maybe String)
charLenSelector Nothing                          = (Nothing, Nothing)
charLenSelector (Just (Selector _ _ mlen mkind)) = (l, k)
  where
    l = charLenSelector' <$> mlen
    k | Just (ExpValue _ _ (ValInteger i _)) <- mkind  = Just i
      | Just (ExpValue _ _ (ValVariable s)) <- mkind = Just s
      -- FIXME: some references refer to things like kind=kanji but I can't find any spec for it
      | otherwise                                    = Nothing

charLenSelector' :: Expression a -> CharacterLen
charLenSelector' = \case
  ExpValue _ _ ValStar        -> CharLenStar
  ExpValue _ _ ValColon       -> CharLenColon
  ExpValue _ _ (ValInteger i _) -> CharLenInt (read i)
  _                           -> CharLenExp

-- | Attempt to recover the 'Value' that generated the given 'CharacterLen'.
charLenToValue :: CharacterLen -> Maybe (Value a)
charLenToValue = \case
  CharLenStar  -> Just ValStar
  CharLenColon -> Just ValColon
  CharLenInt i -> Just (ValInteger (show i) Nothing)
  CharLenExp   -> Nothing

getTypeKind :: SemType -> Kind
getTypeKind = \case
  TInteger   k -> k
  TReal      k -> k
  TComplex   k -> k
  TLogical   k -> k
  TByte      k -> k
  TCharacter _ k -> k
  TCustom    _ -> error "TCustom does not have a kind"
  TArray t _   -> getTypeKind t

setTypeKind :: SemType -> Kind -> SemType
setTypeKind st k = case st of
  TInteger   _ -> TInteger   k
  TReal      _ -> TReal      k
  TComplex   _ -> TComplex   k
  TLogical   _ -> TLogical   k
  TByte      _ -> TByte      k
  TCharacter charLen _ -> TCharacter charLen k
  TCustom    _ -> error "can't set kind of TCustom"
  TArray _ _   -> error "can't set kind of TArray"

charLenConcat :: CharacterLen -> CharacterLen -> CharacterLen
charLenConcat l1 l2 = case (l1, l2) of
  (CharLenExp    , _             ) -> CharLenExp
  (_             , CharLenExp    ) -> CharLenExp
  (CharLenStar   , _             ) -> CharLenStar
  (_             , CharLenStar   ) -> CharLenStar
  (CharLenColon  , _             ) -> CharLenColon
  (_             , CharLenColon  ) -> CharLenColon
  (CharLenInt i1 , CharLenInt i2 ) -> CharLenInt (i1 + i2)

-- | Recover the most appropriate 'TypeSpec' for the given 'SemType', depending
--   on the given 'FortranVersion'.
--
-- Kinds weren't formalized as a syntactic feature until Fortran 90, so we ask
-- for a context. If possible (>=F90), we prefer the more explicit
-- representation e.g. @REAL(8)@. For older versions, for specific type-kind
-- combinations, @DOUBLE PRECISION@ and @DOUBLE COMPLEX@ are used instead.
-- However, we otherwise don't shy away from adding kind info regardless of
-- theoretical version support.
--
-- Array types don't work properly, due to array type info being in a parent
-- node that holds individual elements.
recoverSemTypeTypeSpec :: forall a. a -> SrcSpan
                       -> FortranVersion -> SemType -> TypeSpec a
recoverSemTypeTypeSpec a ss v = \case
  TInteger k -> wrapBaseAndKind TypeInteger k
  TLogical k -> wrapBaseAndKind TypeLogical k
  TByte    k -> wrapBaseAndKind TypeByte k

  TCustom str -> ts (TypeCustom str) Nothing

  TArray     st  _   -> recoverSemTypeTypeSpec a ss v st

  TReal    k ->
      if k == 8 && v < Fortran90
    then ts TypeDoublePrecision Nothing
    else wrapBaseAndKind TypeReal k
  TComplex k ->
      if k == 16 && v < Fortran90
    then ts TypeDoubleComplex Nothing
    else wrapBaseAndKind TypeComplex k

  TCharacter len k   ->
    -- TODO can improve, use no selector if len=1, kind=1
    -- only include kind if != 1
    let sel = Selector a ss (ExpValue a ss <$> charLenToValue len) (if k == 1 then Nothing else Just (intValExpr k))
     in ts TypeCharacter (Just sel)

  where
    ts = TypeSpec a ss
    intValExpr :: Int -> Expression a
    intValExpr x = ExpValue a ss (ValInteger (show x) Nothing)

    -- | Wraps 'BaseType' and 'Kind' into 'TypeSpec'. If the kind is the
    --   'BaseType''s default kind, it is omitted.
    wrapBaseAndKind :: BaseType -> Kind -> TypeSpec a
    wrapBaseAndKind bt k = ts bt sel
      where
        sel =   if k == kindOfBaseType bt
              then Nothing
              else Just $ Selector a ss Nothing (Just (intValExpr k))

--------------------------------------------------------------------------------

-- | Given a 'BaseType' infer the "default" kind (or size of the
-- variable in memory).
--
-- Useful when you need a default kind, but gives you an unwrapped type.
-- Consider using Analysis.deriveSemTypeFromBaseType also.
--
-- Further documentation:
-- https://docs.oracle.com/cd/E19957-01/805-4939/c400041360f5/index.html
kindOfBaseType :: BaseType -> Int
kindOfBaseType = \case
  TypeInteger         -> 4
  TypeReal            -> 4
  TypeDoublePrecision -> 8
  TypeComplex         -> 8
  TypeDoubleComplex   -> 16
  TypeLogical         -> 4
  TypeCharacter{}     -> 1
  TypeByte            -> 1

  -- arbitrary values (>F77 is not tested/used)
  TypeCustom{}        -> 1
  ClassStar           -> 1
  ClassCustom{}       -> 1

getTypeSize :: SemType -> Maybe Int
getTypeSize = \case
  TInteger      k   -> Just k
  TReal         k   -> Just k
  TComplex      k   -> Just k
  TLogical      k   -> Just k
  TByte         k   -> Just k
  TArray     ty _   -> getTypeSize ty
  TCustom       _   -> Just 1
  -- char: treat length as "kind" (but also use recorded kind)
  TCharacter (CharLenInt l) k -> Just (l * k)
  TCharacter _              _ -> Nothing

setTypeSize :: SemType -> Maybe Int -> SemType
setTypeSize ty mk = case (mk, ty) of
  (Just k, TInteger _  ) -> TInteger k
  (Just k, TReal _     ) -> TReal k
  (Just k, TComplex _  ) -> TComplex k
  (Just k, TLogical _  ) -> TLogical k
  (Just k, TByte _     ) -> TByte k
  (_     , TCustom s   ) -> TCustom s
  -- char: treat length as "kind"
  (Just l, TCharacter _ k) ->
    TCharacter (CharLenInt l) k
  (Nothing, TCharacter _ k) ->
    TCharacter CharLenStar k
  _ -> error $ "Tried to set invalid kind for type " <> show ty

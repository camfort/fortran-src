module Language.Fortran.AST.AList where

import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter
import Language.Fortran.Util.Position (Spanned, SrcSpan(..), getSpan)
import Language.Fortran.AST.Annotated ( Annotated )

import Data.Data    (Data)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Text.PrettyPrint.GenericPretty (Out)

-- | A location-tagged list of @t a@s (@t@ decorated with an @a@ annotation).
--
-- The AST is polymorphic on some type @a@, which is used for arbitrary
-- annotations. Since many AST nodes use lists (e.g. executable statements,
-- declarations), we define a dedicated annotated list type to reuse.
--
-- Note that the list itself also holds an @a@ annotation.
data AList t a = AList
  { alistAnno :: a
  , alistSpan :: SrcSpan
  , alistList :: [t a]
  } deriving stock (Eq, Show, Data, Generic)

instance Functor t => Functor (AList t) where
  fmap f (AList a s xs) = AList (f a) s (map (fmap f) xs)

instance FirstParameter (AList t a) a
instance SecondParameter (AList t a) SrcSpan
instance Annotated (AList t)
instance Spanned (AList t a)
instance (Out a, Out (t a)) => Out (AList t a)
instance (NFData a, NFData (t a)) => NFData (AList t a)

-- | Convert a non-empty list to an 'AList'.
fromList :: Spanned (t a) => a -> [ t a ] -> AList t a
fromList a xs = AList a (getSpan xs) xs

-- | Convert a list to an 'AList', returning Nothing iff the list is empty.
fromList' :: Spanned (t a) => a -> [ t a ] -> Maybe (AList t a)
fromList' _ [] = Nothing
fromList' a xs = Just $ fromList a xs

fromReverseList :: Spanned (t ()) => [ t () ] -> AList t ()
fromReverseList = fromList () . reverse

fromReverseList' :: Spanned (t ()) => [ t () ] -> Maybe (AList t ())
fromReverseList' = fromList' () . reverse

aCons :: t a -> AList t a -> AList t a
aCons x (AList a s xs) = AList a s $ x:xs

infixr 5 `aCons`

aEmpty :: a -> SrcSpan -> AList t a
aEmpty a s = AList a s []

aReverse :: AList t a -> AList t a
aReverse (AList a s xs) = AList a s $ reverse xs

aStrip :: AList t a -> [t a]
aStrip (AList _ _ l) = l

aStrip' :: Maybe (AList t a) -> [t a]
aStrip' Nothing = []
aStrip' (Just a) = aStrip a

aMap :: (t a -> r a) -> AList t a -> AList r a
aMap f (AList a s xs) = AList a s (map f xs)

--------------------------------------------------------------------------------

data ATuple t1 t2 a = ATuple
  { atupleAnno :: a
  , atupleSpan :: SrcSpan
  , atupleFst  :: t1 a
  , atupleSnd  :: t2 a
  } deriving stock (Eq, Show, Data, Generic, Functor)

instance FirstParameter (ATuple t1 t2 a) a
instance SecondParameter (ATuple t1 t2 a) SrcSpan
instance Spanned (ATuple t1 t2 a)
instance (Out a, Out (t1 a), Out (t2 a)) => Out (ATuple t1 t2 a)
instance (NFData a, NFData (t1 a), NFData (t2 a)) => NFData (ATuple t1 t2 a)

--------------------------------------------------------------------------------

{-

see issue #231

data AListX ext t a = AListX
  { alistxAnno :: a
  , alistxSpan :: SrcSpan
  , alistxList :: [t a]
  , alistxExt  :: ext
  } deriving stock (Eq, Show, Data, Generic)

instance Functor t => Functor (AListX ext t) where
  fmap f (AListX a s xs ext) = AListX (f a) s (map (fmap f) xs) ext

instance FirstParameter (AListX ext t a) a
instance SecondParameter (AListX ext t a) SrcSpan
instance Annotated (AListX ext t)
instance Spanned (AListX ext t a)
instance (Out a, Out (t a), Out ext) => Out (AListX ext t a)
instance (NFData a, NFData (t a), NFData ext) => NFData (AListX ext t a)

data Brackets = Brackets | OmitBrackets
    deriving stock    (Eq, Show, Data, Generic)
    deriving anyclass (NFData, Out)

-}

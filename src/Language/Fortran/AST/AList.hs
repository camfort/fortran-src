module Language.Fortran.AST.AList where

import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter
import Language.Fortran.Util.Position (Spanned, SrcSpan(..), getSpan)

import Data.Data    (Data, Typeable)
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
data AList t a = AList a SrcSpan [t a] deriving (Eq, Show, Data, Typeable, Generic)
instance Functor t => Functor (AList t) where
  fmap f (AList a s xs) = AList (f a) s (map (fmap f) xs)

instance FirstParameter (AList t a) a
instance SecondParameter (AList t a) SrcSpan
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

data ATuple t1 t2 a = ATuple a SrcSpan (t1 a) (t2 a)
    deriving (Eq, Show, Data, Typeable, Generic, Functor)

instance FirstParameter (ATuple t1 t2 a) a
instance SecondParameter (ATuple t1 t2 a) SrcSpan
instance Spanned (ATuple t1 t2 a)
instance (Out a, Out (t1 a), Out (t2 a)) => Out (ATuple t1 t2 a)
instance (NFData a, NFData (t1 a), NFData (t2 a)) => NFData (ATuple t1 t2 a)

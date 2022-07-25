module Language.Fortran.Repr.Value.Scalar.String where

import GHC.TypeNats
import Data.Text ( Text )
import qualified Data.Text as Text
import Language.Fortran.Repr.Util ( natVal'' )
import Data.Proxy
import Unsafe.Coerce

-- TODO unsafe constructor do not use >:(
-- need context for Reasons(TM)
data FString (l :: Natural) = KnownNat l => FString Text
deriving stock instance Show (FString l)
deriving stock instance Eq   (FString l)
deriving stock instance Ord  (FString l) -- TODO

fString :: forall l. KnownNat l => Text -> Maybe (FString l)
fString s =
    if   Text.length s == fromIntegral (natVal'' @l)
    then Just $ FString s
    else Nothing

fStringLen :: forall l. KnownNat l => FString l -> Natural
fStringLen _ = natVal'' @l

data SomeFString = forall (l :: Natural). KnownNat l => SomeFString (FString l)
deriving stock instance Show SomeFString
instance Eq SomeFString where
    (SomeFString (FString sl)) == (SomeFString (FString sr)) = sl == sr

someFString :: Text -> SomeFString
someFString s =
    case someNatVal (fromIntegral (Text.length s)) of
      SomeNat (_ :: Proxy n) -> SomeFString $ FString @n s

someFStringLen :: SomeFString -> Natural
someFStringLen (SomeFString s) = fStringLen s

-- TODO dunno how to do this without unsafeCoerce because of the type-level nat
-- addition >:( -- oh actually seems this is an expected usage of it. ok
concatFString
    :: forall ll lr. (KnownNat ll, KnownNat lr)
    => FString ll
    -> FString lr
    -> FString (ll + lr)
concatFString (FString sl) (FString sr) =
    unsafeCoerce $ FString @ll $ sl <> sr

concatSomeFString :: SomeFString -> SomeFString -> SomeFString
concatSomeFString (SomeFString l) (SomeFString r) =
    case concatFString l r of s@FString{} -> SomeFString s

fStringBOp :: (Text -> Text -> r) -> FString ll -> FString lr -> r
fStringBOp f (FString l) (FString r) = f l r

someFStringBOp :: (Text -> Text -> r) -> SomeFString -> SomeFString -> r
someFStringBOp f (SomeFString l) (SomeFString r) = fStringBOp f l r

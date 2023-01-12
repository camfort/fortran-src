{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}

{- | Fortran CHAR value representation.

Currently only CHARs of known length.
-}

module Language.Fortran.Repr.Value.Scalar.String where

import GHC.TypeNats
import Language.Fortran.Repr.Compat.Natural
import Data.Text ( Text )
import qualified Data.Text as Text
import Language.Fortran.Repr.Util ( natVal'' )
import Data.Proxy
import Unsafe.Coerce

import Text.PrettyPrint.GenericPretty ( Out )
import Text.PrettyPrint.GenericPretty.ViaShow ( OutShowly(..) )
import Data.Binary
import Data.Data

import Data.Singletons
import GHC.TypeLits.Singletons

-- TODO unsafe constructor do not use >:(
-- need context for Reasons(TM)
data FString (l :: NaturalK) = KnownNat l => FString Text
deriving stock instance Show (FString l)
deriving stock instance Eq   (FString l)
deriving stock instance Ord  (FString l) -- TODO
deriving stock instance KnownNat l => Data (FString l)

{-
instance Data (FString l) where
    --gunfold k z c = k (z (\x -> case someFString x of SomeFString y -> y))
    gunfold k z c = k (z (FString @l))
-}

eqFString :: FString l -> FString r -> Bool
eqFString (FString l) (FString r) = l == r

-- | This is a painful instance to define. We cheat by leveraging the instance
--   of the length-hiding type 'SomeFString', then asserting length. It's CPU
--   and memory inefficient and has backwards dependencies, but is comfortably
--   safe.
instance KnownNat l => Binary (FString l) where
    put t = put (SomeFString t)
    get =
        get @SomeFString >>= \case
          SomeFString (FString t) ->
            case fString @l t of
              Just t' -> pure t'
              Nothing -> fail "FString had incorrect length"

-- | Attempt to a 'Text' into an 'FString' of the given length.
fString :: forall l. KnownNat l => Text -> Maybe (FString l)
fString s =
    if   Text.length s == fromIntegral (natVal'' @l)
    then Just $ FString s
    else Nothing

fStringLen :: forall l. KnownNat l => FString l -> Natural
fStringLen _ = natVal'' @l

data SomeFString = forall (l :: NaturalK). KnownNat l => SomeFString (FString l)
deriving stock instance Show SomeFString
deriving via (OutShowly SomeFString) instance Out SomeFString

instance Eq SomeFString where
    (SomeFString l) == (SomeFString r) = l `eqFString` r

-- TODO impossible??
instance Data SomeFString where

{-
dataSomeFStringT = mkDataType "TODO" [dataSomeFStringC1]
dataSomeFStringC1 = mkConstr dataSomeFStringT "SomeFString" [] Prefix
instance Data SomeFString where
    dataTypeOf _ = dataSomeFStringT
    toConstr = \case
      SomeFString{} -> dataSomeFStringC1
    --gunfold k z c = k (z SomeFString)
    gunfold k z c = k (z (\(FString fstr :: FString l) -> SomeFString @l (FString fstr)))
-}

instance Binary SomeFString where
    put (SomeFString (FString t)) = put t
    get = someFString <$> get @Text

-- | Lift a 'Text' into 'SomeFString'.
someFString :: Text -> SomeFString
someFString t =
    case someNatVal (fromIntegral (Text.length t)) of
      SomeNat (_ :: Proxy l) -> SomeFString $ FString @l t

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

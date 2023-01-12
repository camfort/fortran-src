{- | Low-boilerplate 'Text.PrettyPrint.GenericPretty.Out' instances for
    'Show'ables using @DerivingVia@.

Useful for integrating types that don't work nicely with 'Generic' with
@GenericPretty@. (Really, there should be a class like
'Text.PrettyPrint.GenericPretty.Out' directly in @pretty@, but alas.)

Use as follows:

data EeGadts a where
    C1 :: EeGadts Bool
    C2 :: EeGadts String
deriving stock instance Show (EeGadts a)
deriving via OutShowly (EeGadts a) instance Out (EeGadts a)
-}

{-# LANGUAGE DerivingVia #-}

module Text.PrettyPrint.GenericPretty.ViaShow
  ( module Text.PrettyPrint.GenericPretty.ViaShow
  , Text.PrettyPrint.GenericPretty.Out
  ) where

import Text.PrettyPrint.GenericPretty ( Out(..) )
import qualified Text.PrettyPrint

newtype OutShowly a = OutShowly { unOutShowly :: a }

instance Show a => Out (OutShowly a) where
    doc (OutShowly a) = Text.PrettyPrint.text $ show a
    docPrec n (OutShowly a) = Text.PrettyPrint.text $ showsPrec n a ""

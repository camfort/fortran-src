module Language.Fortran.Generate where

import Language.Fortran.AST
import Test.QuickCheck

import Language.Fortran.Util.Position
import Language.Fortran.PrettyPrint
import Language.Fortran.Version

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- instance Gen SrcSpan where
--   arbitrary = do
--     start <- arbitrary
--     end   <- arbitrary
--     return $ SrcSpan start end

-- instance Gen Position where
--   arbitrary = do
--     absOffset <- arbitrary
--     col       <- arbitrary
--     line      <- arbitrary
--     filePath  <- arbitrary
--     pragmaOffset <- arbitrary
--     return $ Position absOffset col line filePath pragmaOffset

instance Arbitrary a => Arbitrary (Value a) where
  arbitrary = oneof
    [ do x <- arbitrary :: Gen Integer
         pure $ ValInteger (show x) Nothing
    , do s <- arbitrary :: Gen String
         pure $ ValString s
    , ValLogical <$> arbitrary <*> pure Nothing
    , pure $ ValVariable "myVar"
    ]

instance Arbitrary BaseType where
  arbitrary = oneof
    [ pure TypeInteger
    , pure TypeReal
    , pure TypeLogical
    , pure TypeCharacter
    ]

instance Arbitrary a => Arbitrary (TypeSpec a) where
  arbitrary = do
    annotation <- arbitrary
    base       <- arbitrary
    selector   <- arbitrary
    pure $ TypeSpec annotation nullSpan base selector

instance Arbitrary a => Arbitrary (Selector a) where
  arbitrary = do 
    annotation <- arbitrary
    -- Positive length
    len    :: Integer <- abs <$> arbitrary
    -- Kind, powers of two
    kind   :: Integer <- elements [1,2,4,8]
    -- Wrap into expressions, with a chance of being Nothing
    let kindExpr = ExpValue annotation nullSpan (ValInteger (show kind) Nothing)
    lenExprM   <- maybeWrapper (pure (ExpValue annotation nullSpan (ValInteger (show len) Nothing)))
    kindExprM  <- 
       case lenExprM of 
         Nothing -> return $ Just kindExpr  -- If no length, always include kind (cannot have nothing for both)
         Just{}  -> maybeWrapper (pure kindExpr)
    pure $ Selector annotation nullSpan lenExprM kindExprM

maybeWrapper :: Gen a -> Gen (Maybe a)
maybeWrapper gen = oneof [pure Nothing, Just <$> gen]


nullSpan :: SrcSpan
nullSpan = SrcSpan initPosition initPosition

--------------------------------------------------------------------------------
-- Stateful generation
--------------------------------------------------------------------------------

-- | Environment mapping variable names to their declared types.
type Env = Map Name (TypeSpec A0)

-- | Stateful generator: a 'Gen' action that can read/write an 'Env'.
type GenM a = StateT Env Gen a

-- | Lift a plain 'Gen' action into 'GenM'.
liftGen :: Gen a -> GenM a
liftGen = lift


-- Generate a list of 10 values and pretty print
-- the results
demo :: IO ()
demo = do
  values :: [Value ()] <- generate $ vectorOf 10 arbitrary
  let prettyValues = map (pprint' Fortran90) values
  mapM_ (putStrLn . render) prettyValues
  putStrLn $ "Generated " ++ show (length values) ++ " values."

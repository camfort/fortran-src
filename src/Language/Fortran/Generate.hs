module Language.Fortran.Generate where

import Language.Fortran.AST
import Test.QuickCheck

import Language.Fortran.Util.Position
import Language.Fortran.PrettyPrint
import Language.Fortran.Version

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ

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

-- Generate a list of 10 values and pretty print
-- the results
demo :: IO ()
demo = do
  values :: [Value ()] <- generate $ vectorOf 10 arbitrary
  let prettyValues = map (pprint' Fortran90) values
  mapM_ (putStrLn . render) prettyValues
  putStrLn $ "Generated " ++ show (length values) ++ " values."

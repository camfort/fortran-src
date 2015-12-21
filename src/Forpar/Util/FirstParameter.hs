{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Forpar.Util.FirstParameter(FirstParameter(..), GFirstParameter(..)) where

import GHC.Generics

class FirstParameter a e | a -> e where
  getFirstParameter :: a -> e 

  default getFirstParameter :: (Generic a, GFirstParameter (Rep a) e) => a -> e 
  getFirstParameter a = getFirstParameter' . from $ a 

class GFirstParameter f e where
  getFirstParameter' :: f a -> e

instance {-# OVERLAPPING #-} GFirstParameter (K1 i e) e where
  getFirstParameter' (K1 a) = a

instance {-# OVERLAPPABLE #-} GFirstParameter (K1 i a) e where
  getFirstParameter' _ = undefined

instance GFirstParameter a e => GFirstParameter (M1 i c a) e where
  getFirstParameter' (M1 a) = getFirstParameter' a

instance (GFirstParameter a e, GFirstParameter b e) => GFirstParameter (a :+: b) e where
  getFirstParameter' (L1 a) = getFirstParameter' a
  getFirstParameter' (R1 a) = getFirstParameter' a

instance (GFirstParameter a e, GFirstParameter b e) => GFirstParameter (a :*: b) e where
  getFirstParameter' (a :*: _) = getFirstParameter' a

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Forpar.Util.SecondParameter(SecondParameter(..)) where

import GHC.Generics

class SecondParameter a e | a -> e where
  getSecondParameter :: a -> e

  default getSecondParameter :: (Generic a, GSecondParameter (Rep a) e) => a -> e
  getSecondParameter a = getSecondParameter' . from $ a

class GSecondParameter f e where
  getSecondParameter' :: f a -> e

instance GSecondParameter (K1 i a) e where
  getSecondParameter' _ = undefined

instance GSecondParameter a e => GSecondParameter (M1 i c a) e where
  getSecondParameter' (M1 x) = getSecondParameter' x

instance (GSecondParameter a e, GSecondParameter b e) => GSecondParameter (a :+: b) e where
  getSecondParameter' (L1 a) = getSecondParameter' a
  getSecondParameter' (R1 a) = getSecondParameter' a

instance (ParameterLeaf a, GSecondParameter a e, GSecondParameter' b e) => GSecondParameter (a :*: b) e where
  getSecondParameter' (a :*: b) = 
    if isLeaf a 
    then getSecondParameter'' b
    else getSecondParameter' a

class GSecondParameter' f e where
  getSecondParameter'' :: f a -> e

instance GSecondParameter' a e => GSecondParameter' (M1 i c a) e where
  getSecondParameter'' (M1 a) = getSecondParameter'' a

instance GSecondParameter' a e => GSecondParameter' (a :*: b) e where
  getSecondParameter'' (a :*: _) = getSecondParameter'' a

instance {-# OVERLAPPING #-} GSecondParameter' (K1 i e) e where
  getSecondParameter'' (K1 a) = a

instance {-# OVERLAPPABLE #-} GSecondParameter' (K1 i a) e where
  getSecondParameter'' (K1 a) = undefined

class ParameterLeaf f where
  isLeaf :: f a -> Bool

instance ParameterLeaf (M1 i c a) where
  isLeaf _ = True

instance ParameterLeaf (a :*: b) where
  isLeaf _ = False

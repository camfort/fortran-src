{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Language.Fortran.Util.SecondParameter
  ( SecondParameter(..)
  , getSecondParameter, setSecondParameter
  ) where

import Data.Generics.Product.Positions ( HasPosition(position) )
import Optics.Lens ( Lens )
import Optics.Getter ( view )
import Optics.Setter ( set )

class SecondParameter s a | s -> a where
  lensSecondParameter :: Lens s s a a

  default lensSecondParameter :: HasPosition 2 s s a a => Lens s s a a
  lensSecondParameter = position @2

getSecondParameter :: SecondParameter s a => s -> a
getSecondParameter = view lensSecondParameter

setSecondParameter :: SecondParameter s a => a -> s -> s
setSecondParameter = set lensSecondParameter

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Language.Fortran.Util.FirstParameter
  ( FirstParameter(..)
  , getFirstParameter, setFirstParameter
  ) where

import Data.Generics.Product.Positions ( HasPosition(position) )
import Optics.Lens ( Lens )
import Optics.Getter ( view )
import Optics.Setter ( set )

class FirstParameter s a | s -> a where
  lensFirstParameter :: Lens s s a a

  default lensFirstParameter :: HasPosition 1 s s a a => Lens s s a a
  lensFirstParameter = position @1

getFirstParameter :: FirstParameter s a => s -> a
getFirstParameter = view lensFirstParameter

setFirstParameter :: FirstParameter s a => a -> s -> s
setFirstParameter = set lensFirstParameter

{-# LANGUAGE KindSignatures, DataKinds, RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Fortran.Repr.Eval.Op.Intrinsic where

import Language.Fortran.Repr.Eval.Op
import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Array
import Language.Fortran.Repr.Value
import Language.Fortran.Repr.Value.Scalar

import GHC.TypeLits
import Control.Monad.Except

--opIntrinsicInt :: Op'
--opIntrinsicInt = helperTypeOnly $ \ts

helperTypeOnly
    :: forall val ty
    .  ([ty] -> Either String ty) -> Op ty val
helperTypeOnly opTy = Op{..}
  where op _vs = Left "no value-level implementation available"

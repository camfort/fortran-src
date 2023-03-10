{- | Fortran COMPLEX value representation.

A Fortran COMPLEX is simply two REALs of the same kind.
-}

{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Repr.Value.Scalar.Complex where

import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Type.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Real
import GHC.Float ( float2Double )

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

data FComplex
  = FComplex8  {- ^ @COMPLEX(8)@  -} Float  Float
  | FComplex16 {- ^ @COMPLEX(16)@ -} Double Double
    deriving stock (Show, Generic, Data)
    deriving anyclass (Binary, Out)

instance FKinded FComplex where
    type FKindedT FComplex = FTReal
    type FKindedC FComplex a = RealFloat a
    fKind = \case
      FComplex8{}  -> FTReal4
      FComplex16{} -> FTReal8

instance Eq FComplex where (==) = fComplexBOp (==) (&&)

fComplexFromReal :: FReal -> FComplex
fComplexFromReal = \case FReal4 x -> FComplex8  x 0.0
                         FReal8 x -> FComplex16 x 0.0

fComplexBOp'
    :: (Float  -> Float  -> a)
    -> (a -> a -> r)
    -> (Double -> Double -> b)
    -> (b -> b -> r)
    -> FComplex -> FComplex -> r
fComplexBOp' k8f k8g k16f k16g l r =
    case (l, r) of
      (FComplex8  lr li, FComplex8  rr ri) -> k8g  (k8f  lr rr) (k8f  li ri)
      (FComplex16 lr li, FComplex16 rr ri) -> k16g (k16f lr rr) (k16f li ri)
      (FComplex8  lr li, FComplex16 rr ri) ->
        let lr' = float2Double lr
            li' = float2Double li
        in  k16g (k16f lr' rr) (k16f li' ri)
      (FComplex16 lr li, FComplex8  rr ri) ->
        let rr' = float2Double rr
            ri' = float2Double ri
        in  k16g (k16f lr rr') (k16f li ri')

fComplexBOpInplace'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> FComplex -> FComplex -> FComplex
fComplexBOpInplace' k8f k16f = fComplexBOp' k8f FComplex8 k16f FComplex16

fComplexBOp
    :: (forall a. FKindedC FComplex a => a -> a -> b)
    -> (b -> b -> r)
    -> FComplex -> FComplex -> r
fComplexBOp f g = fComplexBOp' f g f g

fComplexBOpInplace
    :: (forall a. FKindedC FComplex a => a -> a -> a)
    -> FComplex -> FComplex -> FComplex
fComplexBOpInplace f = fComplexBOpInplace' f f

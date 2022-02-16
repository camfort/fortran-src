{-| Fortran operation evaluation. Primarily intended for constant expression
    evaluation.

Fortran intrinsic procedures can have many, many properties:

  * work on subset of the intrinsic types, with return type depending on input
  * take optional kind parameter
  * work on arrays too, element-wise (elemental procedures)
  * in gfortran, work differently depending on compiler flags

We choose to pack everything into a simple operator type, and handle these
properties via combinators and a large error sum type. Binary operators, unary
operators and function call-style intrinsic procedures are all put into the same
type. We gain expressibility, but lose the ability to inspect (all you can do is
run the function) and must write lots more code.
-}

{-# LANGUAGE KindSignatures, DataKinds, RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Fortran.Repr.Eval.Op where

import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Array
import Language.Fortran.Repr.Value
import Language.Fortran.Repr.Value.Scalar

import GHC.TypeLits
import Control.Monad.Except

opPlus :: Op'
opPlus = opNumericBin valOp typeOp
  where
    valOp sv1 sv2 = do
        case (sv1, sv2) of
          (FValScalarInt i1, FValScalarInt i2) -> do
            case fValIntSafeAdd i1 i2 of
              (i, True)   -> return $ FValScalarInt i
              (_i, False) -> Left "plus op over/underflow"
          (FValScalarReal r1, FValScalarReal r2) -> do
            return $ FValScalarReal $ fValRealAdd r1 r2
          (FValScalarComplex c1, FValScalarComplex c2) -> do
            return $ FValScalarComplex $ fValComplexAdd c1 c2
          _ -> Left $ "opPlus: not yet supported: given scalars: " <> show (sv1, sv2)
    typeOp sty1 sty2 = do
        case (sty1, sty2) of
          (FTypeScalarInt i1, FTypeScalarInt i2) ->
            return $ FTypeScalarInt $ max i1 i2
          _ -> Left "opPlus: unsupported scalar types"

opMinus :: Op'
opMinus = opNumericBin valOp typeOp
  where
    valOp sv1 sv2 = do
        case (sv1, sv2) of
          (FValScalarInt i1, FValScalarInt i2) -> do
            case fValIntSafeMinus i1 i2 of
              (i, True)   -> return $ FValScalarInt i
              (_i, False) -> Left "plus op over/underflow"
          (FValScalarReal r1, FValScalarReal r2) -> do
            return $ FValScalarReal $ fValRealMinus r1 r2
          (FValScalarComplex c1, FValScalarComplex c2) -> do
            return $ FValScalarComplex $ fValComplexMinus c1 c2
          _ -> Left $ "opMinus: not yet supported: given scalars: " <> show (sv1, sv2)
    typeOp sty1 sty2 = do
        case (sty1, sty2) of
          (FTypeScalarInt i1, FTypeScalarInt i2) ->
            return $ FTypeScalarInt $ max i1 i2
          _ -> Left "opMinus: unsupported scalar types"

opNumericBin
    :: (FValScalar -> FValScalar -> Either String FValScalar)
    -> (FTypeScalar -> FTypeScalar -> Either String FTypeScalar)
    -> Op'
opNumericBin valOp typeOp = Op{..}
  where
    op args = do
        let a1 = args !! 0
            a2 = args !! 1
        case (a1, a2) of
          (FValScalar a1s, FValScalar a2s) ->
            case a1s `valOp` a2s of
              Left  e -> Left e
              Right x -> return $ FValScalar x
          _ -> Left "op: only scalars currently supported"
    opTy args = do
        let a1 = args !! 0
            a2 = args !! 1
        case (a1, a2) of
          (FType _a1sty Nothing,        FType _a2sty (Just _a2ashp)) -> Left "scalar x array"
          (FType _a1sty (Just _a1ashp), FType _a2sty Nothing) -> Left "scalar x array"
          (FType _a1sty (Just a1ashp), FType _a2sty (Just a2ashp)) -> do
            case sameShape a1ashp a2ashp of
              Nothing -> Left "array x array (same shape)"
              Just e  -> Left $ "arrays diff shape: " <> e
          (FType a1sty Nothing, FType a2sty Nothing) -> do
            case a1sty `typeOp` a2sty of
              Left  e -> Left e
              Right x -> return $ FType x Nothing

opNegate :: Op'
opNegate = opNumericUn valOp typeOp
  where
    valOp = \case
      FValScalarInt i -> return $ FValScalarInt $ fValIntNegate i
      sv -> Left $ "opNegate: unsupported scalar value: " <> show sv
    typeOp sty = case sty of
      FTypeScalarInt{} -> Right sty
      FTypeScalarReal{} -> Right sty
      FTypeScalarComplex{} -> Right sty
      _ -> Left $ "opNegate: unsupported scalar type: " <> show sty

opNumericUn
    :: (FValScalar -> Either String FValScalar)
    -> (FTypeScalar -> Either String FTypeScalar)
    -> Op'
opNumericUn valOp typeOp = Op{..}
  where
    op args = do
        let a = args !! 0
        case a of
          FValScalar as ->
            case valOp as of
              Left  e -> Left e
              Right x -> return $ FValScalar x
          _ -> Left "op: only scalars currently supported"
    opTy args = do
        let a = args !! 0
        case a of
          FType _asty (Just _aashp) -> Left "op: only scalars currently supported"
          FType asty Nothing -> do
            case typeOp asty of
              Left  e -> Left e
              Right x -> return $ FType x Nothing

sameShape :: ArrayShape -> ArrayShape -> Maybe String
sameShape _arr1 _arr2 = Just "sameShape: not yet implemented"

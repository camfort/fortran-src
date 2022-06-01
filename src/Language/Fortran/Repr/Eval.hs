{-# LANGUAGE DataKinds #-}

{- TODO
  * faster to use String, Integer or Text for temporary KPs? Or say fuck it, and
    use Word8??
-}

module Language.Fortran.Repr.Eval where

import Prelude hiding ( GT )

import Language.Fortran.AST
import FortranSrc.Repr.Value
import FortranSrc.Repr.Type
import FortranSrc.Repr.Value.Scalar
import FortranSrc.Repr.Value.Scalar.Common

import FortranSrc.Repr.Value.Scalar.Int
import FortranSrc.Repr.Type.Scalar.Int

import FortranSrc.Repr.Value.Scalar.Real
import Language.Fortran.AST.Literal.Real

import FortranSrc.Repr.Value.Scalar.String
import qualified Data.Text as Text

import FortranSrc.Repr.Value.Scalar.Complex
import Language.Fortran.AST.Literal.Complex

import FortranSrc.Repr.Value.Scalar.Logical

import qualified FortranSrc.Repr.Op as Op

import Data.Map ( Map )

class Monad m => MonadEval m where
    lookupVar :: Name -> m (Maybe FVM)
    err :: Error -> m a

-- these should indicate a compile error
data Error
  = ENoSuchVar Name
  | EKindParamConstBadType Name FT
  | ENoSuchKindForType String String
  | EUnsupported String
  | EOp Op.Error
  | ELazy String

-- the 'Value' type can only represent scalars, so that's a nice bonus
evalLit :: MonadEval m => Value a -> m FVSM
evalLit = \case
  ValInteger i mkp -> do
    evalKp "4" mkp >>= \case
      "4" -> return $ FVSMInt $ SomeFKinded $ FIntM4 $ read i
      "8" -> return $ FVSMInt $ SomeFKinded $ FIntM8 $ read i
      "2" -> return $ FVSMInt $ SomeFKinded $ FIntM2 $ read i
      "1" -> return $ FVSMInt $ SomeFKinded $ FIntM1 $ read i
      k   -> err $ ENoSuchKindForType "INTEGER" k
  ValReal r mkp -> do
    evalKp "4" mkp >>= \case
      "4" -> return $ FVSMReal $ SomeFKinded $ FReal4 $ readRealLit r
      "8" -> return $ FVSMReal $ SomeFKinded $ FReal8 $ readRealLit r
      k   -> err $ ENoSuchKindForType "REAL" k
  ValLogical b mkp -> do
    evalKp "4" mkp >>= \case
      "4" -> return $ FVSMInt $ SomeFKinded $ FIntM4 $ boolToMachineInt b
      "8" -> return $ FVSMInt $ SomeFKinded $ FIntM8 $ boolToMachineInt b
      "2" -> return $ FVSMInt $ SomeFKinded $ FIntM2 $ boolToMachineInt b
      "1" -> return $ FVSMInt $ SomeFKinded $ FIntM1 $ boolToMachineInt b
      k   -> err $ ENoSuchKindForType "INTEGER" k
  ValComplex (ComplexLit _ _ cr ci) ->
    -- TODO annoying & tedious. see Fortran 2008 spec 4.4.2.4
    -- 1. evaluate each part
    -- 2. determine kind parameter (largest real, or default if both ints)
    --    - fail here if a named part wasn't real or int
    -- 3. upgrade both parts to that kind
    -- 4. package and return
    err $ EUnsupported "COMPLEX literals"
  ValString s -> return $ FVSMString $ someFString $ Text.pack s

evalComplexPart :: MonadEval m => ComplexPart a -> m FVM
evalComplexPart = \case
  ComplexPartReal  _ _ r mkp -> FVSM <$> evalLit (ValReal    r mkp)
  ComplexPartInt   _ _ i mkp -> FVSM <$> evalLit (ValInteger i mkp)
  ComplexPartNamed _ _ v     -> evalVar v

evalVar :: MonadEval m => Name -> m FVM
evalVar var =
    lookupVar var >>= \case
      Just val -> return val
      Nothing  -> err $ ENoSuchVar var

boolToMachineInt :: Num a => Bool -> a
boolToMachineInt = \case True -> 1; False -> 0

evalKp :: MonadEval m => String -> Maybe (KindParam a) -> m String
evalKp kDef = \case
  Nothing -> return kDef
  Just kp -> case kp of
    KindParamInt _ _ var ->
      lookupVar var >>= \case
        Just val -> case val of
          FVSM (FVSMInt i) -> return $ someFIntMUOp' show show show show i
          _ -> err $ EKindParamConstBadType var (fvmType val)
        Nothing  -> err $ ENoSuchVar var

evalExpr :: MonadEval m => Expression a -> m FVM
evalExpr = \case
  ExpBinary _ _ bop l r -> evalBOp bop l r

evalBOp :: MonadEval m => BinaryOp -> Expression a -> Expression a -> m FVM
evalBOp bop l r = do
    evalExpr l >>= \case
      FVAM _  -> err $ EUnsupported "ops on arrays"
      FVSM l' -> evalExpr r >>= \case
        FVAM _  -> err $ EUnsupported "ops on arrays"
        FVSM r' -> case bop of
          Addition       -> wrapSOp $ Op.opIcNumericBOp (+) l' r'
          Subtraction    -> wrapSOp $ Op.opIcNumericBOp (-) l' r'
          Multiplication -> wrapSOp $ Op.opIcNumericBOp (*) l' r'
          Concatenation  ->
            case (l', r') of
              (FVSMString ls, FVSMString rs) ->
                return $ FVSM $ FVSMString $ concatSomeFString ls rs
              _ -> err $ ELazy "concat strings only please"
          GT -> wrapRelSOp $ error "TODO" -- Op.opIcNumRelBOp (>) l' r'

wrapSOp :: MonadEval m => Either Op.Error FVSM -> m FVM
wrapSOp = \case
  Left  e -> err $ EOp e
  Right v -> return $ FVSM v

-- bools are wrapped into LOGICAL(4) -- which you can figure out by looking at
-- the type signature of 'fLogicalM'!
wrapRelSOp :: MonadEval m => Either Op.Error Bool -> m FVM
wrapRelSOp = \case
  Left  e -> err $ EOp e
  Right b -> return $ FVSM $ FVSMLogical $ SomeFKinded $ fLogicalM b

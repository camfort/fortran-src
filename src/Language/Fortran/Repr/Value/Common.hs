module Language.Fortran.Repr.Value.Common where

data PrimRepr
  = Machine
  -- ^ Representation behaviour intends to match Fortran's. I guess we'll target
  --   gfortran.

  | Idealized
  -- ^ Use "mathematically ideal" representations e.g. 'Integer' for all
  --   @INTEGER(x)@ types. This enables us to check for correctness issues such
  --   as overflow.

data Check
  = Checked
  -- ^ Where relevant/possible, values will be checked for correctness (e.g.
  --   existence of over/underflow), and adjusted accordingly.

  | Unchecked
  -- ^ Values will not be checked for correctness.

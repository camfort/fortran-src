{- | Precise Fortran value model.

Note that we actually think about two different models: one storing values
"machine-like" (@Machine@), one storing them "mathematically idealized"
(@Idealized@). Only certain Fortran types have these split representations,
namely integers and logicals. The rest have a single representation each.

Both representations may be convenient in different own ways:

  * Machine representation is efficient, and should retain common overflow
    behaviours without explicitly handling them.
  * Idealized representation is easier to handle, and enables safe checking for
    overflows.

The same kind algebra is performed for both, so types & kinds should match.

As of 2022-08-15, idealized representation isn't properly supported -- this
module simply re-exports the machine representation.
-}

module Language.Fortran.Repr.Value
  ( module Language.Fortran.Repr.Value.Machine
  ) where

import Language.Fortran.Repr.Value.Machine

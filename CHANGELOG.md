### 0.3.0 (upcoming)

* Introduce datatype for BBGr instead of prior type alias for Gr.
  * Now split into three fields: bbgrGr, bbgrEntries and bbgrExits
  * May require refactoring of code to use bbgrGr field where a Gr was expected before.
* Introduce pragmaOffset field for Position, allowing pragmas to specify an apparent file and line-number.
  * May require refactoring of code that uses the Position constructor.
  * Fifth field is Maybe (Int, String), containing a line-offset and a target filename when present.
  * It's designed such that most Position-based transformations are not affected by the pragmaOffset.
  * They may need to preserve the field, though, as it passes through functions.
  * Default value is 'Nothing'.

### 0.2.1.1 (May 18, 2018)

* Extend Fortran 95 support
* Extend support for legacy extensions

### 0.2.1.1 (December 13, 2017)

* Fortran95Experimental module renamed to Fortran95
* No infinite loops due to symlinks.
* Fortran 95 support.
  * AST extended to support more non-standard statements internally.

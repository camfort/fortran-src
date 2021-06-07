### 0.5.0 (Unreleased)
  * Introduce a second-stage type representation including kind info alongside
    types, and resolving some types to semantic type with preset kinds (e.g.
    `DOUBLE PRECISION` -> `REAL(8)`).
    * The type analysis in Language.Fortran.Analysis.Types uses this
      representation now (`IDType` stores a `SemType` instead of a `BaseType`).
  * Move `CharacterLen` from parsing to type analysis.
    * This makes `BaseType` now a plain tag/enum with no extra info.
  * Add extended Fortran 90 real literal parser (parses kind info).
  * Export some infer monad utils (potentially useful for running just parts of
    type analysis)
  * Parser & lexer tweaks
    * Fortran 77 parser should no longer attempt to parse kind selectors for
      `DOUBLE` types
    * Fix an edge case with the fixed form lexer (#150)

### 0.4.3 (May 25, 2021)

  * Add Haddock documentation to AST module. Many parts of the AST now have
    commentary on meaning and usage, and the Haddock page is sectioned.
  * Add STATIC statement (should be similar/identical to SAVE attribute) to
    fixed-form lexer, support in Fortran 77 Extended parser.
  * Rewrite post-parse transformation handling. Parser modules now export more
    parsers which allow you to select post-parse transformations to apply,
    intended to enable quicker parsing if you know you don't need to certain
    transformations.
  * Support percent data references in fixed-form lexer, enable in Fortran 77
    parser
  * Now also testing on GHC 9.0
  * Cache INCLUDE-ed files to avoid unnecessary re-parsing

### 0.4.2 (March 03, 2021)

  * `FortranVersion` from `ParserMonad` moved to its own module
    `Language.Fortran.Version`. `ParserMonad` will re-export it for now.
  * `Version.deduceVersion` renamed to `deduceFortranVersion` due to often being
    imported non-qualified. `deduceVersion` remains as an alias.
  * Provide a continuation reformatter in `PrettyPrint`. Runs on `String`s and
    doesn't guarantee the output is a valid program, so not enabled by default.
  * Add a diff-like rewriter, similar to
    [reprinter](http://hackage.haskell.org/package/reprinter) but uses
    replacements rather than an annotated AST.
  * Various internal de-duplication and changes.

### 0.4.1

* Ignore comments in structure declaration PR#107 (thanks Jason Xu)

### 0.4.0 (August 29, 2019)

* ModGraph: parse Fortran files and assemble them into a dependency graph in order to construct automated 'build' plans for analysis and summarisation (e.g. with --make-mods option).
* Change name of compilation to summarisation. Remains as '-c' option.
* Allow multiple files and directories to be specified on command line.
* Search includedir recursively for fsmod files.
* Change format of fsmod-files so that they can contain [ModFile] since multiple Fortran files can be summarised into a single mod file.
* Introduce strictness and NFData dependencies across the board.
* Use Pipes to process large amounts of files in order to control memory usage and more efficiently process things.
* Parsing rules for StructStructures (thanks Raoul Charman)

### 0.3.0 (June 13, 2019)

* Add partial Fortran2003 support.
* Introduce datatype for BBGr instead of prior type alias for Gr.
  * Now split into three fields: bbgrGr, bbgrEntries and bbgrExits
  * May require refactoring of code to use bbgrGr field where a Gr was expected before.
* Introduce pragmaOffset field for Position, allowing pragmas to specify an apparent file and line-number.
  * May require refactoring of code that uses the Position constructor.
  * Fifth field is Maybe (Int, String), containing a line-offset and a target filename when present.
  * It's designed such that most Position-based transformations are not affected by the pragmaOffset.
  * They may need to preserve the field, though, as it passes through functions.
  * Default value is 'Nothing'.
* Add --show-flows-to/--show-flows-from features
  * Visualise the dataflow use/def chains using GraphViz.
* Add --show-block-numbers feature.
  * Allows user to get AST-block numbers easily in order to use them with the above visualisation features.
* Fix several bugs with dataflow analysis that had accumulated.
* Eliminate StContinue and StEnddo are eliminated during GroupLabeledDo transformations.
  * To be consistent with unlabeled Do.
* Parse and discard C-comments as a convenience feature for when fortran-src must interact with the output of C preprocessors that insert spurious comments.
* Add type propagation into type analysis, annotating every expression with a type.
  * Additional interface: analyseTypesWithEnv to access a list of type errors found.
* Add dimensional information to CTArray and length/kind to TypeCharacter.
* Stricter checking of the grouping transform - if any statements that should be grouped are not grouped, raise an error.
* Support pragmas that alter the current 'filename and position' tracker, often used by preprocessors to help pinpoint original code locations.
  * Uses a relative offset field called 'posPragmaOffset' so that relative measures continue to function correctly.
* Add constant propagation / parameter variable analysis.
* Add -c feature to compile 'fsmod files' with renaming and type info.

### 0.2.1.1 (May 18, 2018)

* Extend Fortran 95 support
* Extend support for legacy extensions

### 0.2.1.1 (December 13, 2017)

* Fortran95Experimental module renamed to Fortran95
* No infinite loops due to symlinks.
* Fortran 95 support.
  * AST extended to support more non-standard statements internally.

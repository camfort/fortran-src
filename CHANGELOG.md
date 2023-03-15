### 0.13.0 (Mar 14, 2023)
  * better handling for line directives in free form lexer (#248, @mrd)
  * don't inline solo includes in relevant F77 parsers (#245, @RaoulHC)
  * add `-C=opts` CLI option for passing CPP arguments (#250, @mrd)
  * fix reformatting of 73 character long lines in naive mixed form reformatter
    (#251, @ksromanov)
  * assume extended `Fortran77Legacy` rather than `Fortran77` for `*.f`, `*.for`
    etc. files (#260)
  * allow comment lines between continuation lines in F77 parser (in standard)
    (#257, @RaoulHC)
  * refactor Fortran type & value representation & expression evaluator without
    Fortran kind-indexed GADTs; replace constant folder (#253, @raehik)

### 0.12.0 (Oct 19, 2022)
  * clean up F77 include inlining (#245, @RaoulHC)
    * directly export F77 include parser at `f77lIncludesNoTransform`
    * `f77lIncIncludes :: String -> ByteString -> IO [Block A0]` should now be
      defined by the user e.g. `\fn bs -> throwIOLeft $ f77lIncludesNoTransform
      fn bs`
  * `Language.Fortran.Analysis.SemanticTypes`: alter `SemType` constructor
    `TArray` to support assumed-size (e.g. `integer arr(*)`) arrays (#244)
  * `Language.Fortran.Rewriter`: fix inline comment padding (#242, @RaoulHC)

### 0.11.0 (Oct 10, 2022)
  * add strong Fortran value & type representation at `Language.Fortran.Repr`
    (currently unused) (#235, @raehik)
    * operations are accurate to actual Fortran compiler behaviour e.g. integers
      are stored fixed-width based on kind, so overflow behaviour is free
    * can recover a value's precise type (e.g. `INTEGER(8)`, including kind) via
      pattern matching
  * bump minimum compiler version to GHC 9.0
  * improved comment handling in fixed form lexer: parse more comment syntax,
    case sensitive, parse beyond column 72 (#237, @RaoulHC)
  * allow `ExpDataRef` constructor in `varName` (fixes a crash in type analysis
    #238)
  * add `Annotated`, `Spanned` instances for intermediate AST data type
    `ArgumentExpression`
  * export statement-level "pre-prepared" parsers (previously, you would have to
    define the parser yourself using parser utils and the Happy parser export)
  * export `Language.Fortran.Parser.byVerFromFilename :: Parser (ProgramFile
    A0)`, a replacement for the removed
    `Language.Fortran.Parser.Any.fortranParser`

### 0.10.2 (Aug 18, 2022)
  * fix missing parentheses when pretty printing certain syntax #233
  * fix missing export of `ParseErrorSimple` in `Parser`
  * fix inlined includes block order #234

### 0.10.1 (Aug 1, 2022)
  * export `ParseErrorSimple` from `Parser`, not internal module `Parser.Monad`
  * rewriter fixes #232

### 0.10.0 (Jul 13, 2022)
  * Fix parsing kind parameters like `a_1` on literals. Previously, that would
    be parsed as a kind parameter on a kind parameter. Now we don't do that,
    following gfortran's behaviour.
    * Kind parameter representation is changed to explicitly say if it's an
      integer kind or named constant kind, rather than reusing `Expression`.
  * BOZ literals
    * add some syntactic info (to enable checking standards conformance)
    * export `bozAsTwosComp` function for reading as two's complement integer
  * allow named constants in complex literals
  * document `FirstParameter`, `SecondParameter` behaviour/safety, fix erroneous
    instances
  * fiddle with record selectors for some AST nodes (for better Aeson instances)
  * pair IF/CASE conditions with their blocks, rather than splitting between two
    lists
  * `ExpFunctionCall` and `StCall` store procedure arguments in `AList` (`[a]`)
    instead of `Maybe AList` (`Maybe [a]`)
    * Matching is safer because empty lists are always `[]` instead of `Nothing`
      or `Just []`. Construction for empty lists is more awkward.
    * A better solution would be to use an `AList`-like that also stores extra
      syntactic information.
  * refactored a number of small AST nodes
    * `ImpElement`
    * `ForallHeader`
  * add Hackage documentation to many individual AST constructors and fields
  * improve include parser interface #227
  * improve newline handling for block parsers #228
  * fix some source span misses #225

### 0.9.0 (Feb 14, 2022)
  * Restructure parsing-related modules for code deduplication and better user
    experience.
    * Now all user-facing parsers and the combinators to create them are in a
      single module at `Language.Fortran.Parser`.
    * The Happy parsers have fewer dependencies, so should no longer require a
      recompile due to apparently unrelated changes.
  * Remove some deprecated shims (from the restructured modules).
  * Merge fortran-src-extras `Language.Fortran.Extras.ModFiles.Extras` module
    into `Language.Fortran.Util.ModFile`.

### 0.8.0 (Jan 04, 2022)
  * Merge declarator constructors. Now you differentiate between array and
    scalar declarators by looking at the relevant field. See
    `Language.Fortran.AST.Declarator` for details.
  * Add `bozAsNatural :: Num a => Boz -> a` function to resolve BOZ constants as
    integers

### 0.7.0 (Dec 09, 2021)
  * No longer treat `!` in strings as comments in continuation reformatter
    (thanks @envp) #179
  * CI builds on Mac; more release automation #181 #189
  * Handle nonstandard kind parameter in parsing & type analysis #188
  * Fix renamer ambiguity resulting in unusual name-related breakages (e.g.
    `ValVariable` not getting transformed to `ValIntrinsic`) #190
  * Fully parse logical literals early (don't leave as `String`) #185
    * Code that touches `ValLogical` will have to be updated -- it should mean
      removal of user-side parsing.
  * Explicitly parse integer literal kind parameter #191
    * The `String` representation stored should now always be safe to `read` to
      a Haskell `Integral`.
  * Provide real literals in a semi-parsed "decomposed" format #193
    * Kind parameters are also made explicit.
    * Libraries with custom real literal parsing should be able to replace it
      with `readRealLit :: (Fractional a, Read a) => RealLit -> a`.
  * BOZ literal constants receive their own `Value` constructor (instead of
    sharing one with integers) #194
    * Also parse them to an intermediate data type and provide handling
      functions.

Note that kind parameters are disabled in fixed form parsers (F77, F66), so for
codebases targeting older standards, many changes will be along the lines of
`ValInteger x` -> `ValInteger x _`.

### 0.6.1 (Sep 17, 2021)
  * Properly include test data in package dist (in preparation for placing on
    Stackage)

### 0.6.0 (Sep 03, 2021)
  * IF and CASE block constructs are now parsed as blocks at parsing instead of
    as a post-parse transformation (no intermediate statement representation)
    #154
  * add ASSOCIATE block construct (Fortran 2003 parser only) #165
  * `CommonGroup` AST nodes now store `Declarator`s instead of `Expression`s
    #173
    * various bug fixes related to their typing as a result
  * CI now building on Windows, and save Linux & Windows executables for each
    build
  * various bugfixes (#34, #155)

### 0.5.0 (Jun 30, 2021)
  * Introduce a second-stage type representation including kind info alongside
    types, and resolving some types to semantic type with preset kinds (e.g.
    `DOUBLE PRECISION` -> `REAL(8)`).
    * Module is at Language.Fortran.Analysis.SemanticTypes . Includes utils and
      instances.
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

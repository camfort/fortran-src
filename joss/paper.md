---
title: 'fortran-src: Fortran static analysis infrastructure'
tags:
  - Fortran
  - static analysis
  - tooling
authors:
- name: Mistral Contrastin
  orcid: 0000-0002-5409-7122
  affiliation: "1, 2"
- name: Raoul Hidalgo Charman
  orcid: 0000-0002-8401-7672
  affiliation: 3
- name: Matthew Danish
  orcid: 0000-0002-7186-387X
  affiliation: 4
- name: Benjamin Orchard
  orcid: 0000-0002-7543-7675
  affiliation: 5
- name: Dominic Orchard
  orcid: 0000-0002-7058-7842
  affiliation: "5, 6"
- name: Andrew Rice
  orcid: 0000-0002-4677-8032
  affiliation: "1, 7"
- name: Jason Xu
  orcid: 0000-0003-3310-0756
  affiliation: 3

affiliations:
- name: Department of Computer Science and Technology, University of Cambridge, UK
  index: 1
- name : Meta, US
  index: 2
- name : Bloomberg, US
  index: 3
- name : Utrecht University, Netherlands
  index: 4
- name: School of Computing, University of Kent, UK
  index: 5
- name: Institute of Computing for Climate Science, University of Cambridge, UK
  index: 6
- name : GitHub, US
  index: 7

date: TODO
bibliography: paper.bib
---

# Summary

fortran-src is an open-source Haskell library and command-line application
for the lexing, parsing,
and static analysis of Fortran source code. It provides an
 interface to build other tools, e.g., for
static analysis, automated refactoring, verification, and compilation.
The library supports FORTRAN 66, FORTRAN 77, Fortran 90, Fortran 95,
some legacy extensions, and partially Fortran 2003, with
a shared Abstract Syntax Tree representation.
The library has been deployed in several language tool projects in academia and industry.

# Statement of need

As one of the oldest surviving programming languages [@backus1978history], Fortran underpins a vast amount of software; Fortran is not only a mainstay
of legacy software, but is also used to write new software. Fortran remains a popular language in the international scientific community; @vanderbauwhede2022making reports data from 2016 on the UK's \`\`Archer'' supercomputer, showing the
vast majority of use being Fortran code. Fortran is
particularly notable for its prevalence in earth sciences, e.g., for
implementing global climate models that inform international policy
decisions [@mendez2014climate]. In 2024, Fortran re-entered the Top 10 programming languages in
the [TIOBE Index](https://www.tiobe.com/tiobe-index/), showing its enduring popularity.
The continued use of Fortran, particularly in
scientific contexts, was the catalyst for this software package.

A challenge in writing language tools for Fortran is its long
history. There have been several major language standards (FORTRAN
I-IV, FORTRAN 66 and 77, Fortran 90, 95, 2003, 2008, etc.).
Newer standards often deprecate features
which are known to be a ready source of errors, or difficult to
specify or understand. However, compilers often support an amalgam of features across
standards (@urmaetal2014).
This enables developers to keep using deprecated features and mix
language standards. This complicates the development of new tools for manipulating Fortran
source code; one must tame the weight of decades of language evolution.

Our package, fortran-src, provides an open-source unified core for
statically analysing Fortran code across language standards, with
a focus on legacy code over cutting-edge modern Fortran. It is both
a standalone tool and a library, providing
a suite of standard static analyses as a basis for
further programming language tools and systems.

## Related software

A variety of other tools exist for analysing Fortran, but most are commercial and closed source, e.g.,
plusFORT\footnote{\url{https://polyhedron.com/?product=plusfort}} (which includes the SPAG refactoring tool),
the SimCon fpt tool\footnote{\url{http://simconglobal.com/fpt_summary.html}} (which includes further verification
features like dimensional analysis), and Forcheck\footnote{\url{https://codework.com/solutions/developer-tools/forcheck-fortran-analysis/}}.
General commerical static analysis tools, like Coverity\footnote{\url{https://www.synopsys.com/software-integrity/static-analysis-tools-sast/coverity.html}}
and Understand\footnote{\url{https://scitools.com/}}, can also handle Fortran. Photran\footnote{\url{https://projects.eclipse.org/projects/tools.ptp.photran}}
is an open source plugin for refactoring in Eclipse,
but does not provide more general static analysis facilities.
More recent work has developed open-source
tools for refactoring Fortran [@vanderbauwhede2022making]:
[RefactorF4Acc](https://github.com/wimvanderbauwhede/RefactorF4Acc)\footnote{\url{https://github.com/wimvanderbauwhede/RefactorF4Acc}} is an
 open-source tool for upgrading FORTRAN 77 code to Fortran 95.

No comprehensive lexing, parsing, and analysis library was available from which to
build new tools.

# Functionality

  * Lexing and parsing of Fortran to an expressive Abstract Syntax Tree;
  * Various static analyses, e.g., data flow analysis;
  * Type checking;
  * Pretty printing;
  * "Reprinting", or patching sections of source code without removing secondary
    notation such as comments;
  * Exporting to JSON.

fortran-src is primarily a Haskell library but it also packages a command-line
tool. By exporting parsed code to JSON, the
parsing and analyses that fortran-src provides may be utilized by
non-Haskell tools.

The library's top-level module is `Language.Fortran`.

## Lexing and parsing

Static analysis of Fortran requires a choice in the lexing and parsing
front end: either to take the approach of many compilers, allowing an amalgam of features (e.g., gfortran with
its hand-written parser), or to
enforce language standards at the exclusion of some code that is
accepted by major compilers. fortran-src takes roughly the latter
approach, though it has an extended Fortran 77 mode for supporting
legacy extensions influenced by vendor-specific compilers popular in the past.

The Fortran language has evolved through two broad syntactic forms:

  * _fixed source form_, used by FORTRAN 66 and FORTRAN 77 standards, where each
    line of source code follows a strict format (motivated by its original use
    with punched cards). The first 6 columns of a line are reserved for labels
    and continuation markers. The character `C` in column 1 indicates a comment line
    to be ignored by the compiler, else the line properly begins from column 7.

  * _free source form_, first specified in Fortran 90 and subsequent standards, which has fewer restrictions on the line format and a different method
    of encoding line continuations.

Therefore, two lexers are provided: the fixed form lexer, for handling earlier
versions of the language: FORTRAN 66 and FORTRAN 77 (and additional
`Legacy` and `Extended` modes), and the free form lexer, for Fortran
90 onwards.

fortran-src defines one parser per supported standard (grouped
under `Language.Fortran.Parser.Fixed` and `Language.Fortran.Parser.Free` depending
on the lexing form), plus a parser
for handling non-standard extended features.
Later standards such as Fortran 2003 are generally comparable to Fortran
90, but with additional syntactic constructs. The parser `gates' certain features by the language standard being parsed.

The lexers are auto-generated via the [`alex`](https://github.com/haskell/alex) tool.
The suite of parsers is automatically generated from
attribute grammar definitions in the Bison format, via the
[`happy`](https://github.com/haskell/happy) tool.
CPP (the C pre-processor) can be run prior to lexing or parsing.

## Unified Fortran AST

The parsers share a common abstract syntax tree (AST) representation (`Language.Fortran.AST`)
defined via mutually-recursive data
types. All such data types are _parametric data types_, parameterised by
the type of "annotations" that can be stored in the nodes of the
tree. For example, the top-level of the AST is the `ProgramFile a`
  type, which comprises a list of `ProgramUnit a` values, parameterised
  by the annotation type `a` (i.e., that is the generic type parameter).
  The annotation facility is useful for collecting information about types within the tree nodes or flagging whether the particular node of the tree has been rewritten.

Some simple transformations are provided on ASTs:

* Grouping transformation, turning unstructured ASTs into structured ASTs;
(`Language.Fortran.Transformation.Grouping`);
* Disambiguation of array indexing vs. function calls (as they share
the same syntax in Fortran) (`Language.Fortran.Transformation.Disambiguation`),
and intrinsic calls from regular function calls,
(`Language.Fortran.Transformation.Disambiguation.Intrinsic`),
e.g.
`a(i)` is both the syntax for indexing array `a` at index `i` and
for calling a function named `a` with argument `i`;
* Fresh name transformation (obeying scoping) (`Language.Fortran.Analysis.Renaming`).

These transformations are applied to the AST following
parsing (with some slight permutations on the grouping transformations
depending on whether the code is FORTRAN 66 or not).

## Static analyses

Static analysis techniques available within fortran-src:

* Control-flow analysis (building a super graph) (`Language.Fortran.Analysis.BBlocks`);
* General data flow analyses (`Language.Fortran.Analysis.DataFlow`), including:
  - Reaching definitions;
  - Def-use/use-def;
  - Constant evaluation;
  - Constant propagation;
  - Live variable analysis;
  - Induction variable analysis.
* Type analysis (`Language.Fortran.Analysis.Types`);
* Module graph analysis (`Language.Fortran.Analysis.ModGraph`);

An abstract representation
is provided for evaluation of expressions and for semantic analysis
(`Language.Fortran.Repr`).  Constant expression evaluation
(`Language.Fortran.Repr.Eval.Value`) leverages this representation
and enables some symbolic manipulation too, providing some partial evaluation.

A demonstration of fortran-src for static analysis is provided
by a small demo tool which detects if an allocatable array is used
before it has been allocated.\footnote{\url{https://github.com/camfort/allocate-analysis-example}}

## Pretty printing, reprinting, and rewriting

A common feature of language tools is to generate source code.
We thus provide pretty printing features to generate textual source
code from the internal AST (`Language.Fortran.PrettyPrint`).

Furthermore, fortran-src provides a diff-like patching feature for
(unparsed) Fortran source code that accounts for the fixed-form style,
handling the fixed-form lexing of lines, and comments in its
application of patches (`Language.Fortran.Rewriter`). This aids development of refactoring tools.

# Work building on fortran-src

## CamFort

The fortran-src package originated in
the CamFort project\footnote{Funded from 2015-18
by the EPSRC under the project title \emph{CamFort: Automated evolution and
  verification of computational science
  models} \url{https://gow.epsrc.ukri.org/NGBOViewGrant.aspx?GrantRef=EP/M026124/1}}
whose aim was to (1) develop practical tools for scientists to
help reduce the accidental complexity of models through
evolving a code base, and (2) provide tools for automatically verifying
properties of code. The work resulted in the CamFort tool
of which fortran-src is the core infrastructure.

CamFort provides automatic refactoring of
deprecated or error-prone programming patterns, with the goal of helping
to meet core quality requirements, such as maintainability
(@DBLP:conf/oopsla/OrchardR13). For example, it can rewrite
EQUIVALENCE and COMMON blocks (both of which were deprecated in the
Fortran 90 standard) into more modern Fortran style.

CamFort also provides code analysis and
lightweight verification tools (@contrastin2016lightning). Source-code
annotations (comments) provide specifications of certain aspects of a
program's meaning or behaviour. CamFort can then check that code
conforms to these specifications (and for some features can suggest places to insert specifications or infer specifications
from existing code). Facilities include: units-of-measure typing
(@DBLP:journals/corr/abs-2011-06094,@DBLP:journals/jocs/OrchardRO15,@danish2024incremental),
array access patterns (for capturing the shape of stencil computations)
(@orchard2017verifying), deductive reasoning via pre- and
post-conditions in Hoare logic style, and various code safety checks.

CamFort also provides an advanced rewriting alogrithm
that fuses a depth-first traversal of the AST with a textual diff algorithm
on the original source code, called "reprinting" (@clarke2017scrap).

CamFort has been previously
deployed at the Met Office, with its analysis tooling run on the Unified
Model (@walters2017met) to ensure internal code quality standards are met.


## fortran-vars memory model library

`fortran-vars` is a static analysis library built on top of `fortran-src`. Many
static analysis questions depend on knowing the value and type of
expressions. `fortran-vars` provides an API to answer this question.
It has modules for symbol table construction, constant expression evaluation, and type
checking.
Additionally, `fortran-vars` provides a memory model to resolve aliases
introduced by `equivalence` statements, which are very common in legacy Fortran 77
code. It is possible to construct such a memory model because variables in
Fortran 77 are statically allocated by default. Data flow analysis, such as
constant propagation analysis, can be conducted based on memory locations
instead of variable names.

## Nonstandard INTEGER refactoring

fortran-src has been used to build other (closed
source) refactoring tools to help migration and improve the quality
of large legacy codebases, building on top of the library's AST, analysis, and
reprinting features.

One example of this has been an effort to fix a number of issues regarding the
use of integers used where logical types are expected. A tool was written
to refactor many expressions by using the fortran-vars typechecker to find
integer expressions and normalise them while flagging anything
potentially changing behaviour for further manual inspection. These might be situations
in which some code is hard to statically analyse but safe, or it may have uncovered an existing
bug. The tool uncovered many such bugs in a particular codebase during this effort, including several in the
form of the snippet above.

This effort, along with a number of others, allowed the team working at Bloomberg
(a subset of the authors here) to eventually migrate a
codebase from a legacy compiler to a modified GFortran, with no change in
behaviour. Ongoing efforts are using fortran-src to remove the patches on top of
GFortran, as well as to introduce interfaces for more robust type checking in this
code base.

# Project maintenance and documentation

fortran-src may be built and used on Windows, Mac and Linux systems using a recent version of the
[Glasgow Haskell Compiler](https://www.haskell.org/ghc/). The project includes
an expansive test suite covering various parsing edge cases and behaviours,
which is automatically executed for changes to the project (on the above three
systems). Bug reports and other contributions are welcomed at the [fortran-src
GitHub page](https://github.com/camfort/fortran-src).

# Acknowledgements

The initial work on the fortran-src infrastructure was funded by an
EPSRC grant __CamFort: Automated evolution and verification of
computational science models__ (EP/M026124/1), from 2015-18, and by an
EPSRC Impact Acceleration Award and then Knowledge Transfer
Partnership grant from 2018-19. Orchard is also supported by the
generosity of Eric and Wendy Schmidt by recommendation of the Schmidt
Sciences program, through which he carries on his work supporting
scientists through programming languages, tools, and systems as part
of the Institute of Computing for Climate Science at the University of
Cambridge. Furthermore, this work was supported by a grant from Bloomberg.

A number of other people have been associated with the project
and have contributed to the development of the package
over the years (in alphabetical order of surname):

* Daniel Beer
* Anthony Burzillo
* Harry Clarke
* Aiden Jeffrey
* Lukasz Kolodziejczyk
* Vilem-Benjamin Liepelt
* Darius Makovsky
* Benjamin Moon
* Daniel Ruoso
* Eric Seidel
* Poppy Singleton-Hoare
* Jay Torry

# References

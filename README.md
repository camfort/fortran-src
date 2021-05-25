# fortran-src
![CI status badge](https://github.com/camfort/fortran-src/workflows/CI/badge.svg)

Provides lexing, parsing, and basic analyses of Fortran code covering standards: FORTRAN 66, FORTRAN 77, Fortran 90, Fortran 95 and part of Fortran 2003. Includes data flow and basic block analysis, a renamer, and type analysis. For example usage, see the 'camfort' project (https://github.com/camfort/camfort), which uses fortran-src as its front end.

For features that output graphs, the intended usage is to pipe it into the command 'dot -Tpdf' and redirect that into a PDF file. The 'dot' command is part of the GraphViz project (https://www.graphviz.org/), please see their manual for the many other options that can be explored for visualisation purposes.

    Usage: fortran-src [OPTION...] <file>
      -v VERSION, -F VERSION  --fortranVersion=VERSION         Fortran version to use, format: Fortran[66/77/77Legacy/77Extended/90]
      -a ACTION               --action=ACTION                  lex or parse action
      -t                      --typecheck                      parse and run typechecker
      -R                      --rename                         parse and rename variables
      -B                      --bblocks                        analyse basic blocks
      -S                      --supergraph                     analyse super graph of basic blocks
      -r                      --reprint                        Parse and output using pretty printer
                              --dot                            output graphs in GraphViz DOT format
                              --dump-mod-file                  dump the information contained within mod files
      -I DIR                  --include-dir=DIR                directory to search for precompiled 'mod files'
      -c                      --compile                        compile an .fsmod file from the input
                              --show-block-numbers[=LINE-NUM]  Show the corresponding AST-block identifier number next to every line of code.
                              --show-flows-to=AST-BLOCK-ID     dump a graph showing flows-to information from the given AST-block ID; prefix with 's' for supergraph
                              --show-flows-from=AST-BLOCK-ID   dump a graph showing flows-from information from the given AST-block ID; prefix with 's' for supergraph

## Usage/Installation
fortran-src is available on Hackage. Stackage has a very old version and is
definitely not what you want, but you can specify a newer Hackage version in
`stack.yaml`.

### As a dependency
Reference `fortran-src` in your (Stack/Cabal) project dependencies. If you're
using Stack, you can stuff a Hackage reference into `stack.yaml` using
`extra-deps`, like:

```yaml
resolver: ...
...

extra-deps:
- ...
- fortran-src-$VERSION
```

### As a CLI tool
If you have Cabal properly configured, you should be able install fortran-src
from Hackage:

```
cabal install fortran-src
```

Otherwise, we suggest building from source if you want to use the fortran-src
CLI tool. See [#Build from source](#build-from-source) for details.

## Development
As of 2021-04-28, fortran-src supports and is regularly tested on **GHC 8.6,
8.8, 8.10 and 9.0**. Releases prior to/newer than those may have issues. We
welcome fixes that would let us support a wider range of compilers.

### Build from source
#### Stack
Stack 2.x is required. *(Stack 1.x may work with minor alternations
-- you may have to download the resolver manually.)*

```
stack setup
stack build
```

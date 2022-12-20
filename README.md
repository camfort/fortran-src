# fortran-src
![CI status badge](https://github.com/camfort/fortran-src/actions/workflows/ci.yml/badge.svg)

Provides lexing/parsing and early static analyses of Fortran code. The following
Fortran standards are covered:

  * FORTRAN 66 (ANSI X3.9-1966)
  * FORTRAN 77 (ANSI X3.9-1978 / ISO 1539:1980)
  * Fortran 90 (ISO/IEC 1539:1991)
  * Fortran 95 (ISO/IEC 1539-1:1997
  * Fortran 2003 (partial)

Parsing is configurable, and you can select the Fortran standard to target,
including special extended modes for nonstandard FORTRAN 77.

Includes data flow and basic block analysis, a renamer, and type analysis.

This package primarily exports a Haskell library, but also builds an executable
that can be used for testing and debugging. For example usage, see the
[CamFort](https://github.com/camfort/camfort) project, which uses fortran-src as
its front end.

## Obtaining
We provide [prebuilt binaries](https://github.com/camfort/fortran-src/releases)
for Windows, Mac and Linux.

## Usage
Add `fortran-src` as a dependency in your Haskell project. We're on
[Hackage](https://hackage.haskell.org/package/fortran-src) and also on
[Stackage](https://www.stackage.org/package/fortran-src).

### Command-line tool
You can also invoke `fortran-src` on the command line.

For features that output graphs, the intended usage is to pipe it into the
command `dot -Tpdf` and redirect that into a PDF file. The `dot` command is part
of the [GraphViz project](https://www.graphviz.org/), please see their manual
for the many other options that can be explored for visualisation purposes.

```
Usage: fortran-src [OPTION...] <file>
  -v VERSION, -F VERSION  --fortranVersion=VERSION         Fortran version to use, format: Fortran[66/77/77Legacy/77Extended/90]
  -a ACTION               --action=ACTION                  choose the action, possible values: lex|parse
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
```

## Building
You will need the GMP library plus header files: on many platforms, this will be
via the package `libgmp-dev`.

Haskell library dependencies are listed in `package.yaml`. fortran-src supports
building with Stack or Cabal.

fortran-src supports **GHC 9.0 through GHC 9.2**. We regularly test at least the
minimum and maximum supported GHCs. Releases prior to/newer than those may have
issues. We welcome fixes that would let us support a wider range of compilers.

You will likely need **at least 3 GiBs of memory** to build fortran-src.

For installing GHC and build tools, we strongly recommend
[ghcup](https://www.haskell.org/ghcup/).

When **latest recommended** is used, it means the latest version of the tool
that ghcup tags with `recommended`. This sometimes lags behind the
`latest`-tagged version. With ghcup installed, run `ghcup list` for a better
understanding.

Following are general guides for any OS that provides the relevant tools. If you
have trouble, consider checking the CI workflow files in `.github/workflows`.

### Stack
We support the latest recommended version of Stack (as of 2021-09-17, Stack
2.7). Generally, any Stack 2.x should work.  *(Stack 1.x may work with minor
alternations -- you may have to download the resolver manually.)*

```
stack build
```

For an interactive shell:

```
stack build
stack ghci
```

Note that running `stack ghci` before running `stack build` won't work properly,
due to `stack ghci` not running build tools like Alex and Happy. So parser
modules will not be built, and you'll receive an error after building the other
modules. You can cheat a bit and run `stack build` until you see `Building
library for [...]` (= preprocessing has finished), then hit `<Ctrl-C>` to stop
the build and run `stack ghci` as usual.

### Cabal
We support the latest recommended version of Cabal (as of 2021-09-17, Cabal 3.4)

```
cabal build
```

### Testing
Unit tests are stored in `test`. Run with `stack test` or `cabal test`.

## Usage
### As a dependency
fortran-src is available on Hackage and Stackage, so for Cabal or Stack projects
you should only need to add `fortran-src` to your project dependencies.

If you need a specific version of fortran-src in a Stack setup, you can stuff a
Hackage reference into `stack.yaml` using `extra-deps`, like:

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

We provide prebuilt binaries for some platforms: see the
[Releases](https://github.com/camfort/fortran-src/releases) tab.

Otherwise, you can build from source and use convenience commands like `cabal
run`, `stack run`. See [#Building](#building) for details.

## Contributing
We welcome bug reports, fixes and feature proposals. Add an issue or create a
pull request on the GitHub repository.

## Support
You may be able to find maintainers on the [Libera.Chat](https://libera.chat/)
IRC network. Check in #fortran-src and #camfort . Otherwise, you could get into
contact with one of the team on the [CamFort team
page](https://camfort.github.io/team.html) -- or create an issue describing your
problem and we'll have a look.

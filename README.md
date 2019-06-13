# fortran-src

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
)



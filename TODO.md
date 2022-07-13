## Small refactorings
  * Many AST nodes could be upgraded to use `NonEmpty` - some appear to be
    nonsensical for the empty list case (e.g. `Declarator` with `ArrayDecl`)
    * May need another newtype to work with `AList`
  * How to handle empty `AList`s
    * Empty `AList`s are hard to give `SrcSpan`s to (but probably OK to do so)
    * Some syntax allows omitting brackets for empty lists
    * `Maybe AList` fixes the problem, but now we have `Just []` and `Nothing`
    * Ideal solution is probably more `AList`-likes that encode some extra
      syntactic info while storing a regular list. Large scale change
    * For now, moved `ExpFunctionCall` and `StCall` from `Maybe AList` to
      `AList`, with notes on problematic spans

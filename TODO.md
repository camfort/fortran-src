## Small refactorings
  * Many AST nodes could be upgraded to use `NonEmpty` - some appear to be
    nonsensical for the empty list case (e.g. `Declarator` with `ArrayDecl`)
    * May need another newtype to work with `AList`
  * `SrcSpan` for empty `AList`, and brackets problem
    * Moving from `Maybe AList` to `AList` simplifies representation, but means
      we need to provide a `SrcSpan`. To enable correct reprinting, we would
      need to use an `AList`-like wrapper that explicitly states whether there
      were brackets or not.
    * Only problematic for reprintings below `Expression` level, which likely
      aren't done. But moving has opened up that avenue.

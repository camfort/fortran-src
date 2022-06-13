## Small refactorings
  * Many AST nodes could be upgraded to use `NonEmpty` - some appear to be
    nonsensical for the empty list case (e.g. `Declarator` with `ArrayDecl`)
    * May need another newtype to work with `AList`

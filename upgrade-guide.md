# fortran-src upgrade guide
## Release 0.9.0
### Parser restructure
***Necessitates changes.***

Instead of grabbing parsers directly from `Language.Fortran.Parser.FortranXYZ`,
import `Language.Fortran.Parser` qualified and use one of the many provided
functions. If you need to do more complex parser incantations, we recommend
using the combinators in `Parser`.

In general, `parserVersions` and the parsers exported from respective parser
modules can be replaced by `Parser.byVer`, `Parser.f77e` etc. The filepath
argument now comes before the contents bytestring, so you may have to swap
argument order (done to match other parsing libraries and most common usage).

Also, some shims have been removed, primarily the `FortranVersion` re-export
from `ParserMonad`. If you need `FortranVersion`, import
`Language.Fortran.Version`.

## Release 0.8.0
### Declarator constructor refactor
***Necessitates changes.***

`Declarator`s are now a single constructor. The array/scalar part is moved into
a new `DeclaratorType` field. The idea is that scalar and array declarators
share most fields, and this enables skipping some unwanted explicitness when
pattern matching.

This means you have to fix up your pattern matches. For example, the following
case:

```haskell
case decl of
    DeclArray _ _ _ dims _ _ -> Right dims
    _                        -> Left "error, scalar"
```

could be rewritten neater:

```haskell
case declaratorType decl of
    ArrayDecl dims -> Right dims
    ScalarDecl     -> Left "error, scalar"
```

Regardless of context, you can always lazily replace `DeclArray _ _ _ dims _ _`
with `Declarator _ _ _ (ArrayDecl dims) _ _`.

## Release 0.7.0
### BOZ value constructor
***May necessitate changes.***

Previously, BOZ constants were parsed into the `ValInteger` constructor. Now
they're parsed into their own constructor, storing the new parsed BOZ type. Any
code that previously had to split out BOZs by "parsing" `ValInteger` no longer
needs to. For example:

```haskell
case val of
  ValInteger i ->
    case maybeParseInt i of
      Just i' -> WrappedValInt i'
      Nothing -> WrappedValBoz i
```

We no longer need the parsing check:

```haskell
case val of
  ValInteger i -> WrappedValInt (parseInt i)
  ValBoz     b -> WrappedValBoz b
```

### Value representation upgrade
***Necessitates changes.***

Reals and logicals are stored parsed, instead of being stored as strings (which
have previously been matched against a regex). Code that touched them will need
to be changed. It will likely cut out any manual parsing that you had to
perform.

Also, literals which permit a kind parameter now store that in a separate field.
Any code which performed the re-parsing manually should be rewritten. If your
code targeted fixed form Fortran, you probably never cared about kind
parameters, so your changes will include rewriting `ValInteger x` to `ValInteger
x _`  and so on.

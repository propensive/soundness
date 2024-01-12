When working with front-end applications that must be presented to different users in different languages, it's
common that the vast majority of the code which provides the user interface will be identical for every
language, with the exception of the strings which provide the text to be used in that user interface.

It is therefore useful to abstract over just the parts which differ, and to minimise repetition of the parts
which are the same.

### Language types

Languages are represented by types named after their ISO 639-1 code, for example `En` (English), `La` (Latin) or
`De` (German). These are found in the `cosmopolite.languages` object, and each of these would typically be
imported for use in a particular project, for example,
```scala
import language.{En, La, De}
```

Additionally, ten "common" languages are also made available in the `cosmopolite.languages.common` object,
which can be conveniently imported with a wildcard import. These languages are:
- German (`De`)
- English (`En`)
- Spanish (`Es`)
- French (`Fr`)
- Italian (`It`)
- Japanese (`Ja`)
- Polish (`Pl`)
- Portuguese (`Pt`)
- Russian (`Ru`)
- Chinese (`Zh`)

Each of these is an alias for the language type defined in the `cosmopolite.languages` object.

Where more than one language needs to be specified in Cosmopolite, this is done as a union of language types,
for example, the languages English, Polish and Russian would be represented as `En | Pl | Ru`.

The order of the language types in the union is not significant, as Scala will consider `Pl | En | Ru` an
identical type to `En | Pl | Ru`, or any other permutation.

### Multilingual strings

A multilingual string is represented by an instance of `Messages[L]` where `L` is the set of languages that
instance has, as a union type, and which is guaranteed by its construction.

Constructors exist for single-language `Messages` instances of all common languages as prefixed strings, for
example, `en"English text"` or `fr"Texte français" will create instances of `Messages[En]` and `Messages[Fr]`
respectively.

Any pair of messages may be combined with the `&` operator to construct a `Messages` instance of all their
languages combined. For example, `en"English text" & fr"Texte français"` will create an instance of
`Messages[En | Fr]`.

Using these construction methods makes it possible to define instances of `Messages` whose type is
guaranteed to reflect their content.

The `&` combinator adds the further guarantee that every additional language combined with an existing
multilingual string must not already be defined, so it should be impossible to accidentally overwrite a string
for one language with another when constructing a `Messages` instance.

#### Defining constructors for other languages

Convenient constructors for languages that are not in the `common` object can be provided as extensions on
Scala's built-in `StringContext` type, and follow this pattern,
```scala
extension (ctx: StringContext)
  def la(msgs: Messages[La]*): Messages[La] = Messages(ctx.parts, msgs)
```
which defines a constructor for the `La` language (Latin) on strings starting with the prefix `la""`.

### The `Language` type

In addition to representing product types for a collection of languages, we need a type that represents a single
language chosen from a collection of languages.

This `Language` instance will typically be constructed from a value that is only known at runtime, as it will be
used to pick one particular language from a multilingual string. To construct a new `Language[L]` instance from
a set of languages, `L`, we can parse it from a `String` with the `Languages.parse` method.

As long as `L` is concretely known, Cosmopolite will build a parser that checks its input string against each of
languages in the union, and returns an `Option[Language[L]]` instance.

For example,
```scala
Language.parse[En | Fr]("fr")
```
would return `Some(Language[Fr])` while,
```scala
Language.parse[En | Fr]("es")
```
would return `None`.

### Type Aliases

It's likely that an application will use the same set of languages globally, so it's useful to define a global
type alias for these languages, for example,
```scala
type Langs = En | Fr | De | Es
```
and to use the `Langs` type everywhere instead of any specific language types. The type will always dealias to
the union of language types, and the Scala compiler can continue to perform all necessary checks as before.

Consequently, adding a language to such an alias can be a useful way to statically identify every multilingual
string that needs to be adapted to include the messages.

For example, changing the `Langs` definition above to,
```scala
type Langs = En | Fr | De | Es | It
```
would suddenly result in multiple compile errors: one for each usage of a multilingual string that fails to
provide an Italian string.

This offers an additional level of safety when developing for multilingual applications, as it becomes
impossible to compile code which does not provide language strings for every language required.

### Abstract Languages

It is also largely possible to define methods and classes which are language-independent. It is possible to
define a method which takes both `Messages[L]` instances and a `Language[L]` instance, and combines them without
ever concretely knowing the language `L`.

As long as the type parameter of the `Messages` instance is a supertype of the type parameter of the `Language`
instance—and most likely the types would be equal—they may be combined to produce a string.

As one example use case, we could write a language-independent login form (for whatever framework we like) that
takes field and button labels as multilingual strings, and a corresponding `Language` instance. In combining
the `Language` coproduct with each of the `Messages` products, their type parameter would be eliminated.

Note, however, that the `Language.parse` method, however, can only be invoked on a concrete type, as it needs
to build a concrete parser for the languages in the union type.




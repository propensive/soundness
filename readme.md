[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/cosmopolite/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/cosmopolite/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/cosmopolite" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Cosmopolite

_Cosmopolite_ provides a typesafe representation for working with multilingual strings, `Messages`, with
convenient constructors which guarantee through their types that translations for a specific set of languages
exist in each instance. Furthermore, a `Language` type provides a coproduct representation of a language from
the same set which can safely select one string from the set.

## Features

- provides a representation for multilingual strings
- provides a representation of a language
- interpolated string constructors for common languages
- support for all [ISO 639-1](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) languages
- users choose their exact set of languages with a union type
- support for all language guaranteed by type for all static strings
- additions to the language set produce compile errors for every incomplete multilingual string
- checks for duplicate languages in construction of multilingual strings

## Getting Started

When working with front-end applications that must be presented to different users in different languages, it's
common that the vast majority of the code which provides the user interface will be identical for every
language, with the exception of the strings which provide the text to be used in that user interface.

It is therefore useful to abstract over just the parts which differ, and to minimise repetition of the parts
which are the same.

## Language types

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

## Multilingual strings

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

### Defining constructors for other languages

Convenient constructors for languages that are not in the `common` object can be provided as extensions on
Scala's built-in `StringContext` type, and follow this pattern,
```scala
extension (ctx: StringContext)
  def la(msgs: Messages[La]*): Messages[La] = Messages(ctx.parts, msgs)
```
which defines a constructor for the `La` language (Latin) on strings starting with the prefix `la""`.

## The `Language` type

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

## Type Aliases

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

## Abstract Languages

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

## Related Projects

The following _Niveau_ libraries are dependencies of _Cosmopolite_:

[![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

No other _Niveau_ libraries are dependents of _Cosmopolite_.

## Status

Cosmopolite is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Cosmopolite is designed to be _small_. Its entire source code currently consists of 304 lines of code.

## Availability

Cosmopolite&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/cosmopolite`.
```
fury layer clone -i propensive/cosmopolite
```
or imported into an existing layer with,
```
fury layer import -i propensive/cosmopolite
```

## Contributing

Contributors to Cosmopolite are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/cosmopolite/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Cosmopolite easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Cosmopolite was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Cosmopolite is so-named because it provides multilingual strings, which are free to travel, and are not associated with any particular nation.

## License

Cosmopolite is copyright &copy; 2021-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

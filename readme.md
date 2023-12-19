[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/xylophone/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/xylophone/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Xylophone

__Typesafe XML for Scala__

_Xylophone_ is an XML library for Scala that takes advantage of many features of the language to
provide intuitive syntax for manipulating XML, as well as better typesafety and static checks.

## Features

- parse and represent XML in Scala
- statically check XML in `x""` interpolators
- substitute standard and custom types (provided by typeclasses) into XML
- automatically derive typeclasses to convert case classes and product types to and from XML
- safe dynamic interface for accessing nested fields


## Availability

Xylophone has not yet been published as a binary.

## Getting Started

### Parsing

A `Text` value containing XML may be parsed with,
```scala
Xml.parse(text)
```
which will return an instance of `Xml`, or throw an `XmlParseError` if the XML is not well-formed.

### XML Literals

`Xml` values may also be constructed using the `x""` interpolator. These will be checked for well-formedness
at compiletime: all syntax must be valid, special characters escaped, and all tags must be closed and nested
correctly.

```scala
val book = x"<book><author>H. G. Wells</author><title>War of the Worlds</title></book>"
```

### XML AST Representation

An `Xml` value is a general type representing XML in three different forms, subtypes of `Xml`:
- `XmlDoc` is a complete XML document, and includes the `<?xml...` header
- `XmlNode` is a single XML node
- `XmlFragment` is a _fragment_ of XML, which may include zero, one or many XML nodes

While all three subtypes represent XML, there is some overlap in what may be represented by each,
but important differences in their behavior (ex, and most Xylophone methods are careful to return precisely-typed
values.

Furthermore, the `Xml` subtypes serve as wrappers around several other AST node types, subtypes of the `Ast`
enumeration:
 - `Element`
 - `Comment`
 - `ProcessingInstruction`
 - `Textual`
 - `CData`
 - `Root`

Of these types, `Element` and `Root` include fields which may include sequences of other `Ast` nodes, thereby
forming a tree structure.

### Accessing elements

Unlike some other data definition languages such as JSON, XML requires a distinction to be made between a
single node (`XmlNode`) and a sequence of nodes (`XmlFragment`). Multiple nodes with the same tag name
may exist as children of another node, while it is common for some variants of XML to use unique tag names
for every child node.

Both approaches are supported in Xylophone with very simple syntax.

For example, given the XML,
```xml
<library>
  <book>
    <author>H. G. Wells</author>
    <title>The War of the Worlds</title>
  </book>
  <book>
    <author>Virginia Woolf</author>
    <title>Mrs. Dalloway</title>
  </book>
</library>
```
as an instance of `XmlNode`, `library`, we can access the two book nodes with `library.book`, as an `XmlFragment`.
Subsequently calling, `library.book.title` would return a new `XmlFragment` consisting of the titles of _both_ books,
specifically,
```xml
<title>The War of the Worlds</title>
<title>Mrs. Dalloway</title>
```

Given an `XmlFragment`, the *nth* node in the sequence may be accessed by applying the node index, for example,
```scala
library.book.title(1)
```
would return,
```xml
<title>Mrs. Dalloway</title>
```
as an `XmlNode`.

The same node could alternatively be accessed with `library.book(1).title(0)`. Note how `0` is applied to the
`title` now (instead of `1`) since we want the first (`0`) `title` element of the second (`1`) 'book' element.
The application of `(0)` returns a single `XmlNode` instance rather than an `XmlFragment` instance.

The `apply` method of `XmlFragment` has a default value of `0` for convenience in the very common case where the
first node is required, e.g. `library.book().title()`

An `XmlFragment` of all the elements inside an `XmlNode` can always be obtained by calling `*` on the `XmlNode`
value. For example, `library.book().*` would return an `XmlFragment` of,
```xml
<title>The War of the Worlds</title>
<author>H. G. Wells</author>
```

### Extracting typed values

An `Xml` value is _dynamic_ in the sense that it could represent a single string value or deeply-nested structured
data. Usually we want to convert `Xml` values to other Scala types in order to use them. This can be achieved by
calling `as[T]` on the value, for an appropriate choice of `T`.

For example, the `XmlNode`,
```scala
val name: XmlNode = x"<author>Virginia Woolf</author>"
```
could be converted to a `Text` with, `name.as[Text]`. Or,
```scala
val age: XmlNode = x"<age>18</age>"
```
can be read as a `Long` with, `age.as[Long]`.

This works for `Text`s and primitive types. But will also work for case classes composed of these types (or
of other nested case classes). For example, given the definition,
```scala
case class Book(title: Text, author: Text)
```
a book from the _library_ example above could be read with:
```scala
library.book().as[Book]
```

The `as` method can also extract collection types (e.g. `Set`, `List` or `Vector`) from an `Xml` value, so
_all_ the books in the library could be accessed with,
```scala
library.book.as[Set[Book]]
```

In general, extraction requires a contextual `XmlReader` typeclass instance for the type to be extracted.
These exist on the `XmlReader` companion object for the basic types, collection types, product types
(e.g. case classes) and coproduct types (e.g. enumerations), but other instances may be provided.

### Writing to XML

Likewise, these same types may be converted to `Xml` by calling the `xml` extension method on them, for
example, given,
```scala
case class Book(title: Text, author: Text)
val book = Book(t"Mrs. Dalloway", t"Virginia Woolf")
```
we could create an `XmlNode` value of,
```xml
<Book>
  <title>Mrs. Dalloway</title>
  <author>Virginia Woolf</author>
</Book>
```
just by calling `book.xml`.

Note that the element labels will be taken from the case class's type name and field names. However, for
nested case classes, a type name will only appear in the XML output for the outermost tag name, since the
field name will be used in these cases.

The type name will also appear in the repeated child nodes of XML produced from a collection type, for
example, writing the `List[Int]`, `List(1, 2, 3)` would produce the XML,
```xml
<List>
  <Int>1</Int>
  <Int>2</Int>
  <Int>3</Int>
</List>
```

In general, the type name will be used for a node if the context does not suggest a more specific name.

The node nade may be controlled, however, using annotations. The `@xmlLabel` attribute may be applied
to a case class or a case class field to change its name when written or read.

For example, the definition,
```
@xmlLabel(t"book")
case class Book(title: Text, author: Text, @xmlLabel(t"type") kind: Text)
```
would ensure that a `Book` instance is written using the lower-case tag name, `book`, and the `kind`
field would be serialized the name `type` (which cannot be used so easily in Scala, as it's a keyword).

### Serialization

XML usually needs to be serialized to a string. Xylophone provides a `show` method that will serialize
an `Xml` value to a `Text` value using a contextual `XmlPrinter`, of which two are available by default:
one which omits all unnecessary whitespace, and one which "pretty prints" the XML with indentation for
nesting.



## Status

Xylophone is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Xylophone is designed to be _small_. Its entire source code currently consists
of 675 lines of code.

## Building

Xylophone can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Xylophone are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/xylophone/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Xylophone easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Xylophone was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A _xylophone_ is a musical instrument made from wood ("xylo-") or trees, and it provides a representation of XML trees. "Xylophone" and "XML" begin with the same infrequently-used letter.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows two angle brackets (or chevrons), representing the most significant symbols in XML, placed next to each other to look like a capital X.

## License

Xylophone is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

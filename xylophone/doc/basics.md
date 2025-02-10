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




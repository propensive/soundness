## XML

### About

Soundness reads and writes [XML](https://en.wikipedia.org/wiki/XML). Text parses into an `Xml`
value; a case class converts to and from XML with the encoder and decoder derived from its shape;
and a document's elements can be navigated with the compiler checking each step. XML written
literally with the `x"…"` interpolator is parsed and checked as the code compiles, so a malformed
fragment is a compile error.

The design mirrors the one Soundness uses for [JSON](json.md): parsing keeps the structure and the
types together, conversions are derived rather than written by hand, and a conversion that cannot be
made raises a typed error naming the reason.

### On XML

The usual XML API is the [DOM](https://en.wikipedia.org/wiki/Document_Object_Model): an untyped tree
whose elements are fetched by string name, whose text is pulled out and parsed by hand, and whose
mismatch with what the code expects surfaces as a `null` or a cast failure. The verbosity of XML is
compounded by an interface that checks nothing.

Soundness derives the conversion between XML and a Scala type from the type itself — a case class
becomes an element with a child per field, an enumeration becomes an element named for its case — so
there is nothing to keep in step by hand. Navigation is checked, literals are checked as they are
written, and a failed conversion is a typed `Xml.Error`. Everything comes from the `soundness`
package, with a schema and an error strategy in scope:

```scala
import soundness.*
import strategies.throwUnsafely

given XmlSchema = XmlSchema.Freeform
```

An XML value that is immutable, with every edit returning a new value, follows [immutability](../philosophy/immutability.md).

### Parsing

Text becomes an `Xml` value with `read`, and `load` reads a whole document, keeping its `<?xml?>`
header:

```scala
val doc = t"<message>hello world</message>".read[Xml]
```

The distinction matters. `read` yields the content; `load` yields a `Document`, pairing the root
with its `Header` — the version, and the encoding and standalone declarations where they are
given — so a document round-trips with its declaration intact rather than losing it on the way in:

```scala
import threading.platformThreading

supervise:
  t"""<?xml version="1.0"?><root>content</root>""".load[Xml]
// Document(elem(t"root", TextNode(t"content")), Header(t"1.0", Unset, Unset))
```

Anything in the prolog — a comment, a processing instruction, a `<!DOCTYPE>` — is kept as a node
before the root rather than discarded, so a stylesheet instruction survives a read and a write.

### The node types

An `Xml` value is one of the node kinds the specification defines, and each is a distinct type
rather than a tagged string. `Element` holds a name, attributes and children; `TextNode` holds
character data; and the rest carry what other libraries tend to flatten away:

- `Cdata` keeps a `<![CDATA[…]]>` section as a section, so text that contains `<not a tag>`
  survives a round trip without being escaped into something else.
- `Comment` keeps `<!-- … -->`, wherever it appears.
- `ProcessingInstruction` keeps a target and its data, including the empty-data case `<?target?>`.
- `Doctype` keeps a `<!DOCTYPE>` declaration, which serializes back verbatim.
- `Fragment` holds a sequence of nodes with no single parent, which is what a prolog plus a root
  amounts to.

The five predefined entities — `&amp;`, `&lt;`, `&gt;`, `&quot;` and `&apos;` — resolve on the way
in and are re-escaped on the way out, as are numeric character references in both decimal and
hexadecimal.

### Namespaces

The parser resolves namespaces as it reads. A declaration, `xmlns="…"` or `xmlns:p="…"`, is kept
as an attribute, so a document writes back exactly as it was read, and it also binds the prefix for
the element and its descendants. Every `Element` carries the bindings in force at it as its
`scope`, and resolves its own name through them:

```scala
val doc = t"""<r xmlns:a="urn:a"><a:x>1</a:x></r>""".read[Xml]
doc.`a:x`().namespace   // urn:a
doc.`a:x`().localName   // x
doc.`a:x`().qualified   // Xml.Name(urn:a, x), shown as {urn:a}x
```

Two names are the same name when their namespace URIs and local parts agree, whatever prefixes
bind them. A prefixed name selects by resolved name when its prefix is bound, whether by a
declaration in the document or by a `Namespace` given in scope, so the prefix the code uses need
not be the one the document used:

```scala
given svg: ("svg" is Namespace of "http://www.w3.org/2000/svg") = Namespace()
picture.`svg:rect`()                                   // every rect in the SVG namespace
picture.elements(Xml.Name(t"http://www.w3.org/2000/svg", t"rect"))
```

A `Namespace` binds a prefix, its `Self`, to a URI, its `Topic`, both as singleton types, so the
binding is checked wherever it is used. An `x"…"` literal that uses a prefix it does not declare
takes the binding from the givens in scope, and an `xp"…"` path does the same for its name tests,
so `xp"//svg:rect"` matches by namespace. `XPath#in(scope)` rebinds a path's prefixes.

A prefix with no binding is an error, as the Namespaces recommendation requires: parsing `<p:a/>`
without a declaration of `p` raises a `Parse.Error`, and writing `x"<p:a/>"` without a
`Namespace` given for `p` does not compile. Import `namespaceOptions.lenientNamespaces` to have an
unbound prefix resolve to no namespace instead.

When an `Xml` tree is written, an element whose scope binds a prefix it uses (or its default
namespace) which nothing above it has declared gets the declaration written for it, so a subtree
built in code, or cut from a document with a selection, serializes namespace-well-formed. The
scope takes no part in equality: a parsed element equals the same element built by hand.

A derived codec puts a case class in a namespace with an annotation:

```scala
@xmlns("urn:shop")
case class Order(id: Int, item: Item)
```

`Order(1, Item(t"a", 2)).in[Xml]` writes `<Order xmlns="urn:shop">` with its fields' elements in
the same namespace, and `as[Order]` reads a document whose elements resolve to that namespace,
however it prefixes them. `@xmlns("urn:shop", qualified = false)` on the class, or `@unqualified`
on a field, keeps the fields' elements in no namespace, as a schema with
`elementFormDefault="unqualified"` has them. A type which cannot be annotated takes its namespace
from a given: `given Rect is Xml.Namespaced = Xml.Namespaced(t"http://www.w3.org/2000/svg")`.
A type with no namespace of its own matches its fields' elements by name alone, whatever
namespace the document puts them in.

### Reading values

An `Xml` value converts to a Scala type with `as`. Content that cannot be read as the target type
raises an `Xml.Error`:

```scala
x"<message>42</message>".as[Int]   // 42
```

### Case classes

A case class needs no annotation to take part in XML. Its encoder and decoder are derived from its
fields: the value becomes an element named for the type, with a child element for each field, and
reads back the same way regardless of the order of the children:

```scala
case class Worker(name: Text, age: Int)

Worker(t"Alice", 30).in[Xml]
// x"<Worker><name>Alice</name><age>30</age></Worker>"

t"<Worker><name>Alice</name><age>30</age></Worker>".read[Worker in Xml]
// Worker(t"Alice", 30)
```

A field marked `@attribute` becomes an attribute rather than a child element, and round-trips as
one:

```scala
case class Book(title: Text, @attribute isbn: Text)

Book(t"Dune", t"0441013597").in[Xml]
// x"""<Book isbn="0441013597"><title>Dune</title></Book>"""
```

`@name` renames the wire label where the Scala name and the XML name should differ, and
`@name[Xml]` confines the rename to XML, leaving other formats to their own. It renames the
element of an enumeration's *variant* too, so a `Light.Stop` may travel as `<red>`:

```scala
enum Light:
  @name[Xml](t"red") case Stop(seconds: Int)
  @name(t"green") case Go(seconds: Int)
  case Wait(seconds: Int)

(Light.Stop(30): Light).in[Xml]   // x"<red><seconds>30</seconds></red>"
```

A sum type decodes by its element label, so the element's name selects the variant; a label
naming no variant raises an `Xml.Error` rather than falling through to a default.

An `Optional` field is simply absent when it is `Unset` — no child element, or for an
`@attribute` field no attribute — and a document without the element decodes it as `Unset`
rather than as an error. A `Map` field becomes one child element per entry, each holding a
`<key>` and a `<value>`, so a key need not be a valid element name and may be any XML-encodable
type:

```scala
case class Profile(name: Text, nickname: Optional[Text], stock: Map[Text, Int])

Profile(t"Ann", Unset, Map(t"apple" -> 3)).in[Xml]
// x"<Profile><name>Ann</name><stock><key>apple</key><value>3</value></stock></Profile>"
```

### What decoding tolerates

Real XML is untidy, and a decoder that insists on tidiness is of little use. Text, comments and
processing instructions interleaved between the children a type expects are ignored, so a
document with prose between its fields decodes as though the prose were not there:

```scala
t"<root>hello<name>A</name><!--c--><?pi data?> <age>4</age>bye</root>".read[Worker in Xml]
// Worker(t"A", 4)
```

Entities in leaf text expand as they are read. Repeated elements gather into a collection field in
document order, and they need not be contiguous — a `<songs>` before the `<name>` and another
after it still form one list, in the order they appeared. Recursive types tie through their own
derivation, so a tree of arbitrary depth encodes and decodes without a hand-written codec.

Where a nested value is missing altogether, a `Default` for its type turns what would be a cascade
of sub-field errors into a single error at the point the value should have been, with the default
used to carry on — which is what a validation pass reporting to a human wants.

### Writing XML literally

The `x"…"` interpolator writes XML directly and checks it as the code compiles. Holes substitute
values, and a malformed fragment is rejected where it is written:

```scala
val name = t"Alice"
x"<user>$name</user>"
```

### Navigating

With dynamic access enabled, a child element is reached as though it were a member, and the steps
chain; an index picks among repeated elements:

```scala
import dynamicAccess.dynamicXml

val data = t"<a><b><c>42</c></b></a>".read[Xml]
data.b().c().as[Int]   // 42

val list = t"<r><x>1</x><x>2</x></r>".read[Xml]
list.x(Sec).as[Int]    // 2 — the second <x>
```

The import grants `Xml is Dynamical` to the rest of its scope; `dynamically[Xml]:` grants
it to a single block instead, as described in the JSON tutorial.

### Updating

An element is updated through a lens, which reaches through several levels and may carry optics such
as `Each` to touch every matching element at once. Because XML values are immutable, an update
returns a new document:

```scala
import dynamicAccess.dynamicXml

val document = t"<doc><x>1</x><x>2</x><x>3</x></doc>".read[Xml]
document.lens(_.x = x"<x>9</x>").show   // the first <x> replaced
document.lens(_(Each) = x"<x>0</x>").show   // every <x> replaced
```

### Formatting

The output format is a given in scope: compact formatting omits whitespace, while indented formatting
adds newlines and indentation for reading:

```scala
import formatting.indentedXmlFormatting

Worker(t"Alice", 30).in[Xml].show   // indented across several lines
```

`show` renders the whole node into one `Text`. A large document need not be held in memory
before it is sent. The push form of `Xml.emit` serializes a `Document[Xml]`, with its header, on
the caller's thread and hands each block to a function as it fills — as text with `emit[Text]`,
or as UTF-8 bytes with `emit[Data]`, escaped and encoded straight from the serializer — which is
the shape for writing to a file, a socket or an `OutputStream`:

<!-- doccheck: skip -->
```scala
Xml.emit[Data](document, chunk => socket.write(chunk))
```

Where even the copy into each `Data` chunk is unwanted, `Xml.lend` lends each filled block
instead: the consumer sees the writer's own buffer as a `Region`, with a branded interval that
proves every index in range, valid only for the duration of the call — the same discipline as a
stream's `lend`:

<!-- doccheck: skip -->
```scala
Xml.lend(document): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))
```

Where a consumer pulls instead, `Xml.emit(document)` serializes on a fiber and hands out the text
through an iterator, and a `Document[Xml]` is `Streamable` by `Text` on the same terms, under a
`supervise` block:

<!-- doccheck: skip -->
```scala
supervise:
  Xml.emit(document).each(chunk => out.write(chunk))
```

### Paths

An [XPath](https://en.wikipedia.org/wiki/XPath)-like path names a location within a document. The
`xp"…"` interpolator writes one and checks it as the code compiles, with a step's ordinal
defaulting to the first match and `@` naming an attribute:

```scala
xp"/root[1]/child[2]".encode   // t"/root[1]/child[2]"
xp"/root[1]/@id".encode        // t"/root[1]/@id"
xp"/root/child".encode         // t"/root[1]/child[1]"
```

Paths are what positions and accrued errors are reported against, so an error from decoding a
large document names the element that caused it.

### Querying with XPath

The same interpolator writes a full [XPath 1.0](https://www.w3.org/TR/xpath-10/) expression, and
a document answers it. `select` returns the matching nodes as a fragment, `selectText` the text
of the first match, and `evaluate` the value of any expression — a number, a boolean, a string or
a node set — as an `XPath.Value`. Axes, predicates on attributes and text, and the core function
library are all supported, which is what a browser-automation locator or a configuration lookup
needs:

```scala
val page = t"""<app><div id="main"><button data-test="submit">Submit</button>
                 <ul><li>one</li><li>two</li><li>three</li></ul></div></app>""".read[Xml]

page.selectText(xp"//button[text()='Submit']/@data-test")   // t"submit"
page.selectText(xp"/app/div[1]/ul/li[2]")                    // t"two"
page.evaluate(xp"count(//li)")                               // XPath.Value.Numeric(3)
```

An expression the engine does not support, or a variable it was not given, raises an
`XPath.Error` saying so.

### Typed records from an XML Schema

Where a document's shape is given as an XML Schema (XSD) rather than a Scala type, an
`Xml.Provider` reads the schema at compiletime and produces typed records from matching XML. A
provider object holds the schema, and its `record` method turns an `Xml` value into a record with
one member per child element and attribute of the schema's root element, each read at the type the
schema declares:

<!-- doccheck: skip -->
```scala
import classloaders.threadContextClassloader

object PurchaseOrder extends Xml.Provider(cp"/xsd/po.xsd")

val order = PurchaseOrder.record(document)
order.shipTo.city                          // Text
order.items.item.map(_.quantity)           // List[Long], a restriction of xs:positiveInteger
order.comment                              // Optional[Text], from minOccurs="0"
order.items.item.map(_.partNum)            // Text, checked against the SKU pattern as it is read
```

The schema may be an `Xsd` value, an `Xml` document, the schema's text, or anything readable as
text, such as a classpath resource. Where a schema declares several global elements the root is
the one with complex content, or is chosen with `root = t"Envelope"`.

Child elements and attributes become fields by local name. A field is `Optional` when its element
has `minOccurs="0"` or is `nillable`, and a `List` when `maxOccurs` allows more than one; each
alternative of a `choice` is `Optional`. An attribute that shares a name with a child element is
reached as `` `@name` ``, and the text of an element with simple content and attributes as `text`
(`` `#text` `` if an attribute is called `text`). An extension's base type contributes its fields
first.

| XSD type | Scala type |
|---|---|
| `xs:string` and the token, name and identifier types, `xs:QName`, the binary types, `xs:gYear` and relatives | `Text` |
| `xs:boolean` | `Boolean` |
| `xs:int`, `xs:short`, `xs:byte` | `Int`, `Short`, `Byte` |
| `xs:integer`, `xs:long` and the unsigned, positive and negative integer types | `Long`, with the implied bounds checked |
| `xs:decimal`, `xs:double`, `xs:float` | `Double`, `Double`, `Float` |
| `xs:dateTime`, `xs:date`, `xs:time`, `xs:duration`, `xs:anyURI` | the type an interface in scope instantiates, else `Text` |
| `xs:NMTOKENS`, `xs:IDREFS` and any `xs:list` | `List[Text]` |
| `xs:anyType`, `xs:any`, a recursive type, an unresolved reference | `Xml` |

A restriction's facets — `enumeration`, `pattern`, the inclusive and exclusive bounds, the length
facets and the digit facets — are checked as the field is read, raising `Xml.Provider.Error` with
the reason. The provider matches a document's elements by resolved name: a child is expected in the
schema's target namespace when the schema qualifies it (`elementFormDefault`, a `form`, or a `ref`
to a global element) and in no namespace otherwise, whatever prefixes the document uses; an element
in another namespace is absent.

Substitution groups, `xsi:type`, identity constraints and mixed content are read as the schema
declares without them; an `import` or `include` is recorded in the `Xsd` but not fetched, so a
type from another schema reads as raw `Xml`. The provider does not validate documents: it checks
what a program reads.

### Positions and errors

A malformed document raises a `ParseError` whose position is not merely a line number but a range:
the offset and length of the text at fault, so a tool can underline exactly the mismatched closing
tag or the unterminated attribute rather than the whole line.

Positions of *well-formed* content are recorded on request, in the same way as for
[JSON](json.md#source-positions):

```scala
import parsing.trackPositions

val source = t"<root><child/></root>"
val tracked = source.load[Xml]
```

Under an accruing strategy, decoding reports every fault in the document at once, each with the
path to the element that failed, rather than stopping at the first.

### XML over HTTP

An `Xml` value serves as a request or response body with the `application/xml` media type, and a
body parses back to `Xml` on arrival, so an XML API is consumed and offered with no glue between
the XML and [HTTP](http-client.md) layers.

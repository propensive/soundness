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
written, and a failed conversion is a typed `XmlError`. Everything comes from the `soundness`
package, with a schema and an error strategy in scope:

```scala
import soundness.*
import strategies.throwUnsafely

given XmlSchema = XmlSchema.Freeform
```

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

Namespace declarations are attributes, and prefixed names are names, so both survive parsing
unaltered:

```scala
t"""<a xmlns="http://example.com"/>""".read[Xml]
t"""<a xmlns:p="http://example.com"/>""".read[Xml]
t"<p:a/>".read[Xml]
```

Nothing is rewritten into a resolved form, so a document written with prefixes is written back
with the same prefixes — which is what matters where the document's exact bytes are covered by a
signature, or where a downstream consumer matches on the prefix.

### Reading values

An `Xml` value converts to a Scala type with `as`. Content that cannot be read as the target type
raises an `XmlError`:

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
  case @name[Xml](t"red") Stop(seconds: Int)
  case @name(t"green") Go(seconds: Int)
  case Wait(seconds: Int)

(Light.Stop(30): Light).in[Xml]   // x"<red><seconds>30</seconds></red>"
```

A sum type decodes by its element label, so the element's name selects the variant; a label
naming no variant raises an `XmlError` rather than falling through to a default.

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

### Positions and errors

A malformed document raises a `ParseError` whose position is not merely a line number but a range:
the offset and length of the text at fault, so a tool can underline exactly the mismatched closing
tag or the unterminated attribute rather than the whole line.

Positions of *well-formed* content are recorded on request, in the same way as for
[JSON](json.md#source-positions):

```scala
import parsing.trackPositions

val tracked = source.load[Xml]
```

Under an accruing strategy, decoding reports every fault in the document at once, each with the
path to the element that failed, rather than stopping at the first.

### XML over HTTP

An `Xml` value serves as a request or response body with the `application/xml` media type, and a
body parses back to `Xml` on arrival, so an XML API is consumed and offered with no glue between
the XML and [HTTP](http-client.md) layers.

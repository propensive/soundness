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

Worker(t"Alice", 30).xml
// x"<Worker><name>Alice</name><age>30</age></Worker>"

t"<Worker><name>Alice</name><age>30</age></Worker>".read[Worker in Xml]
// Worker(t"Alice", 30)
```

A field marked `@attribute` becomes an attribute rather than a child element, and `@name` renames the
wire label where the Scala name and the XML name should differ. An enumeration encodes as an element
named for its case.

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
import dynamicXmlAccess.enabled

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
import dynamicXmlAccess.enabled

val document = t"<doc><x>1</x><x>2</x><x>3</x></doc>".read[Xml]
document.lens(_.x = x"<x>9</x>").show   // the first <x> replaced
document.lens(_(Each) = x"<x>0</x>").show   // every <x> replaced
```

### Formatting

The output format is a given in scope: compact formatting omits whitespace, while indented formatting
adds newlines and indentation for reading:

```scala
import formatting.indentedXmlFormatting

Worker(t"Alice", 30).xml.show   // indented across several lines
```

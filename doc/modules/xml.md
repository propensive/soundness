## XML

### About

[XML](https://en.wikipedia.org/wiki/XML) text parses into an `Xml` value. A case class converts
to and from XML with an encoder and decoder [derived][derivation] from its shape, and a
document's elements can be navigated and queried. XML written literally, with the `x"…"`
[interpolator], is parsed and checked at [compiletime], so a malformed fragment does not
compile.

The design mirrors that of [JSON](json.md). Parsing keeps the structure and the types together.
Conversions are derived, not written by hand. A conversion which cannot be made raises a typed
error which names the reason.

### On XML

The usual API for XML is the [DOM](https://en.wikipedia.org/wiki/Document_Object_Model). It is
an untyped tree, whose elements are fetched by string name, and whose text is pulled out and
parsed by hand. Where the document does not match what the code expects, the mismatch surfaces
as a `null` or a failed cast. XML is verbose already, and an interface which checks nothing
makes it worse.

Here the conversion between XML and a Scala type is derived from the type. A case class becomes
an element with a child for each field, and an enumeration becomes an element named for its
case, so there is nothing to keep in step by hand. Literals are checked as they are written,
and a failed conversion is a typed `Xml.Error`. An `Xml` value is [immutable], and every edit
gives a new value, in keeping with [immutability](../philosophy/immutability.md).

Everything comes from the `soundness` package. A schema and an error [strategy] are in scope,
and the last import lets the samples print their results:

```scala
import soundness.*
import temporaryDirectories.javaBaseTemporaryDirectory
import strategies.throwUnsafely
import xmlSchemas.freeformXmlSchema
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

The schema is a choice, imported as a charset or a strategy is. `freeformXmlSchema` admits any
element and any attribute. An [XSD](#typed-records-from-an-xml-schema) takes its place when a
document's shape is known.

### Parsing

`read[Xml]` parses text into an `Xml` value, and `show` writes one back out:

```scala
val message = t"<message>hello world</message>".read[Xml]
```

```scala fuel
run
##
Out.println(message.show)
```

Text which is not well-formed XML is an error, which says what was wrong and where:

```scala fuel
run
errors show
##
t"<message>hello world</mesage>".read[Xml]
```

`load` reads a whole document, and keeps its `<?xml?>` header. Where `read` gives the content,
`load` gives a `Document`, which pairs the root with its `Header`: the version, and the
encoding and standalone declarations where they are given. A document is therefore written back
with its declaration intact:

```scala
import threads.platformThreads

val loaded = supervise:
  t"""<?xml version="1.0"?><root>content</root>""".load[Xml]
```

Both `read` and `load` take a source of bytes as readily as text: a [file](filesystem.md), the
body of an [HTTP response](http-client.md), a [classpath](classpath.md) resource. The bytes are
parsed directly, with no `Charset` in scope, since the parser reads UTF-8 itself:

```scala fuel
typecheck
##
val feed: Path on Linux = temporaryDirectory[Path on Linux] / "feed.xml"
val document = feed.load[Xml]
```

Anything in the prolog, such as a comment, a processing instruction or a `<!DOCTYPE>`, is kept
as a node before the root. A stylesheet instruction therefore survives being read and written.

### The node types

An `Xml` value is one of the kinds of node which the specification defines. Each is a distinct
type, in the `Xml` companion, and not a tagged string. Beyond elements and text, they hold what
other libraries tend to flatten away:

```tabular
column  Type
column  Holds
row  `Xml.Element`  a name, attributes and children
row  `Xml.Text`  character data
row  `Xml.Cdata`  a `<![CDATA[…]]>` section, kept as a section
row  `Xml.Comment`  a `<!-- … -->` comment, wherever it appears
row  `Xml.ProcessingInstruction`  a target and its data, including the empty-data case `<?target?>`
row  `Xml.Doctype`  a `<!DOCTYPE>` declaration, which serializes back verbatim
row  `Xml.Fragment`  a sequence of nodes with no single parent, such as a prolog plus a root
```

A CDATA section stays a section, so text which contains `<not a tag>` survives being read and
written, without being escaped into something else.

The predefined entities are resolved as a document is read, and escaped again as it is written.
So are numeric character references, in both decimal and hexadecimal:

```tabular
column  Entity
column  Character
row  `&amp;`  `&`
row  `&lt;`  `<`
row  `&gt;`  `>`
row  `&quot;`  `"`
row  `&apos;`  `'`
row  `&#NNN;`, `&#xHHH;`  the character with that code point
```

### Reading values

`as` converts an `Xml` value to a Scala type. Content which cannot be read as that type raises
an `Xml.Error`:

```scala fuel
run
##
Out.println(x"<message>42</message>".as[Int].show)
```

```scala fuel
run
errors show
##
x"<message>forty-two</message>".as[Int]
```

### Case classes

A case class takes part in XML as it stands. Its encoder and decoder are derived from its
fields. The value becomes an element named for the type, with a child element for each field,
and is read back whatever the order of the children:

```scala
case class Worker(name: Text, age: Int)

val alice = t"<Worker><age>30</age><name>Alice</name></Worker>".read[Worker in Xml]
```

```scala fuel
run
##
Out.println(Worker(t"Alice", 30).in[Xml].show)
Out.println(alice.inspect)
```

`in[Text over Xml]` encodes a value straight to its XML text, as the mirror of
`read[Worker in Xml]`, and `in[Data over Xml]` writes the same text as bytes, in the `Codepage`
in scope.

A field marked `@attribute` becomes an attribute instead of a child element, and is read back
as one:

```scala
case class Book(title: Text, @attribute isbn: Text)
```

```scala fuel
run
##
Out.println(Book(t"Dune", t"0441013597").in[Xml].show)
```

`@name` renames an element or attribute where the Scala name and the XML name should differ.
`@name[Xml]` confines the rename to XML, and leaves other formats to their own. It also renames
the element of an enumeration's *variant*, so a `Light.Stop` may be written as `<red>`:

```scala
enum Light:
  @name[Xml](t"red") case Stop(seconds: Int)
  @name(t"green") case Go(seconds: Int)
  case Wait(seconds: Int)

val stop: Light = Light.Stop(30)
```

```scala fuel
run
##
Out.println(stop.in[Xml].show)
```

A sum type is decoded by its element's name, which selects the variant. A name which matches no
variant raises an `Xml.Error`, and never falls through to a default.

An `Optional` field which is `Unset` is simply absent: there is no child element, or, for an
`@attribute` field, no attribute. A document without the element decodes the field as `Unset`.
A `Map` field becomes one child element for each entry, holding a `<key>` and a `<value>`, so
a key need not be a valid element name, and may be of any type which can be encoded in XML:

```scala
case class Profile(name: Text, nickname: Optional[Text], stock: Map[Text, Int])
```

```scala fuel
run
##
Out.println(Profile(t"Ann", Unset, Map(t"apple" -> 3)).in[Xml].show)
```

In summary:

```tabular
column  Scala
column  XML
row  a case class  an element named for the type
row  a field  a child element named for the field
row  an `@attribute` field  an attribute
row  an `@name` or `@name[Xml]` label  the element or attribute renamed
row  a variant of a sum type  an element named for the variant
row  an `Optional` field that is `Unset`  no element, or no attribute
row  a collection field  one element per item, named for the field, in order
row  a `Map` field  one element per entry, holding a `<key>` and a `<value>`
```

[Annotations](annotations.md) describes `@name` and `@attribute` and how they are read.

### What decoding tolerates

Real XML is untidy, and a decoder which insists on tidiness is of little use. Text, comments
and processing instructions which sit between the children a type expects are ignored. A
document with prose between its fields decodes as though the prose were not there:

```scala
val untidy =
  t"<root>hello<name>A</name><!--c--><?pi data?> <age>4</age>bye</root>".read[Worker in Xml]
```

```scala fuel
run
##
Out.println(untidy.inspect)
```

Entities in text are expanded as they are read. Repeated elements gather into a collection
field, in document order, and they need not be next to each other: a `<songs>` before the
`<name>` and another after it still form one list, in the order they appeared. A recursive type
is handled through its own derivation, so a tree of any depth encodes and decodes without a
hand-written codec.

Where a nested value is missing altogether, a `Default` for its type turns what would be a
cascade of errors about its fields into a single error, at the point where the value should
have been. The default is used to carry on, which is what a validation pass reporting to a
person wants.

A field which is required, and missing, is an error:

```scala fuel
run
errors show
##
t"<Worker><name>Alice</name></Worker>".read[Worker in Xml]
```

An `Optional` field has two further policies, from the package `optionalityOptions`, as
[Decoding](decoding.md#policies-for-optional-fields) describes:

```tabular
column  A field whose element or attribute is
column  Reads as
column  Unless
row  omitted from the document  `Unset`  `strictXmlAbsence` makes it an error
row  present but rejected by the inner decoder  an error  `lenientXmlFaults` reads it as `Unset`
```

Under `strictXmlAbsence`, a missing element or attribute is an `Xml.Error.Reason.Missing`. XML
has no null. An empty element is a present value, so there is no policy for nulls.

```scala
case class Tally(name: Text, count: Optional[Int])

val tolerant =
  import optionalityOptions.lenientXmlFaults
  x"<root><name>Ann</name><count>many</count></root>".as[Tally]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

### Writing XML literally

The `x"…"` interpolator writes XML directly, and checks it at compiletime. Values are
substituted into it:

```scala
val name = t"Alice"
val user = x"<user>$name</user>"
```

```scala fuel
run
##
Out.println(user.show)
```

A malformed fragment is rejected where it is written:

```scala fuel
compile
errors show
##
x"<user>$name</usr>"
```

### Navigating

With dynamic access enabled, a child element is reached as though it were a member, and the
steps chain. An index picks among repeated elements:

```scala
import dynamicAccess.dynamicXml

val data = t"<a><b><c>42</c></b></a>".read[Xml]
val list = t"<r><x>1</x><x>2</x></r>".read[Xml]
```

```scala fuel
run
##
Out.println(data.b().c().as[Int].show)
Out.println(list.x(Sec).as[Int].show)
```

The import grants the permission `Xml is Dynamical` to the rest of its scope.
`dynamically[Xml]:` grants it to a single block instead, as
[JSON](json.md#fields-as-members) describes.

### Updating

An element is updated through a [lens](optics.md). A lens reaches through several levels, and
may carry optics such as `Each`, to change every matching element at once. An update gives a
new document, since `Xml` values are immutable. The lens and its optics are givens, imported by
name from the package `optics`:

```scala
import optics.{xmlEachOptical, xmlLens}

val numbers = t"<doc><x>1</x><x>2</x><x>3</x></doc>".read[Xml]
```

```scala fuel
run
##
Out.println(numbers.lens(_.x = x"<x>9</x>").show)
Out.println(numbers.lens(_(Each) = x"<x>0</x>").show)
```

The value on the right of the `=` need not be XML already. Anything which can be encoded in
XML, such as a case class, an `Int` or a `Text`, is encoded as it is assigned, with no import.
The other formats ask for one, such as `conversions.encodableToJson`.

```scala
val staff = t"<staff><Worker><name>x</name><age>1</age></Worker></staff>".read[Xml]
```

```scala fuel
run
##
Out.println(staff.lens(_.Worker = Worker(t"Alice", 30)).show)
```

### Paths and queries

An [XPath](https://en.wikipedia.org/wiki/XPath) names a location within a document. The `xp"…"`
interpolator writes one, and checks it at compiletime. A step with no position means the first
match, and `@` names an attribute:

```scala fuel
run
##
Out.println(xp"/root[1]/child[2]".encode)
Out.println(xp"/root[1]/@id".encode)
Out.println(xp"/root/child".encode)
```

Positions and accumulated errors are reported as paths, so an error from decoding a large
document names the element which caused it.

The same interpolator writes a full [XPath 1.0](https://www.w3.org/TR/xpath-10/) expression,
and a document answers it:

```tabular
column  Method
column  Returns
row  `select`  the matching nodes, as a fragment
row  `selectText`  the text of the first match
row  `evaluate`  the value of any expression, whether a number, a boolean, a string or a node set, as an `XPath.Value`
```

Axes, predicates on attributes and on text, and the core function library are all supported.
That is what a locator for [browser automation](web-automation.md) needs, or a lookup in a
configuration file:

```scala
val page = t"""<app><div id="main"><button data-test="submit">Submit</button>
                 <ul><li>one</li><li>two</li><li>three</li></ul></div></app>""".read[Xml]
```

```scala fuel
run
##
Out.println(page.selectText(xp"//button[text()='Submit']/@data-test").inspect)
Out.println(page.selectText(xp"/app/div[1]/ul/li[2]").inspect)
Out.println(page.evaluate(xp"count(//li)").inspect)
```

An expression which the engine does not support, or a variable it was not given, raises an
`XPath.Error` which says so.

### Namespaces

The parser resolves namespaces as it reads. A declaration, `xmlns="…"` or `xmlns:p="…"`, is
kept as an attribute, so a document is written back exactly as it was read. The declaration
also binds the prefix for the element and its descendants. Every `Xml.Element` carries the
bindings in force at it, as its `scope`, and resolves its own name through them:

```scala
val prefixed = t"""<r xmlns:a="urn:a"><a:x>1</a:x></r>""".read[Xml]

val element = prefixed.`a:x`() match
  case Xml.Fragment(element: Xml.Element) => element
```

```scala fuel
run
##
Out.println(element.namespace.inspect)
Out.println(element.localName.inspect)
Out.println(element.qualified.inspect)
```

Two names are the same name when their namespace URIs and their local parts agree, whatever
prefixes bind them. A prefixed name selects by resolved name when its prefix is bound, whether
by a declaration in the document or by an `Xml.Scope` given in scope. The prefix the code uses
therefore need not be the one the document used:

```scala
given svg: ("svg" is Namespace of "http://www.w3.org/2000/svg") = Namespace()
given svgScope: Xml.Scope = Xml.Scope(t"svg" -> t"http://www.w3.org/2000/svg")

val picture = t"""<svg xmlns="http://www.w3.org/2000/svg"><rect/><rect/></svg>""".read[Xml]

val byPrefix = picture.`svg:rect`
val byName = picture.elements(Xml.Name(t"http://www.w3.org/2000/svg", t"rect"))
```

A `Namespace` binds a prefix, its `Self`, to a URI, its `Topic`. Both are singleton types, so
the binding is checked wherever it is used. An `x"…"` literal which uses a prefix it does not
declare takes the binding from the givens in scope. An `xp"…"` path does the same for the names
it tests, so `xp"//svg:rect"` matches by namespace. `XPath#in(scope)` rebinds a path's
prefixes.

A prefix with no binding is an error, as the Namespaces recommendation requires. Parsing
`<p:a/>` without a declaration of `p` raises a `Parse.Error`:

```scala fuel
run
errors show
##
t"<p:a/>".read[Xml]
```

Writing `x"<p:a/>"` without a `Namespace` given for `p` does not compile. Importing
`namespaceOptions.lenientNamespaces` makes an unbound prefix resolve to no namespace instead.

When an `Xml` tree is written, an element may use a prefix, or a default namespace, which its
scope binds but which nothing above it has declared. The declaration is then written for it. A
subtree which was built in code, or cut from a document by a selection, is therefore written
with every namespace it needs. The scope takes no part in equality: a parsed element equals the
same element built by hand.

A derived codec puts a case class in a namespace with an annotation:

```scala
case class Item(sku: Text, quantity: Int)

@xmlns("urn:shop")
case class Order(id: Int, item: Item)
```

```scala fuel
run
##
Out.println(Order(1, Item(t"a", 2)).in[Xml].show)
```

The fields' elements are in the same namespace as the class, and `as[Order]` reads a document
whose elements resolve to that namespace, however it prefixes them. The alternatives are:

```tabular
column  Declaration
column  Effect
row  `@xmlns("urn:shop")` on the class  the class's element and its fields' elements are in the namespace
row  `@xmlns("urn:shop", qualified = false)` on the class  the fields' elements are in no namespace, as under `elementFormDefault="unqualified"`
row  `@unqualified` on a field  that field's element is in no namespace
row  `given Rect is Xml.Namespaced = Xml.Namespaced(t"…")`  a namespace for a type which cannot be annotated
row  none  the fields' elements are matched by name alone, whatever namespace the document puts them in
```

### Formatting

The output format is a given in scope. Compact formatting leaves out whitespace, and indented
formatting adds newlines and indentation for a human reader:

```scala fuel
run
##
locally:
  import formatting.indentedXmlFormatting

  Out.println(Worker(t"Alice", 30).in[Xml].show)
```

`show` renders the whole node into one `Text`. A large document need not be held in memory
before it is sent:

```tabular
column  Form
column  Serializes on
column  Hands out
row  `show`  the caller's thread  one `Text`
row  `Xml.emit[Text](document, f)`  the caller's thread  each block of text to `f`, as it fills
row  `Xml.emit[Data](document, f)`  the caller's thread  each block as UTF-8 bytes to `f`, as it fills
row  `Xml.lend(document)`  the caller's thread  each block lent to a function, not copied
row  `Xml.emit(document)`  a fiber  the text, through an iterator
row  `Streamable` by `Text`  a fiber  the text, as a stream
```

The push form of `Xml.emit` serializes a `Document[Xml]`, with its header, and hands each block
over as it fills. That suits writing to a file, a [socket](sockets.md) or an `OutputStream`:

```scala fuel
typecheck
##
Xml.emit[Data](document, chunk => socket.write(chunk))
```

`Xml.lend` avoids even the copy into each `Data` block. It lends the consumer the writer's own
buffer as a `Region`, together with an interval which proves that every index in it is in
range. Both are valid only for the duration of the call, the same rule as a
[stream's](streams.md) `lend`:

```scala fuel
typecheck
##
Xml.lend(document): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))
```

Where the consumer pulls instead, `Xml.emit(document)` and the `Streamable` instance of a
`Document[Xml]` serialize on a fiber, under a [`supervise`](concurrency.md) block:

```scala fuel
typecheck
##
import probates.cancelProbate

supervise:
  Xml.emit(document).each(chunk => out.write(chunk))
```

### Skipping the tree

A record with an `Xml.Printable` instance is written directly, with no tree built, wherever
the formatting is compact. Indented formatting still renders through the tree. The printer is
derived at compiletime, for a record with no `@name` or `@attribute` annotations and no
namespace, and writes the same text:

```scala fuel
alternative Through a tree
  code
      Worker(t"Alice", 30).in[Text over Xml]
alternative Direct
  code
      val direct =
        given Worker is Xml.Printable = Xml.Direct.derived[Worker].printable
        Worker(t"Alice", 30).in[Text over Xml]
##
```

`Xml.Printable.emit` writes the text a block at a time, handing each block to a function as it
fills. A record can generate both a parser and a printer at once, into its companion, with
`derives Xml.Direct`. A single import then selects the route for every record in scope, as it
does for [JSON](json.md#skipping-the-tree):

```tabular
column  Import
column  For a record
column  For any other type
row  `codecs.direct`  a generated parser and printer  the derived codecs, through the tree
row  `codecs.derived`  the derived codecs, even where a direct instance is in scope  the derived codecs, through the tree
row  `codecs.strict`  a generated parser and printer  a compile error naming the type
```

Under `codecs.strict`, the error arises wherever `read[Foo in Xml]` or `in[Text over Xml]`
would otherwise fall back to the tree. The modes are alternatives, so import one at a time.

### Typed records from an XML Schema

Sometimes a document's shape is given as an XML Schema (XSD), not as a Scala type. An
`Xml.Provider` reads the schema at compiletime, and produces typed records from XML which
matches it. A provider object holds the schema:

```scala fuel
typecheck
##
import charsets.utf8Charset
import classloaders.threadContextClassloader
import textSanitizers.skipSanitizer

object PurchaseOrder extends Xml.Provider(cp"/xsd/po.xsd")
```

Its `record` method turns an `Xml` value into a record. The record has one member for each
child element and attribute of the schema's root element, and each has the type the schema
declares:

```scala fuel
typecheck
##
val order = PurchaseOrder.record(cp"/xsd/po.xml".read[Xml])

val city: Text = order.shipTo.city
val quantities: List[Long] = order.items.item.map(_.quantity)
val comment: Optional[Text] = order.comment
val parts: List[Text] = order.items.item.map(_.partNum)
```

`quantity` is a restriction of `xs:positiveInteger`, `comment` has `minOccurs="0"`, and
`partNum` is checked against the schema's pattern for a part number as it is read.

The schema may be an `Xsd` value, an `Xml` document, the schema's text, or anything which can
be read as text, such as a classpath resource. Where a schema declares several global elements,
the root is the one with complex content, or is chosen with `root = t"Envelope"`.

Child elements and attributes become fields, by local name, shaped by the schema:

```tabular
column  The schema has
column  The field is
row  an element with `minOccurs="0"`, or `nillable`  `Optional`
row  an element whose `maxOccurs` allows more than one  a `List`
row  an alternative of a `choice`  `Optional`
row  an attribute sharing a name with a child element  reached as `` `@name` ``
row  an element with simple content and attributes  its text reached as `text`, or `` `#text` `` if an attribute is called `text`
row  an extension  preceded by its base type's fields
```

Each field has the Scala type of its XSD type:

```tabular
column  XSD type
column  Scala type
row  `xs:string` and the token, name and identifier types, `xs:QName`, the binary types, `xs:gYear` and relatives  `Text`
row  `xs:boolean`  `Boolean`
row  `xs:int`, `xs:short`, `xs:byte`  `Int`, `Short`, `Byte`
row  `xs:integer`, `xs:long` and the unsigned, positive and negative integer types  `Long`, with the implied bounds checked
row  `xs:decimal`, `xs:double`, `xs:float`  `Double`, `Double`, `Float`
row  `xs:dateTime`, `xs:date`, `xs:time`, `xs:duration`, `xs:anyURI`  the type an interface in scope instantiates, else `Text`
row  `xs:NMTOKENS`, `xs:IDREFS` and any `xs:list`  `List[Text]`
row  `xs:anyType`, `xs:any`, a recursive type, an unresolved reference  `Xml`
```

A restriction's facets are checked as the field is read: `enumeration`, `pattern`, the
inclusive and exclusive bounds, the length facets and the digit facets. A failed check raises
an `Xml.Provider.Error` with the reason, as does a missing required element, or a value which
its type cannot read. Every error is raised through the [tactic] of the code which reads the
field, so `validate`, with a `venture` for each field,
[collects every problem](errors.md#ventures-and-guards) in a document.

The provider matches a document's elements by resolved name. A child is expected in the
schema's target namespace when the schema qualifies it, through `elementFormDefault`, a `form`,
or a `ref` to a global element. Otherwise it is expected in no namespace, whatever prefixes the
document uses. An element in another namespace is treated as absent.

Substitution groups, `xsi:type`, identity constraints and mixed content are read as the schema
declares without them. An `import` or `include` is recorded in the `Xsd`, but not fetched, so a
type from another schema is read as raw `Xml`. The provider does not validate documents: it
checks what a program reads. [JSON](json.md#typed-records-from-a-schema) has the same facility
for JSON Schema, and [Records](records.md) describes the records themselves.

### Positions and errors

A malformed document raises a `Parse.Error`. Its position is not merely a line number but a
range: the offset and length of the text at fault. A tool can therefore underline exactly the
mismatched closing tag or the unterminated attribute, not the whole line.

The positions of *well-formed* content are recorded on request, in the same way as for
[JSON](json.md#source-positions):

```scala
import parsing.trackPositions

val source = t"<root><child/></root>"
val tracked = source.load[Xml]
```

Under a strategy which [accumulates failures](errors.md#accumulating-failures), decoding
reports every fault in the document at once, each with the path to the element which failed,
instead of stopping at the first.

### XML over HTTP

An `Xml` value is a request body or a response body, with the media type `application/xml`,
and a body is parsed back to `Xml` on arrival. An XML API is therefore consumed and offered
with no glue between the XML and [HTTP](http-client.md) layers.

### Related

- [HTML](html.md) uses the same node types and interpolation for web pages.
- [SVG](svg.md) and [mathematical markup](mathematical-markup.md) are XML vocabularies with
  typed builders.
- [JSON](json.md) and [YAML](yaml.md) share the vocabulary of `read`, `as` and `in`.

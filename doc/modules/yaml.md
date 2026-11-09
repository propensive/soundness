## YAML

### About

[YAML](https://en.wikipedia.org/wiki/YAML) is handled in the same way as [JSON](json.md). Text
parses into a `Yaml` value. A `Yaml` value converts to and from case classes with
[derived][derivation] codecs. The `y"…"` [interpolator] writes YAML which is checked at
[compiletime].

The parser covers the language as it is really used: block and flow styles, quoting, block
scalars, tags, anchors and aliases, and streams of several documents. A `Yaml` value is written
back out as clean block-style text.

Navigation, updating through lenses, tracking of source positions, and collecting several
decoding errors at once all have the same shape as they do for JSON. Knowing one format is
knowing the other.

### On YAML

YAML is the configuration language of half the modern toolchain, and it is notoriously easy to
consume badly. Its indentation is structure. Its scalars are typed by guesswork. Its extras,
such as anchors, tags and multi-document files, defeat casual parsers. A consumer typically
reads it into an untyped tree and picks values out by string key, discovering any mismatch at
[runtime].

Here the language is parsed properly, and the untyped tree is then left behind. A document
decodes into the case class it should be. If it cannot, a typed error names what was missing or
of the wrong type and, with tracking on, the line and column where it sat. Decoding straight to
a typed value, through the same derivation as every other format, is
[correctness](../philosophy/correctness.md) with one definition of the data.

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Parsing and decoding

`read[Yaml]` parses text into a `Yaml` value, and `as` decodes it to a Scala type. Reading
straight to a type, with `read[Person in Yaml]`, combines the two steps. Flow style and block
style give the same result:

```scala
case class Person(name: Text, age: Int)

val alice = t"{name: Alice, age: 30}".read[Yaml].as[Person]
val bob = t"name: Bob\nage: 40".read[Person in Yaml]
```

```scala fuel
run
##
Out.println(alice.inspect)
Out.println(bob.inspect)
```

A document which lacks a required field, or holds a value of the wrong type, does not decode,
and the error says why:

```scala fuel
run
errors show
##
t"name: Carol".read[Person in Yaml]
```

```scala fuel
run
errors show
##
t"{name: Dave, age: unknown}".read[Person in Yaml]
```

A stream of several documents, separated by `---`, reads as a list:

```scala fuel
run
##
Out.println(t"---\n1\n---\n2\n---\n3".read[List[Yaml]].map(_.as[Int]).inspect)
```

A source need not be text held in memory. The parser reads straight from a
[stream](streams.md) of bytes or characters, refilling as it goes, without first joining the
input together. A large document is therefore parsed without ever being held whole, and a `---`
marker which is split across two chunks is still a document boundary.

### Encoding

`in[Yaml]` encodes a value as `Yaml`, and `show` writes it as block-style text, in the
formatting in scope:

```scala
import formatting.blockYamlFormatting
```

```scala fuel
run
##
Out.println(Person(t"Alice", 30).in[Yaml].show)
```

`in[Text over Yaml]` encodes a value straight to YAML text, in the `Yaml.Formatting` in scope,
and `in[Data over Yaml]` writes the same text as bytes, in the `Codepage` in scope.

The `@name` annotation renames a field in the document, exactly as it does for
[JSON](json.md#renaming-fields). An enumeration encodes with a discriminator field, chosen by
importing `yamlByTypeDiscriminable` or `yamlByKindDiscriminable`, as a
[tagged union](json.md#tagged-unions) does.

### Optional fields

An `Optional` field has three policies, from the package `optionalityOptions`, as
[Decoding](decoding.md#policies-for-optional-fields) describes:

```tabular
column  A key which is
column  Reads as
column  Unless
row  omitted from the mapping  `Unset`  `strictYamlAbsence` makes it an error
row  given an explicit `null`  `Unset`  `strictYamlNulls` hands it to the inner decoder
row  present but rejected by the inner decoder  an error  `lenientYamlFaults` reads it as `Unset`
```

```scala
case class Member(name: Text, age: Optional[Int])

val eve = t"name: Eve\nage: null".read[Yaml].as[Member]

val strict =
  import optionalityOptions.strictYamlNulls
  capture[Yaml.Error](t"name: Eve\nage: null".read[Yaml].as[Member]).reason
```

```scala fuel
run
##
Out.println(eve.inspect)
Out.println(strict.inspect)
```

### Scalars, quoting and block styles

Most of YAML's surprises live in its scalars. Each style is handled as the specification says:

```tabular
column  Scalar
column  Resolves as
row  plain `true`  a boolean
row  plain `42`  an integer
row  plain `~` or `null`  a null
row  `'…'`, single-quoted  text, taking no escapes; a quote is doubled to include one
row  `"…"`, double-quoted  text, taking the full escape set
```

A quoted scalar is always text, so `"42"` stays a string:

```scala fuel
run
errors show
##
t"""age: "42"""".read[Yaml].as[Map[Text, Int]]
```

A block scalar carries its own handling of newlines. It has a style indicator, and an optional
*chomping* indicator for the trailing newline:

```tabular
column  Indicator
column  Effect
row  `|`  keeps line breaks
row  `>`  folds line breaks into spaces
row  no chomping indicator  keeps one trailing newline
row  `-`  strips the trailing newline
row  `+`  keeps every trailing newline
```

```scala fuel
run
##
Out.println(t"text: |\n  line1\n  line2".read[Yaml].as[Map[Text, Text]].inspect)
Out.println(t"text: >\n  line1\n  line2".read[Yaml].as[Map[Text, Text]].inspect)
Out.println(t"text: |-\n  line1\n  line2".read[Yaml].as[Map[Text, Text]].inspect)
```

Comments are ignored wherever they may appear, whether after a scalar or on a line of their
own. Leading whitespace, trailing whitespace and whitespace at the end of a line are all
treated as the specification requires.

### Tags

A tag overrides the type which a scalar's content would otherwise be given. It is how YAML says
"this number is a string", and the reverse:

```tabular
column  Tag
column  Resolves a scalar as
row  `!!str`, `!`, `!!`  text
row  `!!int`  an integer
row  `!!float`  a decimal
row  `!!bool`  a boolean, from `true`, `True` or `TRUE`, and likewise `false`
row  `!!null`  a null
```

```scala fuel
run
##
Out.println(t"!!str 42".read[Yaml].as[Text])
Out.println(t"!!int \"42\"".read[Yaml].as[Int].show)
Out.println(t"!!float 7".read[Yaml].as[Double].inspect)
```

### Anchors and aliases

An anchor names a node, and an alias refers back to it, so a document need not repeat itself.
Aliases are resolved as the document is read, whatever the anchored node was: a scalar, a flow
sequence or mapping, or a whole block.

```scala
case class Inner(n: Int)
```

```scala fuel
run
##
Out.println(t"a: &x 1\nb: *x".read[Yaml].as[Map[Text, Int]].inspect)
Out.println(t"defaults: &d\n  n: 7\nuse: *d".read[Yaml].as[Map[Text, Inner]].inspect)
```

This is what makes YAML configuration files terse, and what makes a naive parser get them
wrong.

### Writing YAML literally

The `y"…"` interpolator writes YAML directly. It is checked at compiletime, and values are
substituted into it. The same literal takes a document apart in a pattern, as
[`j"…"`](json.md#matching-json) does:

```scala
val name = t"Alice"

val person = y"""
  name: $name
  age: 30
""".as[Person]
```

```scala fuel
run
##
Out.println(person.inspect)
```

### Navigating and updating

With dynamic access enabled, a field is read as a member, and assigning to a field gives a new
document. Assigning `Unset` removes a field:

```scala
import dynamicAccess.dynamicYaml

val doc = t"{name: Alice, age: 30}".read[Yaml]
```

```scala fuel
run
##
Out.println(doc.name.as[Text])
Out.println((doc.age = 31).show)
```

The import grants the permission `Yaml is Dynamical` to the rest of its scope.
`dynamically[Yaml]:` grants it to a single block instead, as
[JSON](json.md#fields-as-members) describes.

A deeper change goes through a [lens](optics.md), with optics such as `Each` and `Filter` to
reach many elements at once:

```scala
val older = doc.lens(_.age = 31.in[Yaml])
```

### Positions and accumulated errors

Bringing `parsing.trackPositions` into scope switches on tracking, as it does for
[JSON](json.md#source-positions). Every value then remembers its line and column, so a decoding
failure can point into the source file. That is the difference between "field missing" and
"field missing at line 12".

```scala
import parsing.trackPositions

val tracked = t"{a: 1, b: 2}".read[Yaml]
val position = tracked.locate(YamlPath()(t"a"))
```

Under a strategy which [accumulates failures](errors.md#accumulating-failures), decoding
collects every fault in a document instead of stopping at the first. Each is tagged with its
path, such as `#/age` or `#/email`, so the problems of a whole configuration file are reported
together.

### Numbers

A YAML number, like a JSON one, has no limit on its precision. A number beyond the range of a
`Long` or a `Double` is kept exactly, as a binary-coded decimal. A long identifier or a precise
decimal read from a configuration file is therefore never silently rounded on the way through.

### Skipping the tree

The conversions above all go through the tree. Text is parsed to a `Yaml` and decoded, or a
value is encoded to a `Yaml` and written out. A record can instead carry a *direct* parser and
printer, derived once into its companion with `derives Yaml.Direct`. A single import then
selects the route for every record in scope, as it does for
[JSON](json.md#skipping-the-tree):

```tabular
column  Import
column  For a record
column  For any other type
row  `codecs.direct`  a direct parser and printer  the tree codecs, unless it has a direct instance of its own
row  `codecs.derived`  the tree codecs, even where a direct instance is in scope  the tree codecs
row  `codecs.strict`  a direct parser and printer  a compile error naming the type, unless it has a direct instance of its own
```

A type which is not a record is an enumeration or a sealed hierarchy. The modes are
alternatives, so import one at a time.

A direct printer writes a record's text straight from its fields, with no tree built. The text
is the same as `show` writes.

A direct parser reads the fields straight from the nodes of the parsed document. No `Yaml` is
built around any of them, and no codec is looked up for each field. The document is still
parsed whole first, though. Anchors and aliases mean that a node may refer back to any earlier
one, so a record cannot be read before the whole document is held.

### Conformance

The parser is checked against the [YAML test suite](https://github.com/yaml/yaml-test-suite),
the community's shared collection of cases, including those designed to break parsers. Cases
outside the supported subset are recorded as known gaps, not quietly skipped, so the coverage
is stated and not merely claimed.

### YAML over HTTP

A `Yaml` value is a request body or a response body, with the media type `application/yaml`,
and a body is parsed back to `Yaml` on arrival. A YAML API is therefore consumed and offered
with the same code as a [JSON one](json.md#json-over-http). Only the format's type differs.

### Related

- [Annotations](annotations.md), [CBOR](cbor.md), [CSV](csv.md), [XML](xml.md), [network addresses](network-addresses.md) and [OpenAPI](openapi.md) share YAML's codecs and carry its documents.

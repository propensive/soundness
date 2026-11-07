## YAML

### About

[YAML](https://en.wikipedia.org/wiki/YAML) is handled the way Soundness handles [JSON](json.md):
text parses into a `Yaml` value, a `Yaml` converts to and from case classes with derived codecs,
and the `y"…"` interpolator writes YAML checked as the code compiles. The parser covers the
language as it is really used — block and flow styles, quoting, block scalars, tags, anchors and
aliases, and multi-document streams — and a `Yaml` value renders back to clean block-style text.

Navigation, updating through lenses, position tracking for error reporting, and accumulating
several decoding errors at once all follow the same shapes as their JSON counterparts, so knowing
one is knowing the other.

### On YAML

YAML is the configuration language of half the modern toolchain, and it is notoriously easy to
consume badly. Its indentation is structure, its scalars are typed by guesswork, and its extras —
anchors, tags, multi-document files — defeat casual parsers. Consumers typically read it into an
untyped tree and pick values out by string key, discovering mismatches at runtime.

Soundness parses the language properly and then leaves untyped trees behind: a document decodes
into the case class it should be, with a typed error naming what was missing or mis-typed — and,
with tracking on, the line and column where it sat. Everything comes from the `soundness`
package:

```scala
import soundness.*
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
```

Decoding straight to a typed value, through the same derivation as every other format, is [correctness](../philosophy/correctness.md) with one definition of the data.

### Parsing and decoding

Text reads as a `Yaml` value, and decodes to a Scala type with `as`; reading straight to a type
combines the steps:

```scala
case class Person(name: Text, age: Int)

t"{name: Alice, age: 30}".read[Yaml].as[Person]   // Person(t"Alice", 30)
t"name: Alice\nage: 30".read[Person in Yaml]      // block style, same result
```

`in[Text over Yaml]` is the mirror, encoding a value straight to YAML text in the
`Yaml.Formatting` in scope:

```scala
import formatting.blockYamlFormatting
Person(t"Alice", 30).in[Text over Yaml].read[Person in Yaml]   // Person(t"Alice", 30)
```

`in[Data over Yaml]` writes the same text as bytes, encoded in the `Codepage` in scope.

### Choosing the path for every type

Both of those go through the tree: the text is parsed to a `Yaml` and decoded, or the value is
encoded to a `Yaml` and rendered. A record can instead carry a *direct* parser and printer, derived
once into its companion with `derives Yaml.Direct`, and a single import selects the path for every
record in scope, as it does for the other formats:

```tabular
column  Import
column  For a record
column  For any other type
row  `codecs.direct`  a direct parser and printer  the tree codecs, unless it has a direct instance of its own
row  `codecs.derived`  the tree codecs, even where a direct instance is in scope  the tree codecs
row  `codecs.strict`  a direct parser and printer  a compile error naming the type, unless it has a direct instance of its own
```

A direct printer writes the record's text straight from its fields, with no tree built; the text
is the same as `show` renders. A direct parser reads the fields straight off the parsed
document's nodes, with no `Yaml` built around any of them and no codec dispatched per field — but
the document is still parsed whole first: YAML's anchors and aliases mean a node may refer back
to any earlier one, so there is no reading a record before the document is held. A type that is
not a record is an enumeration or a sealed hierarchy, say. The modes are alternatives; import one
at a time.

An `Optional` field has three policies, from the `optionalityOptions` package, as
[decoding](decoding.md) describes:

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

t"name: Eve\nage: null".read[Yaml].as[Member]   // Member(t"Eve", Unset)

val strict =
  import optionalityOptions.strictYamlNulls
  capture[Yaml.Error](t"name: Eve\nage: null".read[Yaml].as[Member]).reason
  // Yaml.Error.Reason.NotType(Yaml.Primitive.Null, Yaml.Primitive.Integer)
```

A multi-document stream — documents separated by `---` — reads as a list:

```scala
t"---\n1\n---\n2\n---\n3".read[List[Yaml]].map(_.as[Int])   // List(1, 2, 3)
```

A source need not be text held in memory. The parser reads straight from a byte or character
[stream](streams.md), refilling as it goes, rather than concatenating the input first — so a
large document parses without ever being materialized, and a `---` marker split across two chunks
is still a document boundary.

### Scalars, quoting and block styles

YAML's scalars are where most of its surprises live, and each style is handled as specified:

```tabular
column  Scalar
column  Resolves as
row  plain `true`  a boolean
row  plain `42`  an integer
row  plain `~` or `null`  a null
row  `'…'`, single-quoted  text, taking no escapes; a quote is doubled to include one
row  `"…"`, double-quoted  text, taking the full escape set
```

Quoted scalars are always text, so `"42"` stays a string.

Block scalars carry their own newline handling, with a style indicator and an optional chomping
indicator for the trailing newline:

```tabular
column  Indicator
column  Effect
row  `|`  keeps line breaks
row  `>`  folds line breaks into spaces
row  no chomping indicator  keeps one trailing newline
row  `-`  strips the trailing newline
row  `+`  keeps every trailing newline
```

```scala
t"text: |\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]   // t"line1\nline2\n"
t"text: >\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]   // t"line1 line2\n"
t"text: |-\n  line1\n  line2".read[Yaml].as[Map[Text, Text]]  // t"line1\nline2"
```

Comments are ignored wherever they may appear — after a scalar, on a line of their own — and
leading, trailing and line-trailing whitespace is handled as the specification requires rather
than as it happens to fall.

### Tags

A tag overrides the resolution a scalar's content would otherwise get, which is how YAML says
"this number is a string" and the reverse:

```tabular
column  Tag
column  Resolves a scalar as
row  `!!str`, `!`, `!!`  text
row  `!!int`  an integer
row  `!!float`  a decimal
row  `!!bool`  a boolean, from `true`, `True` or `TRUE`, and likewise `false`
row  `!!null`  a null
```

For example:

```scala
t"!!str 42".read[Yaml].as[Text]       // t"42"
t"!!int \"42\"".read[Yaml].as[Int]    // 42
t"!!float 7".read[Yaml].as[Double]    // 7.0
```

### Anchors and aliases

An anchor names a node and an alias refers back to it, so a document need not repeat itself.
Aliases resolve as the document is read, whatever the anchored node was — a scalar, a flow
sequence or mapping, or a whole block:

```scala
case class Inner(n: Int)

t"a: &x 1\nb: *x".read[Yaml].as[Map[Text, Int]]
// Map(t"a" -> 1, t"b" -> 1)

t"defaults: &d\n  n: 7\nuse: *d".read[Yaml].as[Map[Text, Inner]]
// Map(t"defaults" -> Inner(7), t"use" -> Inner(7)): both keys hold the same mapping
```

This is what makes YAML configuration files terse, and what makes a naive parser get them wrong.

### Numbers

A YAML number, like a JSON one, has no inherent precision limit. Numbers beyond a `Long`'s or a
`Double`'s range are kept exactly, as binary-coded decimals, so a long identifier or a precise
decimal read from a configuration file is not silently rounded on the way through.

### Conformance

The parser is checked against the [YAML test suite](https://github.com/yaml/yaml-test-suite), the
community's shared corpus of cases — including the ones designed to break parsers. Cases outside
the supported subset are recorded as known gaps rather than quietly skipped, so the coverage is
stated rather than claimed.

### Encoding

A value encodes to YAML with `in[Yaml]`, and renders as block-style text with `show`, given a
formatting in scope:

```scala
import formatting.blockYamlFormatting

Person(t"Alice", 30).in[Yaml].show
// name: Alice
// age: 30
```

The `@name` annotation renames a field on the wire, exactly as it does for JSON, and an
enumeration encodes with a discriminator field chosen by importing `yamlByTypeDiscriminable` or
`yamlByKindDiscriminable`.

### Writing YAML literally

The `y"…"` interpolator writes YAML directly, checks it as the code compiles, and substitutes
values into its holes; the same literal deconstructs a document in a pattern:

```scala
val name = t"Alice"
val person = y"""
  name: $name
  age: 30
""".as[Person]
```

### Navigating and updating

With dynamic access enabled, fields read as members and updates produce new documents — assigning
`Unset` removes a field. Deeper changes go through a lens, with optics such as `Each` and `Filter`
reaching many elements at once:

```scala
import dynamicAccess.dynamicYaml

val doc = t"{name: Alice, age: 30}".read[Yaml]
doc.name.as[Text]      // t"Alice"
(doc.age = 31)         // a new document
doc.lens(_.age = 31.in[Yaml])
```

The import grants `Yaml is Dynamical` to the rest of its scope; `dynamically[Yaml]:` grants
it to a single block instead, as described in the JSON tutorial.

### Positions and accumulated errors

With tracking switched on — by bringing `parsing.trackPositions` into scope, as for
[JSON](json.md#source-positions) — every value remembers its line and column, so a decoding
failure can point into the source file — the difference between "field missing" and "field
missing at line 12":

```scala
import parsing.trackPositions

val tracked = t"{a: 1, b: 2}".read[Yaml]
tracked.locate(YamlPath()(t"a"))   // the position of a
```

Under an accruing strategy, decoding collects every fault in a document rather than stopping at
the first, each tagged with its path — `#/age`, `#/email` — so a whole configuration file's
problems are reported together.

### YAML over HTTP

A `Yaml` value serves as a request or response body with the `application/yaml` media type, and a
body parses back to `Yaml` on arrival, so a YAML API is consumed and offered with the same code
as a [JSON](json.md) one — only the format's type differs.

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

A multi-document stream — documents separated by `---` — reads as a list:

```scala
t"---\n1\n---\n2\n---\n3".read[List[Yaml]].map(_.as[Int])   // List(1, 2, 3)
```

A source need not be text held in memory. The parser reads straight from a byte or character
[stream](streams.md), refilling as it goes, rather than concatenating the input first — so a
large document parses without ever being materialized, and a `---` marker split across two chunks
is still a document boundary.

### Scalars, quoting and block styles

YAML's scalars are where most of its surprises live, and each style is handled as specified.
Plain scalars resolve by their content — `true` is a boolean, `42` an integer, `~` and `null` a
null — while quoted scalars are always text, so `"42"` stays a string. Single quotes take no
escapes but double them to include one; double quotes take the full escape set.

Block scalars carry their own newline handling. `|` keeps line breaks and `>` folds them into
spaces, and a chomping indicator says what to do with the trailing newline — `-` strips it, `+`
keeps every one:

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

### Positions and accumulated errors

With tracking switched on, every value remembers its line and column, so a decoding failure can
point into the source file — the difference between "field missing" and "field missing at line
12":

```scala
given Yaml.Tracking = Yaml.Tracking.On

val tracked = t"{a: 1, b: 2}".read[Yaml]
tracked.locate(YamlPath()(t"a"))   // the position of a
```

Under an accruing strategy, decoding collects every fault in a document rather than stopping at
the first, each tagged with its path — `#/age`, `#/email` — so a whole configuration file's
problems are reported together.

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

### Encoding

A value encodes to YAML with `yaml`, and renders as block-style text with `show`, given a
formatting in scope:

```scala
import formatting.blockYamlFormatting

Person(t"Alice", 30).yaml.show
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
import dynamicYamlAccess.enabled

val doc = t"{name: Alice, age: 30}".read[Yaml]
doc.name.as[Text]      // t"Alice"
(doc.age = 31)         // a new document
doc.lens(_.age = 31.yaml)
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

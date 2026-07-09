## TEL

### About

[TEL](https://github.com/propensive/tel) — the Typed Element Language — is a tree-structured,
indentation-based data language with schemas, and Soundness is its reference implementation. A TEL
document parses into a `Tel` value that preserves its presentation — comments, layout and blank
lines survive a round trip — and decodes into case classes with derived codecs. A schema, itself
expressible in TEL, assigns types to a document and validates it, and a schema-typed document
navigates with the compiler checking each field.

Alongside the text form, BinTEL is TEL's compact binary encoding: the same data, schema-guided,
rendered as bytes for storage and transmission, with a content hash for integrity.

### On TEL

Configuration languages force a choice: the human-friendliness of YAML with its ambiguities, the
rigour of JSON with its noise, or a schema language bolted on afterwards. TEL is designed to be
all three at once — indentation-structured for people, schema-typed for machines, and
presentation-preserving, so a program that modifies a configuration file does not destroy its
comments and formatting.

A TEL document is a tree of *compounds*: a keyword, its space-separated atoms, and indented
children. The schema layer assigns each compound a type, and Soundness carries that typing through
to Scala. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### The language

A compound per line, a keyword then its atoms, indentation for nesting:

```
name Alice
age 30
address
  street 1 High Street
  city Northtown
```

### Parsing and decoding

Text reads as a `Tel`, and decodes to a case class with `as` — or in one step, reading straight to
the type. A field per child compound, matched by keyword:

```scala
case class Person(name: Text, age: Int)

t"name Alice\nage 30\n".read[Tel].as[Person]   // Person(t"Alice", 30)
t"name Alice\nage 30\n".read[Person in Tel]    // the same, in one step
```

Encoding runs the other way: `tel` produces a `Tel` from a value, a compound per field, ready to
render or to embed in a larger document.

### Verified navigation

`verify` checks a document against the shape of a type and re-types it, after which its fields
navigate with full compiletime checking — a misspelled field, or a field the type does not have, is
a compile error:

```scala
case class Office(name: Text, city: Text)
case class Assignment(worker: Person, office: Office)

val doc = Assignment(Person(t"Bob", 2), Office(t"Main", t"Town")).encode

doc.verify[Assignment].office.city.as[Text]   // t"Town"
```

Unverified dynamic access — reaching into a document whose shape is only informally known — is
available too, enabled by `import dynamicTelAccess.enabled`, keeping the checked and unchecked
styles visibly distinct.

### Schemas

A `Tels` is a TEL schema: the structure a document must have, with named record, scalar and select
definitions, composable in layers. A schema derives from a Scala type, or is itself read from a TEL
document — the schema language is TEL:

```scala
val schema = Tels.tels[Person](t"person")
```

`Tel.Type.assign` types a document against a schema, validating it and reporting failures as typed
errors with a path to the offending element; under an accruing strategy every fault in a document is
collected and reported together.

### BinTEL

A typed document encodes to BinTEL — a compact binary form guided by the schema — and decodes back;
a value hash fingerprints the content for integrity checks:

```scala
val binary = doc.bintel(schema)      // the document as bytes
doc.valueHash(schema)                // a BLAKE3 digest of the content
```

A *self-contained* BinTEL frame carries its schema with it, so a document can travel to a reader
that has never seen its schema and still decode.

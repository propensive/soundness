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

Typing a document against its schema at compiletime is [safety by construction](../philosophy/safety-by-construction.md) applied to configuration.

A TEL document is a tree of *compounds*: a keyword, its space-separated atoms, and indented
children. The schema layer assigns each compound a type, and Soundness carries that typing through
to Scala. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
import charEncoders.utf8Encoder
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

Encoding runs the other way: `in[Tel]` produces a `Tel` from a value, a compound per field, ready
to render or to embed in a larger document.

A literal document is written with the `tel"…"` interpolator, parsed as the code compiles, with
each substitution encoded through its own static type. A malformed literal is a compile error
positioned at the offending range of the Scala source, not at the start of the string:

```scala
val name = t"Alice"
val contact = tel"name $name"
```

The same syntax is an extractor, binding the captures a pattern names:

```scala
contact match
  case tel"name $name" => name.as[Text]
  case _               => t"anonymous"
```

### Verified navigation

`verify` checks a document against the shape of a type and re-types it, after which its fields
navigate with full compiletime checking — a misspelled field, or a field the type does not have, is
a compile error:

```scala
case class Office(name: Text, city: Text)
case class Assignment(worker: Person, office: Office)

val doc = Assignment(Person(t"Bob", 2), Office(t"Main", t"Town")).in[Tel]

doc.verify[Assignment].office.city.as[Text]   // t"Town"
```

Unverified dynamic access — reaching into a document whose shape is only informally known — is
available too, enabled by `import dynamicAccess.dynamicTel`, keeping the checked and unchecked
styles visibly distinct. A keyword may legitimately repeat, which a single-valued accessor cannot
express, so `fields` returns every matching child in document order:

```scala
t"item 1\nitem 2\nitem 3\n".read[Tel].fields(t"item").map(_.primaryAtom)
// Array(t"1", t"2", t"3")
```

### Editing without destroying the file

The reason a TEL document keeps its comments, blank lines and layout is so that a program can
change one value and write the file back without reformatting everything a person wrote.

`modify` replaces a field's compound, or appends it where the field is absent:

```scala
val document = t"name Alice\nage 30\n".read[Tel]
```

```scala
import dynamicAccess.dynamicTel

document.modify("name", Tel.scalar(t"Bob"))
```

Finer edits go through mutation operations addressed by a pointer, which change exactly what they
name and leave the surrounding presentation alone — rewriting a single atom of a line, attaching a
remark to a compound, inserting or removing a child:

```scala
val pointer = Tel.Pointer.of(t"name")
Mutation(document, List(Mutation.Op.UpdateAtom(pointer, 0, t"Bob"))).show   // t"name Bob\n"
```

The result is a document, not a string, so a sequence of edits composes and the file is rendered
once at the end.

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

A schema is built in *layers*: a base structure, and overlays that add or refine fields. This is
how a schema shared between several programs is extended by one of them without forking it, and
how an optional feature's fields appear only where the feature is in use.

Field values are checked by named *validators*, and the built-in registry covers the kinds a
configuration language needs — `string` accepting anything, `identifier` accepting kebab-case
names and rejecting a leading hyphen, and the numeric and enumerated kinds. A schema may name its
own validators, so a domain constraint lives in the schema rather than in every program that reads
it.

A scalar may also be constrained by one or more RE2 *patterns*, which match against the whole
value and combine by intersection:

```
scalar Code
  pattern [A-Z]{2}-[0-9]{4}
```

Patterns are the one constraint whose meaning a schema can inspect. A validator is just a name, so
a layer may only ever add validators; but RE2 excludes backreferences and lookaround, which makes
containment between two patterns decidable, so a layer may *replace* a scalar's patterns provided
the new language is contained in the old. Widening is rejected, and so is a containment the
analysis cannot prove within its budget — the check fails closed, keeping a layer from ever
loosening what the base accepted.

### Recovering from errors

An indentation-based language is easy to get slightly wrong, and a parser that gives up at the
first mistake is unhelpful when a person is editing a file. Parsing therefore recovers, and the
schema informs the recovery: a line indented ambiguously is attached to the candidate the schema
admits, so a `child` keyword valid inside `parent` but not at the root is recovered to the deeper
position rather than the nearer one.

Errors are reported with their line and column, and a document whose faults were recovered still
yields a tree — which is what an editor, a language server, or a tool reporting several problems
at once requires.

### Documents in a stream

A single source may hold several documents in sequence, and reads as a list of them:

```scala
val source = t"name Alice\n"

source.read[List[Tel]]
```

### Typed records from a schema

Where the schema is authoritative, a `TelBlueprint` — an object declared in a file of its own,
holding the schema and the one-line `record` macro — reads it at compiletime and produces typed
records from matching documents. Each field reads at the type the schema declares, an optional
field is absent where the document omits it, and a *flag* field — a keyword with no value —
reads as a boolean, `true` where present and `false` where not:

<!-- doccheck: skip -->
```scala
object ContactRecords extends TelBlueprint(Tels.tels[Person](t"person")):
  transparent inline def record(tel: Tel): Record = ${build('tel)}
```

<!-- doccheck: skip -->
```scala
val record = ContactRecords.record(t"name Alice\nage 30\n".read[Tel])
record.name       // t"Alice", per the schema
record.age        // 30
```

A field the schema does not describe is a compile error, so the shape of the data is taken from
the schema and checked by the compiler, with no Scala type restating it.

### BinTEL

A typed document encodes to BinTEL — a compact binary form guided by the schema — and decodes back;
a value hash fingerprints the content for integrity checks:

```scala
val binary = doc.bintel(schema)      // the document as bytes
doc.valueHash(schema)                // a BLAKE3 digest of the content
```

A *self-contained* BinTEL frame carries its schema with it, so a document can travel to a reader
that has never seen its schema and still decode:

```scala
val greeting = t"name Alice\n".read[Tel]
val greetingSchema = t"name greeting\n\ndocument\n  field name Identifier\n".read[Tel]

Bintel.selfContained(greeting, greetingSchema)
```

The alternative is to send the data alone and identify the schema by its *signature* — a short
fingerprint computed from the schema itself, so a reader can confirm it holds the right schema
before decoding a byte of data. A schema with no layers signs in thirty-three bytes.

The value hash is over the document's content, not its bytes: two documents that differ only in
comments, whitespace or key order hash identically, while a document whose values differ does not.
That makes it usable as a cache key or a change detector, where hashing the file would report a
change whenever someone reflowed it.

```scala
document.valueHash(schema)   // deterministic; unchanged by presentation
```

Numbers in BinTEL are varint-encoded and values are typed by the schema rather than tagged in the
stream, which is where the compactness comes from; the format's framing carries a length so that
several documents may share one stream.

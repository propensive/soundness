## TEL

### About

[TEL](https://github.com/propensive/tel), the Typed Element Language, is a tree-structured
data language, written with indentation, which has schemas. Soundness is its reference
implementation.

A TEL document parses into a `Tel` value, which preserves its presentation: comments, layout
and blank lines survive being read and written. It decodes into case classes with
[derived][derivation] codecs. A schema, which can itself be written in TEL, assigns types to a
document and validates it, and a document typed by a schema is navigated with the compiler
checking each field.

Beside the text form there is BinTEL, a compact binary encoding of the same data. It is guided
by the schema, written as bytes for storage and transmission, and has a content hash for
integrity.

### On TEL

Configuration languages force a choice. YAML is friendly to people, and ambiguous. JSON is
rigorous, and noisy. A schema language can be bolted onto either afterwards. TEL is designed to
be all three at once:

- structured by indentation, for people;
- typed by a schema, for machines;
- preserving of presentation, so that a program which modifies a configuration file does not
  destroy its comments and formatting.

A TEL document is a tree of *compounds*. A compound is a keyword, its *atoms*, which are
separated by spaces, and its children, which are indented beneath it. The schema assigns each
compound a type, and that typing is carried through to Scala. Typing a document against its
schema at [compiletime] is
[safety by construction](../philosophy/safety-by-construction.md) applied to configuration.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*
import threads.virtualThreads
import strategies.throwUnsafely
import codepages.utf8Codepage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### The language

Each line is a compound: a keyword, then its atoms. Indentation nests one compound inside
another:

```
name Alice
age 30
address
  street 1 High Street
  city Northtown
```

### Parsing and decoding

`read[Tel]` parses text into a `Tel`, and `as` decodes it to a case class. Each field is read
from the child compound with the matching keyword. `read[Person in Tel]` does both steps at
once:

```scala
case class Person(name: Text, age: Int)

val alice = t"name Alice\nage 30\n".read[Tel].as[Person]
val bob = t"name Bob\nage 40\n".read[Person in Tel]
```

```scala fuel
run
##
Out.println(alice.inspect)
Out.println(bob.inspect)
```

A document which lacks a field, or holds a value of the wrong type, does not decode:

```scala fuel
run
errors show
##
t"name Carol\n".read[Person in Tel]
```

```scala fuel
run
errors show
##
t"name Dave\nage unknown\n".read[Person in Tel]
```

A single source may hold several documents in sequence, and is read as a list of them with
`read[List[Tel]]`.

### Encoding

`in[Tel]` produces a `Tel` from a value, with a compound for each field, ready to be written
out or embedded in a larger document. `show` writes a `Tel` as text:

```scala fuel
run
##
Out.println(Person(t"Alice", 30).in[Tel].show)
```

`in[Text over Tel]` encodes a value straight to its TEL text, and `in[Data over Tel]` writes
the same text as bytes, in the `Codepage` in scope.

`show` renders a `Tel` into one `Text`. A large document need not be held in memory before it
is sent:

```tabular
column  Form
column  Serializes on
column  Hands out
row  `show`  the caller's thread  one `Text`
row  `Tel.emit[Text](document, f)`  the caller's thread  each block of text to `f`, as it fills
row  `Tel.emit[Data](document, f)`  the caller's thread  each block as UTF-8 bytes to `f`, as it fills
row  `Tel.lend(document)`  the caller's thread  each block lent to a function, not copied
row  `Tel.emit(document)`  a fiber  the text, through an iterator
row  `Streamable` by `Text`  a fiber  the text, as a stream
```

The push forms suit writing to a file, a [socket](sockets.md) or an `OutputStream`. The pull
forms suit a consumer which asks for each block:

```scala fuel
typecheck
##
import probates.cancelProbate

val outgoing = Person(t"Alice", 30).in[Tel]

Tel.emit[Data](outgoing, chunk => socket.write(chunk))

Tel.lend(outgoing): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))

supervise:
  Tel.emit(outgoing).each(chunk => out.write(chunk))
```

[JSON](json.md#formatting) describes each of these forms.

### Optional fields

An `Optional` field has two policies, from the package `optionalityOptions`, as
[Decoding](decoding.md#policies-for-optional-fields) describes:

```tabular
column  A field whose keyword is
column  Reads as
column  Unless
row  missing, or present with neither an atom nor a child  `Unset`  `strictTelAbsence` makes it an error
row  present but rejected by the inner decoder  an error  `lenientTelFaults` reads it as `Unset`
```

A missing keyword and an empty one are TEL's two spellings of absence. Under
`strictTelAbsence`, absence is a `Tel.Error.Reason.Absent`, and the field is marked as required
in the derived schema. The direct parser enforces the policy for absence only.

```scala
case class Flags(active: Boolean, verbose: Optional[Boolean])

val tolerant =
  import optionalityOptions.lenientTelFaults
  t"active false\nverbose maybe\n".read[Tel].as[Flags]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

### Writing TEL literally

The `tel"…"` [interpolator] writes a literal document. It is parsed at compiletime, and each
substitution is encoded through its own static type:

```scala
val name = t"Alice"
val contact = tel"name $name"
```

The same syntax is a pattern, which binds the values it names:

```scala
val matched = contact match
  case tel"name $name" => name.as[Text]
  case _               => t"anonymous"
```

```scala fuel
run
##
Out.println(contact.show)
Out.println(matched)
```

A malformed literal is a compile error, positioned at the offending part of the literal, not
at the start of the string.

### Verified navigation

`verify` checks a document against the shape of a type, and gives it that type. Its fields are
then navigated with full compiletime checking:

```scala
case class Office(name: Text, city: Text)
case class Assignment(worker: Person, office: Office)

val doc = Assignment(Person(t"Bob", 2), Office(t"Main", t"Town")).in[Tel]
```

```scala fuel
run
##
Out.println(doc.verify[Assignment].office.city.as[Text])
```

A misspelled field, or a field the type does not have, does not compile:

```scala fuel
compile
errors show
##
doc.verify[Assignment].office.town
```

Unverified, dynamic access reaches into a document whose shape is known only informally. It is
enabled by `import dynamicAccess.dynamicTel` or, for a single block, by `dynamically[Tel]:`, so
the checked and unchecked styles stay visibly distinct, as they do for
[JSON](json.md#fields-as-members).

A keyword may legitimately repeat, which an accessor for a single value cannot express.
`fields` gives every matching child, in document order:

```scala fuel
run
##
Out.println(t"item 1\nitem 2\nitem 3\n".read[Tel].fields(t"item").map(_.primaryAtom).to[List].inspect)
```

### Paths

A location in a document is a `Telp`, a TEL path. Its first character chooses the delimiter.
Each further component names a keyword, or selects one occurrence of a repeatable keyword by a
key value or an index. A `telp"…"` literal is checked at compiletime, and a path is the same
path whichever delimiter writes it:

```scala fuel
run
##
Out.println((telp"/office/city" == telp".office.city").inspect)
```

A literal which is not a path does not compile:

```scala fuel
compile
errors show
##
telp"office/city"
```

The path which a decoding error is tagged with, such as `/office/city`, is itself a `Telp`.

### Editing without destroying the file

A TEL document keeps its comments, blank lines and layout so that a program can change one
value and write the file back, without reformatting everything a person wrote.

`modify` replaces a field's compound, or appends it where the field is absent:

```scala
import dynamicAccess.dynamicTel

val document = t"# The account holder\nname Alice\n\nage 30\n".read[Tel]
val renamed = document.modify("name", Tel.scalar(t"Bob"))
```

```scala fuel
run
##
Out.println(renamed.show)
```

Finer edits are *mutations*, addressed by a pointer. A mutation changes exactly what it names,
and leaves the surrounding presentation alone. It may rewrite a single atom of a line, attach a
remark to a compound, or insert or remove a child:

```scala
val pointer = Tel.Pointer.of(t"name")
val mutated = Mutation(document, List(Mutation.Op.UpdateAtom(pointer, 0, t"Bob")))
```

```scala fuel
run
##
Out.println(mutated.show)
```

The result is a document, not a string, so a sequence of edits composes, and the file is
written once at the end.

### Schemas

A `Tels` is a TEL schema: the structure a document must have, with named definitions of
records, scalars and selects, which compose in layers. A schema is derived from a Scala type,
or is itself read from a TEL document, since the schema language is TEL:

```scala
val schema = Tels.tels[Person](t"person")
```

Each record or select in a derived schema is named after its type's simple name. Where two
different types reached from the root share a simple name, such as `Phrase.Code` and
`Element.Code`, each is qualified by its enclosing type, as `PhraseCode` and `ElementCode`. A
derived schema whose names cannot be told apart is rejected (E210), as a hand-written one with
two definitions of one name is.

`Tel.Type.assign` types a document against a schema. It validates the document, and reports
failures as typed errors, each with a path to the offending element. Under a strategy which
[accumulates failures](errors.md#accumulating-failures), every fault in a document is collected
and reported together.

A schema is built in *layers*: a base structure, and overlays which add or refine fields. This
is how a schema shared between several programs is extended by one of them without forking it,
and how the fields of an optional feature appear only where the feature is in use.

The values of fields are checked by named *validators*. The built-in registry covers the kinds
a configuration language needs: `string`, which accepts anything, `identifier`, which accepts
kebab-case names and rejects a leading hyphen, and the numeric and enumerated kinds. A schema
may name validators of its own, so that a constraint of the domain lives in the schema, not in
every program which reads it.

A scalar may also be constrained by one or more RE2 *patterns*, which must match the whole
value, and which combine by intersection:

```
scalar Code
  pattern [A-Z]{2}-[0-9]{4}
```

Patterns are the one constraint whose meaning a schema can inspect. A validator is just a name,
so a layer may only ever add validators. But RE2 excludes backreferences and lookaround, which
makes it decidable whether one pattern's language contains another's. A layer may therefore
*replace* a scalar's patterns, provided the new language is contained in the old. Widening is
rejected, and so is a containment which the analysis cannot prove within its budget. The check
fails closed, so a layer can never loosen what the base accepted.
[Patterns](patterns.md) describes the RE2 engine.

### Recovering from errors

A language structured by indentation is easy to get slightly wrong, and a parser which gives up
at the first mistake is unhelpful to a person editing a file. Parsing therefore recovers, and
the schema informs the recovery. A line which is indented ambiguously is attached to the
candidate which the schema admits. A `child` keyword which is valid inside `parent`, but not at
the root, is recovered to the deeper position.

Errors are reported with their line and column. A document whose faults were recovered still
yields a tree, which is what an editor, a [language server](lsp.md), or a tool reporting
several problems at once requires.

### Typed records from a schema

Where the schema is authoritative, a `Tel.Provider` reads it at compiletime, and produces typed
records from matching documents. The provider is an object, declared in a file of its own,
which holds the schema. Each field is read as its schema declares it:

```tabular
column  The schema declares
column  The field reads as
row  a scalar  the type its validator names
row  an optional field  an `Optional`, absent where the document omits it
row  a required field  its value, failing with a `Tel.Provider.Error` when it is missing
row  a repeatable field  a `List` of every occurrence
row  a struct, or a reference to a record definition  a nested record
row  a select  one optional field per variant
row  a *flag*, a keyword with no value  a `Boolean`: `true` where present, `false` where not
```

A missing required field fails through the [tactic] of the code which reads it. The built-in
scalars are provided:

```tabular
column  Scalar
column  Validator
column  Reads as
row  `String`  `string`  `Text`
row  `Identifier`  `identifier`  `Text`
row  `TypeName`  `type-name`  `Text`
row  `Sigil`  `sigil`  `Char`, failing on an atom of more than one character
```

A custom validator is read through a given with the same label, in scope where `record` is
called, such as
`given ("handle" is Intensional in Tel.Provider from Tel to Handle) = Intensional(…)`.

A provider, and a record read through it:

```scala fuel
typecheck
##
object ContactRecords extends Tel.Provider(Tels.tels[Person](t"person"))
```

```scala fuel
typecheck
##
val record = ContactRecords.record(t"name Alice\nage 30\n".read[Tel])
val recordName: Text = record.name
val recordAge: Int = record.age
```

A field which the schema does not describe is a compile error. The shape of the data is taken
from the schema and checked by the compiler, with no Scala type restating it.
[JSON](json.md#typed-records-from-a-schema) and
[XML](xml.md#typed-records-from-an-xml-schema) have the same facility, and
[Records](records.md) describes the records themselves.

### BinTEL

A typed document encodes to BinTEL, a compact binary form guided by the schema, and decodes
back. A *value hash* fingerprints the content, for integrity checks:

```scala
val binary = document.bintel(schema)
val hash = document.valueHash(schema)
```

The value hash is taken over the document's content, not its bytes. Two documents which differ
only in comments, whitespace or the order of keys have the same hash, while a document whose
values differ does not. It is therefore usable as a cache key, or to detect a change, where
hashing the file would report a change whenever someone reflowed it.

A *self-contained* BinTEL frame carries its schema with it, so a document can travel to a
reader which has never seen its schema, and still be decoded:

```scala
val greeting = t"name Alice\n".read[Tel]
val greetingSchema = t"name greeting\n\ndocument\n  field name Identifier\n".read[Tel]

val frame = Bintel.selfContained(greeting, greetingSchema)
```

The alternative is to send the data alone, and identify the schema by its *signature*. A
signature is a short fingerprint computed from the schema itself, so a reader can confirm that
it holds the right schema before it decodes a byte of data. A schema with no layers has a
signature of thirty-three bytes.

Numbers in BinTEL are encoded in a variable number of bytes, and values are typed by the
schema, not tagged in the stream, which is where the compactness comes from. Every framed
document also declares its own length, immediately after the magic number. A reader can
therefore delimit, skip or forward documents without holding their schema, and several
documents may share one stream:

```tabular
column  Method
column  Reads
row  `Bintel.decodeStream`  every document, in order
row  `Bintel.decodeDocument`  one document, reporting where its continuation begins
row  `Bintel.decodeWholeDocument`  one document, insisting that nothing follows
```

### Acceptances

Two peers may hold different libraries of schemas. A reader then tells a writer what it can
consume, with an *acceptance*. An acceptance is a small TEL document which lists, in order of
preference, the compositions the reader accepts, each as a schema signature. Beside each it
lists the further layers or atoms which the reader can resolve, and would like included if the
writer holds them, named by a prefix of their hash.

The message is itself TEL, so it travels in three forms:

```tabular
column  Form
column  Written by
column  Read back by
row  TEL text  `acceptance.encode`  `as[Tel.Acceptance]` on the text
row  framed BinTEL  `acceptance.framed`  `Tel.Acceptance(framed)`
row  one BASE-256 word  `acceptance.text`  `Tel.Acceptance(text)`
```

The BinTEL form may also be the bare root of the document, for an embedding which already
knows what it is carrying. In full:

```scala fuel
parse
##
val acceptance = Tel.Acceptance
  ( Tel.Acceptance.Alternative
      ( Tel.Acceptance.Signature(contact.signature(List(t"with-address"))),
        components = List(Tel.Acceptance.Component(contact.prefix(t"with-phone"))) ),
    Tel.Acceptance.Alternative(Tel.Acceptance.Signature(contact.signature(List())), selfContained = true) )

val asText = acceptance.encode
val asBintel = acceptance.framed
val asWord = acceptance.text

val fromText = acceptance.encode.as[Tel.Acceptance]
val fromBintel = Tel.Acceptance(acceptance.framed)
val fromWord = Tel.Acceptance(acceptance.text)
```

Here `contact` is a `SchemaSignature.Lineage`: a base schema, with its layers and atoms and
their hashes. A signature for any selection of layers is computed from it, and a writer decodes
against it the signatures which a reader sends.

A writer which holds a value under some composition serves the first alternative it can, with
the richest composition that alternative permits. The reader takes the first alternative whose
schema the arriving document's schema is a subtype of:

```scala fuel
parse
##
val served = Tel.Acceptance.serve(acceptance, contact, composition, element)

served.let: served =>
  Tel.Acceptance.receive(acceptance, library, served.document)
```

Compatibility is the subtype relation on composed schemas:

```tabular
column  Definition
column  A subtype
row  record  extends it
row  select  narrows it
row  scalar  tightens it
```

`Tels.Subtyping.subtype` decides it, and `check` reports the first premise which fails.
`Tels.Projection` restricts a document to what a supertype can address.

#### Acceptances from Scala types

Where the formats are Scala types, the acceptance and the reading of it are derived. Each
type's required fields form the base which a reader requires. Its optional fields are offered
as atoms, so two types which agree on their required fields interoperate, however their
optional fields differ.

A field can instead be offered as part of a named layer. A layer is declared once, as an object
whose name is part of its type, and a field is assigned to it with `@assign`:

```scala fuel
typecheck
##
object withPhone extends Tel.Layer("with-phone")

case class Contact(name: Text, email: Optional[Text], @assign(withPhone) phone: Optional[Text])
case class ContactV1(name: Text, email: Optional[Text])
```

A tuple names the accepted types, in order of preference, and what arrives is decoded to their
union:

```scala fuel
typecheck
##
val accepting = Tel.Acceptance[(Contact, ContactV1)]()
send(accepting.acceptance.framed)

accepting.read(reply) match
  case contact: Contact   => contact.name
  case contact: ContactV1 => contact.name
```

A layer can never add a case to a sum, but it can take cases away. A case of an `enum`, or of a
sealed trait, which is annotated `@retract` remains a variant of the base, and the named layer
excludes it. A reader which accepts that layer thereby says that it will never receive the
case. A case may be retracted from several layers, though a layer cannot retract every case of
a sum:

```scala fuel
typecheck
##
object lean extends Tel.Layer("lean")

enum Message:
  @retract(lean) case Ping
  @retract(lean) case Data(payload: Text)
  case Stop
```

A writer answers from a value with `contact.fulfil(accepting.acceptance)`. It leaves out any
layer which retracts a case the value uses, so that what it serves is always valid under the
layers it names.

### Related

- [Daemons](daemons.md) and [RPC](rpc.md) carry BinTEL messages, and [optics](optics.md) updates a TEL document.

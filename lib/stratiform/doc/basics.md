All Stratiform terms and types are in the `stratiform` package, and exported to the
`soundness` package. So we begin either by importing,
```scala
import stratiform.*
```
or:
```scala
import soundness.*
```

### Parsing

A TEL document is parsed via the standard turbulence `read` / `load` extensions: any value
that is `Streamable by Data` (bytes, files, network buffers) or `Streamable by Text` (a
`Text` value, an `InputStream`, an HTTP body) can be parsed by asking for the result type.
```scala
import contingency.*, strategies.throwUnsafely
import charEncoders.utf8Encoder

val tel: Tel = t"name Alice\nemail a@b.c\n".read[Tel]
```

`text.load[Tel]` returns a `Document[Tel]` pairing the parsed value with the document's
prologue (interpreter directive, pragma, line endings) in `Tel.Metadata`:
```scala
val doc: Document[Tel] = t"tel 1.0\nname Alice\n".load[Tel]
doc.root             // Tel value
doc.metadata.pragma  // Optional[Tel.Pragma]
```

`Tel.show(tel)` reverses the parse, preserving formatting, comments, blank lines, and remark
text.

### Generic decoding

Case classes decode directly from a `Tel`:
```scala
case class Contact(name: Text, email: Text) derives CanEqual
val parsed = t"name Alice\nemail a@b.c\n".read[Tel].as[Contact]
```

The derivation maps camelCase field names to kebab-case TEL keywords automatically, so a
field `firstName: Text` is read from a TEL compound whose keyword is `first-name`.

### Statically-checked literals

The `tel"…"` interpolator parses TEL at compile time and substitutes typed holes through the
`Encodable in Tel` instance derived for each insertion's static type:
```scala
val name = t"Alice"
val contact = tel"name $name"
```

A malformed literal is a compile-time error, with the source position pointing at the offending
range in the `.scala` file.

The matching extractor binds atom-text captures from a `Tel`:
```scala
contact match
  case tel"name $name" => name.as[Text]
  case _               => t"anonymous"
```

### Mutation

The `Revision` DSL builds presentation-preserving rewrites:
```scala
val updated = tel.edited
                ( Revision.at(Tel.Pointer.of(t"name")).update(t"Bob")
               ++ Revision.at(Tel.Pointer.of(t"name")).attachRemark(t"primary") )
```

Surrounding atoms, comments, and unrelated children stay untouched.

### Schemas

A `Tels` describes the structural shape of a TEL document family. Schemas declare records,
scalars and selects, with field- and selectRef-level polarity for required/optional and
repeatable/irrepeatable axes. Layers refine a base schema; type-assignment of a document
against the composed schema produces a semantic `Tel.Element` tree and reports E2xx/E3xx
violations on mismatch.

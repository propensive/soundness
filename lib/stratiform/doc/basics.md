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

A TEL document is parsed from raw bytes:
```scala
import contingency.*, strategies.throwUnsafely
val source: IArray[Byte] = ???
val tel: Tel = Tel.parse(source)
```

The result is a `Tel` value wrapping the presentation AST. The same AST round-trips through
`Tel.show(tel)` back to bytes, preserving formatting, comments, blank lines, and remark text.

### Generic decoding

Case classes decode directly from a `Tel`:
```scala
case class Contact(name: Text, email: Text) derives CanEqual
val parsed = Tel.parse(bytes).as[Contact]
```

The derivation maps camelCase field names to kebab-case TEL keywords automatically, so a
field `firstName: Text` is read from a TEL compound whose keyword is `first-name`.

### Statically-checked literals

The `tel"…"` interpolator parses TEL at compile time and substitutes typed holes through the
`Encodable in Tel` instance derived for each insertion's static type:
```scala
val name = Text("Alice")
val contact = tel"name $name"
```

A malformed literal is a compile-time error, with the source position pointing at the offending
range in the `.scala` file.

The matching extractor binds atom-text captures from a `Tel`:
```scala
contact match
  case tel"name $name" => name.as[Text]
  case _               => Text("anonymous")
```

### Mutation

The `Edit` DSL builds presentation-preserving rewrites:
```scala
val updated = tel.edited
                ( Edit.at(TelPointer.of(Text("name"))).update(Text("Bob"))
               ++ Edit.at(TelPointer.of(Text("name"))).attachRemark(Text("primary")) )
```

Surrounding atoms, comments, and unrelated children stay untouched.

### Schemas

A `TelSchema` describes the structural shape of a TEL document family. Schemas declare records,
scalars and selects, with field- and selectRef-level polarity for required/optional and
repeatable/irrepeatable axes. Layers refine a base schema; type-assignment of a document
against the composed schema produces a semantic `TelElement` tree and reports E2xx/E3xx
violations on mismatch.

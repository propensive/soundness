# A field which the schema does not require

A JSON Schema says which properties must be present. A field which is not required may be
missing from a valid document, so the record gives it as an `Optional`, and the program
decides what its absence means.

```scala fuel
compile
frame
  label The schema requires only the name
  highlight Optional[Int]  not in the schema's required list
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      object Play extends Json.Provider(t"""{
        "type": "object",
        "required": ["name"],
        "properties": { "name": { "type": "string" }, "acts": { "type": "integer" } }
      }""".read[Json])

      val record = Play.record(t"""{"name": "Hamlet"}""".read[Json])
      val acts: Optional[Int] = record.acts
frame
  label A default, where the value is needed
  highlight or(5)  what absence means here
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      object Play extends Json.Provider(t"""{
        "type": "object",
        "required": ["name"],
        "properties": { "name": { "type": "string" }, "acts": { "type": "integer" } }
      }""".read[Json])

      val record = Play.record(t"""{"name": "Hamlet"}""".read[Json])
      val acts: Int = record.acts.or(5)
##
```

[Using records](../../soundness/modules/records.md#using-records) gives the type of each kind of
field, and [optionality](../../philosophy/optionality.md) explains `or`.

# Fields from a schema

A `Json.Provider` reads a JSON Schema as the code compiles, and gives each document that
matches it a record with the schema's fields at the schema's types. A field which the schema
does not have is not a member of the record.

```scala fuel
compile
substitute t"""{.."""  schema
frame
  label A field at the type the schema gives it
  highlight record.name
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      object Play extends Json.Provider(t"""{
        "type": "object",
        "required": ["name"],
        "properties": { "name": { "type": "string" }, "acts": { "type": "integer" } }
      }""".read[Json])

      val record = Play.record(t"""{"name": "Hamlet", "acts": 5}""".read[Json])
      val name: Text = record.name
##
```

[Typed records from a schema](../../soundness/modules/json.md#typed-records-from-a-schema) says
how each construct of a schema is read.

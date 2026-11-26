# A named tuple from a schema

A provider turns a document into a record, whose fields are read as they are used, or into a
named tuple, whose fields are all read at once. The tuple's names, types and order are the
schema's, and it is taken apart by position like any other tuple.

```scala fuel
compile
frame
  label A record, read field by field
  highlight record  reads each field when it is selected
    occurrence 1
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      object Play extends Json.Provider(t"""{
        "type": "object",
        "required": ["name", "acts"],
        "properties": { "name": { "type": "string" }, "acts": { "type": "integer" } }
      }""".read[Json])

      val record = Play.record(t"""{"name": "Hamlet", "acts": 5}""".read[Json])
      val name: Text = record.name
frame
  label A tuple, read whole
  highlight (name,..acts)  in the order of the schema
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      object Play extends Json.Provider(t"""{
        "type": "object",
        "required": ["name", "acts"],
        "properties": { "name": { "type": "string" }, "acts": { "type": "integer" } }
      }""".read[Json])

      val tuple = Play.tuple(t"""{"name": "Hamlet", "acts": 5}""".read[Json])
      val (name, acts) = tuple
##
```

[Tuples instead of records](../../soundness/modules/records.md#tuples-instead-of-records)
compares the two forms.

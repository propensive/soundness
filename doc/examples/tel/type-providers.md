# Records typed by a TEL schema

A `Tel.Provider` holds a schema, and reads a document which matches it as a record. The
record's members are the schema's fields, at the types the schema gives them, and no case class
restates them.

```scala fuel
compile
frame
  label Members at the types of the schema
  highlight record.name  a member the schema declares
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      val schema = t"name player\n\ndocument\n  field name String\n  field patron String optional\n"

      object Players extends Tel.Provider(schema.read[Tel].as[Tels])

      val record = Players.record(t"name Viola\n".read[Tel])
      val name: Text = record.name
      val patron: Optional[Text] = record.patron
##
```

[Typed records from a schema](../../soundness/modules/tel.md#typed-records-from-a-schema)
tabulates how each declaration of a schema is read.

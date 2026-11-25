# Newline-delimited JSON, with no type of its own

A file of JSON documents, one to a line, is a stream of records which are each read as JSON.
Splitting a source into lines is the work of the streaming library, and reading each one is the
work of this one. Neither knows of the other.

```scala fuel
compile
frame
  label A source of text, split at its line endings
  highlight delineate.records  from the streaming library
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import lineSeparation.adaptiveLinefeedLineSeparation

      val cast = t"""{"name": "Viola"}\n{"name": "Feste"}\n{"name": "Orsino"}"""
      val lines = cast.source[Text].delineate.records
frame
  label Each line, read as JSON
  highlight read[Json]  each record, parsed alone
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import lineSeparation.adaptiveLinefeedLineSeparation
      import dynamicAccess.dynamicJson

      val cast = t"""{"name": "Viola"}\n{"name": "Feste"}\n{"name": "Orsino"}"""
      val lines = cast.source[Text].delineate.records
      val names = lines.map(_.read[Json].name.as[Text]).to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(names.inspect)
```

[NDJSON](../../soundness/modules/json.md#ndjson) has the detail, and
[records](../../soundness/modules/streams.md#records) the other ways to divide a source.

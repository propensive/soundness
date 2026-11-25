# Positions, for the programs which ask

A parser which records where every value sat does work most programs never use. Parsing keeps
no positions unless `parsing.trackPositions` is in scope, and the same `locate` then gives the
line of a value where before it gave nothing.

```scala fuel
compile
frame
  label Ordinary parsing
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      val cast = t"{\n  \"fool\": \"Feste\",\n  \"duke\": \"Orsino\"\n}".read[Json]
      val line = cast.locate(JsonPointer()(t"duke")).let(_.line)
frame
  label Parsing which tracks positions
  highlight parsing.trackPositions  read records each value's extent
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import parsing.trackPositions

      val cast = t"{\n  \"fool\": \"Feste\",\n  \"duke\": \"Orsino\"\n}".read[Json]
      val line = cast.locate(JsonPointer()(t"duke")).let(_.line)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line.inspect)
```

Without the import, `line` is `Unset`.
[Source positions](../../soundness/modules/json.md#source-positions) says what a position
covers.

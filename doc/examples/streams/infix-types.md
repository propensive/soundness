# A type which says how it becomes a stream

A type joins `read` and `writeTo` by a single given, whose type reads as a sentence: a `Cue`
is streamable, by text. Nothing else is defined for it.

```scala fuel
compile
frame
  label A type of one's own
  code
      import strategies.throwUnsafely

      case class Cue(speaker: Text, line: Int)
frame
  label How it becomes a stream
  highlight (Cue..Credit)  read as a sentence
  code
      import strategies.throwUnsafely

      case class Cue(speaker: Text, line: Int)

      given cueStreamable: (Cue is Streamable by Text over Credit) =
        cue => Stream(t"${cue.speaker}, line ${cue.line}")
frame
  label It is read like any other source
  highlight read[Text]  needs nothing more
  code
      import strategies.throwUnsafely

      case class Cue(speaker: Text, line: Int)

      given cueStreamable: (Cue is Streamable by Text over Credit) =
        cue => Stream(t"${cue.speaker}, line ${cue.line}")

      val prompt = Cue(t"Feste", 42).read[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(prompt)
```

[Making a value readable or writable](../../soundness/modules/streams.md#making-a-value-readable-or-writable)
has the sink's side, and [infix types](../../philosophy/infix-types.md) the words such types
are made of.

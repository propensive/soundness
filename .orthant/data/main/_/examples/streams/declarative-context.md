# The encoding between text and bytes

Text is read as bytes under an encoding, and the encoding is chosen by an import. The same
line is a different number of bytes under each, and the code which reads it does not change.

```scala fuel
compile
alternative UTF-8
  highlight utf8Codepage  one to four bytes for a character
  code
      import codepages.utf8Codepage, strategies.throwUnsafely

      val bytes = t"Où sont les neiges d’antan?".read[Data]
alternative UTF-16
  highlight utf16Codepage  two or four
  code
      import codepages.utf16Codepage, strategies.throwUnsafely

      val bytes = t"Où sont les neiges d’antan?".read[Data]
##
```

[Reading a source](../../soundness/modules/streams.md#reading-a-source) describes `read`, and
[declarative context](../../philosophy/declarative-context.md) the reasons for an import.

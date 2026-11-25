# Text does not become bytes by itself

Text is characters, and bytes are bytes. Reading one as the other needs an encoding, and
there is no default to forget about: without one in scope, the read does not compile.

```scala fuel
compile
frame
  label Text, read as bytes, in no encoding
  errors show
  code
      import strategies.throwUnsafely

      val bytes = t"Où sont les neiges d’antan?".read[Data]
frame
  label In UTF-8
  highlight utf8Codepage  the encoding, stated
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      val bytes = t"Où sont les neiges d’antan?".read[Data]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bytes.length.show)
```

The line has twenty-seven characters, and more bytes than that.
[Reading a source](../../soundness/modules/streams.md#reading-a-source) describes `read`, and
[honest signatures](../../philosophy/honest-signatures.md) the principle.

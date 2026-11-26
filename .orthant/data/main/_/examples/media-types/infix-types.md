# A detector, described by its type

A detector for a new format is one given, and its type is a sentence: which media type it
detects, how certain a match is, what it reads from, and what the content is read as.

```scala fuel
compile
frame
  label The signature of an SQLite file
  highlight "application/vnd.sqlite3"..Data  what, how certain, from what, as what
  code
      given sqlite: ("application/vnd.sqlite3" is Detectable under Likely over Probe to Data) =
        Detectable.signature(0x53, 0x51, 0x4c, 0x69, 0x74, 0x65)
frame
  label A pattern which uses it
  highlight Media["application/vnd.sqlite3"](database)  found through the given above
  code
      given sqlite: ("application/vnd.sqlite3" is Detectable under Likely over Probe to Data) =
        Detectable.signature(0x53, 0x51, 0x4c, 0x69, 0x74, 0x65)

      def identify(content: Data): Text = content match
        case Media["application/vnd.sqlite3"](database) => t"a database"
        case _                                          => t"something else"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(identify(hex"53514c69746520666f726d6174203300"))
Out.println(identify(hex"89504e470d0a1a0a"))
```

[Writing a detector](../../soundness/modules/media-types.md#writing-a-detector) covers the
formats which need more than a signature, and [infix types](../../philosophy/infix-types.md)
the words the type is made of.

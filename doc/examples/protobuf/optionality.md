# Absent, or present and empty

Proto3 cannot ordinarily tell a field which was never set from one set to its default. A field
of type `Optional[Text]` has explicit presence: unset, it writes nothing, and an empty text is
still written.

```scala fuel
compile
frame
  label A name which is not there
  highlight Unset  writes nothing
  code
      case class Player(@field(1) name: Optional[Text])

      val absent = Player(Unset).in[Protobuf].encode.length
frame
  label A name which is there, and empty
  highlight t""  writes a tag and a length
  code
      case class Player(@field(1) name: Optional[Text])

      val absent = Player(Unset).in[Protobuf].encode.length
      val empty = Player(t"").in[Protobuf].encode.length
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(absent.show)
Out.println(empty.show)
```

[Presence, repetition and maps](../../soundness/modules/protobuf.md#presence-repetition-and-maps)
covers lists and maps, and [optionality](../../philosophy/optionality.md) the type itself.

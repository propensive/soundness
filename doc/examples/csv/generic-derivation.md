# A nested case class takes as many columns as it has fields

A row is decoded to a case class by taking its cells in order. A field which is itself a case
class takes one cell for each of its own fields, and the number is computed from the types as
the code compiles.

```scala fuel
compile
frame
  label Three fields, three columns
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Player(id: Int, name: Text, age: Int)

      val bottom = t"1,Nick Bottom,36".read[Player in Dsv]
frame
  label A field of two columns
  highlight name..Name  spans two columns
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Name(first: Text, last: Text)
      case class Player(id: Int, name: Name, age: Int)

      val bottom = t"1,Nick,Bottom,36".read[Player in Dsv]
      val spans = Spannable.derived[Player].spans().to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bottom.inspect)
Out.println(spans.inspect)
```

[Column spans](../../soundness/modules/csv.md#column-spans) says what this means for data
which is read by position.

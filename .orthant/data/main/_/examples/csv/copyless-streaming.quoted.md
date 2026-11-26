# A quoted cell which straddles two blocks

A stream delivers characters in blocks which know nothing of rows or cells. `rows` parses each
row as it is asked for, carrying an unfinished cell from one block into the next, so a quoted
cell with a comma in it is one cell, wherever the stream happened to be cut.

```scala fuel
compile
frame
  label A cell cut in two by the blocks of the stream
  highlight Bottom..Nick  one cell, in two blocks
  code
      import dsvFormats.csvFormat, strategies.throwUnsafely

      case class Part(name: Text, lines: Int)

      val blocks = Chain(t"\"Bottom,", t" Nick\",261\nPuck,206")
frame
  label The rows, parsed as they are pulled
  highlight .rows  each row is read when the consumer asks for it
  code
      import dsvFormats.csvFormat, strategies.throwUnsafely

      case class Part(name: Text, lines: Int)

      val blocks = Chain(t"\"Bottom,", t" Nick\",261\nPuck,206")
      val names = Stream(blocks).rows.map(_.as[Part].name).to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(names.inspect)
```

[Reading rows](../../soundness/modules/csv.md#reading-rows) compares a sheet, which holds its
rows, with a stream of them.

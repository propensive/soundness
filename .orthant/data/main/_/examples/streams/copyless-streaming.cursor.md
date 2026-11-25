# A word taken from across two blocks

A parser reads a stream through a `Cursor`, which sees one continuous run of characters however
the source was cut into blocks. `hold` keeps what is passed in the buffer, a `mark` names a
position, and `grab` gives everything between two marks as one value, though it began in one
block and ended in the next.

```scala fuel
compile
frame
  label A cursor over two blocks
  highlight t"Exit..bear"
  code
      val cursor = Cursor(Iterator(t"Exit, pur", t"sued by a bear"))
frame
  label A mark where the word starts
  highlight cursor.mark
  code
      val cursor = Cursor(Iterator(t"Exit, pur", t"sued by a bear"))

      val word = cursor.hold:
        for i <- 1 to 6 do cursor.next()
        val start = cursor.mark
        start
frame
  label Everything between two marks
  highlight cursor.grab..mark)
  code
      val cursor = Cursor(Iterator(t"Exit, pur", t"sued by a bear"))

      val word = cursor.hold:
        for i <- 1 to 6 do cursor.next()
        val start = cursor.mark
        for i <- 1 to 7 do cursor.next()
        cursor.grab(start, cursor.mark)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(word)
```

[Parsing with a cursor](../../soundness/modules/streams.md#parsing-with-a-cursor) describes the
rest of a cursor's methods.

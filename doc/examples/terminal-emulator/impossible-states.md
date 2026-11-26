# The third row is `Ter`

An escape sequence counts rows and columns from one, and an array counts them from zero. A
cell of the screen is addressed by ordinals, which are neither count, so the two cannot be
confused, and a bare number is not accepted.

```scala fuel
compile
frame
  label The cursor moved to row 3, column 5
  code
      import strategies.throwUnsafely

      val esc = 0x1b.toChar
      val screen = Pty(80, 24).consume(t"$esc[3;5HX").buffer
frame
  label The cell, by numbers
  errors show
  code
      import strategies.throwUnsafely

      val esc = 0x1b.toChar
      val screen = Pty(80, 24).consume(t"$esc[3;5HX").buffer
      val mark = screen.char(5, 3)
frame
  label The cell, by ordinals
  highlight Quin  the fifth column
  highlight Ter  the third row
  code
      import strategies.throwUnsafely

      val esc = 0x1b.toChar
      val screen = Pty(80, 24).consume(t"$esc[3;5HX").buffer
      val mark = screen.char(Quin, Ter)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(mark.inspect)
```

[Inspecting the screen](../../soundness/modules/terminal-emulator.md#inspecting-the-screen)
reads a cell's style and hyperlink by the same address.

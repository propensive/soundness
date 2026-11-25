# Text which may not be on the screen

`find` looks for text on the screen, and gives the cells which hold it. The text may not be
there, so the result is optional, and a style cannot be read from it until absence has been
dealt with.

```scala fuel
compile
frame
  label The style of text which may be absent
  errors show
  code
      import strategies.throwUnsafely

      val esc = 0x1b.toChar
      val screen = Pty(40, 2).consume(t"Exit, pursued by a $esc[3mbear").buffer

      val italic = screen.find(t"bear").style(Prim, Prim).italic
frame
  label Read only if it was found
  highlight let  applies to a value which is present
  code
      import strategies.throwUnsafely

      val esc = 0x1b.toChar
      val screen = Pty(40, 2).consume(t"Exit, pursued by a $esc[3mbear").buffer

      val italic = screen.find(t"bear").let(_.style(Prim, Prim).italic)
      val absent = screen.find(t"lion").let(_.style(Prim, Prim).italic)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(italic.inspect)
Out.println(absent.inspect)
```

[Inspecting the screen](../../soundness/modules/terminal-emulator.md#inspecting-the-screen)
describes what a cell holds, and [optionality](../../philosophy/optionality.md) the operations
on a value which may be absent.

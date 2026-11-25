# Styled text, checked on a screen

Styled text is rendered to escape sequences, and a `Pty` interprets escape sequences. Neither
knows of the other, and together they answer the question a test asks: is the first word bold
on the screen?

```scala fuel
compile
frame
  label Styled text
  code
      import strategies.throwUnsafely

      val cue = e"$Bold(Exit), pursued by a bear"
frame
  label Rendered for a terminal
  highlight render  text with escape sequences in it
  code
      import strategies.throwUnsafely

      val cue = e"$Bold(Exit), pursued by a bear"
      val output = cue.render(termcapDefinitions.xterm256Termcap)
frame
  label Consumed by one
  highlight consume(output)  the sequences take effect
  code
      import strategies.throwUnsafely

      val cue = e"$Bold(Exit), pursued by a bear"
      val output = cue.render(termcapDefinitions.xterm256Termcap)
      val screen = Pty(40, 2).consume(output).buffer
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(screen.char(Prim, Prim).inspect)
Out.println(screen.style(Prim, Prim).bold.inspect)
Out.println(screen.style(Sen, Prim).bold.inspect)
```

[Inspecting the screen](../../soundness/modules/terminal-emulator.md#inspecting-the-screen)
has the other properties of a cell, and [Terminal](../../soundness/modules/terminal.md#styled-text)
the text which was rendered.

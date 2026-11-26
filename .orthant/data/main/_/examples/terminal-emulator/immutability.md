# A screen which is a value

A real terminal is mutated by everything written to it. A `Pty` is not: consuming text gives a
new terminal, and the one before is still there, so a test can hold each state of a screen and
compare them.

```scala fuel
compile
frame
  label A blank terminal
  code
      import strategies.throwUnsafely

      val blank = Pty(40, 4)
frame
  label Text written to it
  highlight consume  gives a new terminal
  code
      import strategies.throwUnsafely

      val blank = Pty(40, 4)
      val asked = blank.consume(t"Who's there?")
frame
  label And more, to that
  highlight asked.consume  asked keeps its own screen
  code
      import strategies.throwUnsafely

      val blank = Pty(40, 4)
      val asked = blank.consume(t"Who's there?")
      val answered = asked.consume(t" Nay, answer me.")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(blank.buffer.char(Prim, Prim).inspect)
Out.println(asked.buffer.char(Prim, Prim).inspect)
Out.println(asked.buffer.find(t"Nay").absent.inspect)
Out.println(answered.buffer.find(t"Nay").absent.inspect)
```

[Feeding a terminal](../../soundness/modules/terminal-emulator.md#feeding-a-terminal) shows
escape sequences consumed in the same way, and [immutability](../../philosophy/immutability.md)
what is gained by it.

# A broken escape sequence is declared

A real terminal quietly mangles a sequence it does not understand. A `Pty` raises a
`Pty.Error`, and `consume` says so in its type, so a method which feeds a terminal must
handle the error or declare it too.

```scala fuel
compile
frame
  label An erase command with a mode which does not exist
  errors show
  code
      val esc = 0x1b.toChar

      def erase(pty: Pty): Pty = pty.consume(t"$esc[9J")
frame
  label Declared
  highlight raises..Pty.Error  the caller now knows
  code
      val esc = 0x1b.toChar

      def erase(pty: Pty): Pty raises Pty.Error = pty.consume(t"$esc[9J")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

Out.println(capture[Pty.Error](erase(Pty(80, 24))).message.show)
```

[Feeding a terminal](../../soundness/modules/terminal-emulator.md#feeding-a-terminal) describes
the error, and [error handling](../../philosophy/error-handling.md) the reasons for naming it.

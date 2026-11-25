# The fault in a sequence, as a value

A `Pty.Error` holds a reason, and the reason is a case of an enumeration with the offending
parts as fields. A test which finds a malformed sequence can report the command and the
parameter, and need not search a message for them.

```scala fuel
compile
frame
  label The error, as a message
  code
      val esc = 0x1b.toChar

      def check(output: Text): Text =
        recover:
          case error: Pty.Error => error.message.text
        . protect:
            Pty(80, 24).consume(output)
            t"well formed"
frame
  label The reason, taken apart
  highlight BadCsiParameter(number,..command)  typed fields, and no parsing
  code
      import Pty.Error.Reason.BadCsiParameter

      val esc = 0x1b.toChar

      def check(output: Text): Text =
        recover:
          case Pty.Error(BadCsiParameter(number, command)) => t"$command has no mode $number"
          case error: Pty.Error                            => error.message.text
        . protect:
            Pty(80, 24).consume(output)
            t"well formed"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(check(t"$esc[2J"))
Out.println(check(t"$esc[9J"))
Out.println(check(t"$esc[38;5;999m"))
```

[Feeding a terminal](../../soundness/modules/terminal-emulator.md#feeding-a-terminal) says
when the error is raised, and [expressive errors](../../philosophy/expressive-errors.md) why it
has fields.

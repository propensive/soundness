# The same fault, as a value

The parser which checks a literal pattern is also the one which parses a pattern built at
runtime. There it raises a `Regex.Error`, which holds the reason as a case of an enumeration
and the position as a number.

```scala fuel
compile
frame
  label A pattern from outside the program
  highlight raises..Regex.Error  the parser of the literal, at runtime
  code
      def pattern(source: Text): Regex raises Regex.Error = Regex(source)
frame
  label Its fault, captured
  highlight capture[Regex.Error]  the error, as a value
  code
      import errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely

      def pattern(source: Text): Regex raises Regex.Error = Regex(source)

      val error = capture[Regex.Error](pattern(t"hello( world"))
      val reason: Regex.Error.Reason = error.reason
      val character: Int = error.index
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.message.show)
Out.println((reason == Regex.Error.Reason.UnclosedGroup).inspect)
Out.println(character.show)
```

[Compiletime checking](../../soundness/modules/patterns.md#compiletime-checking) lists the
reasons, and [expressive errors](../../philosophy/expressive-errors.md) why an error is more
than its message.

# Why text was not a number

Text can fail to be a `Byte` in two ways: it is not a number at all, or it is a number which
does not fit. A `Number.Error` holds which of the two it was, as a value that code can match
on, and not only as a message.

```scala fuel
compile
frame
  label The error, captured
  highlight capture[Number.Error]  the error, as a value
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def failure(text: Text): Number.Error = capture[Number.Error](text.as[Byte])
frame
  label Its reason
  highlight reason  a value to match on
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def failure(text: Text): Number.Error = capture[Number.Error](text.as[Byte])

      val tooLarge = failure(t"300").reason
      val nonsense = failure(t"hey nonny").reason
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(tooLarge.inspect)
Out.println(nonsense.inspect)
Out.println(failure(t"300").message.show)
```

[Decoding a value](../../soundness/modules/decoding.md#decoding-a-value) describes the error,
and [expressive errors](../../philosophy/expressive-errors.md) why its reason is typed.

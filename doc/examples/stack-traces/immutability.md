# A stack trace which is a value

The stack trace of a `Throwable` is mutable state, to be printed. A `StackTrace` is a case
class: it is compared with `==`, and trimming its frames gives another trace and leaves the
first as it was.

```scala fuel
compile
frame
  label A trace, captured from an exception
  highlight stackTrace  any Throwable has one
  code
      val exception = Exception("Something is rotten in the state of Denmark")
      val trace: StackTrace = exception.stackTrace
frame
  label Without its innermost frame
  highlight drop(1)  a new trace
  code
      val exception = Exception("Something is rotten in the state of Denmark")
      val trace: StackTrace = exception.stackTrace
      val outer: StackTrace = trace.drop(1)
frame
  label The same exception, captured again
  highlight again  equal to the first
  code
      val exception = Exception("Something is rotten in the state of Denmark")
      val trace: StackTrace = exception.stackTrace
      val outer: StackTrace = trace.drop(1)
      val again: StackTrace = exception.stackTrace
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((trace == outer).inspect)
Out.println((trace == again).inspect)
Out.println((outer == again.drop(1)).inspect)
```

[Capturing a trace](../../soundness/modules/stack-traces.md#capturing-a-trace) describes what
the value holds, and [immutability](../../philosophy/immutability.md) what follows from its
never changing.

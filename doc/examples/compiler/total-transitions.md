# A compiler crash is a value

A compilation ends in one of three ways, and a `Compiler.Result` has a case for each. A crash
of the compiler is not an exception which escapes the tool that started it. It is the third
case, with its stack trace, and a `match` which forgets it is incomplete.

```scala fuel
compile
frame
  label Success and failure
  code
      def verdict(result: Compiler.Result): Text = result match
        case Compiler.Result.Success => t"compiled"
        case Compiler.Result.Failure => t"rejected"
        case _                       => t"something else"
frame
  label And the third case
  highlight Crash(trace)  the compiler's own failure, with its stack trace
  code
      def verdict(result: Compiler.Result): Text = result match
        case Compiler.Result.Success      => t"compiled"
        case Compiler.Result.Failure      => t"rejected"
        case Compiler.Result.Crash(trace) => t"the compiler crashed: ${trace.message}"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(verdict(Compiler.Result.Failure))
```

[Compiling](../../soundness/modules/compiler.md#compiling) describes the process which gives
the result, and [total transitions](../../philosophy/total-transitions.md) the principle.

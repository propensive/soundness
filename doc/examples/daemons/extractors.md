# A signal, taken apart

A signal which reaches an invocation is a value, and a trap is a pattern match on it. The
interrupt and what the launcher attached to it, such as the new size of the terminal, are
bound by the pattern, and the size is optional because not every signal has one.

```scala fuel
compile
frame
  label Any signal
  code
      import backstops.stackTraceBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, threads.virtualThreads

      def watch(): Unit = cli:
        execute:
          trap:
            case signal => SignalResponse.Accept

          Exit.Ok
frame
  label A change in the size of the terminal
  highlight Signal(Interrupt.Winch,  the terminal was resized
  code
      import backstops.stackTraceBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, threads.virtualThreads

      def watch(): Unit = cli:
        execute:
          trap:
            case Signal(Interrupt.Winch, columns, rows, _) =>
              Out.println(t"now ${columns.or(0)}×${rows.or(0)}")
              SignalResponse.Accept

            case signal => SignalResponse.Accept

          Exit.Ok
##
```

The example is compiled, and is not run, since it defines a whole program.
[Signals and shutdown](../../soundness/modules/daemons.md#signals-and-shutdown) lists what each
signal carries.

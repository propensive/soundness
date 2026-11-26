# A cooked terminal, for one block

A daemon's launcher keeps the terminal in raw mode, which is wrong for a command that wants a
line of input with echo and Backspace. `cooked` asks for canonical mode for the length of a
block, and raw mode comes back when the block ends.

```scala fuel
compile
frame
  label A question, asked in raw mode
  code
      import backstops.stackTraceBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, threads.virtualThreads

      def ask(): Unit = cli:
        execute:
          Out.println(t"Who's there?")
          Exit.Ok
frame
  label The same question, in a cooked terminal
  highlight resident.cooked:  canonical mode, until the block ends
  code
      import backstops.stackTraceBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, threads.virtualThreads

      def ask(): Unit = cli:
        execute:
          resident.cooked:
            Out.println(t"Who's there?")

          Exit.Ok
##
```

The example is compiled, and is not run, since it defines a whole program.
[Asking for a cooked terminal](../../soundness/modules/daemons.md#asking-for-a-cooked-terminal)
reads the answer, and [delimited scopes](../../philosophy/delimited-scopes.md) explains why
the mode is lent to a block and not switched.

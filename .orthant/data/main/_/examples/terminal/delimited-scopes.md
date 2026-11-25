# Raw mode which always ends

A terminal left in raw mode after a crash wrecks the shell which started the program.
`interactive` is a block: the terminal exists only inside it, and is restored when the block
ends, however it ends.

```scala fuel
compile
frame
  label A question, with no terminal to ask it on
  errors show
  code
      import strategies.throwUnsafely

      def name(): Text = LineEditor().ask(identity(_))
frame
  label Inside a session
  highlight interactive  raw mode begins and ends with this block
  code
      import environments.javaBaseEnvironment, probates.awaitProbate
      import strategies.throwUnsafely, threads.virtualThreads

      def name()(using Console): Text =
        supervise:
          interactive:
            LineEditor().ask(identity(_))
##
```

The example is compiled, and is not run, since it reads a terminal.
[Keyboard input](../../soundness/modules/terminal.md#keyboard-input) describes the session, and
[delimited scopes](../../philosophy/delimited-scopes.md) the pattern.

# What may not happen while the user presses tab

Tab completion runs the program's own handling of its arguments. A method which does
something irreversible asks for `Effectful`, which exists only inside `execute`, so it cannot
be reached from the code which completion runs.

```scala fuel
compile
frame
  label An effect in the prelude, where completion would run it
  errors show
  code
      import backstops.genericErrorMessageBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def burn(library: Text)(using Effectful): Unit = ()

      def prospero(args: List[Text]): Unit = application(args):
        val library = Flag[Text](t"library").require()
        burn(t"books")
        execute(Exit.Ok)
frame
  label The effect inside `execute`, which runs only on a real invocation
  highlight execute:  supplies `Effectful`
  code
      import backstops.genericErrorMessageBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def burn(library: Text)(using Effectful): Unit = ()

      def prospero(args: List[Text]): Unit = application(args):
        val library = Flag[Text](t"library").require()

        execute:
          burn(library())
          Exit.Ok
##
```

The example is compiled, and is not run, since it is a whole program.
[The executive, and effectful methods](../../soundness/modules/cli.md#the-executive-and-effectful-methods)
describes the token, and [honest signatures](../../philosophy/honest-signatures.md) the
principle.

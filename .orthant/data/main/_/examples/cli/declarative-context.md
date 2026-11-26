# How the arguments are read, by an import

Whether `-vq` is one flag or two is a convention, and not every program follows the same one.
The convention is an interpreter, chosen by an import where the entry point is defined, and
the program which reads the flags is the same under each.

```scala fuel
compile
alternative POSIX
  highlight interpreters.posixInterpreter  `--verbose` and `-v`
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import systems.javaBaseSystem, strategies.throwUnsafely
      import interpreters.posixInterpreter

      def theatre(args: List[Text]): Unit = application(args):
        val verbose = Switch(t"verbose", aliases = List('v'))()
        val quiet = Switch(t"quiet", aliases = List('q'))()
        Exit.Ok
alternative Clustered
  highlight interpreters.posixClusteringInterpreter  `-vq` is read as `-v -q`
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import systems.javaBaseSystem, strategies.throwUnsafely
      import interpreters.posixClusteringInterpreter

      def theatre(args: List[Text]): Unit = application(args):
        val verbose = Switch(t"verbose", aliases = List('v'))()
        val quiet = Switch(t"quiet", aliases = List('q'))()
        Exit.Ok
##
```

The example is compiled, and is not run, since it is a whole program.
[Arguments](../../soundness/modules/cli.md#arguments) lists the interpreters, and
[declarative context](../../philosophy/declarative-context.md) says why the choice is an
import.

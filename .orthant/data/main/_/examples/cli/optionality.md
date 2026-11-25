# A flag which may not have been given

A user may leave any flag out. Reading one gives an optional value, so the program cannot
use the number of prompters before it has said how many there are when nobody chose.

```scala fuel
compile
frame
  label A flag, used as though it were always given
  errors show
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def theatre(args: List[Text]): Unit = application(args):
        val prompters: Int = Flag[Int](t"prompters")()
        Out.println(t"rehearsing with $prompters prompters")
        Exit.Ok
frame
  label A flag, and the value which stands for its absence
  highlight or(1)  what leaving the flag out means
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def theatre(args: List[Text]): Unit = application(args):
        val prompters: Int = Flag[Int](t"prompters")().or(1)
        Out.println(t"rehearsing with $prompters prompters")
        Exit.Ok
##
```

The example is compiled, and is not run, since it is a whole program.
[Flags](../../soundness/modules/cli.md#flags) has the parameters of a flag, and
[optionality](../../philosophy/optionality.md) describes `or`.

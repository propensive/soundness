# The cases of an enumeration as the values of a flag

A flag whose type is an enumeration needs no parser and no list of its values. The compiler
knows the cases, so the flag reads one by name, and a name which is not a case is not a value.

```scala fuel
compile
frame
  label A flag of text, which accepts anything
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def theatre(args: List[Text]): Unit = application(args):
        val genre: Optional[Text] = Flag[Text](t"genre")()
        Exit.Ok
frame
  label A flag of an enumeration
  highlight Flag[Genre]  reads `--genre tragedy`
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      import enumIdentification.kebabCaseIdentifiable

      enum Genre:
        case Comedy, History, Tragedy

      def theatre(args: List[Text]): Unit = application(args):
        val genre: Optional[Genre] = Flag[Genre](t"genre")()
        Exit.Ok
##
```

The example is compiled, and is not run, since it is a whole program.
[Conditional completions](../../soundness/modules/cli.md#conditional-completions) reads such
a flag, and says how the style of its names is chosen.

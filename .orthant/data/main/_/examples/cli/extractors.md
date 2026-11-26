# A subcommand is a pattern

A `Subcommand` is declared once, with the description which help and completions show. The
same value is an extractor, so the program chooses what to do by matching its arguments
against the commands it declared.

```scala fuel
compile
frame
  label Two subcommands
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      val Rehearse = Subcommand(t"rehearse", t"run through a scene")
      val Perform = Subcommand(t"perform", t"stage the whole play")
frame
  label The arguments, matched against them
  highlight Rehearse()  matches the argument `rehearse`
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      val Rehearse = Subcommand(t"rehearse", t"run through a scene")
      val Perform = Subcommand(t"perform", t"stage the whole play")

      def theatre(args: List[Text]): Unit = application(args):
        arguments match
          case Rehearse() :: _ => Out.println(t"rehearsing"); Exit.Ok
          case Perform() :: _  => Out.println(t"performing"); Exit.Ok
          case _               => Exit.Fail(1)
##
```

The example is compiled, and is not run, since it is a whole program.
[Subcommands](../../soundness/modules/cli.md#subcommands) describes nesting and groups.

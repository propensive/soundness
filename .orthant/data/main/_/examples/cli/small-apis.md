# A flag of any type which text can be read as

There is no `intFlag`, `listFlag` or `pathFlag`. A `Flag` takes the type of its value, and
whatever can be decoded from text can be the value of a flag.

```scala fuel
compile
frame
  label A number
  highlight Flag[Int]
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def theatre(args: List[Text]): Unit = application(args):
        val seats: Optional[Int] = Flag[Int](t"seats")()
        Exit.Ok
frame
  label An address
  highlight Flag[Ipv4]  decoded as `t"…".as[Ipv4]` would be
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def theatre(args: List[Text]): Unit = application(args):
        val seats: Optional[Int] = Flag[Int](t"seats")()
        val host: Optional[Ipv4] = Flag[Ipv4](t"host")()
        Exit.Ok
frame
  label Every occurrence of a repeated flag
  highlight Flag[List[Text]]  `--role Viola --role Feste`
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def theatre(args: List[Text]): Unit = application(args):
        val seats: Optional[Int] = Flag[Int](t"seats")()
        val host: Optional[Ipv4] = Flag[Ipv4](t"host")()
        val roles: Optional[List[Text]] = Flag[List[Text]](t"role", repeatable = true)()
        Exit.Ok
##
```

The example is compiled, and is not run, since it is a whole program.
[Flags](../../soundness/modules/cli.md#flags) describes how a flag is read, and
[small APIs](../../philosophy/small-apis.md) why there is one name.

# Printing needs somewhere to print

Standard output is a capability, and not a global. `Out.println` prints through the `Stdio`
in scope, so a method which prints says so in its signature, and code which was given no
`Stdio` prints nothing.

```scala fuel
compile
frame
  label A method which prints, and does not say so
  errors show
  code
      def announce(name: Text): Unit = Out.println(t"Enter $name")
frame
  label Standard output, as a parameter
  highlight (using..Stdio)  where the output goes
  code
      def announce(name: Text)(using Stdio): Unit = Out.println(t"Enter $name")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

announce(t"Malvolio")
```

[Standard streams](../../soundness/modules/streams.md#standard-streams) redirects and captures
them, and [honest signatures](../../philosophy/honest-signatures.md) has the principle.

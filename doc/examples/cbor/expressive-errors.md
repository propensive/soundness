# The byte at which a message went wrong

A `Cbor.Error` holds its reason as a value, and a reason about the bytes holds the offset at
which the parser stopped. A handler takes the offset from a pattern, and never from the text of
a message.

```scala fuel
compile
frame
  label Any failure, handled alike
  code
      def diagnose(bytes: Data): Text =
        recover:
          case error: Cbor.Error => t"malformed"
        . protect(Cbor.Ast.parse(bytes).show)
frame
  label A truncated message, and where it stopped
  highlight Truncated(offset)  the offset, as a Long
  code
      import Cbor.Error.Reason.Truncated

      def diagnose(bytes: Data): Text =
        recover:
          case Cbor.Error(Truncated(offset)) => t"ends early, at byte $offset"
          case error: Cbor.Error             => t"malformed"
        . protect(Cbor.Ast.parse(bytes).show)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(diagnose(hex"83010203"))
Out.println(diagnose(hex"830102"))
```

The second message promises an array of three elements and holds two.
[Errors](../../soundness/modules/cbor.md#errors) lists the reasons, and
[expressive errors](../../philosophy/expressive-errors.md) says why they are values.

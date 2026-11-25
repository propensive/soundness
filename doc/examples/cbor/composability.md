# Bytes written in hex, read as CBOR

A `hex"…"` literal comes from the library of base encodings, and is checked as it compiles.
The CBOR parser takes its bytes, and `show` gives the diagnostic notation which the CBOR
standard defines, so a message is read in the format's own words.

```scala fuel
compile
frame
  label An array of three numbers
  code
      import strategies.throwUnsafely

      val message = Cbor.Ast.parse(hex"83010203")
frame
  label A string of four bytes
  highlight hex"4401020304"  checked as it compiles
  code
      import strategies.throwUnsafely

      val message = Cbor.Ast.parse(hex"83010203")
      val payload = Cbor.Ast.parse(hex"4401020304")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(message.show)
Out.println(payload.show)
```

[Diagnostic notation](../../soundness/modules/cbor.md#diagnostic-notation) describes the
rendering, and [base encoding](../../soundness/modules/base-encoding.md) the literals.

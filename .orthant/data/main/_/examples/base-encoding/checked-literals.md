# Bytes written as a literal

A `hex"…"` literal is bytes, written in hexadecimal and read by the compiler. A character
which is not a hexadecimal digit is a compile error, and the bytes of a well-formed literal
need no decoding as the program runs.

```scala fuel
compile
frame
  label A letter which is not a digit
  errors show
  code
      val magic: Data = hex"cafebabg"
frame
  label Four bytes
  code
      val magic: Data = hex"cafebabe"
frame
  label The same bytes, in Base64
  highlight serialize[Base64]  six bits to a character
  code
      import alphabets.base64Standard

      val magic: Data = hex"cafebabe"
      val text: Text = magic.serialize[Base64]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
```

[Other bases](../../soundness/modules/base-encoding.md#other-bases) has the bases a literal's
bytes may be written in.

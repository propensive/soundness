# An identifier the compiler has read

A UUID written in the source is parsed as the code compiles. One with a digit which is not
hexadecimal is not a `Uuid` which fails later, because it is never a `Uuid` at all.

```scala fuel
compile
frame
  label Two letters which are not digits
  errors show
  code
      val prospero = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbccXY"
frame
  label Thirty-two digits
  highlight 8c  hexadecimal digits
  code
      val prospero = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(prospero.text)
Out.println(prospero.msb.show)
```

[Literals](../../soundness/modules/uuid.md#literals) has the rest, and
[safety by construction](../../philosophy/safety-by-construction.md) the reason a value is
checked where it is made.

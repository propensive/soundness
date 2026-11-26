# A hexadecimal color, checked as it is compiled

A color written as `rgb"…"` is read by the compiler. A digit which is not hexadecimal is a
compile error, and never a color which is silently black.

```scala fuel
compile
frame
  label The last digit is not hexadecimal
  errors show
  code
      val sky = rgb"#abcdeg"
frame
  label Six hexadecimal digits
  highlight #abcdef  read by the compiler
  code
      val sky = rgb"#abcdef"
##
```

[Hex and the terminal](../../soundness/modules/colors.md#hex-and-the-terminal) shows the color
on a terminal, and [safety by construction](../../philosophy/safety-by-construction.md) has
the case for literals like this.

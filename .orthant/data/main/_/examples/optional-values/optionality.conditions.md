# A value which counts only if it passes

A condition turns a plain value into an optional one. `unless` discards a value which
satisfies a predicate, and `only` keeps the result of a partial function where it is defined,
so the test and the absence it implies are one expression.

```scala fuel
compile
frame
  label A test, and a separate absence
  code
      val lines = 0
      val length: Optional[Int] = if lines == 0 then Unset else lines
frame
  label Discarded when a predicate holds
  highlight unless(_..0)  zero lines is no length at all
  code
      val lines = 0
      val length: Optional[Int] = lines.unless(_ == 0)
frame
  label Kept, and changed, where a pattern matches
  highlight only  Unset wherever the case is not defined
  code
      val lines = 0
      val length: Optional[Int] = lines.unless(_ == 0)
      val pages: Optional[Int] = lines.only { case count if count > 0 => count/40 + 1 }
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(length.inspect)
Out.println(pages.inspect)
```

[Building an optional](../../soundness/modules/optional-values.md#building-an-optional) has
`puncture`, for a single value which stands for absence.

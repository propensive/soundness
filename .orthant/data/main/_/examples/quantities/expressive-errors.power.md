# A length is not an area

Two quantities of the same dimension, raised to different powers, are different things. A
comparison between a length and an area is refused, and the refusal is worded as the mistake
it is.

```scala fuel
compile
frame
  label A length compared with an area
  errors show
  code
      val fits = 7*Metre >= 2*Metre*Metre
frame
  label An area compared with an area
  highlight 7*Metre*Metre  both in square meters
  code
      val fits = 7*Metre*Metre >= 2*Metre*Metre
frame
  label In different units, which are converted
  highlight 60*Foot*Foot  square feet, against square meters
  code
      val fits = 60*Foot*Foot >= 2*Metre*Metre
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(fits.inspect)
```

[Comparing](../../soundness/modules/quantities.md#comparing) says how units are converted in a
comparison, and [expressive errors](../../philosophy/expressive-errors.md) why the message is
worded so.

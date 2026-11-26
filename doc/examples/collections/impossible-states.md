# A set has no order to reverse

A `Set` holds each element once, in no order. The operations which need an order are absent
from it, and are not defined to do something arbitrary.

```scala fuel
compile
frame
  label A set, reversed
  errors show
  code
      val witches = Set(t"First", t"Second", t"Third")
      val backwards = witches.reverse
frame
  label A list, reversed
  highlight List  has an order
  code
      val witches = List(t"First", t"Second", t"Third")
      val backwards = witches.reverse
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(backwards.inspect)
```

[Duplicates and order](../../soundness/modules/collections.md#duplicates-and-order) says which
operations a set lacks.

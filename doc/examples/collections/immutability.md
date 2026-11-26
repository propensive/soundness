# A map with one more entry

Every collection is immutable. `define` does not change a map: it gives a new one, and the
map it was called on is as it was for everything else which holds it.

```scala fuel
compile
frame
  label A map
  code
      val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
frame
  label A new map, with a key added
  highlight define  gives a new map
  code
      val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
      val more = ages.define(t"Edgar", 29)
frame
  label And another, with a key removed
  highlight omit  gives another
  code
      val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
      val more = ages.define(t"Edgar", 29)
      val fewer = ages.omit(t"Kent")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(ages.inspect)
Out.println(more.inspect)
Out.println(fewer.inspect)
```

[Sets and maps](../../soundness/modules/collections.md#sets-and-maps) lists the operations,
and [immutability](../../philosophy/immutability.md) says what they buy.

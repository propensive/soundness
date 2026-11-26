# A key which may not be in the map

Looking up a key which a map does not have gives `Unset`. It does not throw, and it does not
give `null`, so the missing case is answered at the lookup.

```scala fuel
compile
frame
  label A lookup, assumed to succeed
  errors show
  code
      val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
      val age: Int = ages.at(t"Edgar")
frame
  label A lookup which may find nothing
  highlight Optional[Int]  Unset for a key which is not there
  code
      val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
      val age: Optional[Int] = ages.at(t"Edgar")
frame
  label A sentence for either case
  highlight lay  a fallback, then a function
  code
      val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
      val age: Optional[Int] = ages.at(t"Edgar")
      val line: Text = age.lay(t"Edgar's age is not known")(t"Edgar is " + _.show)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line)
```

[Sets and maps](../../soundness/modules/collections.md#sets-and-maps) has the other operations
of a map.

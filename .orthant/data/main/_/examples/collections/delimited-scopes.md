# A map filled in place, then frozen

A large map is quicker to fill in place than to grow an entry at a time. `Map.build` lends a
builder to a block, and the builder cannot leave it, so the map is immutable from the moment
the block ends.

```scala fuel
compile
frame
  label Grown one entry at a time
  code
      val court = Map(t"Lear" -> 80).define(t"Kent", 48).define(t"Gloucester", 60)
frame
  label Filled within a block
  highlight map  the builder exists only here
    line 1
  code
      val court = Map.build[Text, Int]: map =>
        map(t"Lear") = 80
        map(t"Kent") = 48
        map(t"Gloucester") = 60
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(court.at(t"Kent").inspect)
```

[Sets and maps](../../soundness/modules/collections.md#sets-and-maps) describes `build`, and
[delimited scopes](../../philosophy/delimited-scopes.md) the pattern.

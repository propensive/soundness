# A map whose keys the compiler has hashed

`Map.fixed` takes keys which are literals, and finds a perfect hash of them as the code
compiles. A map which cannot be given one, such as a map with a key written twice, does not
compile.

```scala fuel
compile
frame
  label A key given twice
  errors show
  code
      val ages = Map.fixed(t"Lear" -> 80, t"Kent" -> 48, t"Lear" -> 81)
frame
  label Each key once
  highlight Map.fixed  hashed as the code compiles
  code
      val ages = Map.fixed(t"Lear" -> 80, t"Kent" -> 48, t"Edgar" -> 28)
      val age = ages.at(t"Edgar")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(age.inspect)
```

[Maps with keys known in advance](../../soundness/modules/collections.md#maps-with-keys-known-in-advance)
says what else is checked.

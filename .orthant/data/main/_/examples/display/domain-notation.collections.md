# A collection whose kind can be seen

`inspect` writes each kind of collection in a form of its own, so the output shows what a
value is without naming its type: a list is in square brackets, a set in braces, and a map
has arrows.

```scala fuel
compile
frame
  label Three collections of the same numbers
  code
      val list: List[Int] = List(1, 2)
      val set: Set[Int] = Set(1, 2)
      val map: Map[Int, Int] = Map(1 -> 2)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(list.inspect)
Out.println(set.inspect)
Out.println(map.inspect)
Out.println(Map[Int, Int]().inspect)
```

The last line is an empty map, which is still not mistaken for an empty list.
[Inspecting](../../soundness/modules/display.md#inspecting) has the other forms.

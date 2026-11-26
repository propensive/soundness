# A binary search which cannot use the wrong order

A binary search is right only if the elements are sorted, and sorted by the order the search
compares with. A `Sorting` is made from a collection and a key, and holds the two together, so
a search of it is always by the key it was sorted by.

```scala fuel
compile
frame
  label Players, in no order
  code
      case class Player(name: Text, age: Int)

      val players = List(Player(t"Orsino", 31), Player(t"Viola", 19))
frame
  label Sorted by age
  highlight Sorting(players)(_.age)  the elements, kept in order of the key
  code
      import sortingAlgorithms.timsort

      case class Player(name: Text, age: Int)

      val players = List(Player(t"Orsino", 31), Player(t"Viola", 19))
      val cast = Sorting(players)(_.age)
frame
  label Searched by age
  highlight find(19)  a binary search by that key
  code
      import sortingAlgorithms.timsort

      case class Player(name: Text, age: Int)

      val players = List(Player(t"Orsino", 31), Player(t"Viola", 19))
      val cast = Sorting(players)(_.age)
      val youngest = cast.find(19).let(_.name)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(youngest.inspect)
```

[Searching what is sorted](../../soundness/modules/collections.md#searching-what-is-sorted) has
`locate` and the bounds.

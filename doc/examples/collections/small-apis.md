# One vocabulary, and every shape gives back its own

`filter` is one operation, provided by a typeclass, and each collection gives back the shape
it was given. Text is traversed as its characters, and gives back text.

```scala fuel
compile
alternative A list
  highlight List[Int]  a list gives a list
  code
      val odd: List[Int] = List(1, 2, 3, 4, 5).filter(_%2 == 1)
alternative A set
  highlight Set[Int]  a set gives a set
  code
      val odd: Set[Int] = Set(1, 2, 3, 4, 5).filter(_%2 == 1)
alternative A lazy chain
  highlight Chain[Int]  still lazy
  code
      val odd: Chain[Int] = Chain.iterate(1)(_ + 1).filter(_%2 == 1)
alternative Text
  highlight Text  characters give text
  code
      val odd: Text = t"12345".filter(_.toInt%2 == 1)
##
```

[Traversing and transforming](../../soundness/modules/collections.md#traversing-and-transforming)
has the other operations, and [small APIs](../../philosophy/small-apis.md) the argument for
one vocabulary.

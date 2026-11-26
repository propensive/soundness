# A query is a predicate

A lookup gives a `Set` of references, and a `Set` already has `filter` and `map`. The query
language of a database is therefore the one the rest of the program is written in, checked in
the same way.

```scala fuel
compile
frame
  label Every box on the shelf
  highlight lookup[Box]
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = Shelf(t"top").store()
      val boxes = List(t"arden", t"athens", t"belmont").map(Box(_).store())
      val stored = boxes.each(shelf.assign(_))

      val found = shelf.lookup[Box]
frame
  label Those whose names begin with an A
  highlight filter(_().name.starts(t"a"))  a reference is applied to give its value
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = Shelf(t"top").store()
      val boxes = List(t"arden", t"athens", t"belmont").map(Box(_).store())
      val stored = boxes.each(shelf.assign(_))

      val found = shelf.lookup[Box].filter(_().name.starts(t"a"))
frame
  label And their names
  highlight map(_().name)
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = Shelf(t"top").store()
      val boxes = List(t"arden", t"athens", t"belmont").map(Box(_).store())
      val stored = boxes.each(shelf.assign(_))

      val found = shelf.lookup[Box].filter(_().name.starts(t"a")).map(_().name)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(found.inspect)
```

[Querying](../../soundness/modules/database.md#querying) says more, and
[Collections](../../soundness/modules/collections.md) has the operations of a `Set`.

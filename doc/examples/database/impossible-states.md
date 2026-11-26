# A relation the schema does not have

The relations between entities are the type of the database. A shelf may hold boxes, and
nothing says that a box may hold a shelf, so assigning one to the other is not a program.

```scala fuel
compile
frame
  label A shelf is assigned to a box
  errors show
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = Shelf(t"top").store()
      val box = Box(t"alpha").store()

      val assigned = box.assign(shelf)
frame
  label A box is assigned to a shelf
  highlight shelf.assign(box)  the relation Shelf -< Box is declared
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = Shelf(t"top").store()
      val box = Box(t"alpha").store()

      val assigned = shelf.assign(box)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shelf.lookup[Box].map(_()).inspect)
```

[Relating](../../soundness/modules/database.md#relating) has the other operations, and
[impossible states](../../philosophy/impossible-states.md) the idea behind them.

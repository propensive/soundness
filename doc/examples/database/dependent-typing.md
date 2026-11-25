# A reference belongs to its database

The type of a reference names the database which made it, as a value: `folio.type`. Two
databases of the same schema therefore have references of different types, and one cannot be
used in the other.

```scala fuel
compile
frame
  label A shelf from another database
  errors show
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      val folio: Database of (Shelf -< Box, Box -< Text) = Database()
      val quarto: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = quarto.store(Shelf(t"top"))
      val box = folio.store(Box(t"alpha"))
      val assigned = folio.assign(shelf, box)
frame
  label Both from the same database
  highlight folio  a Ref of Shelf in folio.type
    line 9
  code
      import strategies.throwUnsafely

      case class Shelf(name: Text)
      case class Box(name: Text)

      val folio: Database of (Shelf -< Box, Box -< Text) = Database()
      val quarto: Database of (Shelf -< Box, Box -< Text) = Database()

      val shelf = folio.store(Shelf(t"top"))
      val box = folio.store(Box(t"alpha"))
      val assigned = folio.assign(shelf, box)
##
```

[Storing and referencing](../../soundness/modules/database.md#storing-and-referencing) shows
the usual form, with one database as a `given`.

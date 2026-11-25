# A schema which reads as a sentence

A database is declared *of* its relations. `Cabinet -< Shelf` says that a cabinet may have
shelves, and `Box >- Shelf` says the same of shelves and boxes, from the box's side.

```scala fuel
compile
frame
  label One to many
  highlight -<  a cabinet may have shelves
    occurrence 1
  code
      import strategies.throwUnsafely

      case class Cabinet(name: Text)
      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Cabinet -< Shelf, Shelf -< Box) = Database()

      val shelf = Shelf(t"top").store()
      val assigned = shelf.assign(Box(t"alpha").store())
frame
  label Many to one
  highlight >-  a box belongs to a shelf
  code
      import strategies.throwUnsafely

      case class Cabinet(name: Text)
      case class Shelf(name: Text)
      case class Box(name: Text)

      given db: Database of (Cabinet -< Shelf, Box >- Shelf) = Database()

      val shelf = Shelf(t"top").store()
      val assigned = shelf.assign(Box(t"alpha").store())
##
```

The two declarations are the same type, so the assignment beneath them is unchanged.
[Declaring a database](../../soundness/modules/database.md#declaring-a-database) begins the
tutorial, and [infix types](../../philosophy/infix-types.md) explains the words.

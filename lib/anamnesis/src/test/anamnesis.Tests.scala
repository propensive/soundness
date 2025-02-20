package anamnesis

import soundness.*

import strategies.throwUnsafely

case class Cabinet(name: Text)
case class Shelf(name: Text)
case class Box(name: Text)
case class Pencil(name: Text)

val top = Shelf(t"top")
val middle = Shelf(t"middle")
val bottom = Shelf(t"bottom")

val red = Pencil(t"red")
val green = Pencil(t"green")
val blue = Pencil(t"blue")

object Tests extends Suite(t"Anamnesis tests"):
  def run(): Unit =
    given db: Database of (
           Cabinet -< Shelf,
                      Shelf -< Box,
                               Box -< Pencil,
                       Text >- Box) = Database()

    val alpha: Ref of Box in db.type = Box(t"Alpha").store()
    val beta: Ref of Box in db.type = Box(t"Beta").store()

    test(t"Database is initally empty"):
      alpha.lookup[Pencil]

    . assert(_ == Set())

    test(t"Can add an item"):
      red.store()
      alpha.assign(red.ref())

    . assert()

    test(t"Table now contains item"):
      alpha.lookup[Pencil].map(_())

    . assert(_ == Set(red))

    test(t"Can insert multiple items"):
      val greenRef = green.store()
      alpha.assign(greenRef)
      alpha.lookup[Pencil].map(_())

    . assert(_ == Set(red, green))

    test(t"Can delete an item"):
      val redRef = red.store()
      alpha.unassign(redRef)
      alpha.lookup[Pencil].map(_())

    . assert(_ == Set(green))

    test(t"Other values are unaffected"):
      beta.lookup[Pencil]

    . assert(_ == Set())

    test(t"Can't insert a pencil onto a shelf"):
      demilitarize:
        top.store().assign(red.ref())

    . assert(_.nonEmpty)

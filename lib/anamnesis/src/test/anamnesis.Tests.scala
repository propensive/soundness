package anamnesis

import soundness.*

case class Cabinet(name: Text)
case class Shelf(name: Text)
case class Box(name: Text)
case class Pencil(name: Text)

val top = Shelf(t"top")
val middle = Shelf(t"middle")
val bottom = Shelf(t"bottom")

val alpha = Box(t"Alpha")
val beta = Box(t"Beta")

val red = Pencil(t"red")
val green = Pencil(t"green")
val blue = Pencil(t"blue")

object Tests extends Suite(t"Anamnesis tests"):
  def run(): Unit =
    given Database of (
           Cabinet -< Shelf,
                      Shelf -< Box,
                               Box -< Pencil,
                       Text >- Box) = Database()

    test(t"Database is initally empty"):
      alpha.lookup[Pencil]

    . assert(_ == Set())

    test(t"Can add an item"):
      alpha.assign(red)

    . assert()

    test(t"Table now contains item"):
      alpha.lookup[Pencil]

    . assert(_ == Set(red))

    test(t"Can insert multiple items"):
      alpha.assign(green)
      alpha.lookup[Pencil]

    . assert(_ == Set(red, green))

    test(t"Can delete an item"):
      alpha.unassign(red)
      alpha.lookup[Pencil]

    . assert(_ == Set(green))

    test(t"Other values are unaffected"):
      beta.lookup[Pencil]

    . assert(_ == Set())


    test(t"Can't insert a pencil onto a shelf"):
      demilitarize:
        top.assign(red)

    . assert(_.nonEmpty)

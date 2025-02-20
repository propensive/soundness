package anamnesis

import soundness.*

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
    given Database of (Shelf -< Box, Box -< Pencil) = Database()

    test(t"Database is initally empty"):
      alpha.select[Pencil]

    . assert(_ == Set())

    test(t"Can add an item"):
      alpha.insert(red)

    . assert()

    test(t"Table now contains item"):
      alpha.select[Pencil]

    . assert(_ == Set(red))

    test(t"Can insert multiple items"):
      alpha.insert(green)
      alpha.select[Pencil]

    . assert(_ == Set(red, green))

    test(t"Can delete an item"):
      alpha.delete(red)
      alpha.select[Pencil]

    . assert(_ == Set(green))

    test(t"Other values are unaffected"):
      beta.select[Pencil]

    . assert(_ == Set())


    test(t"Can't insert a pencil onto a shelf"):
      demilitarize:
        top.insert(red)

    . assert(_.nonEmpty)

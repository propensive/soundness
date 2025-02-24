package satirical

import soundness.*

import charEncoders.utf8

object Tests extends Suite(t"Satirical tests"):
  def run(): Unit =
    test(t"Parse empty world"):
      t"world example-world {   }".read[Wit]
    . assert(_ == Wit(World(w"example-world", Nil)))

    test(t"Parse empty interface"):
      t"interface example-interface {   }".read[Wit]
    . assert(_ == Wit(Interface(w"example-interface", Nil)))

    test(t"Parse empty interface ignoring comment"):
      t"// a comment\n // another\ninterface example-interface {   } ".read[Wit].tap(println)
    . assert(_ == Wit(Interface(w"example-interface", Nil)))

    test(t"Parse empty world and interface"):
      t"interface eg-int {   }\nworld eg-world {}".read[Wit]
    . assert(_ == Wit(Interface(w"eg-int", Nil), World(w"eg-world", Nil)))

    test(t"Parse package"):
      t"package namespace:name;\ninterface eg-int {   }\nworld eg-world {}".read[Wit]
    . assert(_ == Wit(Interface(w"eg-int", Nil), World(w"eg-world", Nil)))

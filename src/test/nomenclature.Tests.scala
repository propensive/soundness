package nomenclature

import contingency.*, strategies.throwUnsafely
import fulminate.*, errorDiagnostics.stackTraces
import gossamer.*
import prepositional.*
import probably.*
import spectacular.*

erased trait Id
erased trait Id2

object Tests extends Suite(t"Nomenclature tests"):
  def run(): Unit =
    given Id is Nominative under MustEnd["!"] & MustNotStart["0"] & MustNotContain["."] as id =
      Nominative()

    given Id2 is Nominative under MustNotEqual["."] & MustNotEqual[".."] as id2 = Nominative()

    test(t"Create a successful new name"):
      Name[Id](t"hello!")
    .assert(_.text == t"hello!")
    
    test(t"Create a successful new name with inference"):
      val name: Name[Id] = Name(t"hello!")
      name
    .assert(_.text == t"hello!")
    
    test(t"Name must not start with 0"):
      capture[NameError](Name[Id](t"0hello!")).message.show
    .assert(_ == t"the name 0hello! is not valid because it must not start with 0")
      
    test(t"Name must end with !"):
      capture[NameError](Name[Id](t"hello!9")).message.show
    .assert(_ == t"the name hello!9 is not valid because it must end with !")
      
    test(t"Name must not contain ."):
      capture[NameError](Name[Id](t"hello.world!")).message.show
    .assert(_ == t"the name hello.world! is not valid because it must not contain .")
    
    test(t"Name must not equal ."):
      capture[NameError](Name[Id2](t".")).message.show
    .assert(_ == t"the name . is not valid because it must not equal .")
      
    test(t"Name must not equal .."):
      capture[NameError](Name[Id2](t"..")).message.show
    .assert(_ == t"the name .. is not valid because it must not equal ..")
    
    test(t"Construct a new name at compiletime"):
      n"hello": Name[Id2]
    .assert(_.text == t"hello")
    

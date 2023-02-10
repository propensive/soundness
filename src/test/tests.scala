package polyvinyl

import rudiments.*
import gossamer.*
import probably.*

object Tests extends Suite(t"Polyvinyl tests"):
  
  def run(): Unit =

    val data1 = Map(
      "name" -> "Jack",
      "age"  -> 25
    )
    
    val data2 = Map(
      "name" -> "Jill",
      "age"  -> 28
    )
    
    val data3 = Map(
      "city" -> "Manchester",
      "houseNo"  -> 2,
      "street"  -> "High Street",
    )

    test(t"Simple schema access"):
      val rec = Person.record(data1.apply)
      rec.name
    .assert(_ == "Jack")
    
    test(t"Simple schema access 2"):
      val rec = Person.record(data2.apply)
      rec.age
    .assert(_ == 28)
    
    test(t"Access with generalized schema definition"):
      val rec = Address.record(data3.apply)
      rec.city
    .assert(_ == "Manchester")
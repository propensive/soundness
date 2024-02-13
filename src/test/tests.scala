package typonym

import probably.*
import gossamer.*
import rudiments.*

object Tests extends Suite(t"Typonym tests"):
  def run(): Unit =
    test(t"Get a set of strings"):
      reify[TypeSet[("one", "two", "three")]]
    .assert(_ == Set("one", "two", "three"))
    
    test(t"Get a list of strings"):
      reify[TypeList[("one", "two", "three")]]
    .assert(_ == List("one", "two", "three"))
    
    test(t"Get a map of strings"):
      reify[TypeMap[((1, "one"), (2, "two"), (3, "three"))]]
    .assert(_ == Map(1 -> "one", 2 -> "two", 3 -> "three"))

    test(t"Get a multimap of strings"):
      reify[TypeMap[((1, TypeList[("one", "un", "ein")]), (2, TypeList[("two", "zwei", "deux")]))]]
    .assert(_ == Map(1 -> List("one", "un", "ein"), 2 -> List("two", "zwei", "deux")))
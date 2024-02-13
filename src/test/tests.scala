/*
    Typonym, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
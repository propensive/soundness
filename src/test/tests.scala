/*
    Polyvinyl, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
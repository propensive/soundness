/*
    Monotonous, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package monotonous

import probably.*
import anticipation.*
import contingency.*
import gossamer.*

import scala.compiletime.*

object Tests extends Suite(t"Monotonous tests"):

  val numbers = IArray[Byte](0, 1, 2, 3, -125, -126, -127, -128, -4, -3, -2, -1)
  val numberList = List[Byte](0, 1, 2, 3, -125, -126, -127, -128, -4, -3, -2, -1)

  def run(): Unit =

    test(t"Serialize to Binary"):
      numbers.serialize[Binary]
    .assert(_ == t"000000000000000100000010000000111000001110000010100000011000000011111100111111011111111011111111")

    test(t"Serialize to Octal"):
      import alphabets.octal.standard
      numbers.serialize[Octal]
    .assert(_ == t"00000402007016024030037477377377")

    test(t"Serialize to Hex"):
      import alphabets.hex.lowerCase
      numbers.serialize[Hex]
    .assert(_ == t"0001020383828180fcfdfeff")

    test(t"Serialize to BASE32"):
      import alphabets.base32.upperCase
      numbers.serialize[Base32]
    .assert(_ == t"AAAQEA4DQKAYB7H57376====")

    test(t"Serialize to BASE64"):
      import alphabets.base64.standard
      numbers.serialize[Base64]
    .assert(_ == t"AAECA4OCgYD8/f7/")

    import errorHandlers.throwUnsafely

    test(t"Deserialize from Binary"):
      import alphabets.binary.standard
      t"000000000000000100000010000000111000001110000010100000011000000011111100111111011111111011111111".deserialize[Binary].to(List)
    .assert(_ == numberList)

    test(t"Deserialize from Octal"):
      import alphabets.octal.standard
      t"00000402007016024030037477377377".deserialize[Octal].to(List)
    .assert(_ == numberList)

    test(t"Deserialize from Hex"):
      import alphabets.hex.lowerCase
      t"0001020383828180fcfdfeff".deserialize[Hex].to(List)
    .assert(_ == numberList)

    test(t"Deserialize from BASE32"):
      import alphabets.base32.upperCase
      t"AAAQEA4DQKAYB7H57376====".deserialize[Base32].to(List)
    .assert(_ == numberList)

    test(t"Deserialize from BASE64"):
      import alphabets.base64.standard
      t"AAECA4OCgYD8/f7/".deserialize[Base64].to(List)
    .assert(_ == numberList)

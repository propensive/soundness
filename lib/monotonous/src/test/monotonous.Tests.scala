                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.33.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package monotonous

import soundness.*

import scala.compiletime.*

import randomization.seeded, randomization.sizes.uniformUpto100000
import charEncoders.ascii
import strategies.throwUnsafely
import alphabets.hex.lowerCase
import errorDiagnostics.stackTraces

given Seed = Seed(1L)

object Tests extends Suite(m"Monotonous tests"):

  val numbers = IArray[Byte](0, 1, 2, 3, -125, -126, -127, -128, -4, -3, -2, -1)
  val numberList = numbers.to(List)

  val allNumbers = IArray.from((0 to 18).map(_.toByte))

  val stream = Stream(Bytes(1), Bytes(2, 3), Bytes(4, 5, 6), Bytes(7, 8, 9, 10),
      Bytes(11, 12, 13, 14, 15), Bytes(16, 17, 18, 19, 20, 21), Bytes(22, 23, 24, 25, 26, 27, 28))

  def run(): Unit = stochastic:

    //suite(m"Streaming tests"):
      // test(m"Streaming BASE32"):
      //   val text: Text = allNumbers.serialize
      //   val shredded = Stream(text.bytes).shred(6, 9).map(_.utf8)
      //   println(shredded.to(List).inspect)
      //   val result = shredded.deserialize.toList
      //   println(result.reduce(_ ++ _).to(List).inspect)
      //   result.reduce(_ ++ _).to(List)
      // .assert(_ == allNumbers.to(List))

      // test(m"Streaming BASE64"):
      //   import strategies.throwUnsafely
      //   import alphabets.base64.standard
      //   stream.

    test(m"Serialize to Binary"):
      import alphabets.binary.standard
      numbers.serialize[Binary]
    .assert(_ == t"000000000000000100000010000000111000001110000010100000011000000011111100111111011111111011111111")

    test(m"Serialize to Octal"):
      import alphabets.octal.standard
      numbers.serialize[Octal]
    .assert(_ == t"00000402007016024030037477377377")

    test(m"Serialize to Hex"):
      import alphabets.hex.lowerCase
      numbers.serialize[Hex]
    .assert(_ == t"0001020383828180fcfdfeff")

    test(m"Serialize to BASE32"):
      import alphabets.base32.upperCase
      numbers.serialize[Base32]
    .assert(_ == t"AAAQEA4DQKAYB7H5737Q====")

    test(m"Serialize to BASE64"):
      import alphabets.base64.standard
      numbers.serialize[Base64]
    .assert(_ == t"AAECA4OCgYD8/f7/")

    import strategies.throwUnsafely

    test(m"Deserialize from Binary"):
      import alphabets.binary.standard
      t"000000000000000100000010000000111000001110000010100000011000000011111100111111011111111011111111".deserialize[Binary].to(List)
    .assert(_ == numberList)

    test(m"Deserialize from Octal"):
      import alphabets.octal.standard
      t"00000402007016024030037477377377".deserialize[Octal].to(List)
    .assert(_ == numberList)

    test(m"Deserialize from Hex"):
      import alphabets.hex.lowerCase
      t"0001020383828180fcfdfeff".deserialize[Hex].to(List)
    .assert(_ == numberList)

    test(m"Deserialize from BASE32"):
      import alphabets.base32.upperCase
      t"AAAQEA4DQKAYB7H5737Q====".deserialize[Base32].to(List)
    .assert(_ == numberList)

    test(m"Deserialize from BASE64"):
      import alphabets.base64.standard
      t"AAECA4OCgYD8/f7/".deserialize[Base64].to(List)
    .assert(_ == numberList)

    test(m"Tolerant BASE32"):
      import alphabets.base32.lowerCase
      t"AAAQEA4DQKAYB7H5737Q====".deserialize[Base32].to(List)
    .assert(_ == numberList)

    test(m"Intolerant BASE32"):
      capture[SerializationError]:
        import alphabets.base32.strictLowerCase
        t"AAAQEA4DQKAYB7H5737Q====".deserialize[Base32].to(List)
    .assert(_ == SerializationError(0, 'A'))

    test(m"Bad character offset"):
      capture[SerializationError]:
        import alphabets.base32.lowerCase
        t"AAAQEA4?DQKAYB7H5737Q====".deserialize[Base32].to(List)
    .assert(_ == SerializationError(7, '?'))

    given Seed = Seed(1L)

    stochastic:
      for i <- 1 to 100 do
        val arb = arbitrary[IArray[Byte]]()
        val arbList = arb.to(List)

        locally:
          import alphabets.base64
          for alphabet <- List(base64.standard, base64.unpadded, base64.url, base64.xml,
              base64.imap, base64.yui, base64.radix64, base64.bcrypt, base64.sasl) do
            test(m"Roundtrip BASE64 tests"):
              given Alphabet[Base64] = alphabet
              arb.serialize[Base64].deserialize[Base64].to(List)
            .assert(_ == arbList)

        locally:
          import alphabets.base32
          for alphabet <- List(base32.strictUpperCase, base32.strictLowerCase, base32.upperCase,
              base32.lowerCase, base32.extendedHexUpperCase, base32.extendedHexLowerCase,
              base32.zBase32, base32.zBase32Unpadded, base32.geohash, base32.wordSafe,
              base32.crockford) do
            test(m"Roundtrip BASE32 tests"):
              given Alphabet[Base32] = alphabet
              arb.serialize[Base32].deserialize[Base32].to(List)
            .assert(_ == arbList)

        locally:
          import alphabets.hex
          for alphabet <- List(hex.strictUpperCase, hex.strictLowerCase, hex.upperCase,
              hex.lowerCase, hex.bioctal) do
            test(m"Roundtrip Hex tests"):
              given Alphabet[Hex] = alphabet
              arb.serialize[Hex].deserialize[Hex].to(List)
            .assert(_ == arbList)

        test(m"Roundtrip Octal tests"):
          import alphabets.octal.standard
          arb.serialize[Octal].deserialize[Octal].to(List)
        .assert(_ == arbList)

        test(m"Roundtrip Quaternary tests"):
          import alphabets.quaternary.dnaNucleotide
          arb.serialize[Quaternary].deserialize[Quaternary].to(List)
        .assert(_ == arbList)

        test(m"Roundtrip Binary tests"):
          import alphabets.binary.standard
          arb.serialize[Binary].deserialize[Binary].to(List)
        .assert(_ == arbList)

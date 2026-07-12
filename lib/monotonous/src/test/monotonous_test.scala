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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import randomization.seededRandomization, randomization.sizes.uniformUpto100000
import errorDiagnostics.stackTracesDiagnostics

given Seed = Seed(1L)

object Tests extends Suite(m"Monotonous tests"):

  val numbers = IArray[Byte](0, 1, 2, 3, -125, -126, -127, -128, -4, -3, -2, -1)
  val numberList = numbers.to(List)

  val allNumbers = IArray.from((0 to 18).map(_.toByte))

  val stream = LazyList(Data(1), Data(2, 3), Data(4, 5, 6), Data(7, 8, 9, 10),
      Data(11, 12, 13, 14, 15), Data(16, 17, 18, 19, 20, 21), Data(22, 23, 24, 25, 26, 27, 28))

  def run(): Unit = stochastic:

    //suite(m"Streaming tests"):
      // test(m"Streaming BASE32"):
      //   val text: Text = allNumbers.serialize
      //   val shredded = LazyList(text.bytes).shred(6, 9).map(_.utf8)
      //   println(shredded.to(List).inspect)
      //   val result = shredded.deserialize.toList
      //   println(result.reduce(_ ++ _).to(List).inspect)
      //   result.reduce(_ ++ _).to(List)
      // .assert(_ == allNumbers.to(List))

      // test(m"Streaming BASE64"):
      //   import strategies.throwUnsafely
      //   import alphabets.base64Standard
      //   stream.

    test(m"Serialize to Binary"):
      import alphabets.binaryStandard
      numbers.serialize[Binary]
    . assert(_ == t"000000000000000100000010000000111000001110000010100000011000000011111100111111011111111011111111")

    test(m"Serialize to Octal"):
      import alphabets.octalStandard
      numbers.serialize[Octal]
    . assert(_ == t"00000402007016024030037477377377")

    test(m"Serialize to Hex"):
      import alphabets.hexLowerCase
      numbers.serialize[Hex]
    . assert(_ == t"0001020383828180fcfdfeff")

    test(m"Serialize to BASE32"):
      import alphabets.base32UpperCase
      numbers.serialize[Base32]
    . assert(_ == t"AAAQEA4DQKAYB7H5737Q====")

    test(m"Serialize to BASE64"):
      import alphabets.base64Standard
      numbers.serialize[Base64]
    . assert(_ == t"AAECA4OCgYD8/f7/")

    import strategies.throwUnsafely

    test(m"Deserialize from Binary"):
      import alphabets.binaryStandard
      t"000000000000000100000010000000111000001110000010100000011000000011111100111111011111111011111111".deserialize[Binary].to(List)
    . assert(_ == numberList)

    test(m"Deserialize from Octal"):
      import alphabets.octalStandard
      t"00000402007016024030037477377377".deserialize[Octal].to(List)
    . assert(_ == numberList)

    test(m"Deserialize from Hex"):
      import alphabets.hexLowerCase
      t"0001020383828180fcfdfeff".deserialize[Hex].to(List)
    . assert(_ == numberList)

    test(m"Deserialize from BASE32"):
      import alphabets.base32UpperCase
      t"AAAQEA4DQKAYB7H5737Q====".deserialize[Base32].to(List)
    . assert(_ == numberList)

    test(m"Deserialize from BASE64"):
      import alphabets.base64Standard
      t"AAECA4OCgYD8/f7/".deserialize[Base64].to(List)
    . assert(_ == numberList)

    test(m"Tolerant BASE32"):
      import alphabets.base32LowerCase
      t"AAAQEA4DQKAYB7H5737Q====".deserialize[Base32].to(List)
    . assert(_ == numberList)

    test(m"Intolerant BASE32"):
      capture[SerializationError]:
        import alphabets.base32StrictLowerCase
        t"AAAQEA4DQKAYB7H5737Q====".deserialize[Base32].to(List)
    . assert(_ == SerializationError(0, 'A'))

    test(m"Bad character offset"):
      capture[SerializationError]:
        import alphabets.base32LowerCase
        t"AAAQEA4?DQKAYB7H5737Q====".deserialize[Base32].to(List)
    . assert(_ == SerializationError(7, '?'))

    given Seed = Seed(1L)

    stochastic:
      for i <- 1 to 100 do
        val arb = arbitrary[IArray[Byte]]()
        val arbList = arb.to(List)

        locally:
          import alphabets.given
          for alphabet <- List(base64Standard, base64Unpadded, base64Url, base64Xml,
              base64Imap, base64Yui, base64Radix64, base64Bcrypt, base64Sasl) do
            test(m"Roundtrip BASE64 tests"):
              given Alphabet[Base64] = alphabet
              arb.serialize[Base64].deserialize[Base64].to(List)
            . assert(_ == arbList)

        locally:
          import alphabets.given
          for alphabet <- List(base32StrictUpperCase, base32StrictLowerCase, base32UpperCase,
              base32LowerCase, base32ExtendedHexUpperCase, base32ExtendedHexLowerCase,
              base32ZBase32, base32ZBase32Unpadded, base32Geohash, base32WordSafe,
              base32Crockford) do
            test(m"Roundtrip BASE32 tests"):
              given Alphabet[Base32] = alphabet
              arb.serialize[Base32].deserialize[Base32].to(List)
            . assert(_ == arbList)

        locally:
          import alphabets.given
          for alphabet <- List(hexStrictUpperCase, hexStrictLowerCase, hexUpperCase,
              hexLowerCase, hexBioctal) do
            test(m"Roundtrip Hex tests"):
              given Alphabet[Hex] = alphabet
              arb.serialize[Hex].deserialize[Hex].to(List)
            . assert(_ == arbList)

        test(m"Roundtrip Octal tests"):
          import alphabets.octalStandard
          arb.serialize[Octal].deserialize[Octal].to(List)
        . assert(_ == arbList)

        test(m"Roundtrip Quaternary tests"):
          import alphabets.quaternaryDnaNucleotide
          arb.serialize[Quaternary].deserialize[Quaternary].to(List)
        . assert(_ == arbList)

        test(m"Roundtrip Binary tests"):
          import alphabets.binaryStandard
          arb.serialize[Binary].deserialize[Binary].to(List)
        . assert(_ == arbList)

    suite(m"Streaming serialization tests"):
      import strategies.throwUnsafely
      import alphabets.hexUpperCase, alphabets.base64Standard

      val payload = Data.fill(100)(_.toByte)

      test(m"hex duct serializes a byte stream"):
        Drain.text(payload.stream.via(summon[Alphabet[Hex]]))
      . assert(_ == payload.serialize[Hex])

      test(m"hex duct deserializes a text stream"):
        Drain.data(payload.serialize[Hex].stream.via(summon[Alphabet[Hex]])).to(List)
      . assert(_ == payload.to(List))

      test(m"base64 duct emits padding at end of stream"):
        Drain.text(Stream(Data(1, 2, 3, 4)).via(summon[Alphabet[Base64]]))
      . assert(_ == Data(1, 2, 3, 4).serialize[Base64])

      test(m"base64 duct roundtrips through both directions"):
        val text = Drain.text(payload.stream.via(summon[Alphabet[Base64]]))
        Drain.data(text.stream.via(summon[Alphabet[Base64]])).to(List)
      . assert(_ == payload.to(List))

      // Multi-window payloads whose lengths straddle group and window
      // boundaries, exercising the unrolled fast path, its cross-window carry
      // and the padded tail. Lengths chosen mod 3 = 0, 1, 2.
      for size <- List(9000, 9001, 9002) do
        val large = Data.fill(size)(index => (index*7).toByte)

        test(m"base64 duct serialization matches whole-value ($size bytes)"):
          Drain.text(large.stream.via(summon[Alphabet[Base64]]))
        . assert(_ == large.serialize[Base64])

        test(m"base64 duct roundtrips a multi-window payload ($size bytes)"):
          val text = Drain.text(large.stream.via(summon[Alphabet[Base64]]))
          Drain.data(text.stream.via(summon[Alphabet[Base64]])).to(List)
        . assert(_ == large.to(List))

// Drains a duct-composed pull endpoint, for the streaming serialization
// tests.
object Drain:
  def text(stream: (Stream[Text] over Credit)^, credit: Int = 7): Text =
    val builder = StringBuilder()

    def recur(): Unit = stream.refill(Credit(credit)) match
      case count: Int =>
        val window = unsafely(stream.window).asInstanceOf[Array[Char]]
        builder.append(String(window, stream.start, count))
        stream.skip(count)
        recur()

      case _ => ()

    recur()
    builder.toString.tt

  def data(stream: (Stream[Data] over Credit)^, credit: Int = 7): Data =
    val target = java.io.ByteArrayOutputStream()

    def recur(): Unit = stream.refill(Credit(credit)) match
      case count: Int =>
        val window = unsafely(stream.window).asInstanceOf[Array[Byte]]
        target.write(window, stream.start, count)
        stream.skip(count)
        recur()

      case _ => ()

    recur()
    target.toByteArray.nn.immutable(using Unsafe)

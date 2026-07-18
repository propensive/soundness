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
package hypotenuse

import soundness.*

object Tests extends Suite(m"Hypotenuse tests"):
  def run(): Unit =
    suite(m"Addition tests"):
      test(m"Construct an unsigned integer"):
        val left: U64 = 123

    suite(m"Bitmap tests"):
      test(m"Check first bit of 10"):
        B64(Data(0, 0, 0, 0, 0, 0, 0, 10)).bit(Prim)
      . assert(_ == false)

      test(m"Check second bit of 6"):
        B64(Data(0, 0, 0, 0, 0, 0, 0, 6)).bit(Sec)
      . assert(_ == true)

      test(m"Check third bit of 6"):
        B64(Data(0, 0, 0, 0, 0, 0, 0, 6)).bit(Ter)
      . assert(_ == true)

      test(m"Check fourth bit of 6"):
        B64(Data(0, 0, 0, 0, 0, 0, 0, 6)).bit(Quat)
      . assert(_ == false)

      test(m"Check first two bits of 6")(B64(Data(0, 0, 0, 0, 0, 0, 0, 6))(Prim thru Sec)).assert(_ == B64(2))
      test(m"Check first three bits of 6")(B64(Data(0, 0, 0, 0, 0, 0, 0, 6))(Prim thru Ter)).assert(_ == B64(6))
      test(m"Check first four bits of 6")(B64(Data(0, 0, 0, 0, 0, 0, 0, 6))(Prim thru Quat)).assert(_ == B64(6))
      test(m"Check middle two bits of 6")(B64(Data(0, 0, 0, 0, 0, 0, 0, 6))(Sec thru Ter)).assert(_ == B64(3))

      test(m"Check middle two bits as subsequent")(B64(Data(0, 0, 0, 0, 0, 0, 0, 6))(Prim.subsequent(2)))
      . assert(_ == B64(3))

      test(m"Check first two bits as preceding")(B64(Data(0, 0, 0, 0, 0, 0, 0, 6))(Ter.preceding(2)))
      . assert(_ == B64(2))


    suite(m"Median tests"):
      test(m"Simple median"):
        Iterable[Double](7, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21).median
      . assert(_ == 7.0)

      test(m"Simple median, different pivot"):
        Iterable(25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7).median
      . assert(_ == 7.0)

      test(m"Simple median, even items"):
        Iterable(25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7, 8).median
      . assert(_ == 7.5)

      test(m"Simple median, even items, different order"):
        Iterable(8, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7).median
      . assert(_ == 7.5)

      test(m"Simple median, even items, different elements"):
        Iterable(10, 125, -1, 124, -2, -3, 123, -4, 122, -5, 121, 9).median
      . assert(_ == 9.5)

      test(m"Median of temperatures"):
        Iterable(Fahrenheit(10), Celsius(35), Celsius(40)).median
      . assert(_ == Celsius(35))

      test(m"Median of even number of temperatures"):
        Iterable(Fahrenheit(40), Celsius(35), Celsius(45), Celsius(80)).median
      . assert(_ == Celsius(40))

      test(m"Median of quantities"):
        Iterable(10*Metre/Second, 30*Metre/Second, 5*Metre/Second, 20*Metre/Second).median
      . assert(_ == 15*Metre/Second)

      // type Height = Quanta[(Feet[1], Inches[1])]

      // test(m"Median of quanta"):
      //   Iterable[Quanta[Height]](Quanta[Height](6, 11), Quanta[Height](5, 10), Quanta[Height](6, 2)).median
      // . assert(_ == Quanta[Height](6, 2))

    suite(m"Power tests"):
      test(m"Short ** beyond Short range does not truncate"):
        (200: Short) ** 2.0
      . assert(_ == 40000.0)

      test(m"Short ** preserves fractional results"):
        (1000: Short) ** 1.5
      . assert(_ == math.pow(1000.0, 1.5))

      test(m"Byte ** beyond Byte range does not truncate"):
        (50: Byte) ** 2.0
      . assert(_ == 2500.0)

      test(m"Int ** preserves fractional results"):
        2 ** 0.5
      . assert(_ == math.sqrt(2.0))

      test(m"Long ** preserves fractional results"):
        2L ** 0.5
      . assert(_ == math.sqrt(2.0))

    suite(m"Bit-pattern formatting"):
      test(m"Byte.hex of -1 is two characters"):
        (-1: Byte).hex
      . assert(_ == t"ff")

      test(m"Byte.hex of 0x80 is two characters"):
        (0x80.toByte).hex
      . assert(_ == t"80")

      test(m"Byte.binary of -1 is eight characters"):
        (-1: Byte).binary
      . assert(_ == t"11111111")

      test(m"Byte.octal of -1 is three characters"):
        (-1: Byte).octal
      . assert(_ == t"377")

      test(m"Short.hex of -1 is four characters"):
        (-1: Short).hex
      . assert(_ == t"ffff")

      test(m"Short.binary of -1 is sixteen characters"):
        (-1: Short).binary
      . assert(_ == t"1111111111111111")

      test(m"Short.octal of -1 is six characters"):
        (-1: Short).octal
      . assert(_ == t"177777")

    suite(m"Signed overflow detection"):
      import arithmeticOptions.overflow.checked
      val co = summon[CheckOverflow]

      test(m"S64.MinValue + S64(-1) overflows"):
        safely(co.addS64(S64(Long.MinValue.bits), S64((-1L).bits)))
      . assert(_.absent)

      test(m"S64.MinValue + S64.MinValue overflows"):
        safely(co.addS64(S64(Long.MinValue.bits), S64(Long.MinValue.bits)))
      . assert(_.absent)

      test(m"S64.MaxValue + S64(1) overflows"):
        safely(co.addS64(S64(Long.MaxValue.bits), S64(1L.bits)))
      . assert(_.absent)

      test(m"S64(5) + S64(3) does not overflow"):
        import strategies.throwUnsafely
        co.addS64(S64(5L.bits), S64(3L.bits)).long
      . assert(_ == 8L)

      test(m"S64(-5) + S64(-3) does not overflow"):
        import strategies.throwUnsafely
        co.addS64(S64((-5L).bits), S64((-3L).bits)).long
      . assert(_ == -8L)

      test(m"S32.MinValue + S32(-1) overflows"):
        safely(co.addS32(S32(Int.MinValue.bits), S32((-1).bits)))
      . assert(_.absent)

      test(m"S32.MaxValue + S32(1) overflows"):
        safely(co.addS32(S32(Int.MaxValue.bits), S32(1.bits)))
      . assert(_.absent)

      test(m"S16.MinValue + S16(-1) overflows"):
        safely(co.addS16(S16(Short.MinValue.bits), S16((-1: Short).bits)))
      . assert(_.absent)

      test(m"S16.MaxValue + S16(1) overflows"):
        safely(co.addS16(S16(Short.MaxValue.bits), S16((1: Short).bits)))
      . assert(_.absent)

      test(m"S8.MinValue + S8(-1) overflows"):
        safely(co.addS8(S8(Byte.MinValue.bits), S8((-1: Byte).bits)))
      . assert(_.absent)

      test(m"S8.MaxValue + S8(1) overflows"):
        safely(co.addS8(S8(Byte.MaxValue.bits), S8((1: Byte).bits)))
      . assert(_.absent)

    suite(m"Floor-mod tests"):
      test(m"Float %% positive divisor with positive dividend"):
        7.0f %% 3.0f
      . assert(_ == 1.0f)

      test(m"Float %% positive divisor with negative dividend"):
        -7.0f %% 3.0f
      . assert(_ == 2.0f)

      test(m"Float %% negative divisor with positive dividend"):
        7.0f %% -3.0f
      . assert(_ == -2.0f)

      test(m"Float %% negative divisor with negative dividend"):
        -7.0f %% -3.0f
      . assert(_ == -1.0f)

      test(m"Float %% with exact zero remainder"):
        6.0f %% -3.0f
      . assert(_ == 0.0f)

      test(m"Double %% positive divisor with positive dividend"):
        7.0 %% 3.0
      . assert(_ == 1.0)

      test(m"Double %% positive divisor with negative dividend"):
        -7.0 %% 3.0
      . assert(_ == 2.0)

      test(m"Double %% negative divisor with positive dividend"):
        7.0 %% -3.0
      . assert(_ == -2.0)

      test(m"Double %% negative divisor with negative dividend"):
        -7.0 %% -3.0
      . assert(_ == -1.0)

      test(m"Double %% with exact zero remainder"):
        6.0 %% -3.0
      . assert(_ == 0.0)

    suite(m"Inequality tests"):
      test(m"1.2 < x < 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ < 1.4)
      . assert(_ == List(1.3))

      test(m"1.2 < x <= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ <= 1.4)
      . assert(_ == List(1.3, 1.4))

      test(m"1.2 <= x < 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 <= _ < 1.4)
      . assert(_ == List(1.2, 1.3))

      test(m"1.2 <= x <= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 <= _ <= 1.4)
      . assert(_ == List(1.2, 1.3, 1.4))

      test(m"2 < x < 4"):
        List(1, 2, 3, 4, 5).filter(2 < _ < 4)
      . assert(_ == List(3))

      test(m"2 < x <= 4"):
        List(1, 2, 3, 4, 5).filter(2 < _ <= 4)
      . assert(_ == List(3, 4))

      test(m"2 <= x < 4"):
        List(1, 2, 3, 4, 5).filter(2 <= _ < 4)
      . assert(_ == List(2, 3))

      test(m"2 <= x <= 4"):
        List(1, 2, 3, 4, 5).filter(2 <= _ <= 4)
      . assert(_ == List(2, 3, 4))

      test(m"2L < x < 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L < _ < 4L)
      . assert(_ == List(3L))

      test(m"2L < x <= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L < _ <= 4L)
      . assert(_ == List(3L, 4L))

      test(m"2L <= x < 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L <= _ < 4L)
      . assert(_ == List(2L, 3L))

      test(m"2L <= x <= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L <= _ <= 4L)
      . assert(_ == List(2L, 3L, 4L))

      test(m"2F < x < 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F < _ < 4F)
      . assert(_ == List(3F))

      test(m"2F < x <= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F < _ <= 4F)
      . assert(_ == List(3F, 4F))

      test(m"2F <= x < 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F <= _ < 4F)
      . assert(_ == List(2F, 3F))

      test(m"2F <= x <= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F <= _ <= 4F)
      . assert(_ == List(2F, 3F, 4F))

      test(m"'2' < x < '4'"):
        List('1', '2', '3', '4', '5').filter('2' < _ < '4')
      . assert(_ == List('3'))

      test(m"'2' < x <= '4'"):
        List('1', '2', '3', '4', '5').filter('2' < _ <= '4')
      . assert(_ == List('3', '4'))

      test(m"'2' <= x < '4'"):
        List('1', '2', '3', '4', '5').filter('2' <= _ < '4')
      . assert(_ == List('2', '3'))

      test(m"'2' <= x <= '4'"):
        List('1', '2', '3', '4', '5').filter('2' <= _ <= '4')
      . assert(_ == List('2', '3', '4'))

      test(m"2.toByte < x < 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte < _ < 4.toByte)
      . assert(_ == List(3.toByte))

      test(m"2.toByte < x <= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte < _ <= 4.toByte)
      . assert(_ == List(3.toByte, 4.toByte))

      test(m"2.toByte <= x < 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte <= _ < 4.toByte)
      . assert(_ == List(2.toByte, 3.toByte))

      test(m"2.toByte <= x <= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte <= _ <= 4.toByte)
      . assert(_ == List(2.toByte, 3.toByte, 4.toByte))

      test(m"2.toShort < x < 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort < _ < 4.toShort)
      . assert(_ == List(3.toShort))

      test(m"2.toShort < x <= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort < _ <= 4.toShort)
      . assert(_ == List(3.toShort, 4.toShort))

      test(m"2.toShort <= x < 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort <= _ < 4.toShort)
      . assert(_ == List(2.toShort, 3.toShort))

      test(m"2.toShort <= x <= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort <= _ <= 4.toShort)
      . assert(_ == List(2.toShort, 3.toShort, 4.toShort))



    //   test(m"1.2 > x > 1.4"):
    //     List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 > _ > 1.2)
    //   .assert(_ == List(1.3))

    //   test(m"1.2 >= x > 1.4"):
    //     List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 >= _ > 1.2)
    //   .assert(_ == List(1.3, 1.4))

    //   test(m"1.2 > x >= 1.4"):
    //     List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 > _ >= 1.2)
    //   .assert(_ == List(1.2, 1.3))

    //   test(m"1.2 >= x >= 1.4"):
    //     List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 >= _ >= 1.2)
    //   .assert(_ == List(1.2, 1.3, 1.4))

    //   test(m"2 > x > 4"):
    //     List(1, 2, 3, 4, 5).filter(4 > _ > 2)
    //   .assert(_ == List(3))

    //   test(m"2 >= x > 4"):
    //     List(1, 2, 3, 4, 5).filter(4 >= _ > 2)
    //   .assert(_ == List(3, 4))

    //   test(m"2 > x >= 4"):
    //     List(1, 2, 3, 4, 5).filter(4 > _ >= 2)
    //   .assert(_ == List(2, 3))

    //   test(m"2 >= x >= 4"):
    //     List(1, 2, 3, 4, 5).filter(4 >= _ >= 2)
    //   .assert(_ == List(2, 3, 4))

    //   test(m"2L > x > 4L"):
    //     List(1L, 2L, 3L, 4L, 5L).filter(4L > _ > 2L)
    //   .assert(_ == List(3L))

    //   test(m"2L >= x > 4L"):
    //     List(1L, 2L, 3L, 4L, 5L).filter(4L >= _ > 2L)
    //   .assert(_ == List(3L, 4L))

    //   test(m"2L > x >= 4L"):
    //     List(1L, 2L, 3L, 4L, 5L).filter(4L > _ >= 2L)
    //   .assert(_ == List(2L, 3L))

    //   test(m"2L >= x >= 4L"):
    //     List(1L, 2L, 3L, 4L, 5L).filter(4L >= _ >= 2L)
    //   .assert(_ == List(2L, 3L, 4L))

    //   test(m"2F > x > 4F"):
    //     List(1F, 2F, 3F, 4F, 5F).filter(4F > _ > 2F)
    //   .assert(_ == List(3F))

    //   test(m"2F >= x > 4F"):
    //     List(1F, 2F, 3F, 4F, 5F).filter(4F >= _ > 2F)
    //   .assert(_ == List(3F, 4F))

    //   test(m"2F > x >= 4F"):
    //     List(1F, 2F, 3F, 4F, 5F).filter(4F > _ >= 2F)
    //   .assert(_ == List(2F, 3F))

    //   test(m"2F >= x >= 4F"):
    //     List(1F, 2F, 3F, 4F, 5F).filter(4F >= _ >= 2F)
    //   .assert(_ == List(2F, 3F, 4F))

    //   test(m"'2' > x > '4'"):
    //     List('1', '2', '3', '4', '5').filter('4' > _ > '2')
    //   .assert(_ == List('3'))

    //   test(m"'2' >= x > '4'"):
    //     List('1', '2', '3', '4', '5').filter('4' >=_ > '2')
    //   .assert(_ == List('3', '4'))

    //   test(m"'2' > x >= '4'"):
    //     List('1', '2', '3', '4', '5').filter('4' > _ >= '2')
    //   .assert(_ == List('2', '3'))

    //   test(m"'2' >= x >= '4'"):
    //     List('1', '2', '3', '4', '5').filter('4' >= _ >= '2')
    //   .assert(_ == List('2', '3', '4'))

    //   test(m"2.toByte > x > 4.toByte"):
    //     List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte > _ > 2.toByte)
    //   .assert(_ == List(3.toByte))

    //   test(m"2.toByte >= x > 4.toByte"):
    //     List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte >= _ > 2.toByte)
    //   .assert(_ == List(3.toByte, 4.toByte))

    //   test(m"2.toByte > x >= 4.toByte"):
    //     List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte > _ >= 2.toByte)
    //   .assert(_ == List(2.toByte, 3.toByte))

    //   test(m"2.toByte >= x >= 4.toByte"):
    //     List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte >= _ >= 2.toByte)
    //   .assert(_ == List(2.toByte, 3.toByte, 4.toByte))

    //   test(m"2.toShort > x > 4.toShort"):
    //     List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort > _ > 2.toShort)
    //   .assert(_ == List(3.toShort))

    //   test(m"2.toShort >= x > 4.toShort"):
    //     List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort >= _ > 2.toShort)
    //   .assert(_ == List(3.toShort, 4.toShort))

    //   test(m"2.toShort > x >= 4.toShort"):
    //     List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort > _ >= 2.toShort)
    //   .assert(_ == List(2.toShort, 3.toShort))

    //   test(m"2.toShort >= x >= 4.toShort"):
    //     List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort >= _ >= 2.toShort)
    //   .assert(_ == List(2.toShort, 3.toShort, 4.toShort))

    suite(m"Decimal tests"):
      import errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely
      import java.math as jm

      def reference(unscaled: Long, scale: Int): jm.BigDecimal =
        jm.BigDecimal(jm.BigInteger.valueOf(unscaled), scale).nn

      def plain(value: jm.BigDecimal): String =
        value.stripTrailingZeros.nn.toPlainString.nn

      test(m"zero renders as 0"):
        Decimal(0L).text
      . assert(_ == t"0")

      test(m"an integer renders without a point"):
        Decimal(1234567890123L).text
      . assert(_ == t"1234567890123")

      test(m"a negative scaled value renders plainly"):
        Decimal(-1234567, 4).text
      . assert(_ == t"-123.4567")

      test(m"a small fraction pads with leading zeros"):
        Decimal(7, 5).text
      . assert(_ == t"0.00007")

      test(m"a negative scale expands with trailing zeros"):
        Decimal(42, -3).text
      . assert(_ == t"42000")

      test(m"trailing zeros are canonically stripped"):
        Decimal(1100, 2).text
      . assert(_ == t"11")

      test(m"a Double converts by shortest representation, not binary expansion"):
        Decimal(0.1).text
      . assert(_ == t"0.1")

      test(m"a large Double converts through its exponent form"):
        Decimal(1.0e21).text
      . assert(_ == t"1000000000000000000000")

      test(m"a tiny Double converts through its negative exponent form"):
        Decimal(2.5e-7).text
      . assert(_ == t"0.00000025")

      test(m"a non-finite Double is refused"):
        capture[DecimalError](Decimal(Double.NaN))
      . assert(_ => true)

      test(m"parsing accepts exponents and signs"):
        Decimal.parse(t"-12.34e+2").text
      . assert(_ == t"-1234")

      test(m"parsing rejects malformed text"):
        capture[DecimalError](Decimal.parse(t"1.2.3"))
      . assert(_ => true)

      test(m"a numeric literal constructs a Decimal"):
        val price: Decimal = 12.99
        price.text
      . assert(_ == t"12.99")

      test(m"comparison agrees with ordering"):
        List(Decimal(1, 1) < Decimal(2, 1), Decimal(-3) < Decimal(2), Decimal(10) < Decimal(2))
      . assert(_ == List(true, true, false))

      test(m"negation and absolute value"):
        List((-Decimal(5, 1)).text, Decimal(-5, 1).abs.text)
      . assert(_ == List(t"-0.5", t"0.5"))

      test(m"addition carries across limbs"):
        (Decimal.parse(t"999999999999999999") + Decimal(1)).text
      . assert(_ == t"1000000000000000000")

      test(m"multiplication of many-digit values"):
        (Decimal.parse(t"123456789012345678901234567890")
            * Decimal.parse(t"987654321098765432109876543210")).text
      . assert(_ == t"121932631137021795226185032733622923332237463801111263526900")

      test(m"division to a scale with HalfEven"):
        Decimal(1).divide(Decimal(3), 10, Decimal.Rounding.HalfEven).text
      . assert(_ == t"0.3333333333")

      test(m"division by zero raises"):
        capture[DivisionError](Decimal(1).divide(Decimal(0L), 2, Decimal.Rounding.HalfUp))
      . assert(_ => true)

      test(m"rounding modes match the JDK"):
        val modes = List
          ( Decimal.Rounding.Up -> jm.RoundingMode.UP,
            Decimal.Rounding.Down -> jm.RoundingMode.DOWN,
            Decimal.Rounding.Ceiling -> jm.RoundingMode.CEILING,
            Decimal.Rounding.Floor -> jm.RoundingMode.FLOOR,
            Decimal.Rounding.HalfUp -> jm.RoundingMode.HALF_UP,
            Decimal.Rounding.HalfDown -> jm.RoundingMode.HALF_DOWN,
            Decimal.Rounding.HalfEven -> jm.RoundingMode.HALF_EVEN )

        val cases = List((7L, 2L), (-7L, 2L), (5L, 2L), (-5L, 2L), (1L, 3L), (-1L, 3L), (25L, 4L))

        modes.flatMap: (mine, java) =>
          cases.map: (a, b) =>
            val ours = Decimal(a).divide(Decimal(b), 0, mine).text.s
            val theirs = plain(reference(a, 0).divide(reference(b, 0), 0, java).nn)
            ours == theirs

      . assert(_.forall(identity))

      test(m"differential arithmetic against BigDecimal"):
        val random = scala.util.Random(742938473L)

        (0 until 400).map: _ =>
          val a = random.nextLong(2000000000000000L) - 1000000000000000L
          val b = random.nextLong(2000000000000000L) - 1000000000000000L
          val sa = random.nextInt(25) - 8
          val sb = random.nextInt(25) - 8
          val left = Decimal(a, sa)
          val right = Decimal(b, sb)
          val jleft = reference(a, sa)
          val jright = reference(b, sb)

          val sums = (left + right).text.s == plain(jleft.add(jright).nn)
          val differences = (left - right).text.s == plain(jleft.subtract(jright).nn)
          val products = (left*right).text.s == plain(jleft.multiply(jright).nn)

          val comparisons =
            Decimal.comparison(left, right).sign == jleft.compareTo(jright).sign

          val quotients = b == 0L ||
            ( left.divide(right, 12, Decimal.Rounding.HalfEven).text.s
                == plain(jleft.divide(jright, 12, jm.RoundingMode.HALF_EVEN).nn) )

          sums && differences && products && comparisons && quotients

      . assert(_.forall(identity))

      test(m"differential many-limb arithmetic against BigDecimal"):
        val random = scala.util.Random(93842711L)

        def bigText(digits: Int): String =
          val builder = StringBuilder()
          builder.append(('1' + random.nextInt(9)).toChar)
          for _ <- 1 until digits do builder.append(('0' + random.nextInt(10)).toChar)
          builder.toString

        (0 until 60).map: _ =>
          val aText = bigText(30 + random.nextInt(40))
          val bText = bigText(10 + random.nextInt(25))
          val left = Decimal.parse(aText.tt)
          val right = Decimal.parse(bText.tt)
          val jleft = jm.BigDecimal(aText).nn
          val jright = jm.BigDecimal(bText).nn

          val products = (left*right).text.s == plain(jleft.multiply(jright).nn)

          val quotients =
            left.divide(right, 15, Decimal.Rounding.HalfEven).text.s
              == plain(jleft.divide(jright, 15, jm.RoundingMode.HALF_EVEN).nn)

          val sums = (left + right).text.s == plain(jleft.add(jright).nn)

          products && quotients && sums

      . assert(_.forall(identity))

      test(m"parse round-trips its own rendering"):
        val random = scala.util.Random(555000111L)

        (0 until 200).map: _ =>
          val value = Decimal(random.nextLong(), random.nextInt(30) - 10)
          Decimal.comparison(Decimal.parse(value.text), value) == 0

      . assert(_.forall(identity))

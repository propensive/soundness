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
┃    Soundness, version 0.54.0.                                                                    ┃
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
        List[Double](7, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21).median
      . assert(_ == 7.0)

      test(m"Simple median, different pivot"):
        List(25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7).median
      . assert(_ == 7.0)

      test(m"Simple median, even items"):
        List(25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7, 8).median
      . assert(_ == 7.5)

      test(m"Simple median, even items, different order"):
        List(8, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21, 7).median
      . assert(_ == 7.5)

      test(m"Simple median, even items, different elements"):
        List(10, 125, -1, 124, -2, -3, 123, -4, 122, -5, 121, 9).median
      . assert(_ == 9.5)

      test(m"Median of temperatures"):
        List(Fahrenheit(10), Celsius(35), Celsius(40)).median
      . assert(_ == Celsius(35))

      test(m"Median of even number of temperatures"):
        List(Fahrenheit(40), Celsius(35), Celsius(45), Celsius(80)).median
      . assert(_ == Celsius(40))

      test(m"Median of quantities"):
        List(10*Metre/Second, 30*Metre/Second, 5*Metre/Second, 20*Metre/Second).median
      . assert(_ == 15*Metre/Second)

      // type Height = Quanta[(Feet[1], Inches[1])]

      // test(m"Median of quanta"):
      //   List[Quanta[Height]](Quanta[Height](6, 11), Quanta[Height](5, 10), Quanta[Height](6, 2)).median
      // . assert(_ == Quanta[Height](6, 2))

    suite(m"Power tests"):
      test(m"Short ** beyond Short range does not truncate"):
        (200: Short) ** 2.0
      . assert(_ == 40000.0)

      test(m"Short ** preserves fractional results"):
        (1000: Short) ** 1.5
      . assert(_ == scala.math.pow(1000.0, 1.5))

      test(m"Byte ** beyond Byte range does not truncate"):
        (50: Byte) ** 2.0
      . assert(_ == 2500.0)

      test(m"Int ** preserves fractional results"):
        2 ** 0.5
      . assert(_ == (2.0).sqrt)

      test(m"Long ** preserves fractional results"):
        2L ** 0.5
      . assert(_ == (2.0).sqrt)

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

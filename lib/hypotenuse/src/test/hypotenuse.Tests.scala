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
┃    Soundness, version 0.41.0.                                                                    ┃
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
        B64(Bytes(0, 0, 0, 0, 0, 0, 0, 10)).bit(Prim)
      .assert(_ == false)

      test(m"Check second bit of 6"):
        B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6)).bit(Sec)
      .assert(_ == true)

      test(m"Check third bit of 6"):
        B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6)).bit(Ter)
      .assert(_ == true)

      test(m"Check fourth bit of 6"):
        B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6)).bit(Quat)
      .assert(_ == false)

      test(m"Check first two bits of 6")(B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6))(Prim thru Sec)).assert(_ == B64(2))
      test(m"Check first three bits of 6")(B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6))(Prim thru Ter)).assert(_ == B64(6))
      test(m"Check first four bits of 6")(B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6))(Prim thru Quat)).assert(_ == B64(6))
      test(m"Check middle two bits of 6")(B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6))(Sec thru Ter)).assert(_ == B64(3))

      test(m"Check middle two bits as subsequent")(B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6))(Prim.subsequent(2)))
      . assert(_ == B64(3))

      test(m"Check first two bits as preceding")(B64(Bytes(0, 0, 0, 0, 0, 0, 0, 6))(Ter.preceding(2)))
      . assert(_ == B64(2))


    suite(m"Inequality tests"):
      test(m"1.2 < x < 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ < 1.4)
      .assert(_ == List(1.3))

      test(m"1.2 < x <= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ <= 1.4)
      .assert(_ == List(1.3, 1.4))

      test(m"1.2 <= x < 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 <= _ < 1.4)
      .assert(_ == List(1.2, 1.3))

      test(m"1.2 <= x <= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 <= _ <= 1.4)
      .assert(_ == List(1.2, 1.3, 1.4))

      test(m"2 < x < 4"):
        List(1, 2, 3, 4, 5).filter(2 < _ < 4)
      .assert(_ == List(3))

      test(m"2 < x <= 4"):
        List(1, 2, 3, 4, 5).filter(2 < _ <= 4)
      .assert(_ == List(3, 4))

      test(m"2 <= x < 4"):
        List(1, 2, 3, 4, 5).filter(2 <= _ < 4)
      .assert(_ == List(2, 3))

      test(m"2 <= x <= 4"):
        List(1, 2, 3, 4, 5).filter(2 <= _ <= 4)
      .assert(_ == List(2, 3, 4))

      test(m"2L < x < 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L < _ < 4L)
      .assert(_ == List(3L))

      test(m"2L < x <= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L < _ <= 4L)
      .assert(_ == List(3L, 4L))

      test(m"2L <= x < 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L <= _ < 4L)
      .assert(_ == List(2L, 3L))

      test(m"2L <= x <= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L <= _ <= 4L)
      .assert(_ == List(2L, 3L, 4L))

      test(m"2F < x < 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F < _ < 4F)
      .assert(_ == List(3F))

      test(m"2F < x <= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F < _ <= 4F)
      .assert(_ == List(3F, 4F))

      test(m"2F <= x < 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F <= _ < 4F)
      .assert(_ == List(2F, 3F))

      test(m"2F <= x <= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F <= _ <= 4F)
      .assert(_ == List(2F, 3F, 4F))

      test(m"'2' < x < '4'"):
        List('1', '2', '3', '4', '5').filter('2' < _ < '4')
      .assert(_ == List('3'))

      test(m"'2' < x <= '4'"):
        List('1', '2', '3', '4', '5').filter('2' < _ <= '4')
      .assert(_ == List('3', '4'))

      test(m"'2' <= x < '4'"):
        List('1', '2', '3', '4', '5').filter('2' <= _ < '4')
      .assert(_ == List('2', '3'))

      test(m"'2' <= x <= '4'"):
        List('1', '2', '3', '4', '5').filter('2' <= _ <= '4')
      .assert(_ == List('2', '3', '4'))

      test(m"2.toByte < x < 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte < _ < 4.toByte)
      .assert(_ == List(3.toByte))

      test(m"2.toByte < x <= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte < _ <= 4.toByte)
      .assert(_ == List(3.toByte, 4.toByte))

      test(m"2.toByte <= x < 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte <= _ < 4.toByte)
      .assert(_ == List(2.toByte, 3.toByte))

      test(m"2.toByte <= x <= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte <= _ <= 4.toByte)
      .assert(_ == List(2.toByte, 3.toByte, 4.toByte))

      test(m"2.toShort < x < 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort < _ < 4.toShort)
      .assert(_ == List(3.toShort))

      test(m"2.toShort < x <= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort < _ <= 4.toShort)
      .assert(_ == List(3.toShort, 4.toShort))

      test(m"2.toShort <= x < 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort <= _ < 4.toShort)
      .assert(_ == List(2.toShort, 3.toShort))

      test(m"2.toShort <= x <= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort <= _ <= 4.toShort)
      .assert(_ == List(2.toShort, 3.toShort, 4.toShort))



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

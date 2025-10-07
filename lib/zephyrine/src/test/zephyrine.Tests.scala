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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package zephyrine

import soundness.*

import randomization.unseeded

object Tests extends Suite(m"Zephyrine tests"):
  val bytes = Bytes.fill(1000)(_.toByte)
  def run(): Unit = stochastic:
    for i <- 1 to 10 do
      val stream = Stream(bytes).shred(10.0, 10.0)//.filter(!_.isEmpty)
      test(m"Conduit always starts at first byte"):
        val conduit = Conduit(stream)
        conduit.datum

      . assert(_ == 0.toByte)

      test(m"Conduit second byte is always 1"):
        val conduit = Conduit(stream)
        conduit.next()
        conduit.datum

      . assert(_ == 1.toByte)

      test(m"Can capture first ten bytes"):
        val conduit = Conduit(stream)
        conduit.take(10)

      . assert(_ === Bytes(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

      test(m"Can capture second ten bytes"):
        val conduit = Conduit(stream)
        conduit.skip(10)
        conduit.take(10)

      . assert(_ === Bytes(10, 11, 12, 13, 14, 15, 16, 17, 18, 19))

      test(m"Next ten times reaches same datum"):
        val conduit = Conduit(stream)
        for i <- 1 to 10 do conduit.next()
        conduit.datum

      . assert(_ == 10.toByte)

      test(m"Position on next after save"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        conduit.mark()
        conduit.skip(10)
        (conduit.save().last, conduit.datum)

      . assert(_ == (24.toByte, 25.toByte))

      test(m"Position on next after take"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        (conduit.take(10).last, conduit.datum)

      . assert(_ == (24.toByte, 25.toByte))

      test(m"Breaking before starts on consistent datum"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        conduit.truncate()
        conduit.remainder.head.head

      . assert(_ == 15.toByte)

      test(m"Breaking after starts on consistent datum"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        conduit.break()
        conduit.remainder.head.head

      . assert(_ == 16.toByte)

    suite(m"Search tests"):
      test(m"Can find first byte"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x10)

      . assert(_ == true)

      test(m"Can't find nonexistent byte"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x18)

      . assert(_ == false)

      test(m"Can find sequence of two bytes"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x11, 0x12)

      . assert(_ == true)

      test(m"Can find sequence of two bytes"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x11, 0x12)

      . assert(_ == true)

      test(m"Gets correct offset for sequence of two bytes"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x11, 0x12)
        conduit.ordinal

      . assert(_ == Sec)

      test(m"Gets correct offset for sequence of three bytes when they're at the start"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x10, 0x11, 0x12)
        conduit.ordinal

      . assert(_ == Prim)

      test(m"Finds sequence that crosses block boundary"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x13, 0x14)
        conduit.ordinal

      . assert(_ == Quat)

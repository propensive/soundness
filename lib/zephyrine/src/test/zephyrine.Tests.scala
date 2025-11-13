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
┃    Soundness, version 0.46.0.                                                                    ┃
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

import autopsies.contrastExpectations

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


    suite(m"Cursor tests"):
      test(m"Iterate over elements"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        while cursor.next()
        do builder.append(cursor.datum)
        builder.toString
      . assert(_ == "Hello world!")
      
      test(m"Capture part of first block"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        for i <- 1 to 2 do cursor.next()
        cursor.retain:
          val mark = cursor.mark
          for i <- 1 to 2 do cursor.next()
          cursor.extract(mark, cursor.mark)(builder.append(_))
        builder.toString
      . assert(_ == "ell")

      test(m"Capture spanning block"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        for i <- 1 to 3 do cursor.next()
        cursor.retain:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.extract(mark, cursor.mark)(builder.append(_))
        builder.toString
      . assert(_ == "llo ")

      test(m"Capture multiply-spanning block"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        for i <- 1 to 4 do cursor.next()
        cursor.retain:
          val mark = cursor.mark
          for i <- 1 to 4 do cursor.next()
          cursor.extract(mark, cursor.mark)(builder.append(_))
        builder.toString
      . assert(_ == "lo wo")
      
      test(m"Capture multiply-spanning block with nesting"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        for i <- 1 to 4 do cursor.next()
        cursor.retain:
          val mark1 = cursor.mark
          for i <- 1 to 2 do cursor.next()
          val mark2 = cursor.mark
          cursor.extract(mark1, mark2)(builder.append(_))
          for i <- 1 to 2 do cursor.next()
        builder.toString
      . assert(_ == "lo ")
      
      test(m"Capture multiply-spanning block with nesting 2"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        for i <- 1 to 4 do cursor.next()
        cursor.retain:
          val mark1 = cursor.mark
          for i <- 1 to 2 do cursor.next()
          val mark2 = cursor.mark
          cursor.extract(mark1, mark2)(builder.append(_))
          for i <- 1 to 2 do cursor.next()
          cursor.extract(mark1, cursor.mark)(builder.append(_))
        builder.toString
      . assert(_ == "lo lo wo")
      
      test(m"Rewinding"):
        val cursor = Cursor(Iterator[Text]("01234", "5", "6789a", "bc"))
        val builder = StringBuilder()
        for i <- 1 to 4 do cursor.next()
        cursor.retain:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.goto(mark)
        cursor.datum
      . assert(_ == '3')

      test(m"Rewinding and continuing"):
        val cursor = Cursor(Iterator[Text]("01234", "5", "6789a", "bc"))
        val builder = StringBuilder()
        for i <- 1 to 4 do cursor.next()
        cursor.retain:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.goto(mark)
        cursor.next()
        cursor.datum
      . assert(_ == '4')
      
      test(m"Rewinding and continuing to next block"):
        val cursor = Cursor(Iterator[Text]("01234", "5", "6789a", "bc"))
        val builder = StringBuilder()
        for i <- 1 to 4 do cursor.next()
        cursor.retain:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.goto(mark)
        for i <- 1 to 2 do cursor.next()
        cursor.datum
      . assert(_ == '5')

      test(m"Capture from start to end"):
        val cursor = Cursor(Iterator[Text]("Hello", " ", "world", "!"))
        val builder = StringBuilder()
        var mark2: Cursor.Mark = Cursor.Mark.Initial
        if cursor.next()
        then cursor.retain:
          val mark = cursor.mark
          while cursor.step(mark2 = _) do ()
          cursor.extract(mark, mark2)(builder.append(_))
        builder.toString
      . assert(_ == "Hello world!")

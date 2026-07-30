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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package hallucination

import scala.collection.mutable as scm

// Median-cut colour quantization for GIF encoding: recursively splits the box with the largest
// channel range at its weighted median until the palette fits, then represents each box by its
// weighted average colour.
private[hallucination] object Quantization:
  // Reduces the counted colours to at most `limit`, returning the palette and the assignment
  // of every original colour to its palette index.
  def apply(counts: scm.HashMap[Int, Int], limit: Int): (IArray[Int], scm.HashMap[Int, Int]) =
    val assignment = scm.HashMap[Int, Int]()

    if counts.size <= limit then
      val palette = IArray.from(counts.keys)

      palette.indices.foreach: index => assignment(palette(index)) = index

      (palette, assignment)
    else
      val boxes = scm.ArrayBuffer[Array[Int]](counts.keys.toArray)

      def channel(color: Int, shift: Int): Int = (color >> shift)&0xff

      def range(colors: Array[Int]): (Int, Int) =
        var best = 0
        var bestShift = 16

        for shift <- List(16, 8, 0) do
          var minimum = 255
          var maximum = 0

          colors.foreach: color =>
            val value = channel(color, shift)
            if value < minimum then minimum = value
            if value > maximum then maximum = value

          if maximum - minimum > best then
            best = maximum - minimum
            bestShift = shift

        (best, bestShift)

      while boxes.length < limit do
        // Split the box with the widest channel range; stop when every box is a single colour.
        var candidate = -1
        var widest = 0
        var shift = 16

        for index <- boxes.indices do
          val (spread, spreadShift) = range(boxes(index))

          if spread > widest then
            widest = spread
            shift = spreadShift
            candidate = index

        if candidate == -1 then boxes += Array()
        else
          val sorted = boxes(candidate).sortBy(channel(_, shift))

          var total = 0L

          sorted.foreach: color => total += counts(color)

          var cumulative = 0L
          var split = 0

          while cumulative < total/2 && split < sorted.length - 1 do
            cumulative += counts(sorted(split))
            split += 1

          boxes(candidate) = sorted.take(split.max(1))
          boxes += sorted.drop(split.max(1))

      val palette = IArray.tabulate(boxes.length): index =>
        val colors = boxes(index)

        if colors.isEmpty then 0 else
          var red = 0L
          var green = 0L
          var blue = 0L
          var weight = 0L

          colors.foreach: color =>
            val count = counts(color)
            red += channel(color, 16).toLong*count
            green += channel(color, 8).toLong*count
            blue += channel(color, 0).toLong*count
            weight += count
            assignment(color) = index

          ((red/weight) << 16 | (green/weight) << 8 | blue/weight).toInt

      (palette, assignment)

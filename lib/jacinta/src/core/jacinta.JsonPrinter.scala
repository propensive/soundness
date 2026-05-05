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
package jacinta

import scala.compiletime.*

import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.*

object JsonPrinter:
  def print(json: JsonAst, indentation: Boolean): Text = Text.build:
    def appendString(string: String): Unit =
      append('"')

      string.each:
        case '\"' => append(t"\\\"")
        case '\t' => append(t"\\t")
        case '\n' => append(t"\\n")
        case '\r' => append(t"\\r")
        case '\\' => append(t"\\\\")
        case '\f' => append(t"\\f")
        case ch   => append(ch)
      append('"')

    def printObject(node: IArray[Any], indent: Int): Unit =
      val n = node.length/2
      append('{')
      val last = n - 1

      var index = 0
      while index < n do
        if indentation then
          append('\n')
          for i <- 0 until indent*2 do append(' ')
        appendString(node(index*2).asInstanceOf[String])
        append(':')
        if indentation then append(' ')
        recur(node(index*2 + 1).asInstanceOf[JsonAst], indent + 1)

        if index < last then append(',')
        index += 1

      if indentation then
        append('\n')
        for i <- 0 until indent*2 - 2 do append(' ')
      append('}')

    def printArray(elements: IArray[Any], indent: Int): Unit =
      // Strip the sentinel pad if present (parity-padded heterogeneous
      // arrays carry one for empty/even-length cases).
      val raw = elements.length
      val n =
        if raw > 0 && (elements(raw - 1).asInstanceOf[AnyRef] eq JsonAst.arrayPad)
        then raw - 1 else raw
      append('[')
      val last = n - 1

      var index = 0
      while index < n do
        if indentation then
          append('\n')
          for i <- 0 until indent*2 do append(' ')

        recur(elements(index).asInstanceOf[JsonAst], indent + 1)
        if index < last then append(',')
        index += 1

      if indentation then
        append('\n')
        for i <- 0 until indent*2 - 2 do append(' ')

      append(']')

    def printNumberArray(nums: Array[Double]): Unit =
      val n = nums.length
      append('[')
      val last = n - 1
      var index = 0
      while index < n do
        val d = nums(index)
        // Render whole-valued numbers without a trailing `.0` so that
        // `[1, 2, 3]` round-trips through parse + print unchanged.
        if d.isWhole && d >= Long.MinValue.toDouble && d <= Long.MaxValue.toDouble
        then append(d.toLong.toString)
        else append(d.toString)
        if index < last then append(',')
        index += 1
      append(']')

    def recur(json: JsonAst, indent: Int): Unit = json.asMatchable match
      case nums: Array[Double] @unchecked =>
        printNumberArray(nums)

      case bcd: Array[Long] @unchecked =>
        // High-precision number — emit the canonical JSON-number text from
        // the BCD nibble stream directly; this preserves all digits the
        // parser saw, in contrast to a `Double.toString` round-trip.
        append(bcd.asInstanceOf[Bcd].text.tt)

      case arr: IArray[Any] @unchecked =>
        // Heterogeneous array or object, distinguished by length parity:
        // even = object (alternating key/value); odd = array (with
        // optional sentinel pad on the end).
        if (arr.length & 1) == 0 then printObject(arr, indent)
        else printArray(arr, indent)

      case long: Long =>
        append(long.toString)

      case double: Double =>
        append(double.toString)

      case string: String =>
        appendString(string)

      case boolean: Boolean =>
        append(boolean.toString)

      case _ =>
        append("null")

    recur(json, 1)

trait JsonPrinter:
  def print(json: JsonAst): Text

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
import merino.*
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

    def recur(json: JsonAst, indent: Int): Unit = json.asMatchable match
      case arr: Array[?] @unchecked =>
        if json.isObject then
          val n = json.objectSize
          append('{')
          val last = n - 1

          var index = 0
          while index < n do
            if indentation then
              append('\n')
              for i <- 0 until indent*2 do append(' ')
            appendString(json.objectKey(index))
            append(':')
            if indentation then append(' ')
            recur(json.objectValue(index), indent + 1)

            if index < last then append(',')
            index += 1

          if indentation then
            append('\n')
            for i <- 0 until indent*2 - 2 do append(' ')
          append('}')
        else
          val n = json.arrayLength
          append('[')
          val last = n - 1

          var index = 0
          while index < n do
            if indentation then
              append('\n')
              for i <- 0 until indent*2 do append(' ')

            recur(json.arrayElement(index), indent + 1)
            if index < last then append(',')
            index += 1

          if indentation then
            append('\n')
            for i <- 0 until indent*2 - 2 do append(' ')

          append(']')

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

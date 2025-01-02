/*
    Jacinta, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package jacinta

import anticipation.*
import gossamer.*
import merino.*
import rudiments.*

import scala.compiletime.*

trait JsonPrinter:
  def print(json: JsonAst): Text

object JsonPrinter:
  def print(json: JsonAst, indentation: Boolean): Text = Text.construct:
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
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            append('{')
            val last = keys.length - 1

            keys.indices.each: index =>
              if indentation then
                append('\n')
                for i <- 0 until indent*2 do append(' ')
              appendString(keys(index))
              append(':')
              if indentation then append(' ')
              recur(values(index), indent + 1)

              if index < last then append(',')

            if indentation then
              append('\n')
              for i <- 0 until indent*2 - 2 do append(' ')
            append('}')


      case array: Array[JsonAst] @unchecked =>
        append('[')
        val last = array.length - 1

        array.indices.each: index =>
          if indentation then
            append('\n')
            for i <- 0 until indent*2 do append(' ')

          recur(array(index), indent + 1)
          if index < last then append(',')

        if indentation then
          append('\n')
          for i <- 0 until indent*2 - 2 do append(' ')

        append(']')

      case long: Long       => append(long.toString)
      case double: Double   => append(double.toString)
      case string: String   => appendString(string)
      case boolean: Boolean => append(boolean.toString)
      case _                => append("null")

    recur(json, 1)

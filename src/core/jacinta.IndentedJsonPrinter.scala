/*
    Jacinta, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*
import anticipation.*
import merino.*
import spectacular.*

import scala.compiletime.*

import language.dynamics
import language.experimental.pureFunctions

// FIXME: Implement this
object IndentedJsonPrinter extends JsonPrinter:
  def print(json: JsonAst): Text = Text.construct:
    def appendString(string: String): Unit =
      string.each:
        case '\t' => append("\\t")
        case '\n' => append("\\n")
        case '\r' => append("\\r")
        case '\\' => append("\\\\")
        case '\f' => append("\\f")
        case ch   => append(ch)

    def recur(json: JsonAst, indent: Int): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            append('{')
            val last = keys.length - 1

            keys.indices.each: index =>
              append('"')
              appendString(keys(index))
              append('"')
              append(':')
              recur(values(index), indent)
              append(if index == last then '}' else ',')

      case array: Array[JsonAst] @unchecked =>
        append('[')
        val last = array.length - 1

        array.indices.each: index =>
          recur(array(index), indent)
          append(if index == last then ']' else ',')

      case long: Long =>
       append(long.toString)

      case double: Double =>
        append(double.toString)

      case string: String =>
        append('"')
        appendString(string)
        append('"')

      case boolean: Boolean =>
        append(boolean.toString)

      case _ =>
        append("null")

    recur(json, 0)

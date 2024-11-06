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

import language.dynamics
import language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import gossamer.*
import merino.*
import rudiments.*
import spectacular.*

object MinimalJsonPrinter extends JsonPrinter:
  def print(json: JsonAst): Text = Text.construct:
    def appendString(str: String): Unit =
      str.each:
        case '\t' => append("\\t")
        case '\n' => append("\\n")
        case '\r' => append("\\r")
        case '\\' => append("\\\\")
        case '\f' => append("\\f")
        case char => append(char)

    def recur(json: JsonAst): Unit = json.asMatchable match
      case (keys, values) => (keys.asMatchable: @unchecked) match
        case keys: Array[String] @unchecked => (values.asMatchable: @unchecked) match
          case values: Array[JsonAst] @unchecked =>
            append('{')
            val last = keys.length - 1

            keys.indices.each: i =>
              append('"')
              appendString(keys(i))
              append('"')
              append(':')
              recur(values(i))
              if i != last then append(',')

            append('}')

      case array: Array[JsonAst] @unchecked =>
        append('[')
        val last = array.length - 1
        array.indices.each: i =>
          recur(array(i))
          append(if i == last then ']' else ',')

      case long: Long =>
       append(long.toString)

      case double: Double =>
        append(double.toString)

      case string: String =>
        append('"')
        string.tt.chars.each:
          case '\n' => append("\\n")
          case '\"' => append("\\\"")
          case '\\' => append("\\\\")
          case '\r' => append("\\r")
          case '\t' => append("\\t")
          case '\b' => append("\\b")
          case '\f' => append("\\f")
          case char => append(char)
        append('"')

      case boolean: Boolean =>
        append(boolean.toString)
      case _ =>
        append("null")

    recur(json)

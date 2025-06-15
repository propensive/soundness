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
┃    Soundness, version 0.34.0.                                                                    ┃
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

import language.dynamics
import language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import contingency.*
import merino.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import JsonError.Reason

export Jacinta.JsonPointer

given showable: (js: JsonPrinter) => JsonAst is Showable = js.print(_)

extension (json: JsonAst)
  inline def isNumber: Boolean = isDouble || isLong || isBigDecimal
  inline def isAbsent: Boolean = json == Unset
  inline def isLong: Boolean = json.isInstanceOf[Long]
  inline def isDouble: Boolean = json.isInstanceOf[Double]
  inline def isBigDecimal: Boolean = json.isInstanceOf[BigDecimal]
  inline def isObject: Boolean = json.isInstanceOf[(?, ?)]
  inline def isString: Boolean = json.isInstanceOf[String]
  inline def isBoolean: Boolean = json.isInstanceOf[Boolean]

  inline def isNull: Boolean = json.asMatchable match
    case v: Null => v == null
    case _       => false

  inline def isArray: Boolean = json.isInstanceOf[Array[?]]

  private def expected(jsonPrimitive: JsonPrimitive): Unit raises JsonError =
    raise(JsonError(if isAbsent then Reason.Absent else Reason.NotType(primitive, jsonPrimitive)))

  def array: IArray[JsonAst] raises JsonError =
    if isArray then json.asInstanceOf[IArray[JsonAst]]
    else expected(JsonPrimitive.Array) yet IArray[JsonAst]()

  def double: Double raises JsonError = json.asMatchable match
    case value: Double     => value
    case value: Long       => value.toDouble
    case value: BigDecimal => value.toDouble
    case _                 => expected(JsonPrimitive.Number) yet 0.0

  def bigDecimal: BigDecimal raises JsonError = json.asMatchable match
    case value: BigDecimal => value
    case value: Long       => BigDecimal(value)
    case value: Double     => BigDecimal(value)
    case _                 => expected(JsonPrimitive.Number) yet BigDecimal(0L)

  def long: Long raises JsonError = json.asMatchable match
    case value: Long       => value
    case value: Double     => value.toLong
    case value: BigDecimal => value.toLong
    case _                 => expected(JsonPrimitive.Number) yet 0L

  def primitive: JsonPrimitive =
    if isNumber then JsonPrimitive.Number
    else if isBoolean then JsonPrimitive.Boolean
    else if isString then JsonPrimitive.String
    else if isObject then JsonPrimitive.Object
    else if isArray then JsonPrimitive.Array
    else JsonPrimitive.Null

  def string: Text raises JsonError =
    if isString then json.asInstanceOf[Text]
    else expected(JsonPrimitive.String) yet "".tt

  def boolean: Boolean raises JsonError =
    if isBoolean then json.asInstanceOf[Boolean]
    else expected(JsonPrimitive.Boolean) yet false

  def obj: (IArray[String], IArray[JsonAst]) raises JsonError =
    if isObject then json.asInstanceOf[(IArray[String], IArray[JsonAst])]
    else expected(JsonPrimitive.Object) yet (IArray[String]() -> IArray[JsonAst]())

  def number: Long | Double | BigDecimal raises JsonError =
    if isLong then long else if isDouble then double else if isBigDecimal then bigDecimal
    else expected(JsonPrimitive.Number) yet 0L

extension [entity: Encodable in Json](value: entity) def json: Json = entity.encode(value)

package jsonPrinters:
  given indented: JsonPrinter = JsonPrinter.print(_, true)
  given minimal: JsonPrinter = JsonPrinter.print(_, false)

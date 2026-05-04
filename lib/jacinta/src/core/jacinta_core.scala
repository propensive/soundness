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

import language.dynamics
import language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import merino.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import JsonError.Reason

given showable: (printer: JsonPrinter) => JsonAst is Showable = printer.print(_)

extension (json: JsonAst)
  inline def isNumber: Boolean = isDouble || isLong || isBcd
  inline def isAbsent: Boolean = json == Unset
  inline def isLong: Boolean = json.isInstanceOf[Long]
  inline def isDouble: Boolean = json.isInstanceOf[Double]

  // High-precision number node (parser fallback path or downstream
  // construction via `JsonAst(Bcd(...))`). Backed by `Array[Long]`, which
  // is `[J` at runtime — distinct from the `[Ljava/lang/Object;` array
  // class used for objects and arrays, so `isInstanceOf[Array[Long]]`
  // does not collide.
  inline def isBcd: Boolean = json.isInstanceOf[Array[Long]]
  inline def isString: Boolean = json.isInstanceOf[String]
  inline def isBoolean: Boolean = json.isInstanceOf[Boolean]
  inline def isNull: Boolean = json.asInstanceOf[AnyRef | Null] == null

  // Objects and arrays share a runtime representation (`IArray[Any]`); they
  // are distinguished by length parity. Even-length is an object (alternating
  // key, value, …); odd-length is an array (with sentinel padding when the
  // logical element count is even).
  //
  // The narrower `Array[AnyRef]` test (rather than `Array[?]`) avoids
  // matching `Array[Long]` BCD nodes, which are also arrays at runtime
  // but in a different (primitive) array class.
  inline def isObject: Boolean =
    json.isInstanceOf[Array[AnyRef]]
    && (json.asInstanceOf[Array[?]].length & 1) == 0

  inline def isArray: Boolean =
    json.isInstanceOf[Array[AnyRef]]
    && (json.asInstanceOf[Array[?]].length & 1) == 1

  private def expected(jsonPrimitive: JsonPrimitive): Unit raises JsonError =
    raise(JsonError(if isAbsent then Reason.Absent else Reason.NotType(primitive, jsonPrimitive)))

  // Returns the user-visible element count of an array node, ignoring the
  // sentinel padding (if present).
  inline def arrayLength: Int = JsonAst.arrayLength(json)

  inline def arrayElement(index: Int): JsonAst =
    json.asInstanceOf[IArray[JsonAst]](index)

  // Returns the number of key/value pairs in an object node.
  inline def objectSize: Int = JsonAst.objectSize(json)

  inline def objectKey(index: Int): String =
    json.asInstanceOf[IArray[Any]](index*2).asInstanceOf[String]

  inline def objectValue(index: Int): JsonAst =
    json.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[JsonAst]

  // Linear scan for a key. Returns the value index (in pair units) or -1.
  def objectIndexOf(key: String): Int =
    val arr = json.asInstanceOf[Array[?]]
    val len = arr.length
    var i = 0
    while i < len do
      if arr(i) == key then return i/2
      i += 2
    -1

  def array: IArray[JsonAst] raises JsonError =
    if isArray then
      val full = json.asInstanceOf[IArray[JsonAst]]
      val n = JsonAst.arrayLength(json)
      if n == full.length then full
      else IArray.tabulate(n)(full(_))
    else expected(JsonPrimitive.Array) yet IArray[JsonAst]()

  def double: Double raises JsonError = json.asMatchable match
    case value: Double            => value
    case value: Long              => value.toDouble
    case value: Array[Long] @unchecked => value.asInstanceOf[Bcd].toDouble
    case _                        => expected(JsonPrimitive.Number) yet 0.0

  def bcd: Bcd raises JsonError = json.asMatchable match
    case value: Array[Long] @unchecked => value.asInstanceOf[Bcd]
    case value: Long                   => Bcd(BigDecimal(value))
    case value: Double                 => Bcd(BigDecimal(value))
    case _                             => expected(JsonPrimitive.Number) yet Bcd(BigDecimal(0L))

  def long: Long raises JsonError = json.asMatchable match
    case value: Long              => value
    case value: Double            => value.toLong
    case value: Array[Long] @unchecked => value.asInstanceOf[Bcd].toLong.or(0L)
    case _                        => expected(JsonPrimitive.Number) yet 0L

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

  // Returns a (keys, values) view over an object node. This *materialises*
  // two new IArrays from the flat alternating layout, so prefer
  // `objectKey`/`objectValue` when you only need a few entries.
  def obj: (IArray[String], IArray[JsonAst]) raises JsonError =
    if !isObject then expected(JsonPrimitive.Object) yet (IArray[String]() -> IArray[JsonAst]())
    else
      val arr = json.asInstanceOf[IArray[Any]]
      val n = arr.length/2
      val keys = new Array[String](n)
      val values = new Array[JsonAst](n)
      var i = 0
      while i < n do
        keys(i) = arr(i*2).asInstanceOf[String]
        values(i) = arr(i*2 + 1).asInstanceOf[JsonAst]
        i += 1
      (keys.asInstanceOf[IArray[String]], values.asInstanceOf[IArray[JsonAst]])

  def number: Long | Double | Bcd raises JsonError =
    if isLong then long else if isDouble then double else if isBcd then bcd
    else expected(JsonPrimitive.Number) yet 0L

extension [entity: Encodable in Json](value: entity) def json: Json = value.encode

extension (inline context: StringContext)
  transparent inline def j: Interpolation = interpolation[Json](context)

package jsonPrinters:
  given indented: JsonPrinter = JsonPrinter.print(_, true)
  given minimal: JsonPrinter = JsonPrinter.print(_, false)

package jsonDiscriminables:
  given discriminatedUnionByType: [value] => value is Discriminable in Json =
    Json.discriminatedUnion[value]("type")

  given discriminatedUnionByKind: [value] => value is Discriminable in Json =
    Json.discriminatedUnion[value]("kind")

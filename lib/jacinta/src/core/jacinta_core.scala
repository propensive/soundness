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
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import JsonError.Reason

given showable: (printer: JsonPrinter) => Json.Ast is Showable = printer.print(_)

extension (json: Json.Ast)
  inline def isNumber: Boolean = isDouble || isLong || isBcd
  inline def isAbsent: Boolean = json == Unset
  inline def isLong: Boolean = json.isInstanceOf[Long]
  inline def isDouble: Boolean = json.isInstanceOf[Double]

  // High-precision number node. `Bcd` is `Array[Long]` (`[J`) at runtime
  // — distinct from `Array[Double]` (`[D`, the number-array node) and
  // `Array[AnyRef]` (`[Ljava/lang/Object;`, used for objects and
  // parity-padded heterogeneous arrays).
  inline def isBcd: Boolean = json.isInstanceOf[Array[Long]]
  inline def isString: Boolean = json.isInstanceOf[String]
  inline def isBoolean: Boolean = json.isInstanceOf[Boolean]
  inline def isNull: Boolean = json.asInstanceOf[AnyRef | Null] == null

  // Objects and heterogeneous arrays share a runtime representation
  // (`IArray[Any]` = `[Ljava/lang/Object;`) and are distinguished by
  // length parity: even = object (alternating `key, value, …`), odd =
  // array (with sentinel padding when the logical element count is even).
  // Number-only arrays are stored unboxed as `Array[Double]` (`[D`),
  // a distinct runtime class.
  inline def isObject: Boolean =
    json.isInstanceOf[Array[AnyRef]]
    && (json.asInstanceOf[Array[?]].length & 1) == 0

  inline def isArray: Boolean =
    json.isInstanceOf[Array[Double]]
    || (json.isInstanceOf[Array[AnyRef]]
        && (json.asInstanceOf[Array[?]].length & 1) == 1)

  // True when the array is in the unboxed number-only form.
  inline def isNumberArray: Boolean = json.isInstanceOf[Array[Double]]

  private def expected(jsonPrimitive: JsonPrimitive): Unit raises JsonError =
    raise(JsonError(if isAbsent then Reason.Absent else Reason.NotType(primitive, jsonPrimitive)))

  // Returns the user-visible element count of an array node (excludes the
  // sentinel pad if present).
  inline def arrayLength: Int = Json.Ast.arrayLength(json)

  // Materialise an array element as a `Json.Ast` node. For a parity-padded
  // heterogeneous array this is a direct indexed lookup; for the unboxed
  // `Array[Double]` form, the raw double is recovered as a `Long` for
  // whole values in Long range (preserving the type the parser would have
  // assigned outside the array context) and as a `Double` otherwise.
  def arrayElement(index: Int): Json.Ast = (json: @unchecked) match
    case nums: Array[Double] @unchecked =>
      val d = nums(index)
      if d.isWhole && d >= Long.MinValue.toDouble && d <= Long.MaxValue.toDouble
      then Json.Ast(d.toLong) else Json.Ast(d)
    case _ =>
      json.asInstanceOf[IArray[Json.Ast]](index)

  // Returns the number of key/value pairs in an object node.
  inline def objectSize: Int = Json.Ast.objectSize(json)

  inline def objectKey(index: Int): String =
    json.asInstanceOf[IArray[Any]](index*2).asInstanceOf[String]

  inline def objectValue(index: Int): Json.Ast =
    json.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[Json.Ast]

  // Linear scan for a key. Returns the value index (in pair units) or -1.
  def objectIndexOf(key: String): Int =
    val arr = json.asInstanceOf[IArray[Any]]
    val len = arr.length
    var i = 0
    while i < len do
      if arr(i) == key then return i/2
      i += 2
    -1

  def array: IArray[Json.Ast] raises JsonError = (json: @unchecked) match
    case nums: Array[Double] @unchecked =>
      IArray.tabulate(nums.length)(json.arrayElement(_))
    case _ =>
      if isArray then
        val full = json.asInstanceOf[IArray[Json.Ast]]
        val n = Json.Ast.arrayLength(json)
        if n == full.length then full
        else IArray.tabulate(n)(full(_))
      else expected(JsonPrimitive.Array) yet IArray[Json.Ast]()

  def double: Double raises JsonError = json.asMatchable match
    case value: Double                 => value
    case value: Long                   => value.toDouble
    case value: Array[Long] @unchecked => value.asInstanceOf[Bcd].toDouble
    case _                             => expected(JsonPrimitive.Number) yet 0.0

  def bcd: Bcd raises JsonError = json.asMatchable match
    case value: Array[Long] @unchecked => value.asInstanceOf[Bcd]
    case value: Long                   => Bcd(BigDecimal(value))
    case value: Double                 => Bcd(BigDecimal(value))
    case _                             => expected(JsonPrimitive.Number) yet Bcd(BigDecimal(0L))

  def long: Long raises JsonError = json.asMatchable match
    case value: Long                   => value
    case value: Double                 => value.toLong
    case value: Array[Long] @unchecked => value.asInstanceOf[Bcd].toLong.or(0L)
    case _                             => expected(JsonPrimitive.Number) yet 0L

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
  def obj: (IArray[String], IArray[Json.Ast]) raises JsonError =
    if !isObject then expected(JsonPrimitive.Object) yet (IArray[String]() -> IArray[Json.Ast]())
    else
      val arr = json.asInstanceOf[IArray[Any]]
      val n = arr.length/2
      val keys = new Array[String](n)
      val values = new Array[Json.Ast](n)
      var i = 0
      while i < n do
        keys(i) = arr(i*2).asInstanceOf[String]
        values(i) = arr(i*2 + 1).asInstanceOf[Json.Ast]
        i += 1
      (keys.asInstanceOf[IArray[String]], values.asInstanceOf[IArray[Json.Ast]])

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

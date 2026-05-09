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
package breviloquence

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*

import CborError.{Primitive, Reason}

extension [entity: Encodable in Cbor](value: entity) def cbor: Cbor = value.encode

extension (cbor: CborAst)
  inline def unset: Boolean = cbor == vacuous.Unset
  inline def isInteger: Boolean = cbor.isInstanceOf[Long]
  inline def isFloat: Boolean = cbor.isInstanceOf[Double]
  inline def isTextString: Boolean = cbor.isInstanceOf[String]
  inline def isBoolean: Boolean = cbor.isInstanceOf[Boolean]
  inline def nullary: Boolean = cbor.asInstanceOf[AnyRef | Null] == null
  inline def isTag: Boolean = cbor.isInstanceOf[CborTag]

  // Byte strings have runtime class `[B`; arrays/maps have `[Ljava/lang/Object;`.
  inline def isByteString: Boolean = cbor.isInstanceOf[Array[Byte]]

  // Maps and arrays share the `Array[AnyRef]` runtime layout. Maps have an
  // even-length backing array; arrays are odd-length (with sentinel padding
  // when the logical element count is even).
  inline def isMap: Boolean =
    cbor.isInstanceOf[Array[AnyRef]] && (cbor.asInstanceOf[Array[?]].length & 1) == 0

  inline def isArray: Boolean =
    cbor.isInstanceOf[Array[AnyRef]] && (cbor.asInstanceOf[Array[?]].length & 1) == 1

  def primitive: Primitive =
    if isInteger then Primitive.Integer
    else if isFloat then Primitive.Float
    else if isTextString then Primitive.TextString
    else if isByteString then Primitive.ByteString
    else if isBoolean then Primitive.Boolean
    else if isMap then Primitive.Map
    else if isArray then Primitive.Array
    else if isTag then Primitive.Tag
    else if unset then Primitive.Undefined
    else Primitive.Null

  private def expected(expected: Primitive): Unit raises CborError =
    if unset then abort(CborError(Reason.Absent))
    else abort(CborError(Reason.NotType(primitive, expected)))

  inline def elements: Int = CborAst.length(cbor)
  inline def entries: Int = CborAst.size(cbor)

  def element(index: Int): CborAst = cbor.asInstanceOf[IArray[CborAst]](index)

  inline def key(index: Int): CborAst = cbor.asInstanceOf[IArray[CborAst]](index*2)
  inline def value(index: Int): CborAst = cbor.asInstanceOf[IArray[CborAst]](index*2 + 1)

  def index(key: String): Int =
    val array = cbor.asInstanceOf[IArray[Any]]
    val count = array.length
    var index = 0

    while index < count do
      if array(index) == key then return index/2
      index += 2

    -1

  def long: Long raises CborError =
    if isInteger then cbor.asInstanceOf[Long] else if isFloat then cbor.asInstanceOf[Double].toLong
    else expected(Primitive.Integer) yet 0L

  def double: Double raises CborError =
    if isFloat then cbor.asInstanceOf[Double]
    else if isInteger then cbor.asInstanceOf[Long].toDouble
    else expected(Primitive.Float) yet 0.0

  def string: String raises CborError =
    if isTextString then cbor.asInstanceOf[String] else expected(Primitive.TextString) yet ""

  def byteString: IArray[Byte] raises CborError =
    if isByteString then cbor.asInstanceOf[IArray[Byte]]
    else expected(Primitive.ByteString) yet IArray.empty[Byte]

  def boolean: Boolean raises CborError =
    if isBoolean then cbor.asInstanceOf[Boolean] else expected(Primitive.Boolean) yet false

  def tag: CborTag raises CborError =
    if isTag then cbor.asInstanceOf[CborTag]
    else expected(Primitive.Tag) yet CborTag(0L, vacuous.Unset)

  def array: IArray[CborAst] raises CborError =
    if isArray then
      val full = cbor.asInstanceOf[IArray[CborAst]]
      val count = elements

      if count == full.length then full else IArray.tabulate(count)(full(_))
    else { expected(Primitive.Array); IArray.empty[CborAst] }

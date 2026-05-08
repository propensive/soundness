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

import CborError.{Primitive, Reason}

extension [entity: Encodable in Cbor](value: entity) def cbor: Cbor = value.encode

extension (cbor: CborAst)
  inline def isAbsent: Boolean = cbor == vacuous.Unset
  inline def isInteger: Boolean = cbor.isInstanceOf[Long]
  inline def isFloat: Boolean = cbor.isInstanceOf[Double]
  inline def isTextString: Boolean = cbor.isInstanceOf[String]
  inline def isBoolean: Boolean = cbor.isInstanceOf[Boolean]
  inline def isNull: Boolean = cbor.asInstanceOf[AnyRef | Null] == null
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
    if isInteger     then Primitive.Integer
    else if isFloat       then Primitive.Float
    else if isTextString  then Primitive.TextString
    else if isByteString  then Primitive.ByteString
    else if isBoolean     then Primitive.Boolean
    else if isMap         then Primitive.Map
    else if isArray       then Primitive.Array
    else if isTag         then Primitive.Tag
    else if isAbsent      then Primitive.Undefined
    else                       Primitive.Null

  private def expected(expected: Primitive): Unit raises CborError =
    if isAbsent then abort(CborError(Reason.Absent))
    else abort(CborError(Reason.NotType(primitive, expected)))

  inline def arrayLength: Int = CborAst.arrayLength(cbor)
  inline def mapSize: Int = CborAst.mapSize(cbor)

  def arrayElement(index: Int): CborAst =
    cbor.asInstanceOf[IArray[CborAst]](index)

  inline def mapKey(index: Int): CborAst =
    cbor.asInstanceOf[IArray[Any]](index*2).asInstanceOf[CborAst]

  inline def mapValue(index: Int): CborAst =
    cbor.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[CborAst]

  // Linear scan for a string key. Returns the pair index (in pair units) or -1.
  def mapIndexOf(key: String): Int =
    val arr = cbor.asInstanceOf[IArray[Any]]
    val n = arr.length
    var i = 0
    while i < n do
      if arr(i) == key then return i/2
      i += 2
    -1

  def long: Long raises CborError =
    if isInteger then cbor.asInstanceOf[Long]
    else if isFloat then cbor.asInstanceOf[Double].toLong
    else { expected(Primitive.Integer); 0L }

  def double: Double raises CborError =
    if isFloat then cbor.asInstanceOf[Double]
    else if isInteger then cbor.asInstanceOf[Long].toDouble
    else { expected(Primitive.Float); 0.0 }

  def string: String raises CborError =
    if isTextString then cbor.asInstanceOf[String]
    else { expected(Primitive.TextString); "" }

  def byteString: IArray[Byte] raises CborError =
    if isByteString then cbor.asInstanceOf[IArray[Byte]]
    else { expected(Primitive.ByteString); IArray.empty[Byte] }

  def boolean: Boolean raises CborError =
    if isBoolean then cbor.asInstanceOf[Boolean]
    else { expected(Primitive.Boolean); false }

  def tag: CborTag raises CborError =
    if isTag then cbor.asInstanceOf[CborTag]
    else { expected(Primitive.Tag); CborTag(0L, vacuous.Unset) }

  def array: IArray[CborAst] raises CborError =
    if isArray then
      val full = cbor.asInstanceOf[IArray[CborAst]]
      val n = arrayLength
      if n == full.length then full
      else IArray.tabulate(n)(full(_))
    else { expected(Primitive.Array); IArray.empty[CborAst] }

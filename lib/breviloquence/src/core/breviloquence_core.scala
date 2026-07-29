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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import proscenium.compat.*

import CborError.{Primitive, Reason}


extension (cbor: Cbor.Ast)
  @unexported
  inline def unset: Boolean = cbor == vacuous.Unset
  @unexported
  inline def isInteger: Boolean = cbor.isInstanceOf[Long]
  @unexported
  inline def isFloat: Boolean = cbor.isInstanceOf[Double]
  @unexported
  inline def isTextString: Boolean = cbor.isInstanceOf[String]
  @unexported
  inline def isBoolean: Boolean = cbor.isInstanceOf[Boolean]
  @unexported
  inline def nullary: Boolean = cbor.asInstanceOf[AnyRef] eq Cbor.CborNull
  @unexported
  inline def isTag: Boolean = cbor.isInstanceOf[Cbor.Tag]

  // Byte strings have runtime class `[B`; arrays/maps have `[Ljava/lang/Object;`.
  @unexported
  inline def isByteString: Boolean = cbor.isInstanceOf[scala.Array[Byte]]

  // Maps and arrays share the `Array[AnyRef]` runtime layout. Maps have an
  // even-length backing array; arrays are odd-length (with sentinel padding
  // when the logical element count is even).
  @unexported
  inline def isMap: Boolean =
    cbor.isInstanceOf[scala.Array[AnyRef]] && (cbor.asInstanceOf[scala.Array[?]].length & 1) == 0

  @unexported
  inline def isArray: Boolean =
    cbor.isInstanceOf[scala.Array[AnyRef]] && (cbor.asInstanceOf[scala.Array[?]].length & 1) == 1

  @unexported
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

  @unexported
  inline def elements: Int = Cbor.Ast.length(cbor)
  @unexported
  inline def entries: Int = Cbor.Ast.size(cbor)

  @unexported
  def element(index: Int): Cbor.Ast = cbor.asInstanceOf[IArray[Cbor.Ast]].stdlib(index)

  @unexported
  inline def key(index: Int): Cbor.Ast = cbor.asInstanceOf[IArray[Cbor.Ast]].stdlib(index*2)
  @unexported
  inline def value(index: Int): Cbor.Ast = cbor.asInstanceOf[IArray[Cbor.Ast]].stdlib(index*2 + 1)

  @unexported
  def index(key: String): Int =
    val array = cbor.asInstanceOf[IArray[Any]]
    val count = array.length
    var index = 0

    while index < count do
      if array(index) == key then return index/2
      index += 2

    -1

  @unexported
  def long: Long raises CborError =
    if isInteger then cbor.asInstanceOf[Long] else if isFloat then cbor.asInstanceOf[Double].toLong
    else expected(Primitive.Integer) yet 0L

  @unexported
  def double: Double raises CborError =
    if isFloat then cbor.asInstanceOf[Double]
    else if isInteger then cbor.asInstanceOf[Long].toDouble
    else expected(Primitive.Float) yet 0.0

  @unexported
  def string: String raises CborError =
    if isTextString then cbor.asInstanceOf[String] else expected(Primitive.TextString) yet ""

  @unexported
  def byteString: IArray[Byte] raises CborError =
    if isByteString then cbor.asInstanceOf[IArray[Byte]]
    else expected(Primitive.ByteString) yet IArray.empty[Byte]

  @unexported
  def boolean: Boolean raises CborError =
    if isBoolean then cbor.asInstanceOf[Boolean] else expected(Primitive.Boolean) yet false

  @unexported
  def tag: Cbor.Tag raises CborError =
    if isTag then cbor.asInstanceOf[Cbor.Tag]
    else expected(Primitive.Tag) yet Cbor.Tag(0L, vacuous.Unset)

  @unexported
  def array: IArray[Cbor.Ast] raises CborError =
    if isArray then
      val full = cbor.asInstanceOf[IArray[Cbor.Ast]]
      val count = elements

      if count == full.length then full else IArray.tabulate(count)(full.stdlib(_))
    else
      expected(Primitive.Array)
      IArray.empty[Cbor.Ast]

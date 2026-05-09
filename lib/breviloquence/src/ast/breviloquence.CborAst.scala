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

object CborAst:
  val Sentinel: AnyRef = new Object

  // CBOR major-type representation in storage:
  //   0/1 (integer)            -> `Long` (overflow raises CborError)
  //   2 (byte string)          -> `IArray[Byte]` (= `anticipation.Data`)
  //   3 (text string)          -> `String`
  //   4 (array)                -> odd-length `IArray[Any]` (sentinel-padded if needed)
  //   5 (map)                  -> even-length `IArray[Any]`, alternating key/value
  //   6 (tag)                  -> `CborTag`
  //   7.20–22 (false/true/null) -> `Boolean` / `Null`
  //   7.23 (undefined)         -> `vacuous.Unset`
  //   7.25–27 (floats)         -> `Double`
  opaque type RawCbor =
    Long | Double | String | IArray[Byte] | IArray[Any] | Boolean | Null | vacuous.Unset.type
    | CborTag

  def apply
    ( value
      : Long | Double | String | IArray[Byte] | IArray[Any] | Boolean | Null | vacuous.Unset.type
      | CborTag )
  :   RawCbor =

    value

  def map(keys: IArray[Any], values: IArray[Any]): RawCbor =
    val count = keys.length
    val array = new Array[Any](count*2)
    var index = 0

    while index < count do
      array(index*2) = keys(index)
      array(index*2 + 1) = values(index)
      index += 1

    array.asInstanceOf[IArray[Any]]

  def array(elements: IArray[Any]): RawCbor =
    val count = elements.length

    if (count&1) == 1 then elements else
      val padded = new Array[Any](count + 1)
      System.arraycopy(elements.asInstanceOf[Array[Any]], 0, padded, 0, count)
      padded(count) = Sentinel
      padded.asInstanceOf[IArray[Any]]

  def length(cbor: RawCbor): Int =
    val array = cbor.asInstanceOf[Array[AnyRef]]
    val count = array.length
    if count > 0 && (array(count - 1).asInstanceOf[AnyRef] eq Sentinel) then count - 1 else count

  def size(cbor: RawCbor): Int = cbor.asInstanceOf[IArray[Any]].length/2

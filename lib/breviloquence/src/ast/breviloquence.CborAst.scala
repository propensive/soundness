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

  // Sentinel used to pad a heterogeneous array whose logical length is even,
  // so the stored `IArray[Any]` always has odd length and is therefore
  // distinguishable from a map (always even-length, encoded as alternating
  // `key, value, …`). The sentinel only ever appears at the end of a padded
  // array.
  val arrayPad: AnyRef = new Object

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

  // Build a map node from parallel `keys` and `values` arrays. Stored as a
  // single `IArray[Any]` of length `2 * keys.length` with keys at even
  // indices and values at odd indices. Keys may be any CBOR value.
  def map(keys: IArray[Any], values: IArray[Any]): RawCbor =
    val n = keys.length
    val arr = new Array[Any](n*2)
    var i = 0
    while i < n do
      arr(i*2) = keys(i)
      arr(i*2 + 1) = values(i)
      i += 1
    arr.asInstanceOf[IArray[Any]]

  // Build a heterogeneous array node. Adds a single `arrayPad` sentinel if
  // the logical element count is even, so the stored `IArray[Any]` is always
  // odd-length and distinguishable from a map.
  def arr(elements: IArray[Any]): RawCbor =
    val n = elements.length
    if (n & 1) == 1 then elements
    else
      val padded = new Array[Any](n + 1)
      System.arraycopy(elements.asInstanceOf[Array[Any]], 0, padded, 0, n)
      padded(n) = arrayPad
      padded.asInstanceOf[IArray[Any]]

  // Number of user-visible elements in an array node.
  def arrayLength(cbor: RawCbor): Int =
    val arr = cbor.asInstanceOf[Array[?]]
    val n = arr.length
    if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq arrayPad) then n - 1 else n

  // Number of key/value pairs in a map node.
  def mapSize(cbor: RawCbor): Int = cbor.asInstanceOf[IArray[Any]].length/2

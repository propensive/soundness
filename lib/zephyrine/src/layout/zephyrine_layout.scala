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
package zephyrine

import scala.reflect.ClassTag

import anticipation.*
import contingency.*

package byteOrders:
  // Singleton-typed, so that a derivation knows the order as it expands.
  given bigEndianByteOrder: ByteOrder.BigEndian.type = ByteOrder.BigEndian
  given littleEndianByteOrder: ByteOrder.LittleEndian.type = ByteOrder.LittleEndian

extension (data: Data)
  // The record laid out from `offset`, after one check that the whole of it is there.
  def unpack[record: Structural](offset: Int): record raises Structural.Error =
    if offset < 0 || offset > data.length - record.width
    then abort(Structural.Error(offset, record.width, data.length))
    else record.load(data, offset)

  // `count` records laid out one after another from `offset`, as a repeated section is.
  def unpack[record: {Structural, ClassTag}](offset: Int, count: Int)
  :   Array[record]^{} raises Structural.Error =

    val width = record.width

    if offset < 0 || count < 0 || offset > data.length - width.toLong*count
    then abort(Structural.Error(offset, width, data.length))
    else Array.tabulate[record](count): index => record.load(data, offset + index*width)

extension (expanse: Expanse^)
  // The record laid out from `offset` of an expanse, such as a mapped file: its bytes alone
  // are read. The tactic is a parameter, where a `raises` result would capture the expanse.
  def unpack[record: Structural](offset: Long)(using Tactic[Structural.Error]): record =
    if offset < 0L || offset > expanse.size - record.width
    then abort(Structural.Error(offset, record.width, expanse.size))
    else record.load(expanse.read(offset, record.width), 0)

extension [record: Structural](value: record)
  // The value's bytes: exactly as many as its layout is wide.
  def pack: Data =
    val target: Array[Byte]^ = Array.allocate[Byte](record.width)
    record.store(value, target, 0)
    Array.freeze(target)

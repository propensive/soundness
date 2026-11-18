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

import scala.compiletime.*

import anticipation.*
import fulminate.*
import prepositional.*

// A type with a fixed layout in bytes: a record of fixed-width fields, such as the header of a
// file or a network packet. An instance is derived from a case class of sized integers, with
// `Structural.derived`, and serves both directions: `unpack` reads a value from `Data`, or from
// any `Expanse`, at an offset, and `pack` writes one.
//
// Derivation computes each field's offset when the layout is declared, and expands to a load
// (or a store) per field at that offset, in the byte order in scope. A field whose type is not a
// sized integer needs an instance of its own, which is how records nest, and how a field is
// given a representation the sized integers do not offer.
object Structural:
  // The number of bytes a value of the type occupies, without a value in hand.
  def width[record: Structural]: Int = record.width

  inline def derived[record <: Product]: record is Structural = summonFrom:
    case given ByteOrder.BigEndian.type    => bigEndian[record]
    case given ByteOrder.LittleEndian.type => littleEndian[record]

    case _ =>
      error
        ( "a layout needs a byte order where it is derived: import " +
          "byteOrders.bigEndianByteOrder or byteOrders.littleEndianByteOrder" )

  inline def bigEndian[record <: Product]: record is Structural =
    ${layoutInternal.derive[record]('{true})}

  inline def littleEndian[record <: Product]: record is Structural =
    ${layoutInternal.derive[record]('{false})}

  // Offset ranges outside the bytes being read: a record of `width` bytes was asked for at
  // `offset`, of only `size` bytes.
  case class Error(offset: Long, width: Int, size: Long)(using Diagnostics)
  extends fulminate.Error(60, 3, 0)
    ( m"a record of $width bytes cannot be read at offset $offset of $size bytes" )

trait Structural extends Typeclass:
  // The number of bytes the layout occupies.
  def width: Int

  // The value laid out in the `width` bytes from `offset`, which the caller has checked are
  // there; `unpack` is the checked entry point.
  def load(data: Data, offset: Int): Self

  // Lays the value out in the `width` bytes of `target` from `offset`, which the caller has
  // checked are there; `pack` is the checked entry point.
  def store(value: Self, target: Array[Byte]^, offset: Int): Unit

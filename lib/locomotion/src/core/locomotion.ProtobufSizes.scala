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
package locomotion

import scala.caps

import anticipation.*
import rudiments.*

@unexported
object ProtobufSizes:
  // Reasserts the capability of a size table that travelled as a neutral carrier, for the
  // generated printers spliced into user modules; inline for the reason `ProtobufParser.of` is.
  inline def of(carrier: AnyRef): ProtobufSizes^ = carrier.asInstanceOf[ProtobufSizes^]

// The lengths a direct printer measures before it writes: every length-delimited value — a
// nested message, a string, a packed run — needs its size as a prefix, so a `Protobuf.Printable`
// measures a value in one pass, recording each such size here in the order it is visited, and
// writes it in a second pass, reading the sizes back in the same order. A nested message's slot
// is reserved before its children are measured and filled after, so the table is in pre-order
// on both passes. (protobuf-java memoizes the size on each message object; a case class has
// nowhere to keep one.) What the measuring pass produces and the writing pass needs — a
// string's UTF-8 bytes, a wire value a seam encoded — is stashed alongside, in the same order.
//
// A stateful capability, as the parser is: owned by one `print` call for its duration.
@unexported
final class ProtobufSizes() extends caps.ExclusiveCapability, caps.Stateful:
  // The raw JVM arrays appear here only, as the parser's buffer does: growable cursor storage.
  private var sizes:    scala.Array[Int]    = new scala.Array[Int](32)
  private var stashes:  scala.Array[AnyRef] = new scala.Array[AnyRef](16)
  private var count:    Int = 0
  private var position: Int = 0
  private var stashed:  Int = 0
  private var taken:    Int = 0

  // A slot for a size known only after its children are measured; `set` fills it.
  update def reserve(): Int =
    if count == sizes.length then
      val bigger = new scala.Array[Int](count*2)
      System.arraycopy(sizes, 0, bigger, 0, count)
      sizes = bigger

    val slot = count
    count += 1
    slot

  // The casts re-assert the exclusivity the field's type forgets, as `Scribe`'s writes do: the
  // arrays are reached only through this (exclusive) table.
  update def set(slot: Int, size: Int): Unit =
    sizes.asInstanceOf[scala.Array[Int]^](slot) = size

  update def record(size: Int): Unit = set(reserve(), size)

  // Moves the read position to the start, for the write pass.
  update def rewind(): Unit =
    position = 0
    taken = 0

  update def next(): Int =
    val size = sizes(position)
    position += 1
    size

  update def stash(value: AnyRef): Unit =
    if stashed == stashes.length then
      val bigger = new scala.Array[AnyRef](stashed*2)
      System.arraycopy(stashes, 0, bigger, 0, stashed)
      stashes = bigger

    stashes.asInstanceOf[scala.Array[AnyRef]^](stashed) = value
    stashed += 1

  update def unstash(): AnyRef =
    val value = stashes(taken)
    taken += 1
    value

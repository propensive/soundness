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

import rudiments.*

@unexported
object ProtobufSizes:
  // Reasserts the capability of a size table that travelled as a neutral carrier, for the
  // generated printers spliced into user modules; inline for the reason `ProtobufParser.of` is.
  inline def of(carrier: AnyRef): ProtobufSizes^ = carrier.asInstanceOf[ProtobufSizes^]

  // One table per thread, reused by every `print` on it, so a print allocates nothing but
  // its output. A print that finds the thread's table in use — a seam's `Encodable` printing
  // a nested message through its own `Printable` — takes a fresh one.
  // Held as a neutral carrier, as a registry holds a handle: the capability is reasserted at
  // `acquire`, the one point a table is reached from, and only by the `print` that holds it
  // until `release`.
  private val local: ThreadLocal[AnyRef] =
    ThreadLocal.withInitial { () => ProtobufSizes().asInstanceOf[AnyRef] }.nn

  private[locomotion] def acquire(): ProtobufSizes^ =
    val table = local.get.nn.asInstanceOf[ProtobufSizes^]

    if table.busy then ProtobufSizes() else
      table.busy = true
      table

  private[locomotion] def release(table: ProtobufSizes^): Unit = table.reset()

// The lengths a direct printer measures before it writes: every length-delimited value — a
// nested message, a string, a packed run — needs its size as a prefix, so a `Protobuf.Printable`
// measures a value in one pass, recording each such size here in the order it is visited, and
// writes it in a second pass, reading the sizes back in the same order. A nested message's slot
// is reserved before its children are measured and filled after, so the table is in pre-order
// on both passes. (protobuf-java memoizes the size on each message object; a case class has
// nowhere to keep one.) What the measuring pass produces and the writing pass needs is kept
// alongside, in the same order: a string is encoded once, into the table's byte scratch, and
// copied out by the writing pass (`stashText`/`ProtobufPrinter.stashedText`); a wire value a
// seam encoded is stashed as a reference.
//
// A stateful capability, as the parser is: owned by one `print` call for its duration.
@unexported
final class ProtobufSizes() extends caps.ExclusiveCapability, caps.Stateful:
  // The raw JVM arrays appear here only, as the parser's buffer does: growable cursor storage.
  private var sizes:    scala.Array[Int]    = new scala.Array[Int](32)
  private var stashes:  scala.Array[AnyRef | Null] = new scala.Array[AnyRef | Null](16)
  private var scratch:  scala.Array[Byte] = new scala.Array[Byte](256)
  private var used:     Int = 0
  private var count:    Int = 0
  private var position: Int = 0
  private var stashed:  Int = 0
  private var taken:    Int = 0
  private var busy:     Boolean = false

  // Empties the table for its next print, dropping the references it stashed.
  private update def reset(): Unit =
    java.util.Arrays.fill(stashes, 0, stashed, null)
    used = 0
    count = 0
    position = 0
    stashed = 0
    taken = 0
    busy = false

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

  // Encodes `string` into the scratch, recording where and how long, and returns its encoded
  // length; three bytes per char is the most UTF-8 needs (a surrogate pair's four cover two).
  update def stashText(string: String): Int =
    val room = used + 3*string.length

    if room > scratch.length then
      val bigger = new scala.Array[Byte](room.max(scratch.length*2))
      System.arraycopy(scratch, 0, bigger, 0, used)
      scratch = bigger

    val end = Scribe.utf8Into(scratch.asInstanceOf[scala.Array[Byte]^], used, string, 0)
    val length = end - used
    record(used)
    record(length)
    used = end
    length

  // The scratch the stashed strings were encoded into, for `ProtobufPrinter.stashedText`,
  // which reads the next string's offset and length from the table and copies it out. (A
  // callback taking the three would box its `Int`s: `Function3` is not specialized.)
  private[locomotion] def scratchBytes: scala.Array[Byte] = scratch

  update def stash(value: AnyRef): Unit =
    if stashed == stashes.length then
      val bigger = new scala.Array[AnyRef | Null](stashed*2)
      System.arraycopy(stashes, 0, bigger, 0, stashed)
      stashes = bigger

    stashes.asInstanceOf[scala.Array[AnyRef | Null]^](stashed) = value
    stashed += 1

  update def unstash(): AnyRef =
    val value = stashes(taken).nn
    taken += 1
    value

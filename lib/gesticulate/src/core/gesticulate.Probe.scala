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
package gesticulate

import scala.annotation.tailrec

import anticipation.*
import denominative.*
import fulminate.*
import rudiments.*
import vacuous.*
import zephyrine.*

// The forward-only view of a stream which a sequential `Detectable` reads. It is the only access
// a detector is given, and it can neither mark nor rewind the cursor beneath it: `attempt`, which
// only Gesticulate can call, holds the cursor for the length of the detection and returns it to
// where it began whether the detector matches, declines or throws. So no instance can leave the
// stream anywhere but at its start, and a probe kept beyond its detection refuses every read.
//
// Whatever a detector reads stays buffered until the detection ends, and a read that runs past
// what has arrived blocks on a live stream — so a detector should read no further than it needs.
object Probe:
  // Runs `detect` over a probe of `cursor`, which is left where it was found.
  private[gesticulate] def attempt(cursor: Cursor[Data, {}]^)(detect: Probe^ => Boolean)
  :   Boolean =

    new Probe(cursor).attempt(detect)

final class Probe private (cursor: Cursor[Data, {}]^) extends scala.caps.Mutable:
  private var live: Boolean = false
  private var origin: Long = 0L

  private inline def check(): Unit =
    if !live then panic(m"a Probe was used after its detection had finished")

  // How many bytes have been read from the start of the content.
  def offset: Long = cursor.position.n0 - origin

  // Whether another byte can be read.
  update def more: Boolean =
    check()
    cursor.more

  // The next byte, without reading past it, or `Unset` at the end of the content.
  update def peek: Optional[Byte] =
    check()
    if cursor.more then cursor.peek.asInt.toByte else Unset

  // Reads the next byte, or `Unset` at the end of the content.
  update def next(): Optional[Byte] =
    check()

    if cursor.more then
      val byte = cursor.peek.asInt.toByte
      cursor.advance()
      byte
    else
      Unset

  // Passes over up to `count` bytes, and returns how many there were.
  update def skip(count: Int): Int =
    check()
    advance(count, 0)

  // Reads up to `count` bytes: fewer only if the content ends first.
  update def take(count: Int): Data =
    check()

    cursor.hold:
      val start = cursor.mark
      advance(count, 0)
      cursor.grab(start, cursor.mark)

  // Passes over a UTF-8 byte-order mark, if the content starts with one, and any ASCII
  // whitespace: what may precede the first significant byte of a textual format.
  update def skipWhitespace(): Unit =
    check()
    if offset == 0 && cursor.more && cursor.peek.asInt == 0xef then advance(3, 0)
    whitespace()

  // Whether the next bytes are exactly `signature`, which is read past if so.
  update def expect(signature: Data): Boolean =
    val found: Data = take(signature.length)

    signature.coextent(found).lay(false): shared =>
      signature.spot { index => signature(index) != found(shared(index)) }.absent

  @tailrec
  private update def advance(count: Int, done: Int): Int =
    if done < count && cursor.more then
      cursor.advance()
      advance(count, done + 1)
    else
      done

  @tailrec
  private update def whitespace(): Unit =
    if cursor.more then
      val byte = cursor.peek.asInt

      if byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d then
        cursor.advance()
        whitespace()

  private[gesticulate] update def attempt(detect: Probe^ => Boolean): Boolean =
    val outcome: Boolean | Throwable = cursor.hold:
      val start = cursor.mark
      origin = cursor.position.n0
      live = true

      try detect(this) catch case error: Throwable => error
      finally
        live = false
        cursor.cue(start)

    outcome match
      case matched: Boolean => matched
      case error: Throwable => throw error

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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package enigmatic

import scala.caps

import java.nio as jn
import java.nio.charset as jnc
import java.util as ju

import anticipation.*
import gossamer.*
import spectacular.*

object Password:
  // From cleartext `Text`. The immutable `String` has already pinned the cleartext on the
  // heap, where it cannot be zeroed, so the cloak protects only the copy it stores; prefer
  // the `Array[Char]` overload when the password originates from mutable input.
  def apply(cleartext: Text)(using cloak: Cloak^): Password^{cloak} =
    new Password(cloak.cloak(cleartext.s.getBytes(jnc.StandardCharsets.UTF_8).nn))

  // From mutable chars: encodes to UTF-8, then zeroes both the input array and the
  // intermediate encoding buffer, leaving the cloaked copy as the only cleartext.
  def apply(cleartext: Array[Char])(using cloak: Cloak^): Password^{cloak} =
    val buffer = jnc.StandardCharsets.UTF_8.nn.encode(jn.CharBuffer.wrap(cleartext)).nn
    val bytes = new Array[Byte](buffer.remaining)
    buffer.get(bytes)
    if buffer.hasArray then ju.Arrays.fill(buffer.array.nn, 0.toByte)
    ju.Arrays.fill(cleartext, '\u0000')
    new Password(cloak.cloak(bytes))

  // Never renders the secret: a `Password` is safe to log or embed in a message.
  given showable: Password is Showable = _ => t"Password(•••)"

// A password held opaquely by whichever `Cloak` was in scope at construction, capturing that
// cloak, with no way to read the cleartext back except through `uncloak`, which lends it — as
// a scoped `Cleartext` capability over a mutable char array — to a block, exactly as
// `PrivateKey.uncloak` lends a `Decryptor`. Capture checking confines the capability to the
// block, so it (and any closure over it) cannot escape; `result` is instantiated at the call
// site. The chars are zeroed when the block exits, so they must not escape either.
class Password private[enigmatic] (private[enigmatic] val secret: Secret^):
  def uncloak[result](block: Cleartext^ ?=> result): result =
    secret.uncloak: bytes =>
      val buffer = jnc.StandardCharsets.UTF_8.nn.decode(jn.ByteBuffer.wrap(bytes)).nn
      val chars = new Array[Char](buffer.remaining)
      buffer.get(chars)
      if buffer.hasArray then ju.Arrays.fill(buffer.array.nn, '\u0000')
      try block(using Cleartext(chars)) finally ju.Arrays.fill(chars, '\u0000')

// The scoped view of a password's cleartext, lent within `uncloak` as a mutable char array,
// zeroed when the block exits. A `SharedCapability`, freely aliasable within the block but
// not beyond it. There is deliberately no `Text` accessor: an immutable `String` copy of the
// cleartext could be neither confined to the block nor zeroed after it.
class Cleartext private[enigmatic] (private val secret: Array[Char]) extends caps.SharedCapability:
  def chars: Array[Char] = secret

// The cleartext lent within an `uncloak` block, reached contextually: `cleartext.chars` rather
// than `summon[Cleartext].chars`, following the same idiom as parasite's `monitor`.
transparent inline def cleartext: Cleartext^ = infer[Cleartext^]

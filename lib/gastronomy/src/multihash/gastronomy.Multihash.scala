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
package gastronomy

import anticipation.*
import contingency.*
import corpuscular.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// A self-describing digest envelope: `<varint code><varint length><digest bytes>`, per
// https://github.com/multiformats/multihash. The point is that a digest can cross a boundary
// without the parties agreeing on the algorithm out of band, which is what content-addressing
// systems (IPFS, IPLD, libp2p) and `did:key` rely on.
//
// The code is held as a plain `Int`, not as a gastronomy `Algorithm`: a received envelope may
// name a function this library cannot compute, and such a value must be *representable* rather
// than an error, since a consumer may only need to compare or forward it. Producing one, by
// contrast, goes through `Multicodec`, so only an algorithm with a registered code can be
// enveloped.
object Multihash:
  // The unsigned-varint of the multiformats specification: LEB128, low-order group first, the
  // high bit of each byte marking continuation. The spec caps a value at 63 bits — nine groups —
  // so a longer run is malformed rather than merely large.
  private val varintLimit: Int = 9

  private[gastronomy] def varint(value: Long): scala.List[Byte] =
    def recur(rest: Long, acc: scala.List[Byte]): scala.List[Byte] =
      if rest < 0x80 then (acc :+ rest.toByte)
      else recur(rest >>> 7, acc :+ ((rest & 0x7f) | 0x80).toByte)

    recur(value, scala.Nil)

  // Reads one varint at `offset`, returning it with the offset just past it.
  private[gastronomy] def readVarint(data: Data, offset: Int): (Long, Int) raises Multihash.Error =
    def recur(index: Int, shift: Int, acc: Long, groups: Int): (Long, Int) =
      if index >= data.length then abort(Multihash.Error(Multihash.Reason.Truncated))
      else if groups >= varintLimit then abort(Multihash.Error(Multihash.Reason.Oversize))
      else
        val byte = data.readable(index)
        val value = acc | ((byte & 0x7f).toLong << shift)
        if (byte & 0x80) == 0 then (value, index + 1) else recur(index + 1, shift + 7, value, groups + 1)

    recur(offset, 0, 0L, 0)

  def apply[algorithm <: Algorithm](digest: Digest in algorithm)
     (using codec: algorithm is Multicodec)
  :   Multihash =
    Multihash(codec.code, digest.data)

  // Reads an envelope. The declared length must match the bytes that follow exactly: a short
  // count would silently truncate the digest and a long one would run past the end, and either
  // would compare equal to nothing while looking well-formed.
  def parse(data: Data): Multihash raises Multihash.Error =
    val (code, afterCode) = readVarint(data, 0)
    val (length, afterLength) = readVarint(data, afterCode)
    val available = data.length - afterLength

    if length > available then abort(Multihash.Error(Multihash.Reason.Truncated))
    if length < available then abort(Multihash.Error(Multihash.Reason.Trailing))

    Multihash(code.toInt, Array.frozen(data.readable.slice(afterLength, afterLength + length.toInt)))

  enum Reason:
    case Truncated, Trailing, Oversize

  given Reason is Communicable =
    case Reason.Truncated => m"the data end before the declared digest length"
    case Reason.Trailing  => m"bytes remain after the declared digest length"
    case Reason.Oversize  => m"the varint exceeds the nine groups the specification permits"

  case class Error(reason: Reason)(using Diagnostics)
  extends fulminate.Error(41, reason.ordinal)(m"the multihash could not be read because $reason")

case class Multihash(code: Int, digest: Data):
  // The registered name of the hash function, when it is one this library knows. A decoded
  // envelope naming anything else keeps its code and its bytes, and simply has no name.
  def algorithm: Optional[Text] = Multicodec.name(code)

  def serialize: Data =
    val prefix = Multihash.varint(code.toLong) ++ Multihash.varint(digest.length.toLong)
    Array.unsafeFrozen((prefix ++ digest.readable.toSeq).toArray)

  override def equals(that: Any): Boolean = (that: Any @unchecked) match
    case multihash: Multihash =>
      code == multihash.code && digest.readable.sameElements(multihash.digest.readable)

    case _ => false

  override def hashCode: Int = code*31 + digest.readable.toSeq.hashCode

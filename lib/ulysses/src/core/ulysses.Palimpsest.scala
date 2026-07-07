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
package ulysses

import anticipation.*
import denominative.*
import fulminate.*
import vacuous.*

object Palimpsest:
  // §3 encoding. Build a body of length `cadence.bodyLength(n)`, XOR each
  // hash in at offset `cadence.offset(i)`, then append the trailing byte
  // adjusted so the XOR-fold of every output byte equals the cadence byte.
  def apply(hashes: IndexedSeq[Data])(using cadence: Cadence): Palimpsest =
    if hashes.nil then panic(m"palimpsest requires at least one hash")

    if !hashes.forall(_.length == cadence.hashSize)
    then panic(m"all hashes must have length ${cadence.hashSize}")

    val n        = hashes.length
    val bodyLen  = cadence.bodyLength(n)
    val body     = new Array[Byte](bodyLen)
    val hashSize = cadence.hashSize

    var i = 0

    while i < n do
      val hash = hashes(i)
      val o    = cadence.offset(i)
      var j    = 0

      while j < hashSize do
        body(o + j) = (body(o + j) ^ hash(j)).toByte
        j += 1

      i += 1

    var xor = 0
    var k   = 0

    while k < bodyLen do
      xor = xor ^ (body(k) & 0xff)
      k += 1

    val out = new Array[Byte](bodyLen + 1)
    System.arraycopy(body, 0, out, 0, bodyLen)
    out(bodyLen) = (xor ^ (cadence.byte & 0xff)).toByte

    Palimpsest(out.asInstanceOf[IArray[Byte]], n)

case class Palimpsest(data: Data, length: Int):
  // §4 decoding. Recover the cadence byte from the XOR-fold, derive `n`
  // from the body length, then run the recursive backtracking search:
  // at each step lookup candidate hashes by their first `k_i` (i = 0) or
  // `k_r` (i ≥ 1) bytes, XOR the candidate out, recurse, undo on failure.
  // Returns `Unset` if the cadence byte names a reserved hash-size index,
  // if the byte length is inconsistent with the cadence, if `n` disagrees
  // with the stored length, or if no library candidate combination zeroes
  // out the body.
  def resolve(using bibliography: Bibliography): Optional[List[Data]] =
    val total = data.length

    if total < 2 then Unset else
      var xor = 0
      var i   = 0

      while i < total do
        xor = xor ^ (data(i) & 0xff)
        i += 1

      Cadence.unpack(xor.toByte).let: cadence =>
        val bodyLen = total - 1

        cadence.hashCount(bodyLen).let: n =>
          if n != length then Unset else
            val body: Array[Byte] = new Array[Byte](bodyLen)
            System.arraycopy(data.asInstanceOf[Array[Byte]], 0, body, 0, bodyLen)

            def xor_(hash: Data, offset: Int): Unit =
              var j = 0

              while j < cadence.hashSize do
                body(offset + j) = (body(offset + j) ^ hash(j)).toByte
                j += 1

            def recur(item: Int, matched: List[Data]): Optional[List[Data]] =
              if item == n then
                var allZero = true
                var k       = 0

                while k < bodyLen && allZero do
                  if body(k) != 0.toByte then allZero = false
                  k += 1

                if allZero then matched.reverse else Unset
              else
                val o         = cadence.offset(item)
                val prefixLen = if item == 0 then cadence.initial else cadence.regular
                val prefix    = new Array[Byte](prefixLen)
                System.arraycopy(body, o, prefix, 0, prefixLen)

                val candidates =
                  bibliography.lookup(prefix.asInstanceOf[IArray[Byte]]).iterator

                var found: Optional[List[Data]] = Unset

                while found.absent && candidates.hasNext do
                  val hash = candidates.next()
                  xor_(hash, o)
                  val sub = recur(item + 1, hash :: matched)

                  if sub.absent then xor_(hash, o)
                  else found = sub

                found

            recur(0, Nil)

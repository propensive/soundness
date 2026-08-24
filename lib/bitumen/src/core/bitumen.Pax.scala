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
package bitumen


import anticipation.*
import denominative.*
import contingency.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import rudiments.*
import vacuous.*
import symbolism.*

object Pax:
  def record(key: Text, value: Text): Data =
    val payload: Data = (key.s+"="+value.s+"\n").tt.in[Data]
    val payloadLen: Int = payload.length
    val total: Int = computeLength(payloadLen)
    (total.toString+" "+key.s+"="+value.s+"\n").tt.in[Data]

  def records(pairs: Iterable[(Text, Text)]): Data =
    // Accumulate on the `IArray` side: a `Concatenable` result is fresh, and a fresh capture
    // cannot instantiate `foldLeft`'s accumulator type.
    Array.frozen:
      pairs.foldLeft(scala.IArray.empty[Byte]): (acc, pair) =>
        acc ++ record(pair(0), pair(1)).readable

  def parse(data: Data): Map[Text, Text] raises Tar.Error =
    val builder = scala.collection.immutable.Map.newBuilder[Text, Text]
    var pos = 0

    while pos < data.length do
      var lengthEnd = pos

      while lengthEnd < data.length &&
        data.readUnchecked(lengthEnd) >= '0'.toByte &&
        data.readUnchecked(lengthEnd) <= '9'.toByte
      do lengthEnd += 1

      if lengthEnd == pos || lengthEnd >= data.length || data.readUnchecked(lengthEnd) != ' '.toByte
      then
        raise(Tar.Error(Tar.Error.Reason.BadPaxRecord(data)))
        pos = data.length
      else
        // The scanned slice is all ASCII digits, so the only way `toInt` can fail is an
        // overflowing run of digits; fall back to 0 so the `length < 1` check below rejects it.
        val length =
          try data.segment((pos).z till (lengthEnd).z).ascii.s.toInt
          catch case _: NumberFormatException => 0

        if length < 1 || pos + length > data.length || data.readUnchecked(pos + length - 1) != '\n'.toByte
        then
          raise(Tar.Error(Tar.Error.Reason.BadPaxRecord(data)))
          pos = data.length
        else
          val content: String = data.segment((lengthEnd + 1).z till (pos + length - 1).z).utf8.s
          val eqIdx: Int = content.indexOf('=')

          if eqIdx < 0 then
            // Unlike a malformed length prefix (which desynchronizes the cursor and must
            // terminate the scan), a record with no `=` has a known extent: record its error
            // and skip just that record, so every malformed record accrues rather than only
            // the first.
            raise(Tar.Error(Tar.Error.Reason.BadPaxRecord(data)))
            pos = pos + length
          else
            builder += ((content.substring(0, eqIdx).nn.tt, content.substring(eqIdx + 1).nn.tt))
            pos = pos + length

    builder.result().to(Map)

  private def computeLength(payloadLen: Int): Int =
    var n = 1
    var total = payloadLen+1+n

    while total.toString.length != n do
      n = total.toString.length
      total = payloadLen+1+n

    total

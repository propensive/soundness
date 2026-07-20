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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The PNG (10–15) and TIFF (2) predictors of ISO 32000-2 §7.4.4.4, which decorrelate rows
// before Flate or LZW compression. Cross-reference streams are routinely predictor-encoded,
// so this sits on the critical path of opening a file.
private[facsimile] object Predictor:
  def apply(data: Data, predictor: Int, colors: Int, bits: Int, columns: Int)
  :   Data raises PdfError =

    if predictor <= 1 then data
    else if predictor == 2 then tiff(data, colors, bits, columns)
    else if predictor >= 10 && predictor <= 15 then png(data, colors, bits, columns)
    else abort(PdfError(PdfError.Reason.CorruptStream(t"Predictor")))

  private def tiff(data: Data, colors: Int, bits: Int, columns: Int): Data raises PdfError =
    if bits != 8 then abort(PdfError(PdfError.Reason.CorruptStream(t"Predictor"))) else
      val rowLength = colors*columns
      val out = data.mutable(using Unsafe).clone.nn
      var row = 0

      while row*rowLength < out.length do
        var i = row*rowLength + colors
        val end = ((row + 1)*rowLength).min(out.length)

        while i < end do
          out(i) = (out(i) + out(i - colors)).toByte
          i += 1

        row += 1

      out.immutable(using Unsafe)

  private def png(data: Data, colors: Int, bits: Int, columns: Int): Data raises PdfError =
    val bytesPerPixel = ((colors*bits + 7)/8).max(1)
    val rowLength = (columns*colors*bits + 7)/8
    val out = Array.newBuilder[Byte]
    var previous = new Array[Byte](rowLength)
    var in = 0

    while in < data.length do
      val filter = data(in) & 0xff
      in += 1
      val available = rowLength.min(data.length - in)
      val row = new Array[Byte](rowLength)
      var i = 0

      while i < available do
        row(i) = data(in + i)
        i += 1

      in += available

      def left(i: Int): Int = if i >= bytesPerPixel then row(i - bytesPerPixel) & 0xff else 0
      def up(i: Int): Int = previous(i) & 0xff
      def upLeft(i: Int): Int = if i >= bytesPerPixel then previous(i - bytesPerPixel) & 0xff else 0

      filter match
        case 0 =>
          ()

        case 1 =>
          i = 0

          while i < rowLength do
            row(i) = (row(i) + left(i)).toByte
            i += 1

        case 2 =>
          i = 0

          while i < rowLength do
            row(i) = (row(i) + up(i)).toByte
            i += 1

        case 3 =>
          i = 0

          while i < rowLength do
            row(i) = (row(i) + (left(i) + up(i))/2).toByte
            i += 1

        case 4 =>
          i = 0

          while i < rowLength do
            val prediction =
              val a = left(i)
              val b = up(i)
              val c = upLeft(i)
              val p = a + b - c
              val pa = (p - a).abs
              val pb = (p - b).abs
              val pc = (p - c).abs
              if pa <= pb && pa <= pc then a else if pb <= pc then b else c

            row(i) = (row(i) + prediction).toByte
            i += 1

        case _ =>
          abort(PdfError(PdfError.Reason.CorruptStream(t"Predictor")))

      // A truncated final row yields only the bytes that were present.
      i = 0

      while i < available do
        out += row(i)
        i += 1

      previous = row

    out.result().immutable(using Unsafe)

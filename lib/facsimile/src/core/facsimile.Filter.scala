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

import java.io as ji
import java.util.zip as juz

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Stream filters (ISO 32000-2 §7.4). Image codecs — DCT, JPX, CCITT, JBIG2 — are *terminal*:
// decoding stops before them and the caller receives the still-encoded image bytes, which is
// what any consumer of those formats wants anyway.
private[facsimile] object Filter:
  object Id:
    // Both the full names and the inline-image abbreviations of ISO 32000-2 §8.9.7.
    def parse(name: Text): Optional[Id] = name.s match
      case "FlateDecode" | "Fl"      => Flate
      case "ASCIIHexDecode" | "AHx"  => AsciiHex
      case "ASCII85Decode" | "A85"   => Ascii85
      case "LZWDecode" | "LZW"       => Lzw
      case "RunLengthDecode" | "RL"  => RunLength
      case "Crypt"                   => Crypt
      case "DCTDecode" | "DCT"       => Dct
      case "JPXDecode"               => Jpx
      case "CCITTFaxDecode" | "CCF"  => Ccitt
      case "JBIG2Decode"             => Jbig2
      case _                         => Unset

  enum Id:
    case Flate, AsciiHex, Ascii85, Lzw, RunLength, Crypt
    case Dct, Jpx, Ccitt, Jbig2

    def terminal: Boolean = this match
      case Dct | Jpx | Ccitt | Jbig2 => true
      case _                         => false

  // Normalizes a stream dictionary's `/Filter` (a name or an array of names) and
  // `/DecodeParms` (a dictionary, an array with nulls, or absent) into a decoding plan. Both
  // values must already be resolved: indirect references are the caller's concern.
  def chain(filter: Optional[Cos], parms: Optional[Cos])
  :   List[(Id, Map[Text, Cos])] raises PdfError =

    val names: List[Text] = filter.lay(List()):
      case Cos.Name(name) =>
        List(name)

      case Cos.Sequence(elements) =>
        elements.map: element =>
          element.name.or(abort(PdfError(PdfError.Reason.TypeMismatch(t"Filter", t"a name"))))

      case _ =>
        abort(PdfError(PdfError.Reason.TypeMismatch(t"Filter", t"a name or array of names")))

    val parameters: List[Map[Text, Cos]] = parms.lay(List()):
      case Cos.Dictionary(entries) =>
        List(entries)

      case Cos.Sequence(elements) =>
        elements.map: element =>
          element match
            case Cos.Dictionary(entries) => entries
            case Cos.Nil                 => Map()

            case _ =>
              abort(PdfError(PdfError.Reason.TypeMismatch(t"DecodeParms", t"a dictionary")))

      case _ =>
        abort(PdfError(PdfError.Reason.TypeMismatch(t"DecodeParms", t"a dictionary or array")))

    names.zipWithIndex.map: (name, index) =>
      val id = Id.parse(name).or(abort(PdfError(PdfError.Reason.UnknownFilter(name))))
      (id, if index < parameters.length then parameters(index) else Map())

  // A streaming plan is plain data — closures at most — because ducts, being scoped
  // capabilities, may only be minted at the `via` call site (a lambda cannot return a fresh
  // capability; a method can). `Pdf.spring` interprets these steps into a pipeline.
  private[facsimile] enum Step:
    case Inflate
    case Unlzw(earlyChange: Boolean)
    case Gather(transform: Data => Data)

  // The streaming plan for a decoding chain, stopping before any terminal codec: Flate
  // streams incrementally through turbulence's zlib duct (without the eager path's
  // raw-deflate retry, which is impossible mid-stream); the textual filters gather their
  // input and decode on flush, which is immaterial at their typical sizes.
  def steps(chain: List[(Id, Map[Text, Cos])])(using tactic: Tactic[PdfError])
  :   List[Step^{tactic}] =
    chain.takeWhile(!_(0).terminal).flatMap: (id, parms) =>
      val predicted = parms.at(t"Predictor").let(_.long).or(1L) > 1

      id match
        case Id.Flate =>
          if predicted then List(Step.Inflate, Step.Gather(predict(_, parms)))
          else List(Step.Inflate)

        case Id.Lzw =>
          if predicted then List(Step.Unlzw(earlyChange(parms)), Step.Gather(predict(_, parms)))
          else List(Step.Unlzw(earlyChange(parms)))

        case Id.Crypt =>
          List()

        case other =>
          List(Step.Gather(stage(_, other, parms)))

  // Applies a resolved filter chain eagerly, stopping at the first terminal codec.
  def decode(data: Data, chain: List[(Id, Map[Text, Cos])]): Data raises PdfError =
    chain match
      case (id, parms) :: rest =>
        if id.terminal then data else decode(stage(data, id, parms), rest)

      case _ =>
        data

  private def stage(data: Data, id: Id, parms: Map[Text, Cos]): Data raises PdfError = id match
    case Id.Flate     => predict(flate(data), parms)
    case Id.Lzw       => predict(lzw(data, parms), parms)
    case Id.Ascii85   => Ascii85.decode(data)
    case Id.AsciiHex  => asciiHex(data)
    case Id.RunLength => runLength(data)
    case Id.Crypt     => data // `Identity` until encryption arrives; `Guard` will slot in here
    case _            => data

  private def lzw(data: Data, parms: Map[Text, Cos]): Data raises PdfError =
    try Lzw.decompress(LazyList(data), earlyChange(parms)).foldLeft(IArray.empty[Byte])(_ ++ _)
    catch case _: IllegalStateException =>
      abort(PdfError(PdfError.Reason.CorruptStream(t"LZWDecode")))

  private def earlyChange(parms: Map[Text, Cos]): Boolean =
    parms.at(t"EarlyChange").let(_.long).or(1L) == 1L

  private def predict(data: Data, parms: Map[Text, Cos]): Data raises PdfError =
    val predictor = parms.at(t"Predictor").let(_.long).or(1L).toInt

    if predictor <= 1 then data else
      val colors = parms.at(t"Colors").let(_.long).or(1L).toInt
      val bits = parms.at(t"BitsPerComponent").let(_.long).or(8L).toInt
      val columns = parms.at(t"Columns").let(_.long).or(1L).toInt
      Predictor(data, predictor, colors, bits, columns)

  // FlateDecode is zlib-framed deflate, but raw streams occur in the wild: on a zlib failure,
  // retry nowrap before giving up.
  private def flate(data: Data): Data raises PdfError =
    inflate(data, nowrap = false).or(inflate(data, nowrap = true))
    . or(abort(PdfError(PdfError.Reason.CorruptStream(t"FlateDecode"))))

  private def inflate(data: Data, nowrap: Boolean): Optional[Data] =
    val inflater = juz.Inflater(nowrap)
    val out = ji.ByteArrayOutputStream()
    val buffer = new Array[Byte](8192)
    inflater.setInput(data.mutable(using Unsafe))

    try
      // Stop on completion, or when no more output can be produced from the input that
      // remains: a truncated stream yields what it decoded.
      var stalled = false

      while !inflater.finished && !stalled do
        val count = inflater.inflate(buffer)

        if count > 0 then out.write(buffer, 0, count)
        else stalled = inflater.needsInput || inflater.needsDictionary

      if out.size == 0 && data.length > 0 && !inflater.finished then Unset
      else out.toByteArray.nn.immutable(using Unsafe)
    catch case _: juz.DataFormatException => Unset
    finally inflater.end()

  private def asciiHex(data: Data): Data raises PdfError =
    val bytes = Array.newBuilder[Byte]
    var high = -1
    var done = false
    var i = 0

    while i < data.length && !done do
      val byte = data(i) & 0xff
      i += 1

      if byte == '>' then done = true
      else if !CosLexer.whitespace(byte) then
        val value = CosLexer.hexadecimal(byte)

        if value < 0 then abort(PdfError(PdfError.Reason.CorruptStream(t"ASCIIHexDecode")))
        else if high < 0 then high = value
        else
          bytes += ((high << 4) + value).toByte
          high = -1

    if high >= 0 then bytes += (high << 4).toByte
    bytes.result().immutable(using Unsafe)

  private def runLength(data: Data): Data raises PdfError =
    val bytes = Array.newBuilder[Byte]
    var done = false
    var i = 0

    while i < data.length && !done do
      val length = data(i) & 0xff
      i += 1

      if length == 128 then done = true
      else if length < 128 then
        if i + length + 1 > data.length
        then abort(PdfError(PdfError.Reason.CorruptStream(t"RunLengthDecode")))

        var j = 0

        while j <= length do
          bytes += data(i + j)
          j += 1

        i += length + 1
      else
        if i >= data.length
        then abort(PdfError(PdfError.Reason.CorruptStream(t"RunLengthDecode")))

        var j = 0

        while j < 257 - length do
          bytes += data(i)
          j += 1

        i += 1

    bytes.result().immutable(using Unsafe)

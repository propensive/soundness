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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import pneumatic.*
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
  ( using Tactic[Pdf.Error] )
  :   List[(Id, Map[Text, Cos])] =

    val names: List[Text] = filter.lay(List()):
      case Cos.Name(name) =>
        List(name)

      case Cos.Sequence(elements) =>
        elements.map: element =>
          element.name.or(abort(Pdf.Error(Pdf.Error.Reason.TypeMismatch(t"Filter", t"a name"))))

      case _ =>
        abort(Pdf.Error(Pdf.Error.Reason.TypeMismatch(t"Filter", t"a name or array of names")))

    val parameters: List[Map[Text, Cos]] = parms.lay(List()):
      case Cos.Dictionary(entries) =>
        List(entries)

      case Cos.Sequence(elements) =>
        elements.map: element =>
          element match
            case Cos.Dictionary(entries) => entries
            case Cos.Nil                 => Map()

            case _ =>
              abort(Pdf.Error(Pdf.Error.Reason.TypeMismatch(t"DecodeParms", t"a dictionary")))

      case _ =>
        abort(Pdf.Error(Pdf.Error.Reason.TypeMismatch(t"DecodeParms", t"a dictionary or array")))

    List.of:
      names.stdlib.zipWithIndex.map: (name, index) =>
        val id = Id.parse(name).or(abort(Pdf.Error(Pdf.Error.Reason.UnknownFilter(name))))
        (id, if index < parameters.stdlib.length then parameters.stdlib(index) else Map())

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
  def steps(chain: List[(Id, Map[Text, Cos])])(using tactic: Tactic[Pdf.Error])
  :   List[Step^{tactic}] =
    // The steps capture `tactic`, and capture-carrying elements do not flow through the
    // opaque `List` combinators (boxing), so the interior stays stdlib inside one `List.of`.
    List.of:
      chain.stdlib.takeWhile(!_(0).terminal).flatMap: (id, parms) =>
        val predicted = parms(t"Predictor").let(_.long).or(1L) > 1

        id match
          case Id.Flate =>
            if predicted
            then scala.collection.immutable.List(Step.Inflate, Step.Gather(predict(_, parms)))
            else scala.collection.immutable.List(Step.Inflate)

          case Id.Lzw =>
            if predicted
            then scala.collection.immutable.List
              (Step.Unlzw(earlyChange(parms)), Step.Gather(predict(_, parms)))
            else scala.collection.immutable.List(Step.Unlzw(earlyChange(parms)))

          case Id.Crypt =>
            scala.collection.immutable.List()

          case other =>
            scala.collection.immutable.List(Step.Gather(stage(_, other, parms)))

  // Applies a resolved filter chain eagerly, stopping at the first terminal codec.
  def decode(data: Data, chain: List[(Id, Map[Text, Cos])])(using Tactic[Pdf.Error]): Data =
    chain match
      case (id, parms) :: rest =>
        if id.terminal then data else decode(stage(data, id, parms), rest)

      case _ =>
        data

  private def stage(data: Data, id: Id, parms: Map[Text, Cos])(using Tactic[Pdf.Error]): Data = id match
    case Id.Flate     => predict(flate(data), parms)
    case Id.Lzw       => predict(lzw(data, parms), parms)
    case Id.Ascii85   => Ascii85.decode(data)
    case Id.AsciiHex  => asciiHex(data)
    case Id.RunLength => runLength(data)
    case Id.Crypt     => data // `Identity` until encryption arrives; `Guard` will slot in here
    case _            => data

  private def lzw(data: Data, parms: Map[Text, Cos])(using Tactic[Pdf.Error]): Data =
    try Lzw.decompress(Chain(data), earlyChange(parms)).foldLeft(Array.empty[Byte])(_ ++ _)
    catch case _: IllegalStateException =>
      abort(Pdf.Error(Pdf.Error.Reason.CorruptStream(t"LZWDecode")))

  private def earlyChange(parms: Map[Text, Cos]): Boolean =
    parms(t"EarlyChange").let(_.long).or(1L) == 1L

  private def predict(data: Data, parms: Map[Text, Cos])(using Tactic[Pdf.Error]): Data =
    val predictor = parms(t"Predictor").let(_.long).or(1L).toInt

    if predictor <= 1 then data else
      val colors = parms(t"Colors").let(_.long).or(1L).toInt
      val bits = parms(t"BitsPerComponent").let(_.long).or(8L).toInt
      val columns = parms(t"Columns").let(_.long).or(1L).toInt
      Predictor(data, predictor, colors, bits, columns)

  // FlateDecode is zlib-framed deflate, but raw streams occur in the wild: on a zlib failure,
  // retry nowrap before giving up.
  private def flate(data: Data)(using Tactic[Pdf.Error]): Data =
    inflate(data, nowrap = false).or(inflate(data, nowrap = true))
    . or(abort(Pdf.Error(Pdf.Error.Reason.CorruptStream(t"FlateDecode"))))

  private def inflate(data: Data, nowrap: Boolean): Optional[Data] =
    val builder = DataBuilder()

    try
      val chunks =
        if nowrap then Chain(data).decompress[Deflate] else Chain(data).decompress[Zlib]

      // Forcing the stream incrementally means a truncated (but valid-so-far) input keeps
      // whatever it decoded before the bytes ran out, matching the eager inflater's
      // partial-on-truncation behaviour; corrupt data throws from the backend.
      chunks.each: chunk =>
        builder.addAll(chunk)
    catch case _: IllegalStateException => ()

    val result = builder.result()
    if result.length == 0 && data.length > 0 then Unset else result

  private def asciiHex(data: Data)(using Tactic[Pdf.Error]): Data =
    val bytes = DataBuilder()
    var high = -1
    var done = false

    data.survey: surveyor =>
      while !done && surveyor.more do
        surveyor.next(()): element =>
          val byte = element & 0xff

          if byte == '>' then done = true
          else if !CosLexer.whitespace(byte) then
            val value = CosLexer.hexadecimal(byte)

            if value < 0 then abort(Pdf.Error(Pdf.Error.Reason.CorruptStream(t"ASCIIHexDecode")))
            else if high < 0 then high = value
            else
              bytes += ((high << 4) + value).toByte
              high = -1

    if high >= 0 then bytes += (high << 4).toByte
    bytes.result()

  private def runLength(data: Data)(using Tactic[Pdf.Error]): Data =
    val bytes = DataBuilder()
    var done = false

    data.survey: surveyor =>
      while !done && surveyor.more do
        surveyor.next(()): element =>
          val length = element & 0xff

          if length == 128 then done = true
          else if length < 128 then
            // A literal run of `length + 1` bytes: `take` clamps at exhaustion, so a short
            // read means the stream is corrupt.
            val run = surveyor.take(length + 1)

            if (run: Interval).size <= length
            then abort(Pdf.Error(Pdf.Error.Reason.CorruptStream(t"RunLengthDecode")))

            data.iterate(run) { index => bytes += data.at(index) }
          else
            // One byte, repeated `257 - length` times.
            surveyor.next(abort(Pdf.Error(Pdf.Error.Reason.CorruptStream(t"RunLengthDecode")))):
              byte =>
                var j = 0

                while j < 257 - length do
                  bytes += byte
                  j += 1

    bytes.result()

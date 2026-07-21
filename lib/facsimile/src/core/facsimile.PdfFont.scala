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
import phoenicia.*
import rudiments.*
import vacuous.*

object PdfFont:
  enum Standard:
    case Helvetica, HelveticaBold, HelveticaOblique, HelveticaBoldOblique,
        TimesRoman, TimesBold, TimesItalic, TimesBoldItalic,
        Courier, CourierBold, CourierOblique, CourierBoldOblique, Symbol, ZapfDingbats

  // Everything the variants share, materialized at load time so a `PdfFont` is a pure value
  // that outlives the `open` scope.
  private[facsimile] case class Common
    ( baseFont:     Text,
      standard:     Optional[Standard],
      firstChar:    Int,
      widths:       IArray[Double],
      cidWidths:    Map[Int, Double],
      defaultWidth: Double,
      encoding:     Optional[IArray[Char]],
      differences:  Map[Int, Text],
      toUnicode:    Optional[CharMap],
      embedded:     Optional[Ttf],
      twoByte:      Boolean,
      descriptor:   Map[Text, Cos] )

  // Builds a font from its dictionary; anything unrecognizable is `Unset` rather than an
  // error, since fonts are consulted opportunistically during extraction.
  private[facsimile] def read(value: Cos)(using pdf: Pdf): Optional[PdfFont] raises PdfError =
    value.dictionary.let: entries =>
      val subtype = entries.at(t"Subtype").let(pdf.resolved(_).name).or(t"")
      val baseFont = entries.at(t"BaseFont").let(pdf.resolved(_).name).or(t"")
      val standard = StandardFonts.recognize(baseFont)

      val descriptor = pdf.resolved(entries.at(t"FontDescriptor").or(Cos.Nil))
        . dictionary.or(Map[Text, Cos]())

      val defaultWidth = descriptor.at(t"MissingWidth").let(pdf.resolved(_).double).or(0.0)
      val firstChar = entries.at(t"FirstChar").let(pdf.resolved(_).long).or(0L).toInt

      val widths: IArray[Double] =
        pdf.resolved(entries.at(t"Widths").or(Cos.Nil)).elements.lay(IArray.empty[Double]):
          elements => IArray.from(elements.stdlib.map(pdf.resolved(_).double.or(0.0)))

      val embedded: Optional[Ttf] =
        val program = descriptor.at(t"FontFile2").or:
          descriptor.at(t"FontFile3").let: value =>
            val body = pdf.resolved(value)
            val subtype = body.dictionary.or(Map[Text, Cos]()).at(t"Subtype").let(_.name)
            if subtype == t"OpenType" then value else Unset

        program.let(pdf.resolved(_)).let:
          case body: Cos.Body => safely(Ttf(pdf.payload(body)))
          case _              => Unset

      val toUnicode: Optional[CharMap] =
        pdf.resolved(entries.at(t"ToUnicode").or(Cos.Nil)) match
          case body: Cos.Body => safely(CharMap.parse(pdf.payload(body)))
          case _              => Unset

      // The `/Encoding` entry: a base name, or a dictionary of a base name plus differences.
      def encodingTable(name: Optional[Text]): Optional[IArray[Char]] = name.let:
        case t"WinAnsiEncoding"  => PdfEncoding.winAnsi
        case t"MacRomanEncoding" => PdfEncoding.macRoman
        case t"StandardEncoding" => PdfEncoding.standard
        case _                   => Unset

      val encodingValue = pdf.resolved(entries.at(t"Encoding").or(Cos.Nil))

      val encoding: Optional[IArray[Char]] = encodingValue match
        case Cos.Name(name)          => encodingTable(name)
        case dictionary: Cos.Dictionary => encodingTable(dictionary(t"BaseEncoding").let(_.name))
        case _                       => Unset

      val differences: Map[Int, Text] = encodingValue match
        case dictionary: Cos.Dictionary =>
          pdf.resolved(dictionary(t"Differences").or(Cos.Nil)).elements.lay(Map[Int, Text]()):
            elements =>
              var code = 0
              val builder = scala.collection.immutable.Map.newBuilder[Int, Text]

              elements.each:
                case Cos.Integral(value) =>
                  code = value.toInt

                case Cos.Name(name) =>
                  builder += code ->
                    PdfEncoding.glyph(name).lay(t"�")(_.toString.tt)
                  code += 1

                case _ =>
                  ()

              Map.of(builder.result())

        case _ =>
          Map()

      def common(twoByte: Boolean, cidWidths: Map[Int, Double], default: Double) = Common
        ( baseFont, standard, firstChar, widths, cidWidths, default, encoding, differences,
          toUnicode, embedded, twoByte, descriptor )

      subtype.s match
        case "Type1"   => Type1(common(false, Map(), defaultWidth))
        case "MMType1" => MmType1(common(false, Map(), defaultWidth))
        case "TrueType" => TrueType(common(false, Map(), defaultWidth))

        case "Type3" =>
          val matrix = pdf.resolved(entries.at(t"FontMatrix").or(Cos.Nil)).elements
            . lay(PdfMatrix(0.001, 0, 0, 0.001, 0, 0)): elements =>
                elements.map(pdf.resolved(_).double.or(0.0)) match
                  case List(a, b, c, d, e, f) => PdfMatrix(a, b, c, d, e, f)
                  case _                      => PdfMatrix(0.001, 0, 0, 0.001, 0, 0)

          // Type 3 widths are in glyph space; normalize them to thousandths of an em.
          val scaled = IArray.from(scala.collection.immutable.ArraySeq.unsafeWrapArray(widths.mutable(using Unsafe)).map(_*matrix.a*1000))
          Type3(matrix, common(false, Map(), defaultWidth).copy(widths = scaled))

        case "Type0" =>
          val descendant = pdf.resolved(entries.at(t"DescendantFonts").or(Cos.Nil)).elements
            . lay(Map[Text, Cos]()): elements =>
                elements match
                  case List(first) => pdf.resolved(first).dictionary.or(Map[Text, Cos]())
                  case _           => Map[Text, Cos]()

          val cidDescriptor = pdf.resolved(descendant.at(t"FontDescriptor").or(Cos.Nil))
            . dictionary.or(Map[Text, Cos]())

          val cidEmbedded: Optional[Ttf] =
            cidDescriptor.at(t"FontFile2").or(cidDescriptor.at(t"FontFile3"))
            . let(pdf.resolved(_)).let:
                case body: Cos.Body => safely(Ttf(pdf.payload(body)))
                case _              => Unset

          val defaultCid = descendant.at(t"DW").let(pdf.resolved(_).double).or(1000.0)
          val cidWidths = cidWidthArray(descendant.at(t"W"))

          Type0:
            Common
              ( baseFont, standard, 0, IArray.empty[Double], cidWidths, defaultCid, encoding,
                differences, toUnicode, cidEmbedded, twoByte = true, cidDescriptor )

        case _ =>
          Unset

  // `/W` (ISO 32000-2 §9.7.4.3): `start [w w ...]` lists consecutive widths; `start end w`
  // spans a range.
  private def cidWidthArray(value: Optional[Cos])(using pdf: Pdf)
  :   Map[Int, Double] raises PdfError =

    pdf.resolved(value.or(Cos.Nil)).elements.lay(Map[Int, Double]()): elements =>
      val builder = scala.collection.immutable.Map.newBuilder[Int, Double]

      def recur(elements: List[Cos]): Unit = elements match
        case Cos.Integral(start) :: Cos.Sequence(widths) :: rest =>
          widths.stdlib.zipWithIndex.each: (width, index) =>
            pdf.resolved(width).double.let(builder += (start.toInt + index) -> _)

          recur(rest)

        case Cos.Integral(start) :: Cos.Integral(end) :: width :: rest =>
          pdf.resolved(width).double.let: value =>
            var cid = start.toInt

            while cid <= end.toInt do
              builder += cid -> value
              cid += 1

          recur(rest)

        case _ =>
          ()

      recur(elements.map(pdf.resolved(_)))
      Map.of(builder.result())

// A font as a page's resources declare it (ISO 32000-2 §9): a pure, fully-materialized
// value. Embedded TrueType and OpenType programs surface as phoenicia `Ttf`s; `decode`
// maps show-text operands to Unicode as well as the file allows, preferring the font's own
// `/ToUnicode` map, then its declared encoding and differences.
enum PdfFont:
  case Type1(common: PdfFont.Common)
  case MmType1(common: PdfFont.Common)
  case TrueType(common: PdfFont.Common)
  case Type3(matrix: PdfMatrix, common: PdfFont.Common)
  case Type0(common: PdfFont.Common)

  private def common: PdfFont.Common = this match
    case Type1(common)    => common
    case MmType1(common)  => common
    case TrueType(common) => common
    case Type3(_, common) => common
    case Type0(common)    => common

  def baseFont: Text = common.baseFont
  def standard: Optional[PdfFont.Standard] = common.standard
  def embedded: Optional[Ttf] = common.embedded
  def descriptor: Map[Text, Cos] = common.descriptor

  // The advance of a code, in thousandths of an em.
  def width(code: Int): Double = this match
    case Type0(common) =>
      common.cidWidths.at(code).or(common.defaultWidth)

    case _ =>
      val index = code - common.firstChar

      if index >= 0 && index < common.widths.length && common.widths(index) > 0
      then common.widths(index)
      else common.standard.lay(or(code)) { standard => StandardFonts.width(standard, code) }

  private def or(code: Int): Double = if common.defaultWidth > 0 then common.defaultWidth else 500

  // Word spacing applies only to the single-byte code 32 (ISO 32000-2 §9.3.3).
  private[facsimile] def wordBoundary(code: Int): Boolean = !common.twoByte && code == 32

  // The byte codes of a show-text operand: single bytes, or big-endian pairs for composite
  // fonts (Identity ordering, the overwhelming norm).
  def codes(string: Data): List[Int] =
    if common.twoByte then
      List.range(0, string.length/2).map: index =>
        ((string(index*2) & 0xff) << 8) | (string(index*2 + 1) & 0xff)
    else string.transmute[List].map(_.toInt & 0xff)

  def decode(string: Data): Text =
    val builder = StringBuilder()

    codes(string).each: code =>
      val mapped: Optional[Text] = common.toUnicode.let(_(code)).or:
        common.differences.at(code).or:
          common.encoding.let: table =>
            if code >= 0 && code < table.length then table(code).toString.tt else Unset

      val fallback: Text =
        if !common.twoByte && code >= 32 && code <= 126 then code.toChar.toString.tt else t"�"

      builder.append(mapped.or(fallback).s)

    builder.toString.tt

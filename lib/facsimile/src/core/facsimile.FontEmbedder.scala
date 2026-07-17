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

import java.security as js

import anticipation.*
import contingency.*
import gossamer.*
import hypotenuse.*
import phoenicia.*
import rudiments.*
import vacuous.*

// Embeds a TrueType (or OpenType) font program as a simple, single-byte PDF font with WinAnsi
// encoding: the program becomes a `FontFile2` stream, referenced by a `FontDescriptor` built
// from the font's own tables — bounding box, italic angle, cap height, pitch and style — with
// per-character widths scaled from the font's metrics to the 1000-unit glyph space PDF
// expects. When subset text is given, only the glyphs it needs are embedded, under the
// conventional six-letter subset tag. An unparseable program still embeds, described by
// standard fallback metrics.
private[facsimile] object FontEmbedder:
  def embed(pdf: Pdf, ttf: Ttf, name: Optional[Text], subset: Optional[Text])
  :   Cos.Ref raises PdfError =

    val program = subset.lay(ttf): chars =>
      safely(ttf.subset(chars)).or(ttf)

    val baseName = name.or(postScriptName(ttf)).or(t"Embedded")

    val baseFont = subset.lay(baseName): chars =>
      t"${tag(baseName, chars)}+$baseName"

    val unitsPerEm = safely(program.head.unitsPerEm.int).or(1000).max(1)
    def scaled(units: Int): Long = units.toLong*1000/unitsPerEm

    val fontFile =
      pdf.allocate:
        pdf.newBody(Map(t"Length1" -> Cos.Integral(program.data.length.toLong)), program.data)

    val ascent = safely(scaled(program.hhea.ascender.int)).or(750L)
    val descent = safely(scaled(program.hhea.descender.int)).or(-250L)
    val italicAngle = safely(program.post.italicAngle).or(0.0)
    val monospaced = safely(program.post.monospaced).or(false)
    val capHeight = safely(program.os2.capHeight).let(scaled(_)).or(ascent)

    val boundingBox =
      safely:
        val head = program.head

        integers(scaled(head.xMin.int), scaled(head.yMin.int), scaled(head.xMax.int),
            scaled(head.yMax.int))

      . or(integers(-200, -250, 1100, 1000))

    // Nonsymbolic, plus fixed-pitch and italic when the font declares them.
    val flags = 32L | (if monospaced then 1L else 0L) | (if italicAngle != 0.0 then 64L else 0L)

    val descriptor =
      pdf.allocate:
        Cos.Dictionary:
          Map
            ( t"Type"        -> Cos.Name(t"FontDescriptor"),
              t"FontName"    -> Cos.Name(baseFont),
              t"Flags"       -> Cos.Integral(flags),
              t"FontBBox"    -> boundingBox,
              t"ItalicAngle" -> Cos.Real(italicAngle),
              t"Ascent"      -> Cos.Integral(ascent),
              t"Descent"     -> Cos.Integral(descent),
              t"CapHeight"   -> Cos.Integral(capHeight),
              t"StemV"       -> Cos.Integral(80),
              t"FontFile2"   -> fontFile )

    // Widths for codes 32–255 under WinAnsi: each code's glyph advance, or 0 where the font
    // has no glyph for it.
    val widths = (32 to 255).to(List).map: code =>
      val char = PdfEncoding.winAnsi(code)

      if char == ' ' && code != 32 then Cos.Integral(0L)
      else Cos.Integral(width(program, scaled, char))

    pdf.allocate:
      Cos.Dictionary:
        Map
          ( t"Type"           -> Cos.Name(t"Font"),
            t"Subtype"        -> Cos.Name(t"TrueType"),
            t"BaseFont"       -> Cos.Name(baseFont),
            t"FirstChar"      -> Cos.Integral(32),
            t"LastChar"       -> Cos.Integral(255),
            t"Widths"         -> Cos.Sequence(widths),
            t"Encoding"       -> Cos.Name(t"WinAnsiEncoding"),
            t"FontDescriptor" -> descriptor )

  // The font's advance for a character, in glyph space; zero for one it does not map.
  private def width(ttf: Ttf, scaled: Int => Long, char: Char): Long =
    safely:
      val glyph = ttf.glyph(char)
      if glyph.id == 0 then 0L else scaled(ttf.hmtx.advanceWidth(glyph.id))

    . or(0L)

  // A PostScript name contains no spaces, though some fonts' naming tables do.
  private def postScriptName(ttf: Ttf): Optional[Text] =
    ttf.fontName.let(_.s.replace(" ", "").nn.tt)

  // The conventional six-uppercase-letter subset tag, derived deterministically from the
  // name and subset text, so identical subsets embed identically.
  private def tag(name: Text, chars: Text): Text =
    val md5 = js.MessageDigest.getInstance("MD5").nn
    val digest = md5.digest(t"$name:$chars".s.getBytes("UTF-8")).nn

    val letters = (0 until 6).map: index =>
      ('A' + ((digest(index) & 0xff)%26)).toChar

    String(letters.toArray).tt

  private def integers(values: Long*): Cos = Cos.Sequence(values.to(List).map(Cos.Integral(_)))

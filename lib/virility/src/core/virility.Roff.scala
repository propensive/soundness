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
package virility

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object Roff:
  given showable: Roff is Showable = _.serialize
  given encodable: Roff is Encodable in Text = _.serialize

  // Escaping is unconditional: every `-` becomes the hyphen-minus escape `\-` so that options
  // and command names remain searchable and copy-pasteable in rendered output, and `\` becomes
  // `\[rs]`. A newline within a logical line would let subsequent text be misread as a roff
  // control line, so newlines become spaces; the line-start cases that remain (`.` and `'`)
  // are protected with `\&` in `line`, which every serialized text line passes through.
  // Quoted macro arguments (`.TH`/`.SH` titles, dates) escape `"`, which delimits them, but
  // not `-`: hyphens there are prose, and an escaped hyphen in the `.TH` date defeats
  // mandoc's date parsing.
  def escape(text: Text): Text = escape(text, quotable = false)

  private def escape(text: Text, quotable: Boolean): Text =
    val builder = StringBuilder()

    text.s.foreach:
      case '\\'             => builder.append("\\[rs]")
      case '-' if !quotable => builder.append("\\-")
      case '\n'             => builder.append(' ')
      case '"' if quotable  => builder.append("\\[dq]")
      case other            => builder.append(other)

    builder.toString.nn.tt

  def quote(text: Text): Text = t"\"${escape(text, quotable = true)}\""

  private def line(text: Text): Text =
    if text.starts(t".") || text.starts(t"'") then t"\\&$text" else text

  // Cons on the opaque `List` routes through the compat conversion to the stdlib `List`, so
  // sequences are assembled here by concatenation instead of `::`.
  private def concat[element](lists: List[element]*): List[element] =
    List.of(lists.toList.flatMap(_.stdlib))

  // A `.P` directly after `.SH`/`.SS` is redundant (mandoc lints it), so a section's leading
  // paragraph contributes only its text line.
  private def sectionBody(blocks: List[Block]): List[Text] = blocks match
    case Block.Paragraph(prose) :: rest =>
      concat(List(line(prose.map(_.serialize).join)), rest.flatMap(_.serialize))

    case other => other.flatMap(_.serialize)

  object Inline:
    def plain(text: Text): List[Inline] = List(Inline.Plain(text))
    def bold(text: Text): Inline = Inline.Bold(plain(text))
    def italic(text: Text): Inline = Inline.Italic(plain(text))

  enum Inline:
    case Plain(text: Text)
    case Bold(inlines: List[Inline])
    case Italic(inlines: List[Inline])

    def serialize: Text = this match
      case Plain(text)     => escape(text)
      case Bold(inlines)   => t"\\fB${inlines.map(_.serialize).join}\\fP"
      case Italic(inlines) => t"\\fI${inlines.map(_.serialize).join}\\fP"

  enum Block:
    case Section(title: Text, blocks: List[Block])
    case Subsection(title: Text, blocks: List[Block])
    case Paragraph(prose: List[Inline])
    case Tagged(tag: List[Inline], body: List[Inline])
    case Example(lines: List[Text])
    case Indented(blocks: List[Block])

    def serialize: List[Text] = this match
      case Section(title, blocks) =>
        concat(List(t".SH ${quote(title)}"), sectionBody(blocks))

      case Subsection(title, blocks) =>
        concat(List(t".SS ${quote(title)}"), sectionBody(blocks))

      case Paragraph(prose) => List(t".P", line(prose.map(_.serialize).join))

      case Example(lines) =>
        concat(List(t".EX"), lines.map { text => line(escape(text)) }, List(t".EE"))

      case Tagged(tag, body) =>
        List(t".TP", line(tag.map(_.serialize).join), line(body.map(_.serialize).join))

      case Indented(blocks) => concat(List(t".RS"), blocks.flatMap(_.serialize), List(t".RE"))

case class Roff
  ( title:   Text,
    section: Int,
    date:    Optional[Text]   = Unset,
    source:  Optional[Text]   = Unset,
    manual:  Optional[Text]   = Unset,
    blocks:  List[Roff.Block] = Nil ):

  def serialize: Text =
    val arguments =
      List(title.upper, section.show, date.or(t""), source.or(t""), manual.or(t""))
      . stdlib.reverse.dropWhile(_ == t"").reverse

    val header = Roff.concat(List(t".TH"), List.of(arguments).map(Roff.quote)).join(t" ")

    Roff.concat(List(header), blocks.flatMap(_.serialize)).join(t"", t"\n", t"\n")

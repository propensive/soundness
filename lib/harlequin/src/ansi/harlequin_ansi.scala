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
package harlequin

import escapade.*
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import iridescence.*
import prepositional.*
import spectacular.*
import symbolism.*
import vacuous.*

// A palette maps each accent to a colour, and nothing more. Whether a term or type token
// is a binding or a usage is carried by the token's `Role`, and any additional text
// styling (e.g. italicising bindings) is applied by a separate styling policy — not
// encoded here.
type ScalaSyntaxPalette = Palette:
  type Form = Srgb
  def scalaError: Color in Srgb
  def scalaNumber: Color in Srgb
  def scalaString: Color in Srgb
  def scalaTerm: Color in Srgb
  def scalaType: Color in Srgb
  def scalaKeyword: Color in Srgb
  def scalaSymbol: Color in Srgb
  def scalaParenthesis: Color in Srgb
  def scalaModifier: Color in Srgb
  def scalaComment: Color in Srgb
  def subdued: Color in Srgb
  def accented: Color in Srgb
  def margin: Color in Srgb

package syntaxHighlighting:
  import Accent.*

  given tokenTeletypeable: (palette: ScalaSyntaxPalette) => Token is Teletypeable =
    case Token(text, Error, _, _, _)    => e"${palette.scalaError}($text)"
    case Token(text, Number, _, _, _)   => e"${palette.scalaNumber}($text)"
    case Token(text, Modifier, _, _, _) => e"${palette.scalaModifier}($text)"
    case Token(text, Keyword, _, _, _)  => e"${palette.scalaKeyword}($text)"
    case Token(text, Term, _, _, _)     => e"${palette.scalaTerm}($text)"
    case Token(text, Typal, _, _, _)    => e"${palette.scalaType}($text)"
    case Token(text, String, _, _, _)   => e"${palette.scalaString}($text)"
    case Token(text, Parens, _, _, _)   => e"${palette.scalaParenthesis}($text)"
    case Token(text, Symbol, _, _, _)   => e"${palette.scalaSymbol}($text)"
    case Token(text, Unparsed, _, _, _) => e"${palette.scalaComment}($Italic($text))"

  given numberedTeletypeable: (palette: ScalaSyntaxPalette)
  =>  SourceCode is Teletypeable = source =>
    val indent = source.lastLine.show.length
    lazy val error = e"${Fg(palette.subdued)}(║)"

    val markup = source.focus.lay(e""): span =>
      val startLine = span.startLine.lay(0)(_.n0)
      val endLine = span.endLine.lay(startLine)(_.n0)
      val startColumn = span.startColumn.lay(0)(_.n0)
      val endColumn = span.endColumn.lay(startColumn)(_.n0)

      if startLine != endLine then e"\n" else
        val foreground = Fg(palette.scalaError)

        if startColumn == endColumn
        then e"\n${t" "*(startColumn + indent + 2)}$foreground(╱╲)"
        else e"\n${t" "*(startColumn + indent + 3)}$foreground(${t"‾"*(endColumn - startColumn)})"

    (source.offset to source.lastLine).map: lineNo =>
      val content = source(lineNo).map(_.teletype).join

      source.focus.mask: span =>
        val startLine = span.startLine.lay(0)(_.n0)
        val endLine = span.endLine.lay(startLine)(_.n0)
        startLine != endLine && lineNo > startLine && lineNo <= endLine + 1

      . let: focus =>
          val prefix = lineNo.show.pad(indent, Rtl)
          val bg = Bg(palette.margin)
          val fg = Fg(palette.accented)
          val divider = Fg(palette.subdued)
          e"$bg(${fg}($prefix)${divider}(┋))  $content"

      . or:
          val prefix = lineNo.show.pad(indent, Rtl)
          val bg = Bg(palette.margin)
          val fg = Fg(palette.accented)
          val divider = Fg(palette.subdued)
          e"$bg(${fg}($prefix)${divider}(┋)) $error$content"

    . join(e"", e"\n", markup)

  given unnumberedTeletypeable: ScalaSyntaxPalette => SourceCode is Teletypeable = source =>
    (source.offset to source.lastLine).map: lineNo =>
      source(lineNo).map(_.teletype).join

    . join(e"\n")

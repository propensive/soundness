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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import hieroglyph.*, textMetrics.uniform
import iridescence.*
import spectacular.*
import symbolism.*
import vacuous.*

type ScalaSyntaxPalette = Palette:
  def scalaError: Color
  def scalaNumber: Color
  def scalaString: Color
  def scalaIdentifier: Color
  def scalaTerm: Color
  def scalaType: Color
  def scalaKeyword: Color
  def scalaSymbol: Color
  def scalaParenthesis: Color
  def scalaModifier: Color
  def scalaComment: Color
  def subdued: Color
  def accented: Color
  def margin: Color

package syntaxHighlighting:
  import Accent.*

  given teletypeable: (palette: ScalaSyntaxPalette) => Token is Teletypeable =
    case Token(text, Error)    => e"${palette.scalaError}($text)"
    case Token(text, Number)   => e"${palette.scalaNumber}($text)"
    case Token(text, Modifier) => e"${palette.scalaModifier}($text)"
    case Token(text, Keyword)  => e"${palette.scalaKeyword}($text)"
    case Token(text, Ident)    => e"${palette.scalaIdentifier}($text)"
    case Token(text, Term)     => e"${palette.scalaTerm}($text)"
    case Token(text, Typed)    => e"${palette.scalaType}($text)"
    case Token(text, String)   => e"${palette.scalaString}($text)"
    case Token(text, Parens)   => e"${palette.scalaParenthesis}($text)"
    case Token(text, Symbol)   => e"${palette.scalaSymbol}($text)"
    case Token(text, Unparsed) => e"${palette.scalaComment}($Italic($text))"

  given numbered: (palette: ScalaSyntaxPalette) => SourceCode is Teletypeable = source =>
    val indent = source.lastLine.show.length
    lazy val error = e"${Fg(palette.subdued)}(║)"

    val markup = source.focus.lay(e""):
      case ((startLine, startColumn), (endLine, endColumn)) =>
        if startLine != endLine then e"\n" else
          val foreground = Fg(rgb"#ff0033")
          if startColumn == endColumn
          then e"\n${t" "*(startColumn + indent + 2)}$foreground(╱╲)"
          else e"\n${t" "*(startColumn + indent + 3)}$foreground(${t"‾"*(endColumn - startColumn)})"

    (source.offset to source.lastLine).map: lineNo =>
      val content = source(lineNo).map(_.teletype).join
      source.focus.mask:
        case ((startLine, _), (endLine, _)) =>
          startLine != endLine && lineNo > startLine && lineNo <= endLine + 1

      . let: focus =>
          val prefix = lineNo.show.pad(indent, Rtl)
          e"${Bg(palette.margin)}(${Fg(palette.accented)}($prefix)${Fg(palette.subdued)}(┋))  $content"

      . or:
          val prefix = lineNo.show.pad(indent, Rtl)
          e"${Bg(palette.margin)}(${Fg(palette.accented)}($prefix)${Fg(palette.subdued)}(┋)) $error$content"

    . join(e"", e"\n", markup)

  given unnumbered: ScalaSyntaxPalette => SourceCode is Teletypeable = source =>
    (source.offset to source.lastLine).map: lineNo =>
      source(lineNo).map(_.teletype).join

    . join(e"\n")

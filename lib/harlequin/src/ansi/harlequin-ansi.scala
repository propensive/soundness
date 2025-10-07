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
┃    Soundness, version 0.43.0.                                                                    ┃
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

package syntaxHighlighting:
  import Accent.*

  given teletypeable: SourceToken is Teletypeable =
    case SourceToken(text, Error)    => e"${rgb"#cc0033"}($text)"
    case SourceToken(text, Number)   => e"${rgb"#cc3366"}($text)"
    case SourceToken(text, Modifier) => e"${rgb"#ff9966"}($text)"
    case SourceToken(text, Keyword)  => e"${rgb"#ff6633"}($text)"
    case SourceToken(text, Ident)    => e"${rgb"#ffcc99"}($text)"
    case SourceToken(text, Term)     => e"${rgb"#ffcc33"}($text)"
    case SourceToken(text, Typed)    => e"${rgb"#00cc99"}($text)"
    case SourceToken(text, String)   => e"${rgb"#99ffff"}($text)"
    case SourceToken(text, Parens)   => e"${rgb"#cc6699"}($text)"
    case SourceToken(text, Symbol)   => e"${rgb"#cc3366"}($text)"
    case SourceToken(text, Unparsed) => e"${rgb"#2288aa"}($Italic($text))"

  given numbered: SourceCode is Teletypeable = source =>
    val indent = source.lastLine.show.length
    lazy val error = e"${rgb"#cc0033"}(║)"

    val markup = source.focus.lay(e""):
      case ((startLine, startColumn), (endLine, endColumn)) =>
        if startLine != endLine then e"\n" else
          val foreground = rgb"#ff0033"
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
          e"${Bg(rgb"#003333")}(${rgb"#99cc99"}($prefix)${rgb"#336666"}(┋))  $content"
      . or:
          val prefix = lineNo.show.pad(indent, Rtl)
          e"${Bg(rgb"#003333")}(${rgb"#99cc99"}($prefix)${rgb"#336666"}(┋)) $error$content"

    . join(e"", e"\n", markup)

  given unnumbered: SourceCode is Teletypeable = source =>
    (source.offset to source.lastLine).map: lineNo =>
      source(lineNo).map(_.teletype).join

    . join(e"\n")

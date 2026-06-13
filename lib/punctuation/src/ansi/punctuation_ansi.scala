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
package punctuation

import anticipation.*
import escapade.*
import frontier.*
import gossamer.*
import harlequin.*
import polysyllabic.*
import prepositional.*
import rudiments.*
import vacuous.*

// Code-block formattables for the terminal renderer. Each one tries to match
// the info-string against its claimed language and, on success, runs the
// content through Harlequin's highlighter and out via the `unnumbered` token
// stream. Callers supply a `ScalaSyntaxPalette` to colour the tokens.
package teletypeFormattables:
  given scala: (palette: ScalaSyntaxPalette) => ("scala" is TeletypeFormattable) =
    new TeletypeFormattable:
      type Self = "scala"

      def format(meta: List[Text], content: Text): Optional[Teletype] =
        if meta.prim != t"scala" then Unset
        else
          import syntaxHighlighting.unnumbered
          Scala.highlight(content).teletype

  given java: (palette: ScalaSyntaxPalette) => ("java" is TeletypeFormattable) =
    new TeletypeFormattable:
      type Self = "java"

      def format(meta: List[Text], content: Text): Optional[Teletype] =
        if meta.prim != t"java" then Unset
        else
          import syntaxHighlighting.unnumbered
          Java.highlight(content).teletype

extension (markdown: Markdown of Layout)
  def terminal
    ( width: Int = 80 )
    ( using hyphenation: Hyphenation,
            formattables: Every[TeletypeFormattable],
            palette: MarkdownPalette )
  :   Teletype =

    Renderer.render(markdown, width)

// Drives the markdown renderer from a `Termcap`. Reads the column width from
// `termcap.width` (defaulting to 80 when no width is supplied — the built-in
// `termcapDefinitions` leave the default `Int.MaxValue`, which would otherwise
// produce mile-wide thematic-break rules), renders to a `Teletype`, then
// emits ANSI escapes through the existing `Teletype is Printable` given.
given (Hyphenation, Every[TeletypeFormattable], MarkdownPalette)
=>  (Markdown of Layout) is Printable =

  (markdown, termcap) =>
    val width = if termcap.width >= Int.MaxValue then 80 else termcap.width
    Renderer.render(markdown, width).render(termcap)

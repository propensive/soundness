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
package delicious

import anticipation.*
import escapade.*
import gossamer.*
import harlequin.*
import stenography.*
import vacuous.*

import syntaxHighlighting.unnumberedTeletypeable

extension (message: SemanticMessage)
  /** Render the message as styled terminal text: embedded code samples are
   *  syntax-highlighted by harlequin, and types are re-rendered through
   *  stenography (abbreviated according to the given `Imports`) and then
   *  highlighted in type position. Anything that cannot be reified falls back
   *  to the compiler-printed text. */
  def teletype(reifier: Reifier)(using Imports, ScalaSyntaxPalette, Highlight): Teletype =
    // The compiler may have styled the shown text with ANSI escapes; harlequin
    // re-highlights from scratch, so they must not survive into the tokens.
    def ansiFree(text: Text): Text = text.s.replaceAll("\\u001B\\[[;\\d]*m", "").nn.tt

    def highlight(text: Text, context: Scala.Context): Teletype =
      unnumberedTeletypeable.teletype(Scala.highlight(ansiFree(text), context))

    def recur(nodes: List[Markup]): Teletype = nodes.map(node).join

    def node(markup: Markup): Teletype = markup match
      case Markup.Textual(text)  => e"${ansiFree(text)}"
      case Markup.Code(_, _)     => highlight(markup.plain, Scala.Context.Term)

      case typed@Markup.Typed(_, _, _, _) =>
        highlight(reifier.syntax(typed).let(_.text).or(typed.plain), Scala.Context.Type)

      case Markup.Symbolic(_, _, _, children) => recur(children)
      case Markup.Named(_, _, children)       => recur(children)
      case Markup.Spanned(_, _, children)     => recur(children)

    recur(message.markup)

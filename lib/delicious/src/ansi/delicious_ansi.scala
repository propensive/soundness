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
import rudiments.*
import stenography.*
import vacuous.*
import syntaxHighlighting.unnumberedTeletypeable

extension (message: SemanticMessage)
  // Not exported into `soundness`: `teletype` there is escapade's `Teletypeable`
  // rendering, used in a dozen modules. This one takes an explicit `Reifier`, and
  // every call site already spells it `delicious.teletype(…)`.
  @unexported
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
      case Markup.Tree(_, children)           => recur(children)

      // A node's own line is rewritten from what the node says, in the colours Frontier
      // gives it when it writes for a terminal itself; the lines which draw its children
      // are kept as they are, and the children themselves rendered in turn.
      case Markup.Node(role, name, note, repeated, children) =>
        val (mark, accent) = role match
          case Markup.Role.Resolving => (Chroma(0x870000), Chroma(0xff8700))
          case Markup.Role.Candidate => (Chroma(0xff8700), Chroma(0xffff5f))
          case Markup.Role.Requires  => (Chroma(0x870000), Chroma(0x87d787))
          case Markup.Role.Propose   => (Chroma(0x5fafff), Chroma(0x87d7ff))
          case Markup.Role.Prohibits => (Chroma(0xff8700), Chroma(0x87d787))
          case Markup.Role.Found     => (Chroma(0x00af00), Chroma(0x87ff5f))

        val remarks: Optional[Text] =
          if !repeated then note else note.lay(t"as above") { text => t"$text, as above" }

        val remark = remarks.lay(e""): text => e" ${Fg(Chroma(0x767676))}(($Italic($text)))"

        val line =
          e" ${Fg(mark)}($Bold(${role.glyph})) ${role.word} ${Fg(accent)}($Italic($name))$remark"

        val beneath: List[Markup] = children match
          case Markup.Textual(_) :: rest => rest
          case all                       => all

        e"$line${recur(beneath)}"

    recur(message.markup)

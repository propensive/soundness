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
┃    Soundness, version 0.41.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import digression.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import vacuous.*

object Renderable:
  import html5.Phrasing
  given showable: [value: Showable] => value is Renderable to Phrasing = value => List(value.show)

  given message: Message is Renderable to Phrasing = _.segments.flatMap:
    case message: Message => message.html
    case text: Text       => List(text)
    case _                => Nil


  given abstractable: [value: Abstractable across HtmlContent to List[Sgml]]
        => value is Renderable:
    type Self = value
    type Result = Label

    def html(value: value): Seq[Html[Result]] = value.generic.map(convert)

    private def convert(html: Sgml): Html[?] = html match
      case Sgml.Textual(text)               => text
      case Sgml.Comment(_)                  => "".tt
      case Sgml.ProcessingInstruction(_, _) => "".tt

      case Sgml.Element(label, attributes, children) =>
        Node(label, attributes.map(_.s -> _), children.map(convert(_)))

  given StackTrace is Renderable to html5.Flow = stackTrace =>
    import html5.*

    List:
      Div.stack(H2(stackTrace.component),
                H3(stackTrace.className),
                H4(stackTrace.message.html),
                Table(stackTrace.frames.map: frame =>
                      Tr(Td.at(Code(t"at")),
                      Td.`class`(Code(frame.method.className)),
                      Td.method(Code(frame.method.method)),
                      Td.file(Code(frame.file)),
                      Td(Code(t":")),
                      Td.line(Code(frame.line.let(_.show).or(t""))))))

trait Renderable extends Typeclass:
  type Result <: Label
  def html(value: Self): Seq[Html[Result]]

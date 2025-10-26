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
┃    Soundness, version 0.45.0.                                                                    ┃
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
package caduceus

import anticipation.*
import contingency.*
import fulminate.*
import gesticulate.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import turbulence.*
import urticose.*
import vacuous.*

import charEncoders.utf8

object Email:
  def apply[sendable: Sendable](value: sendable): Email = sendable.email(value)

  object Body:
    def apply(text: Text, html: Text): Body = Body.Alternatives(text, html)
    def apply(text: Text): Body = Body.TextOnly(text)

  enum Body:
    case TextOnly(content: Text)
    case HtmlOnly(content: Text)
    case Alternatives(textContent: Text, htmlContent: Text)

    def html: Optional[Text] = this match
      case Body.TextOnly(_)           => Unset
      case Body.HtmlOnly(html)        => html
      case Body.Alternatives(_, html) => html

    def text: Optional[Text] = this match
      case Body.TextOnly(text)        => text
      case Body.HtmlOnly(_)           => Unset
      case Body.Alternatives(text, _) => text

    def contentType: MediaType = this match
      case Body.TextOnly(_)        => media"text/plain"
      case Body.HtmlOnly(_)        => media"text/html"
      case Body.Alternatives(_, _) => media"multipart/alternative"

  case class Inline(cid: Text, contentType: MediaType, body: Stream[Text])

  case class Content(body: Body, inlines: Inline*):
    def contentType: MediaType =
      if !inlines.isEmpty then media"multipart/related" else body.contentType

  case class Message(content: Content, attachments: List[Asset] = Nil):
    def contentType: MediaType =
      if !attachments.isEmpty then media"multipart/mixed" else content.contentType

case class Email(headers: Map[Text, Text], message: Email.Message):
  def html: Optional[Text] = message.content.body.html
  def text: Optional[Text] = message.content.body.text
  def inlines: List[Email.Inline] = message.content.inlines.to(List)
  def attachments: List[Asset] = message.attachments.to(List)

  def attach[attachable: Attachable](attachment: attachable): Email =
    copy(message = message.copy(attachments = attachments :+ attachable.attachment(attachment)))

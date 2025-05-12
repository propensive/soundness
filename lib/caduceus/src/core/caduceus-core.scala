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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import nettlesome.*
import prepositional.*
import proscenium.*
import vacuous.*

object Email:
  def apply[sendable: Sendable](value: sendable): Email = sendable.email(value)

case class Email
            (headers:     Map[Text, Text],
             text:        Optional[Text],
             html:        Optional[Text],
             content:     Stream[Bytes],
             attachments: List[Attachment])

object Envelope:
  def many[entity: Distinct from List[?]](value: entity | List[entity]): List[entity] = value match
    case many: List[`entity` @unchecked] => many
    case one: (`entity` @unchecked)      => List(one)

  def apply[sendable: Sendable]
       (email:   sendable,
        to:      EmailAddress | List[EmailAddress],
        cc:      EmailAddress | List[EmailAddress],
        bcc:     EmailAddress | List[EmailAddress],
        replyTo: EmailAddress | List[EmailAddress],
        subject: Text)
       (using courier: Courier, sender: Sender): Envelope =
    Envelope
     (sender.email,
      many(to),
      many(cc),
      many(bcc),
      many(replyTo),
      subject,
      sendable.email(email),
      Nil)

case class Envelope
            (from:        EmailAddress,
             to:          List[EmailAddress],
             cc:          List[EmailAddress],
             bcc:         List[EmailAddress],
             replyTo:     List[EmailAddress],
             subject:     Text,
             email:       Email,
             attachments: List[Attachment])

case class CourierError(from: EmailAddress, to: EmailAddress, subject: Text)(using Diagnostics)
extends Error(m"unable to send email from $from to $to with subject $subject")

case class Attachment
            (filename:    Text,
             contentType: MediaType,
             attachment:  Boolean,
             contentId:   Optional[Text],
             data:        Stream[Bytes],
             headers:     Map[Text, Text])

object Sendable:
  given text: Text is Sendable = text => Email(Map(), text, Unset, Stream(), Nil)

trait Sendable:
  type Self
  def email(content: Self): Email

case class Sender(email: EmailAddress)

extension [sendable: Sendable](email: sendable)
  def send
       (to:      EmailAddress | List[EmailAddress],
        cc:      EmailAddress | List[EmailAddress] = Nil,
        bcc:     EmailAddress | List[EmailAddress] = Nil,
        replyTo: EmailAddress | List[EmailAddress] = Nil,
        subject: Text)
       (using courier: Courier, sender: Sender)
  :     courier.Result =

   courier.send(Envelope(email, to, cc, bcc, replyTo, subject))


trait Courier:
  type Result
  def send(envelope: Envelope): Result

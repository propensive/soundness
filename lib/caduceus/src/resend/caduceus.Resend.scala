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
import hieroglyph.*
import jacinta.*
import merino.*
import nettlesome.*
import prepositional.*
import proscenium.*
import telekinesis.*
import turbulence.*
import vacuous.*

import charEncoders.utf8
import jsonPrinters.minimal
import errorDiagnostics.stackTraces
import stdioSources.virtualMachine.ansi

object Resend:
  case class ApiKey(key: Text)
  case class Receipt(id: Text)

package couriers:
  given resend: (Tactic[CourierError], Online, HttpEvent is Loggable, HttpClient)
        => (apiKey: Resend.ApiKey)
        => Courier:
    type Result = Resend.Receipt

    private case class Request
                        (from:         EmailAddress,
                         to:           List[EmailAddress],
                         subject:      Text,
                         bcc:          List[EmailAddress],
                         cc:           List[EmailAddress],
                         scheduled_at: Optional[Text],
                         replyTo:      List[EmailAddress],
                         headers:      Map[Text, Text],
                         html:         Optional[Text],
                         text:         Optional[Text])

    def send(envelope: Envelope): Resend.Receipt =
      val request =
        Request
         (envelope.from,
          envelope.to,
          envelope.subject,
          envelope.bcc,
          envelope.cc,
          Unset,
          envelope.replyTo,
          envelope.email.headers,
          envelope.email.html,
          envelope.email.text)

      mitigate:
        case ConnectError(reason) =>
          Out.println(reason.communicate)
          CourierError(envelope.from, envelope.to.head, envelope.subject)

        case JsonParseError(_, _, reason) =>
          Out.println(reason.communicate)
          CourierError(envelope.from, envelope.to.head, envelope.subject)

        case HttpError(status, _) =>
          Out.println(status.communicate)
          CourierError(envelope.from, envelope.to.head, envelope.subject)

        case JsonError(reason) =>
          Out.println(reason.communicate)
          CourierError(envelope.from, envelope.to.head, envelope.subject)

        case MediaTypeError(_, _) =>
          CourierError(envelope.from, envelope.to.head, envelope.subject)

      . within:
          url"https://api.resend.com/emails".submit
           (Http.Post, authorization = Auth.Bearer(apiKey.key))
           (request.json)
          . receive[Json]
          . as[Resend.Receipt]

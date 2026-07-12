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
package caduceus

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import gesticulate.*
import hieroglyph.*
import jacinta.*
import monotonous.*
import prepositional.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import alphabets.base64Standard
import charEncoders.utf8Encoder
import environments.javaEnvironment
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import httpBackends.virtualMachine
import stdios.virtualMachineStdio
import termcaps.environmentTermcap

package couriers:
  given resend: (Tactic[CourierError], Online, HttpEvent is Loggable, HttpClient)
  =>  ( apiKey: Resend.ApiKey )
  =>  Courier:

    type Result = Resend.Receipt

    private case class Attachment(filename: Text, content: Text)

    private case class Request
      ( from:         EmailAddress,
        to:           List[EmailAddress],
        subject:      Text,
        bcc:          List[EmailAddress],
        cc:           List[EmailAddress],
        scheduled_at: Optional[Text],
        replyTo:      List[EmailAddress],
        headers:      Map[Text, Text],
        html:         Optional[Text],
        text:         Optional[Text],
        attachments:  List[Attachment] )

    def send(message: Document[Email]): Resend.Receipt =
      val email = message.root
      val envelope = message.metadata

      val attachments = email.attachments.map: attachment =>
        Attachment(attachment.name, attachment.stream.read[Data].serialize[Base64])

      val request =
        Request
          ( envelope.from,
            envelope.to,
            envelope.subject,
            envelope.bcc,
            envelope.cc,
            Unset,
            envelope.replyTo,
            email.headers,
            email.html,
            email.text,
            attachments )

      def error = CourierError(envelope.from, envelope.to.head, envelope.subject)

      mitigate:
        case ConnectError(reason)     => Out.println(reason.communicate) yet error
        case ParseError(_, _, reason) => Out.println(reason.describe) yet error
        case HttpError(status, _)     => Out.println(status.communicate) yet error
        case JsonError(reason)        => Out.println(reason.communicate) yet error
        case MediaTypeError(_, _)     => error

      . protect:
          // Sealed locally: the derivation's field summons expect pure evidence
          // (the derivation-boundary blockage; see rep/DECISIONS.md, honest
          // codec capabilities).
          given (Text is Json.Decodable) = caps.unsafe.unsafeAssumePure(Json.text)

          url"https://api.resend.com/emails".submit
            ( Http.Post, authorization = Auth.Bearer(apiKey.key) )
            ( request.in[Json] )

          . receive[Json]
          . as[Resend.Receipt]

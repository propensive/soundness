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
package caduceus

import soundness.*

import errorDiagnostics.stackTracesDiagnostics

class TestCourier() extends Courier:
  type Result = Unit

  var emails: List[Email] = Nil
  var envelopes: List[Envelope] = Nil

  def send(message: Document[Email]): Unit =
    emails = message.root :: emails
    envelopes = message.metadata :: envelopes

case class Report(title: Text, body: Text)

object Tests extends Suite(m"Caduceus tests"):
  def run(): Unit =
    val jack = unsafely(EmailAddress.parse(t"jack@example.com"))
    val jill = unsafely(EmailAddress.parse(t"jill@example.com"))
    val jane = unsafely(EmailAddress.parse(t"jane@example.com"))

    suite(m"Email body tests"):
      test(m"A text-only body has text but no HTML"):
        val body = Email.Body(t"hello")
        (body.text, body.html)
      . assert(_ == (t"hello", Unset))

      test(m"An HTML-only body has HTML but no text"):
        val body = Email.Body.HtmlOnly(t"<p>hello</p>")
        (body.text, body.html)
      . assert(_ == (Unset, t"<p>hello</p>"))

      test(m"An alternatives body has both text and HTML"):
        val body = Email.Body(t"hello", t"<p>hello</p>")
        (body.text, body.html)
      . assert(_ == (t"hello", t"<p>hello</p>"))

      test(m"A text-only body is plain text"):
        Email.Body(t"hello").contentType
      . assert(_ == media"text/plain")

      test(m"An HTML-only body is HTML"):
        Email.Body.HtmlOnly(t"<p>hello</p>").contentType
      . assert(_ == media"text/html")

      test(m"An alternatives body is multipart"):
        Email.Body(t"hello", t"<p>hello</p>").contentType
      . assert(_ == media"multipart/alternative")

    suite(m"Email content tests"):
      test(m"Content without inlines has the body's content type"):
        Email.Content(Email.Body(t"hello")).contentType
      . assert(_ == media"text/plain")

      test(m"Content with an inline is related multipart"):
        val inline = Email.Inline(t"cid1", media"image/png", LazyList())
        Email.Content(Email.Body(t"hello"), inline).contentType
      . assert(_ == media"multipart/related")

      test(m"A message without attachments has the content's type"):
        Email.Message(Email.Content(Email.Body(t"hello"))).contentType
      . assert(_ == media"text/plain")

      test(m"A message with an attachment is mixed multipart"):
        val asset = Asset(t"report.txt", media"text/plain", LazyList())
        Email.Message(Email.Content(Email.Body(t"hello")), List(asset)).contentType
      . assert(_ == media"multipart/mixed")

    suite(m"Sendable tests"):
      test(m"Text becomes a plain-text email"):
        Email(t"hello").text
      . assert(_ == t"hello")

      test(m"Text becomes an email with no HTML"):
        Email(t"hello").html
      . assert(_ == Unset)

      test(m"An email is sendable as itself"):
        val email = Email(t"hello")
        Email(email)
      . assert(_ == Email(t"hello"))

      test(m"An email starts with no headers"):
        Email(t"hello").headers
      . assert(_ == Map())

      test(m"An email starts with no attachments"):
        Email(t"hello").attachments
      . assert(_ == Nil)

      test(m"An email starts with no inlines"):
        Email(t"hello").inlines
      . assert(_ == Nil)

      test(m"Contramap a Sendable onto another type"):
        val sendable = Sendable.text.contramap[Report](_.body)
        sendable.email(Report(t"Q3", t"all good")).text
      . assert(_ == t"all good")

    suite(m"Attachment tests"):
      val asset = Asset(t"report.txt", media"text/plain", LazyList())

      test(m"An Asset attaches as itself"):
        Attachable.asset.attachment(asset)
      . assert(_ == asset)

      test(m"Attaching adds the asset to the email"):
        Email(t"hello").attach(asset).attachments
      . assert(_ == List(asset))

      test(m"Attaching twice keeps both assets in order"):
        val other = Asset(t"data.csv", media"text/csv", LazyList())
        Email(t"hello").attach(asset).attach(other).attachments.map(_.name)
      . assert(_ == List(t"report.txt", t"data.csv"))

      test(m"Attaching does not change the body"):
        Email(t"hello").attach(asset).text
      . assert(_ == t"hello")

      test(m"Contramap an Attachable onto another type"):
        val attachable = Attachable.asset.contramap[Report]: report =>
          Asset(report.title, media"text/plain", LazyList())

        attachable.attachment(Report(t"Q3", t"all good")).name
      . assert(_ == t"Q3")

    suite(m"Envelope tests"):
      test(m"A single recipient becomes a one-element list"):
        Envelope.many[EmailAddress](jack)
      . assert(_ == List(jack))

      test(m"A list of recipients is kept as it is"):
        Envelope.many[EmailAddress](List(jack, jill))
      . assert(_ == List(jack, jill))

      test(m"An empty list of recipients stays empty"):
        Envelope.many[EmailAddress](Nil)
      . assert(_ == Nil)

    suite(m"Sending tests"):
      test(m"Sending records the subject"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = jill)
        courier.envelopes.head.subject
      . assert(_ == t"Greetings")

      test(m"Sending records the sender"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = jill)
        courier.envelopes.head.from
      . assert(_ == jack)

      test(m"Sending records a single recipient"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = jill)
        courier.envelopes.head.to
      . assert(_ == List(jill))

      test(m"Sending records several recipients"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = List(jill, jane))
        courier.envelopes.head.to
      . assert(_ == List(jill, jane))

      test(m"Copied and blind-copied recipients are recorded"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = jill, cc = jane, bcc = jack)
        val envelope = courier.envelopes.head
        (envelope.cc, envelope.bcc)
      . assert(_ == (List(jane), List(jack)))

      test(m"Recipients default to empty lists"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = jill)
        val envelope = courier.envelopes.head
        (envelope.cc, envelope.bcc, envelope.replyTo)
      . assert(_ == (Nil, Nil, Nil))

      test(m"The sent email carries the body"):
        given courier: TestCourier = TestCourier()
        given sender: Sender = Sender(jack)
        t"hello".send(subject = t"Greetings", to = jill)
        courier.emails.head.text
      . assert(_ == t"hello")

    suite(m"Error message tests"):
      test(m"A courier error names both parties and the subject"):
        val error = CourierError(jack, jill, t"Greetings")
        error.message.text
      . assert(_ == t"unable to send email from jack@example.com to jill@example.com with "+
          t"subject Greetings")

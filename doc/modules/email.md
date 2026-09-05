## Email

### About

An email is composed from typed parts — a body in text or HTML or both, inline content, attachments
— and sent through a pluggable *courier*. The recipient addresses are the validated
[email addresses](network-addresses.md) used everywhere else, so a malformed address cannot reach
the sending step; the MIME structure follows from what the message contains; and the transport is a
contextual value, so the same composition code sends through whichever delivery service is in
scope.

### On email

Assembling an email correctly means assembling MIME: a message with both text and HTML is
`multipart/alternative`, one with attachments is `multipart/mixed`, inline images make it
`multipart/related`, and these nest. APIs that expose this structure directly make the common case
— send some text, maybe with an attachment — bear all of that weight; APIs that hide it entirely
make the structured cases impossible.

Soundness derives the structure from the content. A message states what it *is* — text, HTML,
alternatives, with attachments or not — and its content type follows; sending it is a single method
whose envelope names the sender, recipients and subject. Everything comes from the `soundness`
package, with a courier and a sender in scope:

```scala
import soundness.*

import couriers.resendCourier
import internetAccess.online
import strategies.throwUnsafely

given Sender = Sender(email"noreply@example.com")
given Resend.ApiKey = Resend.ApiKey(t"re_123456789")
```

A message assembled from typed parts, with its addresses validated as the code compiles, is [safety by construction](../philosophy/safety-by-construction.md) for mail.

### Sending

Anything *sendable* — text, an [HTML](html.md) document, or a fully-composed `Email` — sends with
`send`, giving the subject and recipients; `cc`, `bcc` and `replyTo` are optional, and each accepts
one address or a list:

```scala
def notify(): Resend.Receipt =
  t"Your order has shipped.".send
    ( subject = t"Shipping confirmation",
      to      = email"customer@example.com" )
```

An HTML document sends as an HTML message the same way, and failure to deliver raises a
`Courier.Error` naming the sender, recipient and subject at fault.

### Composing

A richer message is built as an `Email`: a body of text, HTML, or both — the both-form delivering
`multipart/alternative`, so capable clients show the HTML and others the text — with attachments
added by `attach`. An `Asset` is a named, typed source of bytes, its filename and
[media type](media-types.md) carried with it:

```scala
val textVersion = t"Sales rose 4% in August."
val htmlVersion = t"<p>Sales rose <b>4%</b> in August.</p>"
val report = Asset(t"report.csv", media"text/csv", Chain(t"month,sales\nAugust,104".in[Data]))

val message = Email(Map(), Email.Message(Email.Content(Email.Body(textVersion, htmlVersion))))
  . attach(report)

def distribute(recipients: List[EmailAddress]): Resend.Receipt =
  message.send(subject = t"Monthly report", to = recipients)
```

Plain text is itself sendable, so `Email(t"hello")` is the one-line form, and an email reports
its `text`, `html`, `attachments` and `inlines` back, so a composed message can be inspected
before it goes.

### Couriers

The courier is the delivery mechanism, supplied as a given. The provided courier delivers through
the [Resend](https://resend.com/) HTTP API, returning a receipt with the provider's message id;
another delivery service plugs in by implementing the one-method `Courier` trait, without touching
any composition code.

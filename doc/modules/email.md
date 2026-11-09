## Email

### About

An email is composed from typed parts: a body in text, or HTML, or both, with inline content
and attachments. It is sent through a pluggable *courier*.

The recipients' addresses are the validated
[email addresses](network-addresses.md#email-addresses) used everywhere else, so a malformed
address cannot reach the step of sending. The MIME structure follows from what the message
contains. The transport is a [contextual value], so the same code composes a message
whichever delivery service is in scope.

### On email

Assembling an email correctly means assembling MIME. Alternative bodies, inline images and
attachments each call for a different multipart type, and these nest. An API which exposes
this structure directly makes the common case, of sending some text, perhaps with an
attachment, bear all of that weight. An API which hides it entirely makes the structured
cases impossible.

Here the structure is derived from the content. A message states what it *is*: text, HTML or
both, with attachments or without. Its content type follows. Sending it is a single method,
whose arguments name the recipients and the subject. A message which is assembled from typed
parts, with its addresses validated as the code compiles, is
[safety by construction](../philosophy/safety-by-construction.md) for mail.

Everything comes from the `soundness` package. A courier and a sender are in scope:

```scala
import soundness.*

import codepages.utf8Codepage
import couriers.resendCourier
import httpBackends.javaNetHttp
import internetAccess.online
import strategies.throwUnsafely

given sender: Sender = Sender(email"noreply@example.com")
given apiKey: Resend.ApiKey = Resend.ApiKey(t"re_123456789")
```

The samples on this page would send real mail, so they are compiled but not run.

### Sending

Anything *sendable* is sent with `send`: text, an [HTML](html.md) document, or a fully
composed `Email`. `send` takes the fields of the envelope as parameters:

```tabular
column  Parameter
column  Takes
column  If omitted
row  `subject`  `Text`  required
row  `to`  one `EmailAddress` or a list  required
row  `cc`  one `EmailAddress` or a list  none
row  `bcc`  one `EmailAddress` or a list  none
row  `replyTo`  one `EmailAddress` or a list  none
```

A message to one recipient:

```scala
def notify(): Resend.Receipt =
  t"Your order has shipped.".send
    ( subject = t"Shipping confirmation",
      to      = email"customer@example.com" )
```

An HTML document is sent as an HTML message in the same way. A failure to deliver raises a
`Courier.Error`, which names the sender, the recipient and the subject at fault.

An address which is not valid never gets that far, since it does not compile:

```scala fuel
compile
errors show
##
email"customer.example.com"
```

### Composing

A richer message is built as an `Email`. Its body is text, HTML, or both. The form with both
is delivered as `multipart/alternative`, so that capable clients show the HTML, and others
show the text. Attachments are added with `attach`. An `Asset` is a named, typed source of
bytes, which carries its filename and its [media type](media-types.md):

```scala
val textVersion = t"Sales rose 4% in August."
val htmlVersion = t"<p>Sales rose <b>4%</b> in August.</p>"
val report = Asset(t"report.csv", media"text/csv", Chain(t"month,sales\nAugust,104".in[Data]))

val message = Email(Map(), Email.Message(Email.Content(Email.Body(textVersion, htmlVersion))))
  . attach(report)

def distribute(recipients: List[EmailAddress]): Resend.Receipt =
  message.send(subject = t"Monthly report", to = recipients)
```

The message's content type follows from what it holds, with each layer wrapping the one
inside it:

```tabular
column  The message has
column  Content type
row  a text body only  `text/plain`
row  an HTML body only  `text/html`
row  both a text and an HTML body  `multipart/alternative`
row  inline content  `multipart/related`, around the body
row  attachments  `multipart/mixed`, around the rest
```

Plain text is itself sendable, so `Email(t"hello")` is the form in one line. An email reports
its `text`, `html`, `attachments` and `inlines` back, so a composed message can be inspected
before it goes.

### Couriers

The courier is the mechanism of delivery, and is supplied as a [given]. The courier which is
provided delivers through the HTTP API of [Resend](https://resend.com/), and returns a receipt
with the provider's id for the message. Another delivery service is plugged in by
implementing the `Courier` trait, which has one method, without touching any code which
composes messages.

### Related

- [Network addresses](network-addresses.md#email-addresses) validates the addresses.
- [HTML](html.md) writes the body of an HTML message.
- [HTTP client](http-client.md) carries the request to the delivery service.

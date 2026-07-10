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
import couriers.resend

given Sender = Sender(email"noreply@example.com")
given Resend.ApiKey = Resend.ApiKey(apiKeyText)
```

### Sending

Anything *sendable* — text, an [HTML](html.md) document, or a fully-composed `Email` — sends with
`send`, giving the subject and recipients; `cc`, `bcc` and `replyTo` are optional, and each accepts
one address or a list:

```scala
t"Your order has shipped.".send
  ( subject = t"Shipping confirmation",
    to      = email"customer@example.com" )
```

An HTML document sends as an HTML message the same way, and failure to deliver raises a
`CourierError` naming the sender, recipient and subject at fault.

### Composing

A richer message is built as an `Email`: a body of text, HTML, or both — the both-form delivering
`multipart/alternative`, so capable clients show the HTML and others the text — with attachments
added by `attach`:

```scala
val message = Email(Email.Message(Email.Content(Email.Body(textVersion, htmlVersion))))
  . attach(report)

message.send(subject = t"Monthly report", to = recipients)
```

Anything *attachable* — a named, typed source of bytes — attaches directly, its filename and
[media type](media-types.md) carried with it.

### Couriers

The courier is the delivery mechanism, supplied as a given. The provided courier delivers through
the [Resend](https://resend.com/) HTTP API, returning a receipt with the provider's message id;
another delivery service plugs in by implementing the one-method `Courier` trait, without touching
any composition code.

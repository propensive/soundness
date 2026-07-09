## HTTP Client

### About

Soundness makes [HTTP](https://en.wikipedia.org/wiki/HTTP) requests and reads their responses as
typed values. A request is sent to a [URL](network-addresses.md) checked as the code compiles;
headers are given as named arguments whose values are type-checked against the header they set; a
request body is posted as a typed value that knows its own media type; and the response body is read
as whatever type is asked for. A failing status, or a connection that cannot be made, is a typed
error rather than an exception to guess at.

Making a request requires a capability, `Online`, to be in scope — so a method that reaches the
network says so, and one that should not cannot.

### On HTTP requests

A typical HTTP client takes a URL as a string, headers as a string-to-string map, and a body as
bytes, and hands back a response whose status must be checked by hand and whose body must be decoded
by hand. The URL might be malformed, a header name might be misspelled, the body's media type might
not match its bytes, and none of this is caught until the request runs — if then.

Soundness types each part. The URL is validated where it is written; a header argument is checked
against the header it sets, so `accept` takes a media type and not any string; the body carries its
media type with it; and the response reads into a chosen type. A non-success status and a
connection failure are distinct typed errors. That a request touches the network is itself visible,
as the `Online` capability it demands. Everything comes from the `soundness` package:

```scala
import soundness.*
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import charEncoders.utf8Encoder
```

### A GET request

`fetch` sends a GET request to a URL, and `receive` reads the response body as the type named:

```scala
url"https://example.com/".fetch().receive[Text]
```

The response also reports its status directly, and reading its body as bytes or as a stream is the
same `receive` with a different type:

```scala
url"https://example.com/".fetch().status   // Http.Ok
```

### Headers

Headers are named arguments to the request, each checked against the header it names — `accept`
takes a media type, so a value that is not one does not compile:

```scala
url"https://example.com/".fetch(accept = media"application/json").receive[Text]
```

### Posting a body

`submit` sends a request with a body, the method given as an argument and the body as the last. The
body is a typed value whose media type comes with it — text is `text/plain`, a `Query` is a form
submission — so nothing has to be set by hand to match:

```scala
url"https://example.com/submit".submit(Http.Post, accept = media"application/json")(t"Hello world")
```

`Http.Put`, `Http.Delete` and the rest name the other methods.

### Reading the response

`receive` reads the body as any type that knows how to be read from a response — `Text`, raw bytes,
a stream, or a value parsed from the body. The status is a value to inspect or to receive in its own
right, so a program decides what a given status means rather than having it decided for it.

### Redirects

Redirects are followed by default, up to a limit. Importing a stricter policy stops them, after
which a redirect response is delivered as an `HttpError` carrying the redirect status:

```scala
import httpRedirections.doNotFollowRedirects
```

### Errors

A response outside the success range raises an `HttpError`, which carries the status and headers so
the caller can react to what went wrong:

```scala
capture[HttpError](url"https://example.com/missing".fetch().receive[Text]).status   // Http.NotFound
```

A request that cannot connect at all — an unresolvable host, a refused connection, a TLS failure —
raises a `ConnectError` naming the reason:

```scala
capture[ConnectError](url"http://no-such-host.invalid/".fetch()).reason   // ConnectError.Reason.Dns
```

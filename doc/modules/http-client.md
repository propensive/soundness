## HTTP Client

### About

An [HTTP](https://en.wikipedia.org/wiki/HTTP) request is one expression: a
[URL](network-addresses.md#urls), a method such as `fetch`, and the type the response should be
read as. Each part is typed. The URL is checked at [compiletime], a header's value must suit the
header it sets, a request body carries its own media type, and the response body is read as
whichever type is asked for.

Failure is typed too. A status outside the success range and a connection that cannot be made
are two distinct errors, and a method which makes requests declares both. A request also needs
the [capability] `Online` in scope, so a method that reaches the network says so in its
signature, and one that should not reach it cannot.

### On HTTP requests

A typical HTTP client takes a URL as a string, headers as a map from strings to strings, and a
body as bytes. It hands back a response whose status must be checked by hand and whose body must
be decoded by hand. The URL may be malformed, a header name may be misspelled, and the body's
media type may not match its bytes. None of this is discovered until the request runs, if then.

Here each of those mistakes is found by the compiler:

```tabular
column  Part of a request
column  How it is checked
row  the URL  a `url"…"` [interpolator] validates it where it is written
row  a header  a named argument, whose value has the type that header takes
row  the request body  a typed value which supplies its own `Content-Type`
row  the response body  read with `receive`, as the type named
row  a failing status  an `Http.Error`, declared by the method that may raise it
row  a failed connection  a `Connect.Error`, declared likewise
row  access to the network  the `Online` capability, required in scope
```

Reporting failure through the types, and leaving the caller to decide what a failure does, is
[error handling](../philosophy/error-handling.md) as it works everywhere else.

### What a request needs in scope

Everything comes from the `soundness` package. A request also depends on several decisions that
are made by importing a [given] from a [choice package], so that none is made silently:

```scala
import soundness.*

import charsets.utf8Charset
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
```

```tabular
column  Import
column  Decides
row  `charsets.utf8Charset`, `codepages.utf8Codepage`  how bytes become text, and text becomes bytes
row  `errorDiagnostics.stackTracesDiagnostics`  that an error records a stack trace
row  `httpBackends.javaNetHttp`  which implementation sends the request: here, the JVM's `java.net.http`
row  `internetAccess.online`  that this code may use the network
row  `logging.silentLogging`  that the requests are not logged; [logging](logging.md) has the alternatives
row  `strategies.throwUnsafely`  that an error is thrown as an exception; [errors](errors.md#choosing-a-strategy) has the other strategies
```

### A GET request

`fetch` sends a GET request to a URL. It gives the response, and `receive` reads the response's
body as the type named:

```scala fuel
typecheck
##
val page: Text = url"https://example.com/".fetch().receive[Text]
```

The type passed to `receive` chooses how the body is read. `Text` decodes it with the charset in
scope, `Data` gives its bytes, and a [stream](streams.md) delivers it a part at a time without
holding the whole body in memory. A type with a decoder for a format reads the same way, so a
[JSON](json.md#json-over-http) response is received as `Json`.

The response reports its status without its body being read:

```scala fuel
typecheck
##
val status: Http.Status = url"https://example.com/".fetch().status
```

For a page that exists this is `Http.Ok`.

The URL is a typed value, not a string, so a malformed one never reaches the network. The
interpolator rejects it:

```scala fuel
compile
errors show
##
url"https://example.com:port/"
```

A URL which is not known until the program runs is
[parsed at runtime](network-addresses.md#parsing-at-runtime) instead, and the parsing is where a
bad one is reported.

### Headers

A request header is a named argument to `fetch`. Its name is the header's name in camel case, and
its type is whatever that header carries: `accept` takes a [media type](media-types.md), not any
text.

```scala fuel
typecheck
##
url"https://example.com/".fetch(accept = media"application/json").receive[Text]
```

A value of the wrong type for its header does not compile:

```scala fuel
compile
errors show
##
url"https://example.com/".fetch(accept = t"application/json")
```

Nor does a header whose name is misspelled, since no header of that name takes any value:

```scala fuel
compile
errors show
##
url"https://example.com/".fetch(acept = media"application/json")
```

### Posting a body

`submit` sends a request that has a body. Its first parameter list takes the method and any
headers, and its second takes the body:

```scala fuel
typecheck
##
url"https://example.com/submit".submit(Http.Post, accept = media"application/json")(t"Hello world")
```

`Http.Put`, `Http.Delete` and the others name the remaining methods.

The body is a typed value, and its type decides the request's `Content-Type`, so the header can
never disagree with the bytes that follow it. Text is sent as `text/plain`, a `Query` is sent as
a [form submission](forms.md#submissions), and a `Json` value is sent as `application/json`.
[Types that know their media type](media-types.md#types-that-know-their-media-type) describes how
a type of one's own takes part.

### Reading the response

`receive` reads the body as any type that can be read from a response: `Text`, `Data`, a stream,
or a value parsed from the body. The status is an ordinary value, to inspect or to return, so a
program decides what a given status means.

By default a response outside the success range is not read at all: `receive` raises an
`Http.Error`, described under [Errors](#errors).

### Sessions

A single `fetch` opens a connection and closes it. Where several requests go to the same origin,
a [session] opens one connection and lends it to a block, and every request made in the block
shares it:

```scala fuel
typecheck
##
import soundness.httpUrlSessional
import socketBackends.javaBaseSockets

val request = Http.Request(Http.Get, 1.1, t"example.com".as[Host], t"/", Nil,
    new Spring[Data]:
      def apply()(using Tactic[Truncation.Error]): Stream[Data] over Credit =
        Http.emptyBody())

url"https://example.com".session: session ?=>
  session.fetch(request).status
  session.fetch(request).status
```

The protocol is fixed for as long as the session lasts. For `https`, the server and client agree
on it during the TLS handshake, by
[ALPN](https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation):

```tabular
column  Scheme
column  Negotiated
column  Session
row  `https`  `h2`  *multiplexed* HTTP/2: fetches interleave over one connection
row  `https`  `http/1.1`  *sequential* HTTP/1.1
row  `http`  nothing  *sequential* HTTP/1.1, keep-alive pinning one connection
```

The block's boundary is enforced by the compiler. A response body that is still streaming from
the live connection cannot be returned from the block, because the connection it reads from
closes when the block ends. A value that has been read into memory can be returned freely.

A response that is left unread does no harm. The next fetch first discards whatever remains of
the previous body, so that it starts at the next response.

### Choosing a transport

The implementation that sends requests is a [given], chosen by import:

```tabular
column  Import
column  Sends requests with
row  `httpBackends.javaNetHttp`  the JVM's own `java.net.http` client
row  `httpBackends.soundnessHttp`  Soundness's own HTTP codecs, directly over a [socket](sockets.md)
```

A `locally` block confines a different choice to a few lines:

```scala fuel
typecheck
##
locally:
  import httpBackends.soundnessHttp
  url"https://example.com/".fetch().status
```

The `soundnessHttp` backend keeps HTTP/1.1 connections alive and reuses them for later requests
to the same origin. Over TLS it negotiates HTTP/2 or HTTP/1.1 by ALPN and speaks whichever the
server chose. If a kept connection turns out to have been closed by the server, the request is
retried once on a new connection. Other platforms supply backends of their own in the same way.

### Redirects

A redirect is followed by default, up to a limit. Importing a stricter policy stops that, and a
redirect response is then an `Http.Error` carrying the redirect's status:

```scala
import httpRedirections.doNotFollowRedirects
```

### Errors

A request fails in one of two ways, and each is an error of its own type:

```tabular
column  Error
column  Raised when
column  Carries
row  `Http.Error`  a response is outside the success range  the status and headers
row  `Connect.Error`  no connection can be made: an unresolvable host, a refused connection, a TLS failure  the reason
```

Under the `throwUnsafely` [strategy] imported above, either is thrown as an exception. A method
that should not throw declares the errors instead, with [`raises`][raises], and its caller chooses
what to do about them; [Errors](errors.md) covers the choices.

`capture` runs an expression that is expected to fail, and gives the error. An `Http.Error` has
the status, so the caller can react to what went wrong:

```scala fuel
typecheck
##
capture[Http.Error](url"https://example.com/missing".fetch().receive[Text]).status
```

For a missing page this is `Http.NotFound`.

A `Connect.Error` has a `reason`, which says why no connection was made:

```scala fuel
typecheck
##
capture[Connect.Error](url"http://no-such-host.invalid/".fetch()).reason
```

```tabular
column  Reason
column  Means
row  `Dns`  the host's name could not be resolved
row  `Refused`  the host refused the connection
row  `Ssl(reason)`  the TLS layer could not be established; its own reason is `Handshake`, `Key`, `Peer` or `Protocol`
row  `Timeout`  the server did not respond within the time limit
row  `Unknown`  the failure was not recognized as one of the others
```

A TLS failure is therefore told apart from a network failure, and its reason says at which stage
it happened. An expired certificate, a certificate for the wrong host, a revoked certificate, a
broken cipher suite and an undersized Diffie–Hellman group are each refused during the
handshake. The client is tested against the [badssl.com](https://badssl.com/) collection of
deliberately broken endpoints, so each refusal is verified.

Sometimes a connection must be made to a host whose certificate cannot be validated, such as a
development server with a self-signed certificate. A relaxed `TlsAcceptance` in scope permits it.
Accepting an unvalidated certificate is then a decision that can be found by searching the code
for the import.

### Related

- [HTTP Server](http-server.md) answers requests with the same `Http` vocabulary of methods,
  statuses and headers.
- [JSON over HTTP](json.md#json-over-http) posts a `Json` value and receives one.
- [OAuth](oauth.md#credentials-for-a-typed-client) obtains the token an authorized request sends.
- [Network addresses](network-addresses.md) covers URLs, hostnames and ports.

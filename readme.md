[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/telekinesis/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/telekinesis/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Telekinesis

__A lightweight HTTP client and server for the Loom generation__

__Telekinesis__ is a lightweight HTTP client for sending and HTTP requests.

## Features

- immutable API optimized for Scala 3 and lightweight concurrency with Loom
- Simple and flexible request handling
- HTTP server can be run standalone or wrap a servlet container
- typesafe representations of HTTP request and response headers and MIME types
- transparent typeclass-based request body query parameter serialization and deserialization
- optional pattern-matching on requests
- fast streaming without complexity
- safe parameter and header access
- typesafe representation of URLs

## Availability

Telekinesis has not yet been published as a binary.

## Getting Started

Here is an example of a simple HTTP request:

```scala
import telekinesis.*

val response = url"http://example.com/test".query(flag = t"yes", param = t"7").get().as[Text]
```

### Sending an HTTP request

An HTTP request may be sent by calling one of the HTTP methods—`get`, `post`, `put`, `options`, `head`, `trace`,
`delete`, `connect` or `patch`—on the `Http` object. As a minimum, these methods all take a URL as their first
parameter. This may be provided as a `Url` (see below) or a `Text`, or any type which has a contextual
`ToLocation` instance which can convert it into URL string. This may be useful for integration with alternitave
URL representations.

If the request is successful, a response will be returned synchronously as an `HttpResponse` instance.
`HttpResponse` provides the methods `status` (the HTTP status code), `headers` (a map of HTTP response headers),
and `body` which will be a representation of the response body, in bytes.

The easiest way to access the body is by converting it to another type, using a contextual reader. That can be
achieved by calling `as` with an appropriate type, for example,
```scala
url"https://example.com/service".get().as[Text]
```
or with a suitable JSON library such as [Jacinta](https://propensive.com/opensource/jacinta/),
```scala
import jacinta.*
url"http://example.com/file".post(content).as[Json]
```

### Request and response bodies

The type of `body` is `Body`, defined as an alias for, `Unit | IArray[Byte] | LazyList[IArray[Byte]]`, a union
type corresponding to the cases of an empty response, a response of known length, and a streamed response,
respectively.

This type is commonly used for both requests and responses.

### Error handling

HTTP requests may fail for a variety of reasons. These will be thrown as `HttpError`s only when the `as` method
is invoked (an `HttpResponse` is always returned from `get` or `post`, even in the event of a failure status).
An `HttpError` contains a `status` field of the HTTP status code.

Some HTTP requests will fail, but will still send a useful response body which can be read and interpreted like
any other, albeit from the `HttpError` instance.

Here is an example of an HTTP error being handled:

```scala
try uri.get().as[Text]
catch
  case error@HttpError(HttpStatus.NotFound, _) =>
    t"The page was not found. The server responded with: ${error.as[Text]}"
  case HttpError(_, _) =>
    t"The request failed"
```




## Status

Telekinesis is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Telekinesis is designed to be _small_. Its entire source code currently consists
of 514 lines of code.

## Building

Telekinesis can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Telekinesis are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/telekinesis/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Telekinesis easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Telekinesis was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The library provides a way of handling URIs, performing "action at a distance",
or _telekinesis_; specifically the processing of a request on a remote server.
A man called Uri was also famed for his ability to perform telekinesis with
spoons.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows the characters `://`, which form part of every URL.

## License

Telekinesis is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

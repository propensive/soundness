[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/scintillate/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/scintillate/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/scintillate-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/scintillate-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
<img src="/doc/images/github.png" valign="middle">

# Scintillate

__Scintillate__ is a lightweight HTTP client and server for sending and handling HTTP requests. It is designed
primarily for Scala 3 and the optional server module provides an API for running standalone, or within a Servlet
container, preferably on a Loom-based JVM.

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

## Getting Started

Here is an example of a simple HTTP request 

```scala
import scintillate.*

val response = uri"http://example.com/test".query(flag = "yes", param = "7").get().as[String]
```

## Sending an HTTP request

An HTTP request may be sent by calling one of the HTTP methods—`get`, `post`, `put`, `options`, `head`, `trace`,
`delete`, `connect` or `patch`—on the `Http` object. As a minimum, these methods all take a URL as their first
parameter. This may be provided as a `Uri` (see below) or a `String`, or any type which has a contextual
`ToLocation` instance which can convert it into URL string. This may be useful for integration with alternitave
URL representations.

If the request is successful, a response will be returned synchronously as an `HttpResponse` instance.
`HttpResponse` provides the methods `status` (the HTTP status code), `headers` (a map of HTTP response headers),
and `body` which will be a representation of the response body, in bytes.

The easiest way to access the body is by converting it to another type, using a contextual reader. That can be
achieved by calling `as` with an appropriate type, for example,
```scala
uri"https://example.com/service".get().as[String]
```
or with a suitable JSON library such as [Euphemism](https://propensive.com/opensource/euphemism/),
```scala
import euphemism.*
uri"http://example.com/file".post(content).as[Json]
```

## Request and response bodies

The type of `body` is `Body`, defined as an alias for, `Unit | IArray[Byte] | LazyList[IArray[Byte]]`, a union
type corresponding to the cases of an empty response, a response of known length, and a streamed response,
respectively.

This type is commonly used for both requests and responses.

## Error handling

HTTP requests may fail for a variety of reasons. These will be thrown as `HttpError`s only when the `as` method
is invoked (an `HttpResponse` is always returned from `get` or `post`, even in the event of a failure status).
An `HttpError` contains a `status` field of the HTTP status code.

Some HTTP requests will fail, but will still send a useful response body which can be read and interpreted like
any other, albeit from the `HttpError` instance.

Here is an example of an HTTP error being handled:

```scala
try uri.get().as[String]
catch
  case error@HttpError(HttpStatus.NotFound, _) =>
    s"The page was not found. The server responded with: ${error.as[String]}"
  case HttpError(_, _) =>
    s"The request failed"
```

## Launching an HTTP server

An HTTP server can be launched anywhere by calling,
```scala
HttpServer.listen {
  // handler
}
```

This is a non-blocking call whose body will be executed every time a request is received. The `listen` method
returns an `HttpService` instance whose only method is `HttpService#stop()`, which will cause the server to stop
listening for new requests.

The `listen` block must return a `Response` instance. A `Response` may be instantiated with a single parameter
of the content to be returned. In this case, its `Content-Type` and `Content-Length` would be determined from
the type of the parameter, as well as the HTTP status (which is usually `200`, except in failure cases) and
how its body is sent: all at once, or streamed.

The simplest sever implementation would look something like this,
```scala
HttpServer.listen {
  Response("Hello world!")
}
```
and would respond with a `200` response with the MIME type `text/plain`, and the string `Hello world!` for every
request, regardless of its HTTP method, parameters, body or headers.

Within the body of `listen`, a `Request` instance is contextually available, and may be accessed with the
`request` method. For convenience, the methods `param` and `header` may also be used directly within a `listen`
block to access a parameter or HTTP header, for example:
```scala
HttpServer.listen {
  val name = param("name")
  val age = param("age")

  Response(s"The name is $name and age is $age")
}
```

## Pattern matching on Requests

Another way to work with `Request`s is by pattern matching against them. Several pattern extractors are provided
for this purpose.

A very simple pattern match on a request object might look like this,
```scala
HttpServer.listen {
  request match
    case Path("/")                   => homePage
    case Path("/contact")            => contactUsPage
    case Path(s"/products/$product") => productPage(product)
}
```
where the `Path` extractor is used to match on the part of the URL after the hostname, and before the query, if
there is one.

But other extractors for matching on the HTTP method, headers and parameters also exist, and can be combined in
the same pattern using the `&` combinator, like so:
```scala
HttpServer.listen {
  request match
    case Path("/") & RequestHeader.UserAgent(s"Mozilla/$_")               => mozillaHome
    case Path("/") & Post() & AcceptEncoding(lang) if lang.contains("en") => englishHome
}
```

In these examples, `&` is an extractor which always matches a `Request`, and "extracts" it into two copies. As
an infix extractor, both sides may then be matched with their own patterns. Of course, this can be repeated
any number of times in the same case clause.

## URIs

Scintillate uses the `Uri` type to represent a URI. This will always use either the `http` or `https` URL
scheme, and will not represent URIs containing unescaped characters. The `hostname`, `path` and `queryString`
are also available or `Uri` instances.

Additionally, the HTTP methods `get`, `post`, and others are available to call directly on `Uri` instances, as
an alternative to their equivalent methods in the `Http` object. There is a one-to-one correspondence between
the methods on a `Uri` instance an the `Http` object, except that the first parameter of each of the `Http`
methods—the URI itself—is the subject of the method invocation.

### Queries

A `Uri` instance may include a query string, which would be written following a `?` character after the path.
The `Uri#query` method may be used to append parameters to an existing `Uri`. Usually these are key/value pairs
in the form `key=value`, but plain strings can also be used.

There are two ways to call the `query` method. Firstly, it may be invoked with variadic dynamically-named
arguments, and `String` parameters, like so:
```scala
uri.query(param = "one", value = "two", flag = "three", option = "four")
```
the names of these parameters may be any valid Scala identifier, and do not need to be quoted.

Another variant of the `query` method exists which takes a single parameter of a type that can be interpreted
as a set of parameters, based on a contextual `ToQuery` instance.

Default given `ToQuery` instances are provided for the primitive types, `String` and `Int`, and instances will
be derived for case classes composed of other types for which `ToQuery` instances exist. That includes nested
case classes.

A case class will generate one query parameter for each field, named after that field, except for fields of
nested case class instances: In these cases, each field in the nested type will be prefixed with the outer field
name, separated by a `.`.

For example,
```scala
case class Address(number: Int, street: String, city: String)
case class Person(name: String, address: Address)

val person = Person("Jack", Address(17, "East Street", "Birmingham"))
uri"http://example.com/person/add".query(person).get()
```

The process of serializing this case class instance to query parameters would send the parameters, `name`,
`address.number`, `address.street` and `address.city`.

## Redirection and Missing Pages

The `Redirect` and `NotFound` case classes provide representations of an HTTP `301` redirect repsonse and a
`404` "not found" page respectively.

`Redirect` takes a single parameter, a representation of a location typically as a `Uri` or a `String`, but
other representations can be used provided a `ToLocation` for that type is in contextual scope.

## Related Projects

The following _Niveau_ libraries are dependencies of _Scintillate_:

[![Clairvoyant](https://github.com/propensive/clairvoyant/raw/main/doc/images/128x128.png)](https://github.com/propensive/clairvoyant/) &nbsp; [![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Gastronomy](https://github.com/propensive/gastronomy/raw/main/doc/images/128x128.png)](https://github.com/propensive/gastronomy/) &nbsp; [![Gesticulate](https://github.com/propensive/gesticulate/raw/main/doc/images/128x128.png)](https://github.com/propensive/gesticulate/) &nbsp; [![Slalom](https://github.com/propensive/slalom/raw/main/doc/images/128x128.png)](https://github.com/propensive/slalom/) &nbsp;

The following _Niveau_ libraries are dependents of _Scintillate_:

[![Tarantula](https://github.com/propensive/tarantula/raw/main/doc/images/128x128.png)](https://github.com/propensive/tarantula/) &nbsp;

## Status

Scintillate is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Scintillate is designed to be _small_. Its entire source code currently consists of 922 lines of code.

## Building

Scintillate can be built on Linux or Mac OS with Vex, by running the `vex` script in the root directory:
```sh
./vex
```

This script will download `vex` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Scintillate are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/scintillate/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Scintillate easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Scintillate was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

To _scintillate_ is "to fluoresce momentarily when struck by a charged particle", much as a web server is dormant until an incoming request stimulates a response.

## License

Scintillate is copyright &copy; 2021-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/scintillate/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/scintillate/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Scintillate

__A lightweight HTTP client and server for the Loom generation__

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

## Availability Plan

Scintillate has not yet been published. The medium-term plan is to build Scintillate
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Scintillate.

Subsequently, Scintillate will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Launching an HTTP server

An HTTP server can be launched anywhere by calling,
```scala
HttpServer.listen:
  // handler
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
HttpServer.listen:
  Response("Hello world!")
```
and would respond with a `200` response with the MIME type `text/plain`, and the string `Hello world!` for every
request, regardless of its HTTP method, parameters, body or headers.

Within the body of `listen`, a `Request` instance is contextually available, and may be accessed with the
`request` method. For convenience, the methods `param` and `header` may also be used directly within a `listen`
block to access a parameter or HTTP header, for example:
```scala
HttpServer.listen:
  val name = param("name")
  val age = param("age")

  Response(s"The name is $name and age is $age")
```

### Pattern matching on Requests

Another way to work with `Request`s is by pattern matching against them. Several pattern extractors are provided
for this purpose.

A very simple pattern match on a request object might look like this,
```scala
HttpServer.listen:
  request match
    case Path("/")                   => homePage
    case Path("/contact")            => contactUsPage
    case Path(s"/products/$product") => productPage(product)
```
where the `Path` extractor is used to match on the part of the URL after the hostname, and before the query, if
there is one.

But other extractors for matching on the HTTP method, headers and parameters also exist, and can be combined in
the same pattern using the `&` combinator, like so:
```scala
HttpServer.listen:
  request match
    case Path("/") & RequestHeader.UserAgent(s"Mozilla/$_")               => mozillaHome
    case Path("/") & Post() & AcceptEncoding(lang) if lang.contains("en") => englishHome
```

In these examples, `&` is an extractor which always matches a `Request`, and "extracts" it into two copies. As
an infix extractor, both sides may then be matched with their own patterns. Of course, this can be repeated
any number of times in the same case clause.

### URLs

Scintillate uses the `Url` type to represent a URL. This will always use either the `http` or `https` URL
scheme, and will not represent URLs containing unescaped characters. The `hostname`, `path` and `queryString`
are also available or `Url` instances.

Additionally, the HTTP methods `get`, `post`, and others are available to call directly on `Url` instances, as
an alternative to their equivalent methods in the `Http` object. There is a one-to-one correspondence between
the methods on a `Url` instance an the `Http` object, except that the first parameter of each of the `Http`
methods—the URL itself—is the subject of the method invocation.

#### Queries

A `Url` instance may include a query string, which would be written following a `?` character after the path.
The `Url#query` method may be used to append parameters to an existing `Url`. Usually these are key/value pairs
in the form `key=value`, but plain strings can also be used.

There are two ways to call the `query` method. Firstly, it may be invoked with variadic dynamically-named
arguments, and `String` parameters, like so:
```scala
url.query(param = "one", value = "two", flag = "three", option = "four")
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
url"http://example.com/person/add".query(person).get()
```

The process of serializing this case class instance to query parameters would send the parameters, `name`,
`address.number`, `address.street` and `address.city`.

### Redirection and Missing Pages

The `Redirect` and `NotFound` case classes provide representations of an HTTP `301` redirect repsonse and a
`404` "not found" page respectively.

`Redirect` takes a single parameter, a representation of a location typically as a `Url` or a `String`, but
other representations can be used provided a `ToLocation` for that type is in contextual scope.





## Status

Scintillate is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Scintillate is designed to be _small_. Its entire source code currently consists
of 376 lines of code.

## Building

Scintillate will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Scintillate?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Scintillate's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Scintillate and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `scintillate`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Scintillate's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Scintillate are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/scintillate/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Scintillate
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Scintillate was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

To _scintillate_ is "to fluoresce momentarily when struck by a charged particle", much as a web server is dormant until an incoming request stimulates a response.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows four interlocked three-pointed stars, intended to look like flying sparks or scintillations.

## License

Scintillate is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).


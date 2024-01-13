[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/anticipation/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/anticipation/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Anticipation

__Seamless integration with minimalistic typeclass interfaces foreseen__

__Anticipation__ provides minimalistic typeclass interfaces to absolve end users
of the need for cumbersome dependencies between unrelated projects.

## Features

- minimalistic typeclass interfaces
- integration typeclasses for HTTP, HTML and CSS
- used by [Honeycomb](https://github.com/propensive/honeycomb/), [Cataclysm](https://github.com/propensive/cataclysm/) and [Scintillate](https://github.com/propensive/scintillate/)
- avoids dependencies in either direction between typeclass users and providers


## Availability Plan

Anticipation has not yet been published. The medium-term plan is to build Anticipation
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Anticipation.

Subsequently, Anticipation will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

__Anticipation__ defines several typeclass interfaces to provide interoperability
between different libraries, without requiring a hard dependency between them.

### The Problem

Consider a library _J_ which defines a JSON datatype, and a second library _H_
which can return a different datatypes as HTTP responses. Neither library
fundamentally requires the other, and users should be able to choose whether to
depend on J or H or both.

But it would nevertheless be useful for users of both J and H to be able to
return JSON HTTP responses. The JSON HTTP typeclass instance must depend on
both J and H, which means that either,
1. the typeclass instance is defined in J, and J depends on H
2. the typeclass instance is defined in H, and H depends on J
3. a new module which depends on both J and H should be created

Options 1 and 2 require a dependency—in some direction—between J and H. And
option 3 has the disadvantage that it creates another dependency for users to
manage, both at the source-code and the build level.

The typeclass instance will have a type which looks something akin to
`Http[Json]`, where `Json` is defined in J and `Http` is defined in H. That
furthermore precludes the possibility that the given instance could be defined
in the companion objects of either of those types.

Ideally, users should be able to add both J and H (independently) to their
build, and import no more than the packages of J and H, and automatically get
access to the integration typeclass instance without any further work.

This is what Anticipation provides.

### The Solution

The issues above can be circumvented by predefining a set of minimalistic
typeclass interfaces for each integration point necessary, and having each
library independently depend upon them.

As much as possible, the typeclass interfaces should not require any additional
datatypes to be defined; they should depend only on types in the standard
library. Whilst this may compromise the utility of these typeclasses, they are
intended to be used only by the integration libraries; not by end-users.

Continuing the earlier example, the libraries J and H could both depend on
Anticipation. J would then define a typeclass instance for its JSON type in its
companion object, and H would define a typeclass converter from Anticipation's
typeclass to its own user-facing typeclass interface in its companion object.

Consequently, users of both H and J could depend on both libraries, import both
packages (and nothing more) and automatically be able to use the integration
between them.

### Anticipation Typeclasses

#### `GenericCssSelection` in the `css` module

Used primarily by [Cataclysm](https://github.com/propensive/cataclysm/), this
allows different types to be used in CSS selectors. Implementations are
provided in [Honeycomb](https://github.com/propensive/honeycomb/) for its
representations of CSS class names (`honeycomb.Cls`), DOM IDs
(`honeycomb.DomId`), and HTML tags (`honeycomb.TagType`).

#### Paths, Files and Directories in the `file` module

Six typeclass interfaces provide the ability to read and write representations
of paths, files and directories. These are used by projects such as
[Surveillance](https://github.com/propensive/surveillance/),
[Oubliette](https://github.com/propensive/oubliette/),
[Rudiments](https://github.com/propensive/rudiments/),
[Imperial](https://github.com/propensive/imperial/) and
[Guillotine](https://github.com/propensive/guillotine/) to support interaction
with any library which provides path-like types.
[Galilei](https://github.com/propensive/galilei/) provides instances for its
`galilei.Path`, `galilei.File` and `galilei.Directory` types, and
[Diuretic](https://github.com/propensive/diuretic/) provides instances for
types in the Java standard library, such as `java.io.File` and
`java.nio.file.Path`.

The typeclasses are called,
- `GenericPathReader`
- `GenericFileReader`
- `GenericDirectoryReader`
- `GenericPathMaker`
- `GenericFileMaker`
- `GenericDirectoryMaker`

In a later version of Anticipation, each `Reader` and `Maker` pair may be
combined into a single typeclass.

#### `GenericHtmlAttribute` in the `html` module

An HTML library, most notably
[Honeycomb](https://github.com/propensive/honeycomb/), can support attributes
having typed values (rather than just strings), provided that type is suitable
for the given attribute. The `GenericHtmlAttribute` typeclass, parameterised on
the singleton literal type of the attribute name, and the value type, allows
libraries to make their types usable in such HTML libraries.
[Gesticulate](https://github.com/propensive/gesticulate/) makes
`gesticulate.MediaType`s usable for `formenctype`, `enctype`, `media` and
`type` attributes. [Scintillate](https://github.com/propensive/scintillate/)
allows `scintillate.RequestParam` instances to be used for the `name` parameter
of inputs in a form. [Cataclysm](https://github.com/propensive/cataclysm/)
supports `cataclysm.CssStyle` values to be used on an HTML tag's `style`
attribute. And [Serpentine](https://github.com/propensive/serpentine/) and
[Telekinesis](https://github.com/propensive/telekinesis/) allow
`serpentine.Relative`, `serpentine.GenericPath`, `telekinesis.HttpMethod` and
`telekinesis.Url` instances to be used in the variety of attributes which
support these types, such as `src`.

#### `GenericHttpResponseStream` and `GenericHttpReader` in the `http` module

These typeclasses provide support for working with various types over HTTP. Any
type which can be streamed to HTTP, including an appropriate media type, can be
furnished with a `GenericHttpResponseStream` instance. Libraries which provide
these types include:

- [Caesura](https://github.com/propensive/caesura/), for `caesura.Csv` and `caesura.Tsv` types
- [Cataclysm](https://github.com/propensive/cataclysm/), for `cataclysm.CssStylesheet`s
- [Honeycomb](https://github.com/propensive/honeycomb/), for `honeycomb.HtmlDoc`s
- [Jacinta](https://github.com/propensive/jacinta/), for `Json` values
- [Xylophone](https://github.com/propensive/xylophone/), for `Xml` values

A `GenericHttpReader` instance is provided in
[Jacinta](https://github.com/propensive/jacinta/) for reading a `Json` value
directly from an HTTP response.

#### `GenericInstant` and `GenericDuration` in the `time` module

Any library which needs to work with time values, as instantaneous points in
time or as time durations, should use `GenericInstant` or `GenericDuration`
respectively.  [Parasite](https://github.com/propensive/parasite/) and
[Turbulence](https://github.com/propensive/turbulence/) use these typeclasses
for generic time-related operations, and instances are provided for Java types
in [Diuretic](https://github.com/propensive/diuretic/) and
[Aviation](https://github.com/propensive/aviation/) for `aviation.Instant` and
`aviation.Duration` types.

#### `GenericUrl` in the `url` module

Many libraries need to work with URLs, and the `GenericUrl` provides a generic
interface that's use by [Tarantula](https://github.com/propensive/tarantula/)
and [Telekinesis](https://github.com/propensive/telekinesis/).
[Diuretic](https://github.com/propensive/diuretic/) provides instances for
`java.net.URL` and Telekinesis provides an instance for its own
`telekinesis.Url` type.






## Status

Anticipation is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Anticipation is designed to be _small_. Its entire source code currently consists
of 156 lines of code.

## Building

Anticipation will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Anticipation?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Anticipation's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Anticipation and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `anticipation`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Anticipation's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Anticipation are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/anticipation/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Anticipation
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Anticipation was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Anticipation is the consideration of something before it happens, and _Anticipation_ provides typeclass definitions in expectation of
their future implementation.

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

The logo shows a simple floral pattern.

## License

Anticipation is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).


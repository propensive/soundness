[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/eucalyptus/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/eucalyptus/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Eucalyptus

__Versatile logging__

_Eucalyptus_ is a logging library which makes it easy to add logging to any project. It consciously
makes logging configuration explicit and easily understood, and uses rich ANSI-escaped messages to
make logs easier to read.

## Features

- logging requires explicit configuration in code, avoiding any doubt about whether or where logs are produced
- logging "realms" are defined as givens
- logging configuration may be static or delegate to dynamic criteria
- unlogged messages are not evaluated, avoiding the cost of construction
- simple definitions for the log format
- log messages may contains text styles and color, using ANSI codes; this helps readability
- typeclass-based support for logging different types of object


## Availability Plan

Eucalyptus has not yet been published. The medium-term plan is to build Eucalyptus
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Eucalyptus.

Subsequently, Eucalyptus will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Defining a logger

Libraries which use Eucalyptus for logging will need a contextual `Log` instance before they can
be used. This may be as simple as defining a `given Log` instance in the package where the
libraries are used, for example,
```scala
given Log = logging.silent
```
or,
```scala
given Log = logging.stdout
```
or,
```scala
given Log = logging.syslog
```

A more versatile `Log` can route different log messages to different targets, for example,
```scala
given Log = Log.route:
  case Level.Fail() => Err
  case Level.Warn() => Out
  case _            => Syslog(t"app")
```
would log all `FAIL` level messages to standard error, all `WARN` level messages to standard output, and
everything else to the system log (with the tag `app`).

### Library Code

#### Contextual `Log` instances

Any method which requires logging should request a contextual `Log` parameter. This
is as simple as adding the parameter block, `(using Log)` to the method. Any methods
which call such a method will also need to include a `using Log` parameter.

#### Log messages

Messages may be logged (at a particular level) by calling one of four methods,
- `Log.fine`
- `Log.info`
- `Log.warn`
- `Log.fail`
passing the single parameter of a `Message` containing the message to be logged.

#### Realm

These four methods require a contextual `Realm` instance in scope so that the source of log messages can be
easily discerned from logs. Conventionally, this would 
be declared in the main application package, and called `Realm`, like so,
```scala
package mylibrary
given Realm: Realm = realm"mylibrary"
```
where `mylibrary` is the name that will appear in the logs. The name `realm` is given
explicitly so that a user-defined `Log` instance may be configured to reference this
realm within the package `mylibrary`, for example:
```scala
given Log = Log.route:
  case myLibrary.Realm => Out
```

Since `given` instances are not imported by default with a wildcard import, a `Realm`
definition does not need to be marked as private.

### Configuration

When working with libraries such as [Scintillate](https://github.com/propensive/scintillate) or
[Guillotine](https://github.com/propensive/guillotine), whose methods require
a `Log` instance, it is possible to selectively include logs from specific libraries,
by referring to that library's realm, for example:
```scala
given Log = Log.route:
  case scintillate.Realm => Out
```
As with `Everything`, a level may also be specified:
```scala
given Log = Log.route:
  case Level.Warn() | Level.Fail() => Err
```

And multiple rules may be included as multiple cases in the `Log` constructor, where the `&` pattern operator
can be used to match on more than one property, for example:
```scala
given Log = Log.route:
  case scintillate.Realm & Info() => Out
  case guillotine.Realm & Warn()  => Err
  case probably.Realm             => Out
```





## Status

Eucalyptus is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Eucalyptus is designed to be _small_. Its entire source code currently consists
of 276 lines of code.

## Building

Eucalyptus will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Eucalyptus?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Eucalyptus's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Eucalyptus and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `eucalyptus`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Eucalyptus's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Eucalyptus are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/eucalyptus/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Eucalyptus
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Eucalyptus was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The _Eucalyptus_ tree is flexible and good for logging.

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

The logo shows the cut cross section of a (supposedly) Eucalyptus tree, with a leaf alongside.

## License

Eucalyptus is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).


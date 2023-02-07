[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/eucalyptus/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/eucalyptus/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Eucalyptus

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


## Availability

Eucalyptus has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

### Defining a logger

Libraries which use Eucalyptus for logging will need a contextual `Log` instance before they can
be used. This may be as simple as defining a `given Log()` instance in the package where the
libraries are used:
```scala
given Log()
```

This will construct the simplest possible `Log` which logs nothing.

A better alternative may be a logger which logs everything to `STDOUT`,
```scala
given Log(Everything |-> Stdout)
```
or which logs everything at `Warn` level or hight to `STDOUT`:
```scala
given Log(Everything.warn |-> Stdout)
```

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
passing the single parameter of an `AnsiText` containing the message to be logged.

These methods each take a single parameter which is typically an `AnsiText` or a
`Text`, but can be any type that has a given `AnsiShow` instance available.

#### Realm

These four methods require a contextual `Realm` instance. Conventionally, this would 
be declared in the main application package, and called `realm`, like so,
```scala
package mylibrary
given realm: Realm = Realm("mylibrary")
```
where `mylibrary` is the name that will appear in the logs. The name `realm` is given
explicitly so that a user-defined `Log` instance may be configured to reference this
realm within the package `mylibrary`, for example:
```scala
Log(mylibrary.realm |-> Stdout)
```

Since `given` instances are not imported by default with a wildcard import, a `Realm`
definition does not need to be marked as private.

### Configuration

When working with libraries such as [Scintillate](https://github.com/propensive/scintillate) or
[Guillotine](https://github.com/propensive/guillotine), whose methods require
a `Log` instance, it is possible to selectively include logs from specific libraries,
by referring to that library's realm instead of `Everything`, for example:
```scala
given Log(scintillate.realm |-> Stdout)
```
As with `Everything`, a level may also be specified:
```scala
given Log(scintillate.realm.warn |-> Stdout)
```

And multiple rules may be included as repeated arguments to the `Log` constructor, for example:
```scala
given Log(
  scintillate.realm.info |-> Stdout,
  guillotine.realm       |-> Stdout,
  probably.realm.fail    |-> Stdout
)
```


## Related Projects

The following _Scala One_ libraries are dependencies of _Eucalyptus_:

[![Escapade](https://github.com/propensive/escapade/raw/main/doc/images/128x128.png)](https://github.com/propensive/escapade/) &nbsp; [![Turbulence](https://github.com/propensive/turbulence/raw/main/doc/images/128x128.png)](https://github.com/propensive/turbulence/) &nbsp;

The following _Scala One_ libraries are dependents of _Eucalyptus_:

[![Aviation](https://github.com/propensive/aviation/raw/main/doc/images/128x128.png)](https://github.com/propensive/aviation/) &nbsp; [![Cellulose](https://github.com/propensive/cellulose/raw/main/doc/images/128x128.png)](https://github.com/propensive/cellulose/) &nbsp; [![Dissonance](https://github.com/propensive/dissonance/raw/main/doc/images/128x128.png)](https://github.com/propensive/dissonance/) &nbsp; [![Galilei](https://github.com/propensive/galilei/raw/main/doc/images/128x128.png)](https://github.com/propensive/galilei/) &nbsp; [![Guillotine](https://github.com/propensive/guillotine/raw/main/doc/images/128x128.png)](https://github.com/propensive/guillotine/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp; [![Profanity](https://github.com/propensive/profanity/raw/main/doc/images/128x128.png)](https://github.com/propensive/profanity/) &nbsp; [![Surveillance](https://github.com/propensive/surveillance/raw/main/doc/images/128x128.png)](https://github.com/propensive/surveillance/) &nbsp; [![Telekinesis](https://github.com/propensive/telekinesis/raw/main/doc/images/128x128.png)](https://github.com/propensive/telekinesis/) &nbsp;

## Status

Eucalyptus is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Eucalyptus is designed to be _small_. Its entire source code currently consists
of 109 lines of code.

## Building

Eucalyptus can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Eucalyptus are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/eucalyptus/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Eucalyptus easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Eucalyptus was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The _Eucalyptus_ tree is flexible and good for logging.

## License

Eucalyptus is copyright &copy; 2018-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

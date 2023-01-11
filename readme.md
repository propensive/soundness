[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/anticipation/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/anticipation/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Anticipation

__Anticipation__ provides minimalistic typeclass interfaces to absolve end users
of the need for cumbersome dependencies between unrelated projects.

## Features

- minimalistic typeclass interfaces
- integration typeclasses for HTTP, HTML and CSS
- used by [Honeycomb](https://github.com/propensive/honeycomb/), [Cataclysm](https://github.com/propensive/cataclysm/) and [Scintillate](https://github.com/propensive/scintillate/)
- avoids dependencies in either direction between typeclass users and providers


## Availability

Anticipation has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

__Anticipation__ defines several typeclass interfaces to provide interoperability
between different libraries, without requiring a hard dependency between them.

## The Problem

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

## The Solution

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


## Related Projects

_Anticipation_ has no dependencies.

The following _Scala One_ libraries are dependents of _Anticipation_:

[![Aviation](https://github.com/propensive/aviation/raw/main/doc/images/128x128.png)](https://github.com/propensive/aviation/) &nbsp; [![Caesura](https://github.com/propensive/caesura/raw/main/doc/images/128x128.png)](https://github.com/propensive/caesura/) &nbsp; [![Cataclysm](https://github.com/propensive/cataclysm/raw/main/doc/images/128x128.png)](https://github.com/propensive/cataclysm/) &nbsp; [![Diuretic](https://github.com/propensive/diuretic/raw/main/doc/images/128x128.png)](https://github.com/propensive/diuretic/) &nbsp; [![Euphemism](https://github.com/propensive/euphemism/raw/main/doc/images/128x128.png)](https://github.com/propensive/euphemism/) &nbsp; [![Galilei](https://github.com/propensive/galilei/raw/main/doc/images/128x128.png)](https://github.com/propensive/galilei/) &nbsp; [![Gesticulate](https://github.com/propensive/gesticulate/raw/main/doc/images/128x128.png)](https://github.com/propensive/gesticulate/) &nbsp; [![Honeycomb](https://github.com/propensive/honeycomb/raw/main/doc/images/128x128.png)](https://github.com/propensive/honeycomb/) &nbsp; [![Imperial](https://github.com/propensive/imperial/raw/main/doc/images/128x128.png)](https://github.com/propensive/imperial/) &nbsp; [![Oubliette](https://github.com/propensive/oubliette/raw/main/doc/images/128x128.png)](https://github.com/propensive/oubliette/) &nbsp; [![Punctuation](https://github.com/propensive/punctuation/raw/main/doc/images/128x128.png)](https://github.com/propensive/punctuation/) &nbsp; [![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp; [![Serpentine](https://github.com/propensive/serpentine/raw/main/doc/images/128x128.png)](https://github.com/propensive/serpentine/) &nbsp; [![Surveillance](https://github.com/propensive/surveillance/raw/main/doc/images/128x128.png)](https://github.com/propensive/surveillance/) &nbsp; [![Telekinesis](https://github.com/propensive/telekinesis/raw/main/doc/images/128x128.png)](https://github.com/propensive/telekinesis/) &nbsp; [![Xylophone](https://github.com/propensive/xylophone/raw/main/doc/images/128x128.png)](https://github.com/propensive/xylophone/) &nbsp;

## Status

Anticipation is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Anticipation is designed to be _small_. Its entire source code currently consists
of 69 lines of code.

## Building

Anticipation can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Anticipation are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/anticipation/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Anticipation easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Anticipation was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Anticipation is the consideration of something before it happens, and _Anticipation_ provides typeclass definitions in expectation of
their future implementation.

## License

Anticipation is copyright &copy; 2021-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

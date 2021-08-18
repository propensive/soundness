[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/simplistic" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Simplistic

__Simplistic__ provides minimalistic typeclass interfaces to absolve end users
of the need for cumbersome dependencies between unrelated projects.

## Features

- minimalistic typeclass interfaces
- integration for HTTP, HTML and CSS
- used by [Honeycomb](https://github.com/propensive/honeycomb/), [Cataract](https://github.com/propensive/cataract/) and [Scintillate](https://github.com/propensive/scintillate/)


## Getting Started

__Simplistic__ defines several typeclass interfaces to provide interoperability
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

This is what Simplistic provides.

## The Solution

The issues above can be circumvented by predefining a set of minimalistic
typeclass interfaces for each integration point necessary, and having each
library independently depend upon them.

As much as possible, the typeclass interfaces should not require any additional
datatypes to be defined; they should depend only on types in the standard
library. Whilst this may compromise the utility of these typeclasses, they are
intended to be used only by the integration libraries; not by end-users.

Continuing the earlier example, the libraries J and H could both depend on
Simplistic. J would then define a typeclass instance for its JSON type in its
companion object, and H would define a typeclass converter from Simplistic's
typeclass to its own user-facing typeclass interface in its companion object.

Consequently, users of both H and J could depend on both libraries, import both
packages (and nothing more) and automatically be able to use the integration
between them.


## Status

Simplistic is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Simplistic&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/simplistic`.
```
fury layer clone -i propensive/simplistic
```
or imported into an existing layer with,
```
fury layer import -i propensive/simplistic
```

## Contributing

Contributors to Simplistic are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/simplistic/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Simplistic easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
answers given in private.

## Author

Simplistic was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Simplistic_ means "extreme or misleading simplicity", which serves to describe Simplistic's
typeclasses, which are intended to be an absolute minimum interface between other libraries.

## License

Simplistic is copyright &copy; 2020-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

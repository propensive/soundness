[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/umbrageous/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/umbrageous/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Umbrageous

____

Shading a package, effectively renaming it, is one way to avoid conflicts
between packages with the same name. This is a scenario which frequently arises
in complex builts which include transitive dependencies on different versions
of the same library (often called the "diamond dependency problem").

Normally shading is performed on a binary after compilation, but this is slow
and can introduce new problems.  Umbrageous is a compiler plugin which makes it
trivial to compile a library into a package with a distinct prefix without
modifying its source code.

## Features

- rewrites package names at compiletime
- adds no time to the build process
- avoids the need to modify source code
- avoids slow bytecode rewriting after compilation
- customizable with compiler parameters; no configuration files required
- automatically unshades shaded packages in downstream compilations


## Availability Plan

Umbrageous has not yet been published. The medium-term plan is to build Umbrageous
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Umbrageous.

Subsequently, Umbrageous will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

To use Umbrageous, `scalac` must be invoked with two additional parameters:
```sh
scalac -d bin -Xplugin:umbrageous.jar -P:umbrageous:com.example:shaded *.scala`
```

Firstly, `-Xplugin:umbrageous.jar` points the compiler to a JAR file containing
the packaged plugin.

Secondly, `-P:umbrageous:com.example:shaded` tells the plugin to shade
everything inside the `com.example` package behind the prefix `shaded`, i.e.
rewriting `com.example` to `shaded.com.example`.

This `-P` parameter can be provided multiple times to shade different packages.
If more than one parameter matches a package name (e.g.
`-P:umbrageous:com:shade1` and `-P:umbrageous:com.example:shade2`) then the
prefix corresponding to the longest match will be applied to that package.

A downstream project can include an additional wildcard import at the start to
include everything from its shaded dependencies, for example,
```scala
package myproject
import shade.*  // additional import
import com.example
```
will allow the shaded `com.example` package (shaded by the `shade` prefix)
resolve anywhere in the scope of the import (whether imported, or referred to
by fully-qualified classname).

Including the parameter, `-P:umbrageous:com.example:shade` will automatically
unshade the `com.example` package from subsequent compilations without the need
for the import.

Note that this parameter is the same whether applying or using shading.
However, caution should be taken to avoid including nonexistant shaded
packages: doing so will add the synthetic wildcard import, but it will not
resolve, and a compile error will result.

### Limitations

Umbrageous does not currently match package names that are defined in multiple
`package` declarations, such as `com.example` in:

```scala
package com
package example

object Main // ...
```

Additionally, code which references an absolute name, such as
`_root_.com.example.Main` will not find the entity with its new, shaded name.




## Status

Umbrageous is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Umbrageous is designed to be _small_. Its entire source code currently consists
of 46 lines of code.

## Building

Umbrageous will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Umbrageous?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Umbrageous's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Umbrageous and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `umbrageous`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Umbrageous's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Umbrageous are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/umbrageous/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Umbrageous
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Umbrageous was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Plants which are umbrageous provide shade from light, and _Umbrageous_ shades Scala packages.

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

The logo shows the shadowy side of a mountain range.

## License

Umbrageous is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).


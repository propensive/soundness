[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/galilei/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/galilei/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Galilei

__Making I/O in Scala elegant and typesafe__

__Galilei__ is a library for performing safe disk I/O with Scala. It uses dependent types to provide
precise static representations of paths, files, directories and other filesystem objects, enforcing
filesystem-aware constraints on filenames and metadata. Galilei has a particular focus on precise error
handling and, when enabled, each filesystem operation which might fail must be handled for each possible
failure type; but its innovation is in minimizing that to a set of failure types which depends not
just on the operation, but also the filesystem and in-scope options.

## Features

- intuitive and instructive API for most common filesystem operations
- safe and precise error handling, without unnecessary handling
- read from and write to files on disk with a variety of different types


## Availability

Galilei has not yet been published as a binary.

## Getting Started

### Example

For example, moving a file from `/home/work/file` to `/home/work/copy` should work fine
if there is no pre-existing file at `/home/work/copy`. We can move it with,
`file.moveTo(destination)`. But if `/home/work/copy` already
exists, then we may or may not care about what happens if we try to overwrite it.

The behavior can be specified with a contextual value in scope. Either,
```scala
import filesystemOptions.overwritePreexisting
```
or,
```scala
import filesystemOptions.doNotOverwritePreexisting
```

The `moveTo` operation does not assume one option or the other as a default, and Galilei's
philosophy is that it would be wrong to do so. Instead, invoking `moveTo` _without_ exactly
one of the two contextal values in scope is a compile error, and the user is forced to
decide on the correct behavior. This is both unpresumptuous and instructive, since the
user may not have even considered the decision had to be made.

As a contextual value, the choice of behavior can be limited to a narrow scope, or
imported globally, as needed.

If Scala 3's "safer exceptions" are turned on, then the choice of behavior also affects
which exceptions must be handled when calling `moveTo`. The method invocation may throw
an `IoError` under any circumstances, so that must always be handled, but with
`doNotOverwritePreexisting` in scope, if there _is_ a pre-existing file at the destination,
then an `OverwriteError` will be thrown, which must be handled.

But since it cannot be thrown with `overwritePreexisting` in scope, the obligation to handle it
is also removed.

### Types

Unlike many disk I/O libraries, __Galilei__ provides different types for `Path`s, `File`s, `Directory`s
and other types of node, like `Symlink`s. A `Path` represents the abstract notion of a location within
a filesystem, which may or may not exist and may be a file, a directory or (on Linux, at least) one of
several other filesystem node types. Types such as `File` and `Directory` should only exist to
correspond to a real file or directory on disk.

Of course, the contents of a filesystem can change independently of the JVM, so the existence of
an immutable `File` or `Directory` instance does not guarantee its eternal existence on disk, but
we do, at least, guarantee that the filesystem node existed and had the correct type at the time
of the object's creation.



## Status

Galilei is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Galilei is designed to be _small_. Its entire source code currently consists
of 1146 lines of code.

## Building

Galilei can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Galilei are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/galilei/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Galilei easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Galilei was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Galilei's primary focus is handling Input and Output, or _I/O_, and is a pun on the name of the moon _Io_, one of the four moons of Jupiter discovered by Galileo Galilei.

### Pronunciation

`/ˌɡælɪˈleɪ/`

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo is the Galilean planet, Io.

## License

Galilei is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

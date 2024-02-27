[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/octogenarian/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/octogenarian/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Octogenarian

__Common Git operations for Scala__

Git has become the _de facto_ version control system for programmers, but its
core API is its command line interface, designed with a shell user foremost in
mind, yet with many operations designed for batch operation, or programmatic
usage. _Octogenarian_ is an attempt to provide a Git API in Scala, with all of
the capabilities available on the command-line, but presented through an
idiomatic API.

## Features

- implements core Git commands
- entitities like commits, branches and tags are represented as immutable values
- Git's API is reorganized to be more idiomatic
- long-running operations run asynchronously, and track progress


## Availability Plan

Octogenarian has not yet been published. The medium-term plan is to build Octogenarian
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Octogenarian.

Subsequently, Octogenarian will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

All terms and types are provided in the `octogenarian` package:
```scala
import octogenarian.*
```

### Types

Octogenarian introduces various types representing entities relevant to Git.

A `GitRepo` is an immutable value representing a Git repository, defined only by its filesystem location: its
Git directory, and optionally, its working tree. The contents of the repository are not represented by a
`GitRepo`, and the API presumes that the repository may be changed by other processes.

The opaque types, `Tag`, `Branch` and `CommitHash` represent tags, branches and commit hashes within a Git
repository, and all are subtypes of `Refspec`. Instances of these types may be returned by various Git
operations, and may be used as parameters to some operations.

`Commit` describes the details of a commit, including its hash, tree, parent(s), author, committer, signature
and message.

`SshUrl` is used to specify a SSH URL pointing to a remote Git repository.

Errors are represented by `GitError`s, for operational problems, and `GitRefErrors`, for reference validation
failures.

Finally, `GitProcess` is a mutable type representing a long-running Git process, and can be used to capture
progress updates as well as the final result of an operation.

### Getting a `GitRepo`

A `GitRepo` can be created from an existing repository on the filesystem, initializing a new one, or by cloning
a remote URL.

If the repository is already on disk, constructing it is as simple as invoking the factory, `GitRepo(path)`,
where `path` is some representation of the path on the filesystem, as long as an appropriate
[Anticipation](https://github.com/propensive/anticipation/) `GenericPath` typeclass is available. If using
[Galilei](https://github.com/propensive/galilei/) `Path`s, then it is.

A new, empty repository can be created with `Git.init(path)`, specifying a nonexistent path on the filesystem
to create it. The optional `bare` parameter, defaulting to `false`, makes it possible to create a bare Git
repository, i.e. without a working tree.

A repository can also be cloned from an existing repository, either remotely or locally. The `Git.clone` method
will clone a repository from a URL, a local path or a `SshUrl`, using any Anticipation-aware types, for example:
```scala
import nettlesome.*
import galilei.*

val repo = Git.clone(
  url"https://github.com/propensive/octogenarian",
  % / p"home" / p"work" / p"octogenarian",
  branch = Branch(t"main")
)
```

A further variant of `clone` exists which allows a single `Commit` value to be specified, `cloneCommit`.

### Operations on a `GitRepo`

Various operations are available on an instance of a `GitRepo`. These generally follow the naming of the Git
command-line subcommands, but a single `git` subcommand is sometimes split into more than one method:
 - `checkout`, checks out a `Tag`, `Branch` or `CommitHash`
 - `pull`, pulls from the default remote
 - `switch`, changes the current branch
 - `fetch`, fetches from the specified remote
 - `commit`, commits the currently-staged changes with the specified message
 - `branches`, lists the branches
 - `makeBranch`, creates a new branch
 - `tags`, lists the repository tags
 - `log`, reads changes to the repository as a stream of `Commit`s
 - `pushTags`, pushes tags to the remote

Implementations of other commands remain incomplete, and should be added in due course.

### Asynchronous operations

Certain operations, notably those involving fetching, will run asynchronously, and their progress can be
tracked. Asynchronous operations will return a `GitProcess` instance. Calling its `complete` method will return
its result synchronously, as soon as it finishes. But updates on its progress can be obtained as a
`LazyList[Progress]` with its `progress` member.



## Status

Octogenarian is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Octogenarian is designed to be _small_. Its entire source code currently consists
of 322 lines of code.

## Building

Octogenarian will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Octogenarian?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Octogenarian's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Octogenarian and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `octogenarian`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Octogenarian's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Octogenarian are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/octogenarian/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Octogenarian
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Octogenarian was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

In British English, a "git" is a derogatory term for a cantankerous old man,
quite possibly in his eighties, and thus an octogenarian.

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

The logo shows a stylized version of the Git logo.

## License

Octogenarian is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).


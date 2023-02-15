## Modularity

Every Scala One project strives for modularity, described by
[Wikipedia](https://en.wikipedia.org/wiki/Modularity) as,
> the degree to which a system's components may be separated and recombined,
> often with the benefit of flexibility and variety in use

Scala One is an ecosystem of software which provides a broad variety of diverse
functionality. Users of one small library should not be required to depend on
code (either source or binary) that's not used in the project. But at the same
time, modularization should not compromise how seamlessly different parts can
be recombined.

Scala One's approach to modularity successfully satisfies both criteria.

## Motivation

In a software ecosystem, adding a dependency upon another library is usually
relatively easy: we can include it in a build, and its types and functions
become available for us to use. Unfortunately, this comes at a cost with is
often underestimated. Each additional library we depend upon may conflict with
any other, and the more libraries we depend upon, the greater the risk of a
conflict.

Conflicts can take different forms. Namespace conflicts, where more than one
dependency provides a type or object with the same name, are relatively common,
but entirely possible to resolve: we can reference them by their
fully-qualified names, be selective in import expressions to exclude certain
names, or use a renaming import.

Dependencies may also conflict if two dependency libraries depend (directly or
transitively) on two different versions of a third library. This is known as
the "diamond dependency problem", and can be trivial or hellish to solve,
depending on the libraries involved. In its simplest form, it means finding a
"lowest common denominator" version of the third library which is compatible
with both of its dependents. If such a version does not exist, the problem may
be much harder (or take much longer) to solve, not least because it probably
requires a reliance on other people. If multiple such conflicts exist, as is
more likely to be the case with many dependencies, then the problem can be
exacerbated exponentially. This applies for both binary and source
compatibility, though different options may be available for resolving each
case.

The problem of having multiple conflicting contextual values of the same type
causing contextual (implicit) search problems was a very real problem in Scala
2, but is largely avoided in Scala 3.

For any program which is maintained over a period of time, every additional
dependency comes with a maintenance burden when that dependency is upgraded
(out of need, or just desire for the latest version) where new conflicts could
arise.

Furthermore, every dependency includes code which must be included in its
distribution. That might mean that downloads are larger or build times are
slower, or for many projects it may have negligible impact.

In general, there is a real cost associated with each additional dependency.

## Mitigation

For a library that may be included in other projects, there are several ways to
mitigate the problem:
1. by making it easier to resolve conflicts when they occur
2. by minimizing the number of dependencies that your dependents have to
   maintain
3. by reducing the risk that your library introduces a conflict

Every Scala One project is built using
[Fury](https://github.com/propensive/fury), an experimental source-based build
tool which aims to ease the process of managing dependencies, as well as
simplifying the publication and distribution process by [disincentivizing
fragmentation](https://fury.build/doc/distribution/). More details about how
Fury can help are on the [Fury website](https://fury.build/).

Each project is also conservative about adding dependencies; their inclusion
must be justified to be included as a dependency, particularly if that
dependency provides functionality that is largely unrelated. But often,
seamless operation between two libraries offering distinct functionality is
desirable and possible: imagine an HTTP server that serves XML content. An
HTTP server would not be justified in including a XML library, nor would a
XML library be justified in including an HTTP library. But we don't want to
have to include a third library which provides a small amount of integration
between them. Scala One's solution is to use typeclasses provided by
[Anticipation](https://github.com/propensive/anticipation/), which is a tiny
and common dependency of both
[Scintillate](https://github.com/propensive/scintillate/) and
[Xylophone](https://github.com/propensive/euphemism/). This ensures that
Scintillate and Xylophone are independent of each other, apart from the tiny
amount of code which preemptively agrees the integration between them.

This pattern is repeated throughout Scala One, and from the point of view of a
dependent project, the integration is seamless: no additional dependencies are
required in the build, and no additional imports are required in the code.

A further happy consequence is that Scala One projects have a free choice in
which others they integrate with: any XML library can integrate with
Scintillate, and Xylophone can integrate with any HTTP server, as long as the
appropriate Anticipation typeclass instances are available. These can be
handwritten in the main project, included from an external project, or built
into the dependency if its maintainers are happy to include the tiny amount of
integration code in the project itself.

Finally, every project is designed to be small and single-purpose. This
minimalism is crucial to reducing the likelihood of a conflict.

## Minimalism

No Scala One project contains more than a thousand lines of non-test code, and
most contain much less. Each module has a single area of functionality which it
aims to provide as well as it can. With so little code in each project, its
scope remains clear at all times.

Keeping a project small is also particularly advantageous to developers and
contributors: the scope and bounds of the project are easily understood, and
the entire code (or at least a clear picture of its design) can all fit inside
a programmer's head at the same time, even after just a few hours of
familiarization. The time taken to understand a project's code scales worse
than linearly with its size, so less code should generally be preferred.

Fewer lines of code also leave fewer places for bugs to hide: _obviously no
bugs_ is a much superior condition than _obviously no bugs_.

Sometimes a project's scope can expand into tangential areas, presenting a
candidate to extract into a separate project. It's not always a clear-cut
decision, but several considerations may suggest that it's beneficial to make
the split:
 - if each project is useful as a dependency _without_ the other
 - especially if one project requires a dependency the other does not
 - if the candidate project would benefit from a dependency that shouldn't be
   added to the original
 - if the amount of code has simply grown too large

Conversely, if two independent projects have similar dependencies (one may even
be a dependency of the other) and there's no use-case where a third-party
project would need one but not the other, then the two projects may be
candidates to be combined into one.

This largely follows the Linux approach to project granularity, where reuse and
composability are valued over exhaustive feature-completeness—which is closer
to the Windows model. In an open-source ecosystem, the former is certainly
preferable.

And with _compactness_ comes _stability_: smaller projects need to change less
frequently, whether those changes be compatibility-breaking API changes or
incidental tweaks. Each release is likely to have a longer lifetime, which
reduces the maintenance burden on dependents.

This puts a maintenance overhead on the Scala One maintainers to each new
project, but already having many projects, the marginal overhead of each
additional project is now minimal.




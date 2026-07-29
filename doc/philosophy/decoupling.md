# Decoupling

Soundness modules are decoupled: unrelated libraries do not depend on one another
directly, even when they need to interoperate. Small shared abstractions — typeclasses
describing only what is needed — let two modules cooperate without either knowing of
the other, so each can be adopted à la carte and can evolve on its own. The seamless
integration that users experience comes from these common interfaces, not from a web of
hard dependencies between components that ought to remain separate.

## The problem, stated concretely

A time library and an HTTP library both need to talk about instants: one produces them,
the other puts them in `Date` headers. There are three conventional ways to arrange
that, and all three are bad.

The HTTP library can depend on the time library — imposing it on every user who wanted
only HTTP. The time library can depend on the HTTP library — absurd, but it happens. Or
each can define its own instant type, leaving users to convert at every boundary, and
leaving the two definitions free to disagree.

## The arrangement Soundness uses

A third module defines a *marker* and a pair of typeclasses saying how any type converts
to and from a common representation:

```scala
sealed trait Instants   // the marker: "this conversion is about instants"

given Long is Abstractable across Instants to Long
given Long is Instantiable across Instants from Long
```

The time library declares that *its* instant type is abstractable across `Instants`. The
HTTP library declares that it accepts anything abstractable across `Instants`. Neither
mentions the other, and neither is loaded unless the program uses it.

These markers are small and there are not many: `Instants`, `Durations`, `Dates`,
`Paths`, `Urls`, `HttpStreams`, `Text`, and a handful more. Each names one concept that
several libraries need to agree about, and defines nothing but the agreement.

## What it buys

**À la carte adoption.** A program that wants only JSON parsing gets a dependency graph
containing JSON parsing. The integrations it does not use are not in the build, because
the modules that provide them are not depended upon.

**Foreign types participate.** The typeclass has no privileged implementations, so a
`java.time.Instant`, or a type from an unrelated library, becomes usable wherever an
instant is expected by supplying an instance. Nothing has to be wrapped, and the
integration lives in the program rather than requiring either library to change.

**Independent evolution.** The time library can change its internal representation
without the HTTP library recompiling, because what they share is the marker and the
shape of the conversion, not a type.

## The same principle inside a module

Decoupling operates below the module boundary too, as a *backend seam*: the operations a
capability needs are gathered into one trait, and the platform-specific implementations
are supplied separately.

`FilesystemBackend` collects stat, open, read, write, list, link and delete; everything
in the [filesystem](../modules/filesystem.md) API is defined in terms of them. The
`java.nio` implementation, the WASI implementation over `wasi:filesystem`, and any
future one are interchangeable, and the user-facing API mentions no platform at all. The
same shape gives [sockets](../modules/sockets.md) a `SocketBackend`,
[compression](../modules/compression.md) its per-platform DEFLATE engines, and
[images](../modules/images.md) their codecs.

That is what makes the same code run on the JVM, in a browser, in a native binary and
inside a WebAssembly component: not conditional compilation, but a seam narrow enough
that several implementations can honestly sit behind it.

## What it costs

Indirection is not free to read. Following how a `Yaml` value becomes an HTTP body means
finding the `Abstractable across HttpStreams` instance, which is not where either the
YAML code or the HTTP code is — and a reader who does not know the convention will not
guess where to look.

The mitigation is that there is *one* convention rather than a different mechanism per
integration, so the cost is paid once in learning and not once per boundary. But it is a
real cost, and it is the reason the set of markers is kept small: each one is a piece of
vocabulary every reader must eventually acquire.

See [honest signatures](honest-signatures.md) for why the typeclasses stay minimal, and
[composability](composability.md) for what the shared interfaces make possible.

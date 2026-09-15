# Method Naming Conventions

This document defines the canonical meanings of named factory and initialisation
methods used consistently across the codebase. For *why* names are chosen as they
are — phrase-forming methods, word-length type parameters, and uniqueness across the
whole namespace — see [naming](../philosophy/naming.md) and
[elegant prose](../philosophy/elegant-prose.md).

## Standard names

### `apply`

The standard Scala factory method. Used whenever possible. All other names in
this document exist because `apply` cannot be used — typically due to overload
resolution conflicts with an existing `apply`.

### `build`

Assembles an instance via a caller-provided callback that drives construction.
The two concrete forms are:

- **Allocate → mutate → freeze**: a mutable intermediate structure is allocated,
  passed to the caller's lambda for population, then frozen into an immutable
  result. Examples: `Data.build`, `IArray.build`, `Text.build`.
- **Callback-driven assembly from source data**: the caller supplies a function
  that specifies how to extract or transform fields from an origin value.
  Examples: `Specification.build`, `Addressable.build`.

In both forms the caller controls the assembly process through the function
argument rather than supplying the finished value directly.

### `create`

Brings a resource into existence in an external system, and always has
side-effects. Used for filesystem resources (directories, files,
sockets, FIFOs). The counterpart to `apply`/`build` for operations that are not
pure.

### `make`

Constructs a value from dynamically-named inputs, and corresponds to an
implementation of `applyDynamicNamed`. While other factory may use parameters
with dynamic names, `make` is used where another non-dynamic `apply` method
already exists.

### `initialize`

Returns a fresh operational context for a repeatable operation. Examples
include a new hash-digest run (`Hash.initialize`), a cipher instance
(`Aes.initialize`), an open file transport (`Openable.initialize`), or a new
session (`McpServer.initialize`). The operation may be pure or involve resource
acquisition; what unifies these cases is that the result represents the
beginning of a single use of an operation that can be started again from
scratch.

## Non-standard names

### `init`

Used only for `Git.init`, which follows the naming of the `git init` CLI
command. Everywhere else, `initialize` is preferred.

### `construct`

Reserved exclusively for `wisteria.ProductDerivation.construct`. This is a
deliberate special case used in compile-time typeclass derivation and does not
follow the general factory naming scheme.

### `of`

Used only as an instance method that narrows a type to a phantom-parameterised
form `X of Y`. The body is always a cast (`asInstanceOf`), and the return type
is literally `this.type of topic`. These methods are private to their
respective packages.

## The `unsafe` prefix

A method that takes `vacuous.Unsafe` as a `using` parameter is gated: its caller
must hold the token, which in practice means writing `(using Unsafe)` or sitting
inside an `unsafely` block. Every such method is named with an `unsafe` prefix
followed by a capital letter — `unsafeBuffer`, `unsafeAttested`, `unsafeChild` —
so that the boundary is visible at the call site and not only in the signature.

This is checked. The Consequent rule `S1.1` is `strict` for every component, so a
gated method with an ordinary name fails the build. Where the name cannot take a
prefix, the method is renamed rather than exempted: `Regex.apply(parts)(using
Unsafe)` became `Regex.unsafeFrom(parts)`.

Two definitions are outside the rule because neither has a name to prefix: a
constructor, and a `given`, which is summoned by type. Both are still gated, and
the argument for why the gate is sound belongs in a comment beside them.

The converse — an `unsafe`-prefixed method must be gated — is `S1.2`, and is
advisory rather than strict for now. Ten definitions break it:
`proscenium.Array.unsafeFrozen` and `unsafeJvm`, `vacuous.Optional.unsafeGet`,
`archimedes.Ergo.unsafe` and `unsafeInterpolate`, and the `unsafe` factories on
`urticose.Port`, `octogenarian.Refspec`, `Git.Tag`, `Git.Branch` and `Git.Hash`.

Gating them is wanted and is tracked as a follow-up, not declined: a trial found
the change reaches hundreds of call sites, a good share of which are colon-block
calls (`Array.unsafeFrozen:` over an indented argument) that cannot take a
further argument list without being restructured. `Optional.unsafeGet` needs its
own change first: it sits in an extension group carrying a `using
Optionality[optional.type]` clause, and an explicit `(using Unsafe)` binds to
that leading clause instead, so the method has to leave the group before it can
be gated.

Until then the census counts them as `unsafe-ungated`, which is the reliable
record: a `S1.2` warning is dropped whenever the compiler has already reported
something at an enclosing position in the same file.

The prefix is a marker, not a licence. It says a guarantee stops here, which is
the beginning of an argument for why that is sound, not the end of one.

`unsafely` itself is not covered: the prefix must be a whole word — `unsafe`
alone, or `unsafe` then a capital — so the block that supplies the token is
exempt by construction, while a method named plainly `unsafe` is not.

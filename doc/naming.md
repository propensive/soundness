# Method Naming Conventions

This document defines the canonical meanings of named factory and initialisation
methods used consistently across the codebase.

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

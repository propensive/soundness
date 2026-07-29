# Honest Signatures

A signature in Soundness tells the truth, the whole truth, and nothing but the truth
about what an operation needs. The most important part is the *nothing but*: a
signature declares no requirement unless it is always needed. A typeclass describes the
single capability it abstracts, without widening its methods to accommodate what some
implementation might want — no context parameter "just in case", no error declared
because one implementation can fail.

Requirements that only certain implementations have belong on those implementations. A
`given` declares in its own signature the capabilities its particular implementation
depends on — a network-backed instance requires `Online`, a fallible one a `Tactic` —
and the requirement is discharged where the instance is chosen, not imposed on the
typeclass and every other implementation of it. So the effective requirements of a
method are determined by the instances it actually uses: the same generic code is pure
with a pure instance and networked with a networked one, and its own signature claims
neither.

The result is signatures that can be trusted in both directions. Nothing an operation
requires is hidden, so a caller sees the true cost; and nothing it declares is unused,
so a requirement in a signature is always meaningful. A minimal typeclass is also
easier to implement — the gap between what an interface demands and what most
implementations need is exactly where dishonest signatures come from.

## The requirement travels with the instance

A typeclass says only what it abstracts:

```scala
trait Showable:
  def text(value: Self): Text
```

No error, no context, no capability — because showing a value, in general, needs none.
An instance that *does* need something declares it in its own signature, where it is
true:

```scala
given (Tactic[CryptoError]) => PrivateKey is Showable = …
```

The consequence is that the effective requirements of generic code are decided by the
instances it resolves. The same function is pure when given a pure instance and fallible
when given a fallible one, and its own signature — honestly — claims neither.

## What a dishonest signature looks like

The failure mode is easy to recognise once named. An interface is widened so that the
most demanding implementation fits:

```scala
trait Codec:
  def encode(value: Self)(using Online, Tactic[CodecError]): Text
```

One implementation fetches a schema over the network; one can fail; so the *interface*
requires both, and now every caller of every codec must supply an `Online` capability
and an error strategy, whether or not the instance they are using needs either.

This is worse than an inconvenience. The signature has stopped carrying information: a
reader who sees `Online` can no longer conclude the operation touches the network,
because the requirement is there for a reason that may not apply. Once a few
requirements are known to be spurious, none of them can be trusted, and the discipline
collapses.

## What the signatures do carry

Because they are minimal, the clauses that *are* present mean something:

```scala
def fetch(url: HttpUrl)(using Online): Http.Response
def loadConfiguration(path: Path on Linux): Configuration raises IoError
def install(target: Path on Linux): Unit logs InstallEvent
def fetchAll(urls: List[HttpUrl])(using Monitor): List[Text]
```

`Online` means it reaches the network. `raises` means it can fail, and how. `logs` means
it emits events of that type — so a caller knows what to route where. `Monitor` means it
can suspend the calling strand.

A method with none of these does none of those things, which is a claim worth being able
to make.

## Honesty about capture

There is a subtler dishonesty a signature can commit: returning a value that secretly
depends on something scoped. A stream still holding an open file, a closure still holding
an error handler — the type says `Stream[Text]` and the truth is "a stream, but only
while that file is open".

[Capture checking](capture-checking.md) makes that part of the signature too. A value's
type records the capabilities it captures, so an operation cannot quietly return
something whose validity is narrower than its type suggests. This is why the honesty
principle and capture checking belong together: one is about what an operation *needs*,
the other about what its results *depend on*, and both are cases of a signature stating
the whole truth.

## What it costs

**Instances carry more.** Pushing requirements onto instances means the instances declare
them, and a fallible instance's signature is longer than the naive version's. The reader
who benefits is the one calling the generic code, not the one writing the instance.

**Requirements propagate.** A method calling something that needs `Online` must either
supply it or declare it. That is the mechanism working — the cost of a signature meaning
something is that it must be maintained — but it does mean a change deep in a call chain
can surface at the top.

**Resolution failures are indirect.** When a needed instance is missing, the compiler
reports a failed implicit search rather than "this operation needs `Online`". Importing
`explainMissingContext` turns that into a diagnosis naming the import that would satisfy
it, which is the repair for a cost that would otherwise fall on exactly the people the
honesty is meant to serve.

See [declarative context](declarative-context.md) for how the requirements are supplied,
and [error handling](error-handling.md) for the most common of them.

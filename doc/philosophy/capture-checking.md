# Capture Checking

Soundness uses Scala's capture checking to strengthen the guarantees that its
[scoped capabilities](delimited-scopes.md) already express. Capture checking tracks
which capabilities a value depends on, so the compiler can prove that a resource, an
error handler, or a concurrency context does not escape the block that established it.
What scoping states by structure, capture checking enforces by type.

What becomes impossible is the family of escape bugs that scoping alone cannot stop —
each one a value smuggling a dead capability out of its block:

```scala
// a lazy stream escaping the file that feeds it
val lines = file.open(_.stream[Text])
lines.head   // without capture checking: reads from a closed handle

// a closure escaping its error handler
val f = safely(() => parse(input))
f()          // without capture checking: raises with no handler in scope

// a task escaping its supervision
val task = supervise(async(compute()))
task.await() // without capture checking: awaits under a completed supervisor
```

Under capture checking, each of these is a compile error: the stream captures the open
handle, the closure captures the `Tactic`, the task captures the `Monitor`, and none of
those capabilities may outlive its scope. The fix the compiler forces is the right one —
consume the stream inside the block, run the fallible code where its handler lives,
await the task under its supervisor.

The guarantee runs deeper than discipline could: laziness, closures and concurrency are
exactly the features that defeat by-eye reasoning about lifetimes, because they detach
*when* code runs from *where* it was written. Capture checking reattaches them — the
capability's scope travels in the type, so an escape is impossible to write rather than
inadvisable to attempt.

## Separation checking

Capture checking answers "may this value outlive that scope?". Its companion,
*separation* checking, answers a second question: "may two references to this value exist
at once?"

Some resources are safe to share and some are not. A `Stream` in the streaming kernel
exposes a mutable window into a buffer without copying it, which is what makes the kernel
fast — and which is only sound if exactly one consumer reads it. Two aliases would each
consume bytes the other expected.

So a `Stream` is an *exclusive* capability, and aliasing one does not compile:

```scala
val stream = file.stream
val other = stream          // does not compile: the stream is exclusive
```

The modules that depend on this — the kernel itself and its immediate consumers — are
compiled with separation checking enabled, so the guarantee is checked rather than
documented. This is what allows a zero-copy design to be offered as an ordinary API
instead of as an unsafe one with a warning attached.

## What the compiler asks of you

The honest part of this document. Capture checking is not free, and the costs fall on the
person writing the code, not the person reading it.

**Annotations appear in signatures.** A method returning a value that captures something
says so — `(Stream[Data] over Credit)^` — and a parameter that will be consumed rather
than borrowed is marked `consume`. These are real syntax that a reader must learn, and
they propagate: a signature that returns a capturing value forces its callers'
signatures to acknowledge it.

**The errors are hard.** A capture violation is reported in terms of capture sets and
sometimes of skolem variables, which is a considerable distance from "this stream would
outlive its file". Reading such an error fluently takes practice, and the practice is not
transferable from ordinary Scala.

**It is still maturing.** Capture checking is a research-grade feature of the compiler,
and Soundness uses a fork with fixes not yet upstream. Some idioms that ought to work do
not yet — the aliasing-heavy zlib port in the compression module is compiled with capture
checking but not separation checking, because the stricter ruleset rejects a faithful
port of code that is nonetheless correct.

**Some escape hatches remain.** `caps.unsafe.unsafeAssumePure` and
`@caps.unsafe.untrackedCaptures` exist for the places where the checker cannot yet see
what the author can prove. Each use is a small hole in the guarantee, and each is
commented with the argument for why it is sound — which is the right way to hold such a
thing, but it is not the same as not needing them.

## Why it is worth the cost

Because the alternative is not "no cost" but "the same cost, paid later and by someone
else".

A stream read after its file closed is a bug that reproduces intermittently, under load,
in a stack trace pointing at the read rather than at the escape. A closure that raises
with no handler in scope fails at a call site far from where the closure was made. These
are among the hardest classes of bug to diagnose, precisely because the cause and the
symptom are separated by exactly the mechanisms — laziness, closures, concurrency — that
capture checking exists to track.

The compiler's complaint is early, local, and about the actual mistake. That is a better
trade than it first looks.

See [delimited scopes](delimited-scopes.md) for the structure this enforces, and
[expressive errors](expressive-errors.md) for why errors must be pure — `throw` being the
one channel this analysis cannot see.

# Structured Concurrency

Concurrency in Soundness is structured: tasks are spawned within a scope that owns
them, and that scope does not complete until its tasks have. A failure in one task
propagates to its siblings and its parent rather than vanishing unnoticed, and
cancelling a scope cancels everything beneath it. Concurrent lifetimes nest like the
blocks of ordinary code, so parallel work has the same clear beginning and end as
sequential work, and no task outlives the scope that launched it.

## The scope owns the task

`supervise` opens a scope; `async` spawns within it. The scope does not return until its
children have settled, whether or not anything awaited them:

```scala
supervise:
  val first = async(fetch(urlA))
  val second = async(fetch(urlB))

  (first.await(), second.await())
```

The unstructured equivalent — starting two threads and hoping — differs in what happens
when something goes wrong, which is the only case worth designing for.

## What the structure buys

**A failure has somewhere to go.** A thread that throws has nowhere to report to, so its
exception is printed, or swallowed, or delivered to a handler that has no idea what was
being attempted. A task's failure propagates to the scope that owns it, which cancels
that scope's other children — because work undertaken on behalf of a computation that
has already failed is work nobody wants finished.

**Cancellation reaches the whole subtree.** Cancelling a scope cancels its children,
which cancel theirs. There is no set of thread references to maintain and no possibility
of missing one, because the ownership tree already exists.

**Nothing outlives its reason for existing.** A task cannot outlive the scope that
launched it, so "is that background work still running?" has a structural answer rather
than an empirical one.

## What happens at the boundary

A scope must decide what to do about a child still running when the parent's body
finishes, and that decision is explicit rather than assumed. The *probate* names it:

```scala
import probates.cancelProbate   // unfinished children are cancelled
import probates.awaitProbate    // the scope waits for them
```

Both are defensible and neither is safe to guess at, which is why there is no default.

A `daemon` is the deliberate exception: fire-and-forget work that the scope does not wait
for and cancels when it ends. It still cannot outlive its scope — it is unstructured only
in that nothing awaits its result.

## Suspension is visible

Waiting is an effect, and it appears in the types like any other. Awaiting a task or a
promise, sleeping, and yielding all demand a `Monitor` capability, so a method that can
block says so in its signature:

```scala
def fetchAll(urls: List[HttpUrl])(using Monitor): List[Text] = …
```

This is the same discipline that [honest signatures](honest-signatures.md) apply to
failure. A caller can see, from the type alone, whether a method might park the thread it
is called on.

Underneath, the unit of execution is a `Strand` rather than a thread, with the four
operations suspension needs: interrupt, join, park and unpark. Because a supervisor
supplies strands and a supervisor is a value, a scheduler that is not built on threads —
an event loop over WebAssembly's waitable sets — plugs in without changing any code that
spawns or awaits.

## A daemon's errors do not travel backward

One subtlety follows from taking ownership seriously. A daemon's body cannot capture an
error handler from the code that spawned it, because by the time the daemon fails, that
code has moved on — delivering the error there would mean resuming a computation that
has already finished. A failing daemon therefore escalates as a `Fault`, which a program
intercepts where it handles the unexpected.

This is the kind of case that unstructured concurrency leaves undefined and that
structure forces into the open: the question "where does this error go?" always has an
answer, even when the answer is "not where you might have assumed".

## What it costs

Structure constrains what can be expressed. Work that genuinely must outlive its
initiating scope — a background service started during a request, say — cannot simply be
spawned and forgotten; it must be owned by a scope that lives long enough, which means
arranging one. That is more work than starting a thread, and it is more work for a
reason: an unowned thread is exactly the thing whose lifetime nobody can subsequently
reason about.

See [delimited scopes](delimited-scopes.md) for the general shape this is an instance of,
and [capture checking](capture-checking.md) for how escape is made impossible rather than
merely discouraged.

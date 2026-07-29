# Immutability

Soundness values are immutable: once constructed, a value never changes, and any
operation that would alter it instead returns a new value. This removes whole
categories of bug — aliasing surprises, data races, and changes felt at a distance —
and makes a value safe to share freely across threads and scopes without defensive
copying. An immutable value is also easier to reason about, because its meaning is
fixed at the moment it is created and cannot be invalidated by code running somewhere
else.

## Updating without mutating

The objection to immutability is always the same: changing one field deep inside a
structure means rebuilding every layer above it. That is true, and it is a problem of
syntax rather than of semantics — so Soundness solves it with syntax. A
[lens](../modules/optics.md) takes the update written as an ordinary path assignment and
does the rebuilding:

```scala
case class Role(name: Text, count: Int)
case class Person(name: Text, roles: List[Role])
case class Company(ceo: Person, name: Text)

company.lens(_.ceo.name = t"Bill")
```

The result is a new `Company`; the original is untouched and any other reference to it
still sees what it saw. Several updates apply together, and ones sharing a prefix
rebuild the shared structure once rather than once each:

```scala
company.lens
  ( _.ceo.roles = Nil,
    _.ceo.name = t"Bill" )
```

The same syntax reaches through collections with `Each` and `Filter`, so "give every
role a count of zero" is one expression rather than a rebuild written by hand.

## Sharing without copying

An immutable value needs no defensive copy, because there is nothing to defend against.
A `Data` handed to three threads is read by three threads; a parsed document held in a
cache cannot be altered by whoever fetched it; a configuration passed to a subsystem
cannot come back changed.

That is what makes structural sharing safe. A lens rebuilds only the spine it descends,
and every branch it did not touch is *the same object* in the old value and the new one.
Immutability is therefore what makes the rebuilding cheap: without it, sharing a
sub-structure between two values would be a bug.

## Where mutation survives, and why

Immutability is a property of values, not a prohibition on the machine. Three places in
Soundness are deliberately mutable, and each is confined:

A **builder** mutates a buffer it exclusively owns and yields an immutable value at the
end. Nothing observes the intermediate states, so nothing depends on them.

The **streaming kernel** exposes a mutable window into a buffer, which is precisely what
makes it zero-copy. That window belongs to exactly one consumer, and the compiler
enforces it: a `Stream` is an exclusive capability, so aliasing one, or letting one
escape the scope that owns its source, does not compile. Mutation is permitted here
because [capture and separation checking](capture-checking.md) prove that nobody else
can see it.

A **`Canvas`**, obtained by opening a [raster](../modules/images.md) for writing, mutates
pixels in place — because generating a large image one pixel-function call at a time is
the wrong shape for a drawing algorithm. The mutation is scoped to the `open` block and
gated on the `Write` grant, and `snapshot` takes an independent copy for anything that
must outlive it.

The pattern is the same in all three: mutation is a local implementation technique with
a proof of confinement, never a property of a value that others can see.

## What it costs

Rebuilding is not free. A lens update allocates one object per level it descends, so a
tight loop updating a deep structure allocates more than in-place mutation would. Where
that matters — and it matters far less often than intuition suggests, since the
allocations are young and short-lived — the answer is a builder or a scoped mutable
region, not making the value itself mutable.

The trade is deliberate: pay a little allocation to eliminate a class of bug that is
otherwise found only in production, on a Tuesday, under load.

See [impossible states](impossible-states.md) for the related discipline on what a value
may be, and [capture checking](capture-checking.md) for how the confined mutations are
proved safe.

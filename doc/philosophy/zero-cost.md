# Zero Cost

Soundness pays for its safety at compiletime, not at runtime. Opaque types, inlining,
and type-level computation mean that the guarantees a type carries — the units on a
quantity, a validated name, a checked path — usually compile down to the bare value they
wrap, with no wrapper object allocated and no check left to run. Safety is therefore not
a tax on performance: the very code that is provably correct is also as fast as the
unchecked version a careless programmer would have written by hand.

## What the types cost at runtime

Nothing, in the common case, because they are not there. An opaque type is its underlying
representation with a different name for the compiler's purposes only:

```scala
into opaque type Text <: Matchable & caps.Pure = String & caps.Pure
opaque type Quantity[units <: Measure] = Double
opaque type Unset <: Matchable & caps.Pure = Null & caps.Pure
opaque type Location = Long          // latitude and longitude, packed
opaque type Isin = Long              // a validated security identifier
```

`Optional[value]` is then simply `Unset | value` — a union with a null-backed sentinel,
so an absent value costs one null reference and a present one costs exactly the value.

A `Quantity[Metres[1]]` is a `Double`. A million of them in an array is a million
doubles, not a million objects. The units exist while the code is being typechecked and
are gone by the time it runs.

The same reasoning covers the operations. They are `inline`, so a dimensional calculation
becomes the arithmetic instructions the equivalent bare-`Double` code would have produced
— no wrapper to allocate, no typeclass method to dispatch to:

```scala
def displacement
    ( initial: Quantity[Metres[1] & Seconds[-1]],
      time:    Quantity[Seconds[1]],
      accel:   Quantity[Metres[1] & Seconds[-2]] )
:   Quantity[Metres[1]] =
  initial*time + 0.5*accel*time*time
```

Every dimension in that signature is checked as the code compiles. None of it survives
into the bytecode.

## The claim is tested, not asserted

A performance claim that is only stated tends to stop being true — an inline annotation
removed, a typeclass method that no longer specialises, and the guarantee is quietly
gone while the documentation still promises it.

So the test suite reads the compiled bytecode and fails if the guarantee has lapsed. It
parses the classfile, walks the instructions of representative calculations, and asserts
that no virtual or interface dispatch to a typeclass operation survives and that no
boxing appears:

```scala
def callsTypeclassOp(bytecode: Bytecode): Boolean =
  val ops = Set(t"negate", t"add", t"subtract", t"multiply", t"divide", t"root", t"op")
  bytecode.instructions.exists: instruction =>
    instruction.opcode match
      case Bytecode.Opcode.Invokevirtual(_, name, _)      => ops.has(name)
      case Bytecode.Opcode.Invokeinterface(_, name, _, _) => ops.has(name)
      case _                                              => false
```

The [metaprogramming](../modules/metaprogramming.md) module exists partly to make this
possible: if a claim about what the compiler emitted is worth making, it is worth
checking on every build.

## Where the cost is not zero

Being honest about this matters more than the claim itself.

**Checked arithmetic costs what it checks.** Importing `arithmeticOptions.overflow.checked`
adds a real test to every addition. The default is unchecked, so the cost is paid only
where it is asked for — but where it is asked for, it is paid.

**Validation costs once, at construction.** Turning text into a `Port` parses and range-checks
it. The saving is that nothing downstream checks again, not that the first check is free.

**Some safety is genuinely structural.** A `Stream`'s single-ownership discipline is
enforced by the type system at no runtime cost, but the buffering and windowing that make
it zero-copy are real machinery. Capture checking proves the machinery is safe; it does
not make it disappear.

**Compiletime is the bill.** Type-level computation, inlining and macro expansion are work
done on every build to avoid work on every run. A Soundness project compiles more slowly
than a loosely-typed equivalent, and `-Xmax-inlines` is a setting that occasionally has to
be raised. That is the trade, stated in the direction it actually runs.

## Why this principle is load-bearing

Without it, every other principle in this collection becomes a negotiation. A team that
believes safety costs performance will reach for the unchecked type in the inner loop —
and the inner loop is exactly where a mistake is most expensive.

If the safe version is also the fast version, the question never arises. That is why
"zero cost" is treated as a design constraint on new abstractions rather than as a happy
observation about existing ones: an abstraction that cannot be made to disappear is
reconsidered before it is added.

See [correctness](correctness.md) for what the compiletime bill buys, and
[optionality](optionality.md) for the most pervasive instance of a wrapper removed.

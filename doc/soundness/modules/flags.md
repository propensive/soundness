## Bit Flags

### About

A `Flags[Color]` is a set of on-or-off switches, one for each case of the `Color`
[enumeration], held in the bits of a single `Long`. A flag is set and tested by naming its case,
never by a bit number. The value stays one machine word, so it is as cheap to copy and store as
a number, but it reads like a set.

### On flag sets

Bit flags are one of the oldest data structures in systems programming. A program writes
`flags | 0x10` to switch an option on and `flags & 0x04` to test another, and the meaning of each
bit is recorded in a comment, if anywhere. The compact representation is worth keeping: a
`Set[Color]` costs an allocation and several pointers to hold what fits in one word.

The untyped access is not worth keeping. Nothing stops a program testing a bit that belongs to a
different set of options, or a bit that means nothing at all.

`Flags` keeps the word and gives the bits types. Each case of an enumeration owns the bit at its
ordinal: the first case owns bit 0, the second bit 1, and so on. A flag can only be named by a
case of the right enumeration, so a flag from another set of options is an
[impossible state](../../philosophy/impossible-states.md), rejected at [compiletime]. Everything
comes from the `soundness` package. The second import lets the samples print their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Setting and testing flags

`Flags[Color]()` makes a flag set for the enumeration `Color`, with every flag off:

```scala
enum Color:
  case Red, Orange, Yellow, Green, Blue, Indigo, Violet

val none = Flags[Color]()
```

Assigning `true` to a case switches its flag on. A `Flags` value is [immutable], so the assignment
is an expression whose value is a new flag set, and `none` is left as it was:

```scala fuel
highlight none(Color.Orange) = true  a new `Flags[Color]`, with Orange on
##
val warm = none(Color.Orange) = true
```

Applying a flag set to a case tests its flag:

```scala fuel
run
##
Out.println(warm(Color.Orange).inspect)
Out.println(warm(Color.Red).inspect)
Out.println(none(Color.Orange).inspect)
```

A program that switches several flags usually keeps the current set in a `var`, and assigns each
new set back to it. Assigning `false` switches a flag off again:

```scala
var flags = Flags[Color]()
flags = flags(Color.Orange) = true
flags = flags(Color.Green) = true
flags = flags(Color.Violet) = true
flags = flags(Color.Green) = false
```

Because every step produces a new value, a flag set can be shared between threads, or stored in
another data structure, as safely as the `Long` it is.

### Flags as a set

`set` reads the flags that are on as an ordinary `Set` of the enumeration's cases, ready for
iteration and the [set operations](collections.md#sets-and-maps):

```scala fuel
run
##
Out.println(flags.set.show)
Out.println(none.set.show)
```

### What the compiler rejects

A flag set accepts only the cases of its own enumeration. A case of another enumeration is a type
error, where a raw bit mask would have silently tested whichever bit had the same position:

```scala
enum Weekday:
  case Monday, Tuesday, Wednesday
```

```scala fuel
compile
errors show
##
flags(Weekday.Monday)
```

### How many flags

A flag occupies the bit at its case's ordinal, and a `Long` has 64 bits, so an enumeration of up
to 64 cases fits in one flag set. The cases, their names and their ordinals come from the
enumeration's `Enumerable` instance, which is [derived][derivation] from the `enum` declaration;
[Enumerations by name and position](decoding.md#enumerations-by-name-and-position) shows what
else that instance provides.

### Sets of integers

A `Flags` value is for the cases of one enumeration, and stops at 64 of them. A `Bitset` is the
same idea for any non-negative integers: a set which costs one bit for each possible member, and
grows to its largest. It suits a set of small numbers which is consulted often, such as the rows
of a table which have been visited, or the positions in a text which matched.

Its members are `Ordinal`s, and it is a collection like any other. `has` tests a member, `+` and
`-` give a set with one added or removed, and `+` of two sets is their union:

```scala fuel
run
##
val visited = Bitset(1.z, 5.z, 64.z)
val more = visited + 300.z

Out.println(visited.has(5.z).inspect)
Out.println(visited.has(300.z).inspect)
Out.println(more.has(300.z).inspect)
Out.println((visited + Bitset(5.z, 6.z)).size.show)
```

`intersect` and `except` are as for a `Set`, and a `Bitset` is traversed in ascending order,
whatever order it was made in:

```scala fuel
run
##
visited.intersect(Bitset(5.z, 64.z, 65.z)).each: ordinal =>
  Out.println(ordinal.n0.show)
```

A set which is filled in a loop is built in place. `Bitset.build` lends a block a builder to
write to, and gives back an immutable `Bitset` when the block ends, as
[`Map.build`](collections.md#sets-and-maps) does for a map:

```scala fuel
run
##
val thirds = Bitset.build: bits =>
  (0 until 20).each: index =>
    if index%3 == 0 then bits.add(index.z)

Out.println(thirds.size.show)
```

### Related

- [Numbers](numbers.md) and [binary data](binary-data.md) describe the bits beneath a set of flags.

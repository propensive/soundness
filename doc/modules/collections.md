## Collections

### About

Soundness has its own collection types — `List`, `Sequence`, `Set`, `Map` and the lazy `Chain` —
and one vocabulary of operations over all of them, and over text, which traverses as its
characters. The types are *opaque*: each is a thin, zero-cost wrapper over the standard library's
structure, exposing a smaller, total API in which an operation that could fail returns an
`Optional` rather than throwing, an operation whose cost is not what its name suggests is gated
behind an explicit acknowledgment, and sorting names the algorithm it uses.

The operations are typeclass-driven, so a new shape — a byte buffer, a parsed document, a stream
of records — gains the whole vocabulary by supplying one instance, and the vocabulary reads the
same whatever it is applied to.

### On collections

The standard library's collections are large, partial and quietly expensive. `head` throws on an
empty list; `apply` on a `List` is linear but looks like an array read; `sorted` picks an
algorithm for you and requires an `Ordering` that a `String` supplies without asking which
language's order it means; and every type carries several hundred methods, many of them
duplicates under other names.

Soundness keeps the structures and replaces the surface. The wrappers are opaque, so nothing is
copied and nothing costs more than before — a `List` is still a linked list — but the methods
are the ones a program needs, each returning a type that says what it can and cannot do. Where a
method's cost depends on the structure, the program says it knows: counting a linked list is a
traversal, so a `List`'s `size` is available only where the *linear size* acknowledgment is in
scope. Everything comes from the `soundness` package, with a sorting algorithm chosen for the
examples that sort:

```scala
import soundness.*
import sortingAlgorithms.timsort
import strategies.throwUnsafely
```

### The types

A `List` is a linked list, cheap to prepend to and to traverse; a `Sequence` is indexed, cheap to
read at any position; a `Set` holds each element once; a `Map` pairs keys with values; and a
`Chain` is a lazy sequence whose elements are computed as they are demanded and remembered
afterwards, which may therefore be infinite. Each is constructed by applying its companion, and
`Nil` is the empty list:

```scala
val numbers: List[Int] = List(3, 1, 2)
val letters = Sequence(t"a", t"b", t"c")
val primes = Set(2, 3, 5, 7)
val ages = Map(t"Ada" -> 36, t"Bob" -> 41)
val naturals: Chain[Int] = Chain.iterate(0)(_ + 1)
```

A list built from a known number of elements is typed as *populated*, and its `head`, `last`
and `tail` are total on that type; on a `List` of unknown length they are not available, and the
`Optional`-returning accessors are used instead.

### Positions and access

Positions are [ordinals](numbers.md), not integers: `Prim`, `Sec` and `Ter` name the first three,
and `Ordinal.zerary` converts a zero-based index. Applying a collection to an ordinal reads the
element there, as an `Optional`, since the position may lie beyond the end, and `prim`, `sec`
and `ter` read the first few directly:

```scala
letters(Sec)                  // t"b"
letters(Ordinal.zerary(7))    // Unset
numbers.prim                  // 3
numbers.last                  // 2
```

Indexing a `List` by position is a walk from its head, so it, and the accessors that walk more
than a few elements, are gated behind the *linear access* acknowledgment, `dysasymptotics.linearAccess`,
in the same way that `size` is gated behind `linearSize`. A `Sequence` has neither cost and needs
neither import. The imports are cheap to write and say something true, which is the point: a
program that indexes into a linked list in a loop has a quadratic algorithm, and should know it.

```scala
import dysasymptotics.{linearSize, linearAccess}

numbers.size                  // 3
numbers(Ter)                  // 2
```

### Traversing and transforming

`each` visits every element, with the ordinal of each available as a contextual value;
`map` transforms elements; `filter` keeps those matching a predicate; `bind` — the same operation
the standard library calls `flatMap`, which is also available under that name so that
comprehensions work — flattens the results; and `flat` flattens one level of nesting:

```scala
numbers.map(_*2)                     // List(6, 2, 4)
numbers.filter(_ > 1)                // List(3, 2)
numbers.bind(n => List(n, n))        // List(3, 3, 1, 1, 2, 2)
List(List(1, 2), List(3)).flat       // List(1, 2, 3)

for
  n <- numbers
  m <- List(10, 20)
yield n + m
```

Every operation returns the shape it was given — mapping a `Set` gives a `Set`, filtering a
`Chain` gives a lazy `Chain` — except where the result cannot be that shape: mapping a `Map`
transforms its values and keeps its keys, while `remap` transforms its entries as pairs, and
gives a `Map` where the results are pairs and a `List` where they are not.

```scala
ages.map(_ + 1)                          // Map(t"Ada" -> 37, t"Bob" -> 42)
ages.remap { (name, age) => name }       // List(t"Ada", t"Bob")
```

### Searching and measuring

`has` tests membership; `exists` tests a predicate; `count` counts matches; `fold` accumulates
from an initial state; and `trace` is a `fold` that keeps every intermediate state, initial
first:

```scala
numbers.has(2)                // true
numbers.exists(_ > 2)         // true
numbers.count(_ > 1)          // 2
numbers.fold(0)(_ + _)        // 6
numbers.trace(0)(_ + _)       // List(0, 3, 4, 6)
```

The least and greatest elements are `minimum` and `maximum`, each an `Optional` since the
collection may be empty, and both defined for any element type with a comparison:

```scala
numbers.minimum   // 1
numbers.maximum   // 3
```

### Reshaping

`excerpt` takes a run by position, end-exclusive, and is total: bounds beyond the end clamp
rather than fail. `zip` pairs elements positionally, stopping at the shorter side; `group` keys
elements by a function into a `Map` of the source's own shape; `partition` splits by a
predicate, keeping order on both sides; `span` splits at the first element failing a predicate;
and `batched` cuts a collection into runs of at most a given size:

```scala
numbers.excerpt(1, 3)                   // List(1, 2)
numbers.excerpt(2, 99)                  // List(2)
numbers.zip(letters)                    // List((3, t"a"), (1, t"b"), (2, t"c"))
List(1, 2, 3, 4).group(_%2)             // Map(1 -> List(1, 3), 0 -> List(2, 4))
List(1, 2, 3, 4).partition(_%2 == 0)    // (List(2, 4), List(1, 3))
List(1, 2, 3, 4).span(_ < 3)            // (List(1, 2), List(3, 4))
List(1, 2, 3, 4, 5).batched(2)          // List(List(1, 2), List(3, 4), List(5))
```

`sweep` filters and maps in one pass, through a partial function:

```scala
numbers.sweep { case n if n > 1 => n*10 }   // List(30, 20)
```

### Duplicates and order

`distinct` drops repeated elements, keeping the first of each; `deduplicate` does the same by a
key; and `reverse` reverses any reversible shape, text included:

```scala
List(1, 2, 1, 3, 2).distinct              // List(1, 2, 3)
List(10, 43, 22, 71, 52).deduplicate(_%10)   // List(10, 43, 22, 71)
numbers.reverse                           // List(2, 1, 3)
t"stressed".reverse                       // t"desserts"
```

A `Set` has no order, so it cannot be sorted, excerpted or deduplicated: those operations are
simply absent from it, rather than defined to do something arbitrary.

### Sorting

Sorting has two ingredients, and both are explicit. The *comparison* comes from a `Comparable`
instance for the element type — the numbers have one; [text](text.md) needs a collation to be
chosen, since no order of text is natural — and the *algorithm* is a given selected by import
from `sortingAlgorithms`: Timsort or Powersort for stability and speed on partly-ordered data,
quicksort or heapsort where memory matters, and the simpler algorithms where the input is tiny.
`sort` orders by the elements' own comparison and `order` by a key:

```scala
numbers.sort               // List(1, 2, 3)
numbers.order(-_)          // List(3, 2, 1)
letters.order(_.length)    // Sequence(t"a", t"b", t"c"): stable, so ties keep their order
```

With no algorithm in scope the sort does not compile, and with two in scope it is ambiguous —
which is the compiler insisting that a choice has been made, since the algorithm decides
whether equal elements keep their order and how the sort behaves on data that is nearly sorted
already.

### Sets and maps

A `Set` supports the operations of set theory — `insert`, `except`, `intersect` and `concat` for
union — and membership with `has`. A `Map` reads a value by key with `at`, as an `Optional`
since the key may be absent, tests a key with `defines`, updates or adds with `define`, removes
with `omit`, and exposes its `keys` and `values`:

```scala
primes.insert(11)             // Set(2, 3, 5, 7, 11)
primes.intersect(Set(2, 4))   // Set(2)
primes.except(Set(2, 3))      // Set(5, 7)

ages.at(t"Ada")               // 36
ages.at(t"Cy")                // Unset
ages.defines(t"Ada")          // true
ages.define(t"Cy", 29)        // a new map with three entries
ages.omit(t"Bob")             // Map(t"Ada" -> 36)
ages.keys                     // Set(t"Ada", t"Bob")
```

Every value is immutable: `define` and `omit` return new maps, and the original is unchanged,
as [immutability](../philosophy/immutability.md) asks everywhere else.

### Lazy chains

A `Chain` computes its elements on demand and remembers them, so it can describe an infinite
sequence — every natural number, every line a process will print — as long as only a finite
prefix is ever forced. `#::` prepends an element without forcing what follows, `Chain.iterate`
and `Chain.unfold` generate from a seed, `Chain.continually` repeats a value, and `keep` and
`skip` take and drop a prefix without forcing the rest:

```scala
naturals.keep(5)                   // Chain(0, 1, 2, 3, 4)
naturals.skip(10).keep(2)          // Chain(10, 11)
Chain.continually(7).keep(3)       // Chain(7, 7, 7)

def evens: Chain[Int] = naturals.filter(_%2 == 0)
evens.prim                         // 0
```

Because a chain may be unbounded, `size` is not defined on it unless the program acknowledges
that counting may not terminate, with `dysasymptotics.unboundedSize`. A lazily-parsed
[stream](streams.md) or a recurrence of [dates](time.md) is a `Chain` for exactly this reason:
the consumer decides how much is computed.

### Text as a collection

`Text` traverses as its characters, so the vocabulary above applies to it directly — `has`,
`count`, `filter`, `reverse`, `subsumes` — and reshaping keeps it text where the result is
characters:

```scala
t"hello".has('e')             // true
t"hello".count(_ == 'l')      // 2
t"hello".filter(_ != 'l')     // t"heo"
t"hello world".subsumes(t"lo wo")   // true
```

### Crossing to the standard library

Code that must hand a collection to a library written against the standard library crosses at
the boundary with `stdlib`, which exposes the underlying structure without copying, and comes
back with the companion's `from`:

```scala
numbers.stdlib                              // scala.collection.immutable.List(3, 1, 2)
List.from(scala.collection.immutable.List(1, 2))   // List(1, 2)
```

Keeping the crossing explicit, and greppable, is what lets the rest of a program be written in
the total vocabulary: the partial, larger API is reachable, but only where the code says so.

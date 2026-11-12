## Collections

### About

Soundness has its own collection types: `List`, `Sequence`, `Set`, `Map` and the lazy `Chain`.
One vocabulary of operations works over all of them, and over [text](text.md), which is
traversed as its characters.

Each type is an [opaque type] over the standard library's structure, so it costs nothing extra.
What changes is the API, which is smaller and makes three promises:

- An operation which could fail gives an [optional](optional-values.md) value. It does not
  throw.
- An operation which is slower than its name suggests must be acknowledged with an import.
- Sorting names the algorithm it uses.

The operations are provided through [typeclasses][typeclass]. A new shape, such as a byte
buffer, a parsed document or a stream of records, gains the whole vocabulary by supplying one
instance, and the vocabulary reads the same whatever it is applied to.

### On collections

The standard library's collections are large, partial and quietly expensive:

```tabular
column  In the standard library
column  The pitfall
row  `head`  throws on an empty list
row  `apply` on a `List`  walks the list, but looks like reading an array
row  `sorted`  picks an algorithm unasked, and orders a `String` without asking in which language
row  every collection type  carries several hundred methods, many of them duplicates under other names
```

Soundness keeps the structures and replaces the surface. Nothing is copied, and nothing costs
more than before: a `List` is still a linked list. But the methods are the ones a program needs,
and each has a type which says what it can and cannot do.

Where a method's cost depends on the structure, the program must say that it knows. Counting a
linked list means walking all of it, so a `List`'s `size` is available only where the
*linear size* acknowledgment has been imported.

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### The types

The five types differ in their structure, and so in what is cheap:

```tabular
column  Type
column  Holds
column  Cheap
column  `size` needs
row  `List`  a linked list  prepending, and traversing  `linearSize`
row  `Sequence`  an indexed sequence  reading at any position  nothing
row  `Set`  each element once, unordered  testing membership  nothing
row  `Map`  values paired with keys  reading a value by key  nothing
row  `Chain`  a lazy sequence, remembered once computed, and possibly infinite  forcing only the prefix demanded  `unboundedSize`
```

Each is constructed by applying its companion, and `Nil` is the empty list:

```scala
val numbers: List[Int] = List(3, 1, 2)
val letters = Sequence(t"a", t"b", t"c")
val primes = Set(2, 3, 5, 7)
val ages = Map(t"Lear" -> 80, t"Kent" -> 48)
val naturals: Chain[Int] = Chain.iterate(0)(_ + 1)
```

A list built from a known number of elements is typed as *populated*, and on that type `head`,
`last` and `tail` cannot fail. They are not available on a `List` of unknown length, where the
accessors which give an `Optional` are used instead.

### Positions and access

A position is an [ordinal](numbers.md), not an integer. `Prim`, `Sec` and `Ter` name the first
three positions, and `Ordinal.zerary` converts a zero-based index. Applying a collection to an
ordinal reads the element there, as an `Optional`, since the position may lie beyond the end.
`inspect` displays a present optional value between corner brackets, and an absent one as `○`:

```scala fuel
run
##
Out.println(letters(Sec).inspect)
Out.println(letters(Ordinal.zerary(7)).inspect)
Out.println(numbers.head.inspect)
```

`prim`, `sec` and `ter` read the first few elements directly.

Reading a `List` at a position means walking from its head. Counting one means walking all of
it. Neither is available until the cost has been acknowledged:

```scala fuel
compile
errors show
##
numbers.size
```

Each acknowledgment is a given in the package `dysasymptotics`:

```tabular
column  Acknowledgment
column  Gates
column  Because
row  `linearSize`  `size` and `last` of a `List`, and appending to one with `:+`  each walks the whole list
row  `linearAccess`  reading a `List` at an ordinal beyond the first few  it walks from the head to the position
row  `unboundedSize`  `size` and `last` of a `Chain`  each forces the whole chain, which may be infinite
```

```scala
import dysasymptotics.{linearSize, linearAccess}
```

```scala fuel
run
##
Out.println(numbers.size.show)
Out.println(numbers(Ter).inspect)
Out.println(numbers.last.inspect)
```

A `Sequence` has neither cost, and needs no import. The imports are cheap to write, and they
say something true. A program which reads a linked list by position inside a loop has a
quadratic algorithm, and should know it.

### Traversing and transforming

Five operations cover most traversals:

```tabular
column  Operation
column  Does
row  `each`  visits every element, with the element's ordinal available as a contextual value
row  `map`  transforms each element
row  `filter`  keeps the elements which match a predicate
row  `bind`  transforms each element into a collection, and flattens the results
row  `flat`  flattens one level of nesting
```

```scala fuel
run
##
Out.println(numbers.map(_*2).inspect)
Out.println(numbers.filter(_ > 1).inspect)
Out.println(numbers.bind(n => List(n, n)).inspect)
Out.println(List(List(1, 2), List(3)).flat.inspect)
```

`bind` is the operation the standard library calls `flatMap`. It is available under that name
too, so that `for` comprehensions work:

```scala
val sums =
  for
    n <- numbers
    m <- List(10, 20)
  yield n + m
```

```scala fuel
run
##
Out.println(sums.inspect)
```

Every operation gives back the shape it was given. Mapping a `Set` gives a `Set`, and filtering
a `Chain` gives a lazy `Chain`. The exception is where the result cannot be that shape. `map` on
a `Map` transforms its values and keeps its keys. `remap` transforms its entries as pairs, and
gives a `Map` where the results are pairs, and a `List` where they are not:

```scala fuel
run
##
Out.println(ages.map(_ + 1).inspect)
Out.println(ages.remap { (name, age) => name }.inspect)
```

### Searching and measuring

`has` tests membership, `exists` tests a predicate, and `count` counts the matches. `fold`
accumulates from an initial state, and `trace` is a `fold` which keeps every intermediate
state, beginning with the initial one:

```scala fuel
run
##
Out.println(numbers.has(2).inspect)
Out.println(numbers.exists(_ > 2).inspect)
Out.println(numbers.count(_ > 1).show)
Out.println(numbers.fold(0)(_ + _).show)
Out.println(numbers.trace(0)(_ + _).inspect)
```

`minimum` and `maximum` give the least and greatest elements. Each is an `Optional`, since the
collection may be empty, and both are defined for any element type which can be compared:

```scala fuel
run
##
Out.println(numbers.minimum.inspect)
Out.println(numbers.maximum.inspect)
Out.println(List[Int]().maximum.inspect)
```

### Reshaping

These operations rearrange a collection without changing its elements:

```tabular
column  Operation
column  Gives
row  `excerpt(from, to)`  a run by position, excluding the end; bounds beyond the end are clamped, so it never fails
row  `zip(other)`  pairs of elements by position, stopping at the shorter side
row  `group(f)`  a `Map` from each key to the elements with that key
row  `partition(p)`  the elements which match, and those which do not, each in order
row  `span(p)`  the elements before the first which fails a predicate, and the rest
row  `batched(n)`  runs of at most `n` elements
```

```scala fuel
run
##
Out.println(numbers.excerpt(1, 3).inspect)
Out.println(numbers.excerpt(2, 99).inspect)
Out.println(numbers.zip(letters).inspect)
Out.println(List(1, 2, 3, 4).group(_%2).inspect)
Out.println(List(1, 2, 3, 4).partition(_%2 == 0).inspect)
Out.println(List(1, 2, 3, 4).span(_ < 3).inspect)
Out.println(List(1, 2, 3, 4, 5).batched(2).inspect)
```

`sweep` filters and maps in one pass, through a partial function:

```scala fuel
run
##
Out.println(numbers.sweep { case n if n > 1 => n*10 }.inspect)
```

`join` is the inverse of `batched`. Elements which are themselves joinable, such as collections
or text, are concatenated, with an optional separator of the same type between them. Elements
which are not joinable are interleaved with a separator which is itself an element, giving a
collection of the original shape:

```scala fuel
run
##
Out.println(List(List(1, 2), List(3), List(4, 5)).join.inspect)
Out.println(List(List(1, 2), List(3), List(4, 5)).join(List(0)).inspect)
Out.println(List(1, 2, 3).join(0).inspect)
```

### Duplicates and order

`distinct` drops repeated elements, keeping the first of each. `deduplicate` does the same by a
key. `reverse` reverses any shape which has an order, text included:

```scala fuel
run
##
Out.println(List(1, 2, 1, 3, 2).distinct.inspect)
Out.println(List(10, 43, 22, 71, 52).deduplicate(_%10).inspect)
Out.println(numbers.reverse.inspect)
Out.println(t"stressed".reverse)
```

A `Set` has no order, so it cannot be reversed, sorted, excerpted or deduplicated. Those
operations are absent from it, instead of being defined to do something arbitrary:

```scala fuel
compile
errors show
##
primes.reverse
```

### Sorting

Sorting has two ingredients, and both are explicit.

The *comparison* comes from a `Comparable` instance for the element type. The numeric types
have one. [Text](text.md#sorting-and-comparing-text) needs a collation to be chosen, since no
order of text is natural.

The *algorithm* is a given, selected by import. With no algorithm in scope, a sort does not
compile:

```scala fuel
compile
errors show
##
numbers.sort
```

The algorithms are the givens in the package `sortingAlgorithms`:

```tabular
column  Given
column  Stable
column  Suited to
row  `timsort`  yes  partly-ordered data; the JDK's own sort
row  `powersort`  yes  partly-ordered data
row  `quicksort`  no  bounded memory
row  `heapsort`  no  bounded memory
row  `insertionSort`  yes  tiny inputs
row  `bubbleSort`  yes  tiny inputs
```

The compiler insists on the choice because the algorithm decides things a program may depend
on: whether equal elements keep their order, and how the sort behaves on data which is nearly
sorted already. With two algorithms in scope, a sort is ambiguous, and does not compile either.

```scala
import sortingAlgorithms.timsort
```

`sort` orders elements by their own comparison, and `order` orders them by a key:

```scala fuel
run
##
Out.println(numbers.sort.inspect)
Out.println(numbers.order(-_).inspect)
Out.println(List(t"ccc", t"a", t"bb", t"d").order(_.length).inspect)
```

Timsort is stable, so `t"a"` and `t"d"`, which have the same length, keep their original order.

### The operations at a glance

The operations of the sections above, in one place:

```tabular
column  Operation
column  Gives
row  `each(f)`  nothing; visits every element, with its ordinal in context
row  `map(f)`  each element transformed; a `Map`'s values, keeping its keys
row  `remap(f)`  a `Map`'s entries transformed as pairs
row  `filter(p)`  the elements matching a predicate
row  `bind(f)`, `flatMap(f)`  the results of `f`, flattened
row  `flat`  one level of nesting flattened
row  `sweep(pf)`  the elements a partial function is defined at, mapped by it
row  `has(x)`  whether an element is present
row  `exists(p)`  whether any element matches
row  `count(p)`  the number of matching elements
row  `fold(z)(f)`  the final state of an accumulation
row  `trace(z)(f)`  every state of an accumulation, initial first
row  `minimum`, `maximum`  the least or greatest element, as an `Optional`
row  `excerpt(from, to)`  a run by position, end-exclusive and clamped
row  `zip(other)`  pairs by position, to the shorter side
row  `group(f)`  a `Map` from each key to its elements
row  `partition(p)`  the matching and other elements, in order
row  `span(p)`  the prefix matching a predicate, and the rest
row  `batched(n)`  runs of at most `n` elements
row  `join`, `join(separator)`  the elements concatenated, or interleaved with a separator
row  `distinct`  the first of each repeated element
row  `deduplicate(f)`  the first element for each key
row  `reverse`  the elements in reverse order
row  `sort`  the elements in their own order
row  `order(f)`  the elements in the order of a key
```

### Sets and maps

A `Set` and a `Map` add operations of their own:

```tabular
column  Operation
column  On
column  Gives
row  `:+`  `Set`  the set with an element added
row  `concat`  `Set`  the union of two sets
row  `intersect`  `Set`  the elements in both sets
row  `except`  `Set`  the elements not in the other set
row  `has`  `Set`  whether an element is a member
row  `at`  `Map`  the value for a key, as an `Optional`
row  `defines`  `Map`  whether a key is present
row  `define`  `Map`  the map with a key's value added or replaced
row  `omit`  `Map`  the map without a key
row  `keys`, `values`  `Map`  the keys, or the values
```

```scala fuel
run
##
Out.println((primes :+ 11).inspect)
Out.println(primes.intersect(Set(2, 4)).inspect)
Out.println(primes.except(Set(2, 3)).inspect)
```

Looking up a key which is not in a map gives `Unset`. It does not throw:

```scala fuel
run
##
Out.println(ages.at(t"Lear").inspect)
Out.println(ages.at(t"Edgar").inspect)
Out.println(ages.defines(t"Lear").inspect)
Out.println(ages.define(t"Edgar", 29).inspect)
Out.println(ages.omit(t"Kent").inspect)
Out.println(ages.keys.inspect)
```

Every collection is [immutable]. `define` and `omit` give new maps, and `ages` is unchanged
after both, as [immutability](../philosophy/immutability.md) asks everywhere else.
[Bit flags](flags.md) holds a set of an enumeration's cases in a single word.

### Lazy chains

A `Chain` computes its elements on demand, and remembers them. It can therefore describe an
infinite sequence, such as every natural number, or every line a process will print, provided
only a finite part of it is ever forced. These operations build and cut chains without forcing
what they leave alone:

```tabular
column  Operation
column  Gives
row  `x #:: chain`  the chain with `x` prepended, without forcing what follows
row  `Chain.iterate(seed)(f)`  `seed`, then `f` applied repeatedly
row  `Chain.unfold(seed)(f)`  the elements `f` generates from a seed
row  `Chain.continually(x)`  `x`, repeated
row  `keep(n)`  the first `n` elements, without forcing the rest
row  `skip(n)`  all but the first `n` elements, without forcing the rest
```

```scala
def evens: Chain[Int] = naturals.filter(_%2 == 0)
```

```scala fuel
run
##
Out.println(naturals.keep(5).to[List].inspect)
Out.println(naturals.skip(10).keep(2).to[List].inspect)
Out.println(Chain.continually(7).keep(3).to[List].inspect)
Out.println(evens.keep(3).to[List].inspect)
```

A chain may be unbounded, so `size` is not defined on it unless the program acknowledges, with
`dysasymptotics.unboundedSize`, that counting may never finish. A lazily parsed
[stream](streams.md), or a recurrence of [dates](time.md), is a `Chain` for exactly this reason:
the consumer decides how much is computed.

### Text as a collection

`Text` is traversed as its characters, so the same vocabulary applies to it directly. An
operation whose result is characters gives text back:

```scala fuel
run
##
Out.println(t"hello".has('e').inspect)
Out.println(t"hello".count(_ == 'l').show)
Out.println(t"hello".filter(_ != 'l'))
Out.println(t"hello world".subsumes(t"lo wo").inspect)
```

[Text](text.md) describes the operations which are particular to text.

### Crossing to the standard library

Some code must hand a collection to a library written against the standard library. `stdlib`
exposes the underlying structure without copying, and the companion's `from` comes back:

```scala
val standard: scala.collection.immutable.List[Int] = numbers.stdlib
val returned: List[Int] = List.from(scala.collection.immutable.List(1, 2))
```

Keeping the crossing explicit, and easy to search for, is what lets the rest of a program be
written in the smaller vocabulary. The larger, partial API can still be reached, but only where
the code says so.

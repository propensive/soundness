Metamorphose primarily provides the `Permutation` class, which is in the
`metamorphose` package and exported to the `soundness` package. You can import
it with,
```scala
import metamorphose.*
```
or,
```scala
import soundness.*
```

There are two ways to construct a new `Permutation`. Firstly, from an ordered
sequence of distinct indexes, for example,
```scala
val permutation = Permutation(Vector(1, 4, 2, 3, 0))
```
which interprets the ordering of the indexes to yield the permutation with
factoradic number 45, i.e. `Permutation(Factoradic(45))`.

Or, if we already know the factoradic number of the permutation, we can pass
that in directly, using the `Factoradic` costructor, like so:
```scala
val permutation = Permutation(Factoradic(45))
```

To check that this is the same permutation, we can expand it with,
```scala
val order: List[Int] = permutation.expansion
```
which gives the original sequence back: `List(1, 4, 2, 3, 0)`.

It's also possible to see the Lehmer code derived from this permutation with,
```scala
permutation.lehmer
```
which produces, `List(1, 3, 1, 1, 0)`.

We can apply this permutation to a sufficiently long sequence just by applying
it to the sequence, i.e. `permutation(list)`.

Permutations are always reversible, and `Permutation#inverse` will construct a
new permutation which will permute a sequence permuted by the original
permutation back to the original list.

That is, for all lists and permutations of the right size it is true that,
```scala
permutation.inverse(permutation(list)) == list
```

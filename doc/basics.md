Two values of the same type can be compared with the `contrastWith` method,
provided by Chiaroscuro. This will return an instance of `Semblance`,
describing the similarity of one value with the other, and will be one of three
cases:
- `Identical`, if the two values are the same
- `Different`, if the two values are different, without any further detail
- `Similar`, if the two values are different, with a breakdown of their
  similarities and differences

The last of these three cases is the most interesting, though it is only a
possible result for values of certain types, namely types which can be
destructured into components which can themselves be compared, recursively.
Simple types like `Text` (or `String`) and primitive types like `Int` or
`Double` can only ever be `Identical` or `Different`, but product types, like
case class instances, sum types, like enums or sealed traits, and sequence
types, like `List` or `IArray` can result in semblances which are neither
`Identical` nor `Different` but `Similar`.

The three cases of `Semblance` are defined as follows:
- `Identical(value: Text)`
- `Different(left: Text, right: Text)`
- `Similar(comparison: IArray[(Text, Semblance)], left: Text, right: Text)

`Identical` includes just a textual representation of the two identical values.
`Different` includes textual representations of _both_ values, since they will
not be the same. `Similar` also includes textual representations of the left
and right values, but additionally includes a sequence (an `IArray`) of
labelled `Semblance`s, each of which may be `Identical`, `Different` or another
`Similar`. This sequence represents a breakdown of the different components of
the two objects, comparing like-for-like, however the breakdown depends on the
type of objects being contrasted.

Typically, for product types such as case classes, the comparison sequence will
contain an entry for each parameter of the case class, showing whether that
parameter is the same or differs between the two values. In the case where it
differs, and the parameter is another product type, a nested `Similar` instance
may recursively show 

For sequence types, such as `List`, a diff between the elements of the left
sequence and the elements of the right sequence will yield a comparison
sequence, labeled for the index of the left and/or right sequence, where each
entry represents a one-to-one comparison between two elements, an addition (to
the right side) or a deletion (from the left side). Each one-to-one comparison
may be `Identical`, `Different` or a recursive `Similar` value.

This format provides a convenient and concise way of describing the structural
differences between two values. A contrast between two deep structures with few
differences will yield a tree structure where identical branches are pruned,
and only differing branches are expanded.


All Chiaroscuro terms and types are defined in the `chiaroscuro` package:
```scala
import chiaroscuro.*
```

Two values of the same type can be compared with the `contrastWith` method,
provided by Chiaroscuro. This will return an instance of `Semblance`,
describing the similarity of one value with the other, and will be one of three
cases:
- `Identical`, if the two values are the same
- `Different`, if the two values are different, without any further detail
- `Breakdown`, if the two values are different, with a breakdown of their
  similarities and differences

The last of these three cases is the most interesting, though it is only a
possible result for values of certain types, namely types which can be
destructured into components which can themselves be compared, recursively.
Simple types like `Text` (or `String`) and primitive types like `Int` or
`Double` can only ever be `Identical` or `Different`, but product types, like
case class instances, sum types, like enums or sealed traits, and sequence
types, like `List` or `IArray` can result in semblances which are neither
`Identical` nor `Different` but a `Breakdown`.

The three cases of `Semblance` are defined as follows:
- `Identical(value: Text)`
- `Different(left: Text, right: Text)`
- `Breakdown(comparison: IArray[(Text, Semblance)], left: Text, right: Text)`

`Identical` includes just a textual representation of the two identical values.
`Different` includes textual representations of _both_ values, since they will
not be the same. `Breakdown` also includes textual representations of the left
and right values, but additionally includes a sequence (an `IArray`) of
labelled `Semblance`s, each of which may be `Identical`, `Different` or another
`Breakdown`. This sequence represents a breakdown of the different components of
the two objects, comparing like-for-like, however the breakdown depends on the
type of objects being contrasted.

Typically, for product types such as case classes, the comparison sequence will
contain an entry for each parameter of the case class, showing whether that
parameter is the same or differs between the two values. In the case where it
differs, and the parameter is another product type, a nested `Breakdown` instance
may recursively show 

For sequence types, such as `List`, a diff between the elements of the left
sequence and the elements of the right sequence will yield a comparison
sequence, labeled for the index of the left and/or right sequence, where each
entry represents a one-to-one comparison between two elements, an addition (to
the right side) or a deletion (from the left side). Each one-to-one comparison
may be `Identical`, `Different` or a recursive `Breakdown` value.

This format provides a convenient and concise way of describing the structural
differences between two values. A contrast between two deep structures with few
differences will yield a tree structure where identical branches are pruned,
and only differing branches are expanded.

### _Similar_ elements

When performing a diff between two sequences of elements, whatever their type,
a comparison between any two elements will judge them to be either the same or
different, however small the difference. The non-appearance of an element in
one sequence and its appearance in the other would be considered a deletion or
insertion. In general, the result of a diff could be presented as an
alternating series of blocks of identical elements and blocks of deletions
(from the left side) and insertions (on the right side).

Often this is the best that can be achieved, and Chiaroscuro would present each
deletion and insertion as `Different` nodes: absent on one side, and present on
the other, with no further breakdown possible.

But what if further analysis on the blocks of differences (those between the
blocks of identical elements) were possible, and elements appearing on both
sides deemed _similar_ could be compared to each other? With a definition for
what it means for two elements to be _similar_ (for a given element type)
Chiaroscuro makes this possible, and provides a breakdown of the differences
between similar elements.

For example, consider the board members of an imaginary company,
```scala
import anticipation.Text
import gossamer.t

enum Role:
  case Ceo, Cto, Coo, Cmo, Cfo

case class Member(role: Role, name: Text)

val boardMembers: List[Member] = List(
  Member(Role.Ceo, t"Jane"),
  Member(Role.Cto, t"Leopold"),
  Member(Role.Coo, t"Simon"),
  Member(Role.Cmo, t"Helen")
)
```
being compared to the board members a year later:
```scala
val boardMembers2: List[Member] = List(
  Member(Role.Ceo, t"Leopold"),
  Member(Role.Cto, t"Linda"),
  Member(Role.Coo, t"Simon"),
  Member(Role.Cfo, t"Anna"),
  Member(Role.Cmo, t"Helen")
)
```

A simplistic diff would identify that the COO (Simon) and CMO (Helen) remained
unchanged, and two _sets_ of changes:
 - deletion of Jane (CEO), deletion of Leopold (CTO), insertion of Leopold
   (CEO) and insertion of Linda (CTO), and
 - insertion of Anna (CFO)

However, the first set of changes could be presented more simply if we were to
provide an indication of _similarity_ between two `Member` instances. Two
possibilities suggest themselves:
 1. two `Member`s with the same name are similar (regardless of role)
 2. two `Member`s with the same role are similar (regardless of name)

The presence of a `Similarity` typeclass instance can be used to specify this similarity. Defining,
```scala
given Similarity[Member] = _.role == _.role
```
```
will transform the `Semblance` output  comparing these two sequences to
directly compare `Member(Role.Ceo, t"Jane")` and
`Member(Role.Ceo, t"Leopold")`, resulting in a `Breakdown` indicating
the name having changed, and a similar name change for the corresponding
members with the `Role.Cto` role.

The insertion of a `Member` with the `Role.Cfo` role would remain as an
insertion.

Likewise, providing the alternative `given` definition,
```scala
given Similarity[Member] = _.name == _.name
```
would show Jane as a deletion, Leopold as a role change from CTO to CEO, and
Linda and Anna as insertions.

The implementation of `Similarity` can be any function comparing two elements,
and a good implementation can dramatically improve the readability of the
`Semblance` output. Some ideas for similarity implementations include considering:
- `Text` elements with a minimum edit distance less than 4 to be similar
- `Double` elements which differ by less than 5% to be similar
- `Json` objects with equal `"id"` fields to be similar
- a choice the same case of an enum (regardless of parameters) to indicate similarity




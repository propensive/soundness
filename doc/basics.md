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

The three cases of `Semblance` are defined (recursively) as follows:
- `Identical(value: Text)`
- `Different(left: Text, right: Text)`
- `Similar(comparison: IArray[(Text, Semblance)], left: Text, right: Text)



### Reading CSV Data

CSV data can be read from any value whose type has a `Source` typeclass instance in scope, such as `Text` or a `File` from
[Galilei](https://github.com/propensive/galilei), passing it to the `Csv.parse` method. For example,
```scala
import caesura.*
import galilei.*
val file: File = ...
val csv: Csv = Csv.parse(file)
```

Likewise, TSV data can be read with:
```scala
import caesura.*
import galilei.*
val file: File = ...
val tsv: Tsv = Tsv.parse(file)
```

### `Row`, `Csv` and `Tsv` Types

Both `Csv` and `Tsv` values are nothing more than wrappers around a sequence of `Row` instances, which are themselves `IArray`s
of `Text` values. While `Row` instances are the same regardless of whether their purpose is for CSV or TSV data, when wrapped
in a `Csv` or `Tsv` instance, they become serializable with the appropriate column separator (`','` or `'\t'`) and escaping.

Indeed, the companion objects `Csv` and `Tsv` are just two instances of the `RowFormat` type with different `separator` values
and implementations of the `RowFormat#escape` method, and alternative formats may be created by subclassing `RowFormat` and
overriding parameters.

### Interpreting rows

A `Row` instance may be converted to a case class by calling its `Row#as` method with a target type parameter, for example,
```scala
val row: Row = Csv.parseRow(t"Richard,Smith,38")

case class Person(firstName: Text, lastName, Text, age: Int)
val person: Person = row.as[Person]
```
will instantiate an instance of `Person`, `Person("Richard", "Smith", 38)` by associating the positional fields in the case
class definition with those in the `Row`, and applying the appropriate conversions to construct parameters of the appropriate
types to instantiate the `Person`. In this example, the `age` field is parsed to construct an `Int`.

If a case class definition includes a nested case class, for example,
```scala
case class Person(firstName: Text, lastName: Text, age: Int)
case class Role(title: Text, person: Person, managerial: Boolean)
```
then the structure would first be flattened, so the order of the positional fields for the `Role` case class would be,
`title` (0), `person.firstName` (1), `person.lastName` (2), `person.age` (3), and `managerial` (4).

This means the mapping from rows to case class instances is brittle: an additional field in a nested case class would be
certain to break interpretation of rows. But that is unfortunately the nature of working with CSV.

The `as` method also exists on `Csv` and `Tsv`, and will return a `List` of values of the specified type.

### Serializing to rows

`Csv` and `Tsv` instances may also be serialized to streams of data. Any `Seq[T]` (e.g. `List[T]` or `LazyList[T]`) may be
transformed to CSV or TSV by calling the `csv` or `tsv` extension methods on it. For example,
```scala
val persons: List[Person] =
  List(Person(t"Richard", t"Smith", 38), person2, person3)

val personsTsv: Tsv = persons.tsv
```





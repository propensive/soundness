Creating a table to be displayed in a monospaced font (e.g. for rendering in a console) is easy,
and first requires a `Tabulation` instance to be defined, specifying each column and how it should
be rendered.

For example,
```scala
case class Person(name: Text, age: Int, active: Boolean)

val table = Tabulation[Person](
  Column("Name", _.name),
  Column("Age", _.age),
  Column("Active", p => if p.active then "Yes" else "No")
)
```
describes a table of three columns, `Name`, `Age` and `Active`, defined for rows of type `Person`,
where the content for each column is defined by a lambda, such as `_.name` and `_.age`. The return
types of these lambdas are any types which can be rendered as `AnsiText`s. In other words, any
type for which an `AnsiShow` instance exists.

Given such a definition, any collection of instances of `Person`, `ps`, can be rendered as a table
(a `Seq[Text]` of each output line) of maximum width `width` by calling
`table.tabulate(width, ps)`.

For example,
```scala
val persons = List(Person("Bill", 48, true), Person("Janet", 54, false))
table.tabulate(100, persons)
```
will return a sequence of `Text`s which will print as,
```
┌───────┬─────┬────────┐
│ Name  │ Age │ Active │
├───────┼─────┼────────┤
│ Bill  │ 48  │ Yes    │
│ Janet │ 54  │ No     │
└───────┴─────┴────────┘
```


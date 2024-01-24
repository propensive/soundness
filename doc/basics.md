All Escritoire terms and types are defined in the `escritoire` package:
```scala
import escritoire.*
```

Creating a table to be displayed in a monospaced font (e.g. for rendering in a console) is easy,
and first requires a `Tabulation` instance to be defined, specifying each column and how it should
be rendered.

For example,
```scala
import anticipation.Text
import gossamer.t

case class Person(name: Text, age: Int, active: Boolean)

val table = Table[Person](
  Column(t"Name")(_.name),
  Column(t"Age")(_.age),
  Column(t"Active"): person =>
    if person.active then t"Yes" else t"No"
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
import turbulence.Out
import turbulence.stdioSources.virtualMachine
import escritoire.tableStyles.default
import hieroglyph.textMetrics.uniform

val persons = List(Person(t"Bill", 48, true), Person(t"Janet", 54, false))

def renderTable(): Unit =
  table.tabulate(persons, 100).foreach(Out.println(_))
```
will return and print a sequence of `Text`s as,
```
┌───────┬─────┬────────┐
│ Name  │ Age │ Active │
├───────┼─────┼────────┤
│ Bill  │ 48  │ Yes    │
│ Janet │ 54  │ No     │
└───────┴─────┴────────┘
```




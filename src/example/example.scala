package escritoire

import gossamer.*
import rudiments.*
import spectacular.*
import anticipation.*
import hieroglyph.*, textMetrics.uniform

case class Person(name: Text, age: Int)

@main
def run(): Unit =
  println(t"Hello world")
  
  val table = Table[Person](
    Column(t"Name", sizing = columnSizing.Fixed(20))(_.name),
    Column(t"Age", sizing = columnSizing.Fixed(10))(_.age.show)
  )

  /* This is a comment. */
  val tabulation = table.tabulate(List(
    Person(t"Jon Pretty", 41),
    Person(t"Kyle Murray", 28),
    Person(t"Jimmy O'Dougherty", 59)
  ))

  import tableStyles.default

  tabulation.layout(100).rows.each(println(_))

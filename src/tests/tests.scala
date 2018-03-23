package escritoire

import probation._

object Tests extends TestApp {

  def tests(): Unit = {
    case class Thing(name: String, age: Int, mass: Double)

    Tabulation[Thing](
      Heading("NAME", _.name, align = CenterAlign),
      Heading("AGE", _.age, align = RightAlign),
      Heading("MASS", _.mass, width = ExactWidth(6))
    ).lines(
        66,
        List(
          Thing("It was the best of times; it was the worst of times", 22, 45.0),
          Thing("Last night I dreamed I went to Manderley again", 10, 8.0),
          Thing("The quick brown fox jumps over the lazy dog", 12, 98.0),
          Thing("Donkey", 2, 3.1415926)
        )
      )
      .foreach(println)
  }

}

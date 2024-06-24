import larceny.*

@main def run(): Unit =
  val errors = demilitarize:
    compiletime.error("it failed")

  println(errors)

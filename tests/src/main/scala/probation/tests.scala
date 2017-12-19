package probation.tests

import probation._
import scala.concurrent.ExecutionContext.Implicits.global

object Tests extends TestApp {

  def tests() = {

    test("string can be parsed as an Int") {
      "1".toInt
    }.assert(_ == 1)

    for(i <- 1 to 1000) test("string of a double can be parsed") {
      "3.14159".toDouble
    }.assert(List(3.14159, 0.0).contains(_))

    val two = test("generate a two") {
      2.0.toInt
    }.assert(_ == 2)

    val equalLists = test("check the lists are equal") {
      List(1, 3, 3)
    }.assert(_ == List(1, 2, 3))

    val doubleIt = test("check that two twos are four") {
      2*two()
    }.assert(_ == 4)

    val tripleIt = test("check that three twos are six") {
      3*two()
    }.watching(three = "4", two = two()).assert(_ == 7)

    val diffStrings = test("strings are different") {
      "foo"
    }.assert(_ == "bar")

    val sum = test("check that the previous two sums are ten") {
      tripleIt() + doubleIt()
    }.assert(_ == 10)
  
  
    test("test tests") { captureStdout {
      implicit val runner: Runner = new CliRunner()
      test("simple test") { 1 + 1 }.assert(_ == 2)
      Test.report(); ()
    } }.assert(_ == """foobar""")
  
  }

}

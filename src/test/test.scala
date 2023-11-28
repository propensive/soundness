package perforate

import fulminate.*

//import language.experimental.captureChecking

case class FooError() extends Error(msg"Foo")
case class BarError(num: Int, str: String) extends Error(msg"Bar: $num $str")
case class BazError() extends Error(msg"Baz")
case class QuuxError(barError: BarError) extends Error(msg"Quux")

def foo()(using fooError: Raises[FooError]): String =
  if math.random < 0.3 then abort(FooError()) else "foo"

def bar()(using barError: Raises[BarError]): String =
  if math.random < 0.3 then abort(BarError(12, "two")) else "bar"

def quux()(using quuxError: Raises[QuuxError]): String =
  if math.random < 0.3 then abort(QuuxError(BarError(4, ""))) else "quux"

@main
def run(): Unit =
  println("Hello")
  given Raises[BazError] = ???
  println("Hello 2")
  
  mitigate:
    case _: FooError               => BazError()
    case QuuxError(BarError(n, s)) => BazError()
    case _: BarError               => BazError()
  .within:
    println("Hello 3")
    println(foo())
    println("Hello 4")
  
  println("Hello 5")
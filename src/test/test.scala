package perforate

import fulminate.*

//import language.experimental.captureChecking

def foo()(using fooError: Raises[FooError1]): String =
  if math.random < 0.3 then abort(FooError1()) else "foo"

def bar()(using barError: Raises[BarError1]): String =
  if math.random < 0.3 then abort(BarError1(12, "two")) else "bar"

def quux()(using quuxError: Raises[QuuxError1]): String =
  if math.random < 0.3 then abort(QuuxError1(BarError1(4, ""))) else "quux"

@main
def run(): Unit =
  import unsafeExceptions.canThrowAny
  import errorHandlers.throwUnsafely
  maybe()


def maybe(): Unit raises BazError1 =
  println("Hello")

  mitigate:
    case quux@QuuxError1(BarError1(n, s)) => BazError1(s"$n / $s")
    case bar@BarError1(n, s)            => BazError1(s"$n / $s")
  .within:
    println("Hello 3")
    println(foo()+bar()+quux())
    println("Hello 4")
  
  println("Hello 5")

case class FooError1() extends Error(msg"Foo")
case class BarError1(num: Int, str: String) extends Error(msg"Bar: $num $str")
case class BazError1(m: String) extends Error(msg"Baz: $m")
case class QuuxError1(barError: BarError1) extends Error(msg"Quux")

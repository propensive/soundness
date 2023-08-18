package perforate

import kaleidoscope.*
import rudiments.*
import anticipation.*
import fulminate.*

import errorHandlers.throwUnsafely

def unsafe(): Unit raises UnsetValueError = ()

case class FooError() extends Error(msg"foo")
case class BarError() extends Error(msg"bar")

@main def run(): Unit =
  val result = handle:
    unsafe()
    Regex.parse(List(")".tt))
  
  .mitigate:
    case _: UnsetValueError                          => BarError()
    case RegexError(RegexError.Reason.UnclosedGroup) => FooError()
    case RegexError(x)                               => FooError()
  
  println(result)
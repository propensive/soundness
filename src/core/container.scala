package superlunary

import jacinta.*
import gossamer.*
import inimitable.*
import anticipation.*
import perforate.*, errorHandlers.throwUnsafely

case class Foo(x: Int, y: String)

@main
def run(): Unit =

  given Baz = new Baz {}
  val fn = container[Foo, String]('{ foo =>
    s"This is running on $jvmInstanceId.")
  })
  
  println(s"This is running on $jvmInstanceId")

  println(fn(Foo(11, "hello")))
  println(fn(Foo(13, "hello world!")))

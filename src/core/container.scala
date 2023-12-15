package superlunary

import jacinta.*
import gossamer.*
import anticipation.*
import perforate.*, errorHandlers.throwUnsafely

case class Foo(x: Int, y: String)

@main
def run(): Unit = println(container[Foo, String]('{ foo => Thread.sleep(2000); foo.y.length.toString+"!" })(Foo(12, "hello")))

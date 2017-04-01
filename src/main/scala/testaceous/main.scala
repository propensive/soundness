package testaceous

import scala.scalanative.native._

object Main {

  def main(args: Array[String]): Unit = {

    // Parameter definitions
    val Name = Param[String]('n', 'name)
    val Value = Param[Int]('v', 'value)
    val Square = Param[Unit]('s', 'square)

    val params = ParamMap(args: _*)
    
    // This is the one point where an exception can be thrown
    val parsed = (Name | (Value & Square)).parse(params)

    // This is guaranteed to be a total function by the typesystem
    val result = parsed.handle(
      Name by { name => s"Hello, $name" },
      (Value & Square) by { product =>
        val square = product(Value)*product(Value)
        s"Value squared is $square"
      }
    )

    println(result)
  }
}


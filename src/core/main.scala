/*
  
  Exoskeleton, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

*/
package exoskeleton

//import scala.scalanative.native._

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

  }
}


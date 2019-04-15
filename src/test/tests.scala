/*
  
  Caesura, version 0.1.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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
package caesura

import probably._

object Test extends TestApp {
  def tests(): Unit = {
    test("simple parse") {
      Csv.parse("""hello,world""")
    }.assert(_ == Row("hello", "world"))

    test("simple parse with quotes") {
      Csv.parse(""""hello","world"""") // "
    }.assert(_ == Row("hello", "world"))

    test("empty unquoted field at start") {
      Csv.parse(",hello,world")
    }.assert(_ == Row("", "hello", "world"))

    test("empty unquoted field at end") {
      Csv.parse("hello,world,")
    }.assert(_ == Row("hello", "world", ""))

    test("empty unquoted field in middle") {
      Csv.parse("hello,,world")
    }.assert(_ == Row("hello", "", "world"))

    test("empty quoted field at start") {
      Csv.parse(""""","hello","world"""") // "
    }.assert(_ == Row("", "hello", "world"))

    test("empty quoted field at end") {
      Csv.parse(""""hello","world",""""")
    }.assert(_ == Row("hello", "world", ""))

    test("empty quoted field in middle") {
      Csv.parse(""""hello","","world"""") // "
    }.assert(_ == Row("hello", "", "world"))

    test("quoted comma") {
      Csv.parse(""""hello,world"""") // "
    }.assert(_ == Row("hello,world"))

    test("escaped quotes") {
      Csv.parse(""""hello""world"""") // "
    }.assert(_ == Row("""hello"world"""))

    test("decode case class") {
      Csv.parse("""hello,world""").as[Foo]
    }.assert(_ == Foo("hello", "world"))

    test("decode complex case class") {
      Csv.parse("""0.1,two,three,4,five,six""").as[Bar]
    }.assert(_ == Bar(0.1, Foo("two", "three"), 4, Foo("five", "six")))

    test("encode case class") {
      Row.from(Foo("hello", "world"))
    }.assert(_ == Row("hello", "world"))

    test("encode complex case class") {
      Row.from(Bar(0.1, Foo("two", "three"), 4, Foo("five", "six")))
    }.assert(_ == Row("0.1", "two", "three", "4", "five", "six"))

    test("convert simple row to string") {
      Csv(Row("hello", "world"))
    }.assert(_ == """"hello","world"""") // "

    test("convert complex row to string") {
      Csv(Row("0.1", "two", "three", "4", "five", "six"))
    }.assert(_ == """"0.1","two","three","4","five","six"""") // "

    test("convert row with escaped quote") {
      Csv(Row("hello\"world"))
    }.assert(_ == """"hello""world"""")

    test("simple parse tsv") {
      Tsv.parse("hello\tworld")
    }.assert(_ == Row("hello", "world"))

    test("decode case class from tsv") {
      Tsv.parse("hello\tworld").as[Foo]
    }.assert(_ == Foo("hello", "world"))

    test("convert case class to tsv") {
      Tsv(Row.from(Foo("hello", "world")))
    }.assert(_ == "\"hello\"\t\"world\"") // "
  }
}

case class Foo(one: String, two: String)

case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)

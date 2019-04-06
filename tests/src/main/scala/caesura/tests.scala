package caesura

import probation._

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
      Csv.line(Row("hello", "world"))
    }.assert(_ == """"hello","world"""") // "

    test("convert complex row to string") {
      Csv.line(Row("0.1", "two", "three", "4", "five", "six"))
    }.assert(_ == """"0.1","two","three","4","five","six"""") // "

    test("convert row with escaped quote") {
      Csv.line(Row("hello\"world"))
    }.assert(_ == """"hello""world"""")

    test("simple parse tsv") {
      Tsv.parse("hello\tworld")
    }.assert(_ == Row("hello", "world"))

    test("decode case class from tsv") {
      Tsv.parse("hello\tworld").as[Foo]
    }.assert(_ == Foo("hello", "world"))

    test("convert case class to tsv") {
      Tsv.line(Row.from(Foo("hello", "world")))
    }.assert(_ == "\"hello\"\t\"world\"") // "
  }
}

case class Foo(one: String, two: String)

case class Bar(one: Double, foo1: Foo, four: Int, foo2: Foo)

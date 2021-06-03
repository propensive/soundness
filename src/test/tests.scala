package euphemism

import probably.*
import scala.util.*

case class Foo(x: Int, y: String) derives CanEqual, Json.Serializer, Json.Deserializer

object Tests extends Suite("Euphemism Tests"):
  def run(using Runner): Unit =
    suite("Parsing tests") {
      test("Parse a number") {
        Json.parse("42").as[Int]
      }.assert(_ == 42)

      test("Parse a string") {
        Json.parse("\"string\"").as[String]
      }.assert(_ == "string")
    
      test("Parse true") {
        Json.parse("true").as[Boolean]
      }.assert(identity)

      test("Parse false") {
        Json.parse("false").as[Boolean]
      }.assert(!_)

      test("Parse float") {
        Json.parse("3.1415").as[Float]
      }.assert(_ == 3.1415f)
      
      test("Parse double") {
        Json.parse("3.1415926").as[Double]
      }.assert(_ == 3.1415926)
    }

    suite("Serialization") {
      test("Serialize string") {
        "foo".json.toString
      }.assert(_ == """"foo"""")

      test("Serialize double") {
        3.14159.json.toString
      }.assert(_ == "3.14159")
      
      test("Serialize true") {
        true.json.toString
      }.assert(_ == "true")
    
      test("Serialize false") {
        false.json.toString
      }.assert(_ == "false")
    }
    
    suite("Basic tests") {
      test("Serialize to Json") {
        Foo(1, "two").json
      }.assert(_ == Json.of(x = 1.json, y = "two".json))

      test("Parse from JSON") {
        Json.parse("""{"x": 1}""").debug
      }.assert(_ == Json.of(x = 1.json))

      test("Read case class") {
        Json.parse("""{"x": 1, "y": "two"}""").as[Foo].debug
      }.assert(_ == Foo(1, "two"))

      test("Extract an option") {
        case class OptFoo(x: Option[Int])
        Json.parse("""{"x": 1}""").as[OptFoo].x
      }.assert(_ == Some(1))
      
      test("Extract a None") {
        case class OptFoo(x: Option[Int])
        Json.parse("""{"y": 1}""").as[OptFoo].x
      }.assert(_ == None)
    }
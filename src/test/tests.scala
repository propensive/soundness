package euphemism

import probably.*
import scala.util.*

case class Foo(x: Int, y: String) derives CanEqual, Json.Serializer, Json.Deserializer

object Tests extends Suite("Euphemism Tests"):
  def run(using Runner): Unit =
    suite("Parsing tests") {
      test("Parse a number") {
        Json.parse("42").get.as[Int].get
      }.assert(_ == 42)

      test("Parse a string") {
        Json.parse("\"string\"").get.as[String].get
      }.assert(_ == "string")
    
      test("Parse true") {
        Json.parse("true").get.as[Boolean].get
      }.assert(identity)

      test("Parse false") {
        Json.parse("false").get.as[Boolean].get
      }.assert(!_)

      test("Parse float") {
        Json.parse("3.1415").get.as[Float].get
      }.assert(_ == 3.1415f)
      
      test("Parse double") {
        Json.parse("3.1415926").get.as[Double]
      }.assert(_ == Try(3.1415926))
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
      }.assert(_ == Success(Json.of(x = 1.json)))

      test("Read case class") {
        Json.parse("""{"x": 1, "y": "two"}""").get.as[Foo].debug
      }.assert(_ == Try(Foo(1, "two")))
    }
    
    suite("Roundtrip tests") {
      Generate.stream[Foo].take(1000).foreach { foo =>
        test("Simple case class") {
        foo.debug -> Json.parse(foo.json.toString).get.as[Foo].get
        }.assert { case (a, b) => a == b }
      }
    }
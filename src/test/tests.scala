package gastronomy.tests

import gastronomy._
import probably._

import scala.util.{ Failure, Success, Try }

object Tests extends Suite("Gastronomy tests") {
 
  def run(test: Runner): Unit = {
    test("Sha256, Hex"){
      "Hello world".digest[Sha256].encoded[Hex]
    }.assert(_ == "64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C")

    test("Md5, Base64"){
      "Hello world".digest[Md5].encoded[Base64]
    }.assert(_ == "PiWWCnnbxptnTNTsZ6csYg==")

    test("Sha1, Base64Url"){
      "Hello world".digest[Sha1].encoded[Base64Url]
    }.assert(_ == "e1AsOh9IyGCa4hLN-2Od7jlnP14")
  }
}

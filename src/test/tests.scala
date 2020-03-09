package gastronomy.tests

import gastronomy._
import scala.util.{ Failure, Success, Try }

object Tests {
  
  private[this] def test[T](name: String)(f: => T)(expected: T): Boolean = {
    println(name)
    val result = Try(f)
    result match {
      case Success(`expected`) =>
        println("  passed")
        true
      case Success(unexpected) =>
        println(s"! failed: expected $expected, but was $unexpected")
        false
      case Failure(e) =>
        println(s"!!ERROR: $e")
        e.printStackTrace
        false
    }
  }
  
  def main(args: Array[String]): Unit = {
    val allResults: List[Boolean] =
      test("Sha256, Hex"){
        "Hello world".digest[Sha256].encoded[Hex]
      }("64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C") ::
      test("Md5, Base64"){
        "Hello world".digest[Md5].encoded[Base64]
      }("PiWWCnnbxptnTNTsZ6csYg==") ::
      test("Sha1, Base64Url"){
        "Hello world".digest[Sha1].encoded[Base64Url]
      }("e1AsOh9IyGCa4hLN-2Od7jlnP14") ::
      Nil
    
    val passed = allResults.filter(_ == true).size
    val failed = allResults.filter(_ == false).size
    println(s"${allResults.size} tests: $passed passed, $failed failed")
    System.exit(if(allResults.contains(false)) 1 else 0)
  }
}

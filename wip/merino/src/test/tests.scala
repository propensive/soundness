package merino

import probably.*

object Tests extends Suite("Merino tests"):
  
  def lazyList(str: String): LazyList[IArray[Byte]] =
    LazyList(str.getBytes("UTF-8").asInstanceOf[IArray[Byte]])
  
  def run(using Runner): Unit =
    test("Ten") {
      Parser(lazyList("10"))
    }.assert(_ == 10)
    
    test("big number") {
      Parser(lazyList("65536"))
    }.assert(_ == 65536)
    
    test("negative number") {
      Parser(lazyList("-65536"))
    }.assert(_ == -65536)
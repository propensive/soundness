package rudiments

import probably.*

object Tests extends Suite("Rudiments tests"):
  def run(using Runner): Unit =
    
    val array = (0 until 65536).to(Array).map(_.toByte)
    
    test("read Java `InputStream`, chunked") {
      val in = java.io.ByteArrayInputStream(array)
      Util.read(in, 4096).map(_.to(Vector)).reduce(_ ++ _)
    }.assert(_ == array.to(Vector))
    
    test("read Java `InputStream`, single chunk") {
      val in = java.io.ByteArrayInputStream(array)
      Util.read(in, 65536).map(_.to(Vector)).head
    }.assert(_ == array.to(Vector))
    
    test("read Java `InputStream`, two chunks") {
      val in = java.io.ByteArrayInputStream(array)
      Util.read(in, 32768).map(_.to(Vector)).length
    }.assert(_ == 2)

    test("initialize array") {
      val iarray: IArray[String] = IArray.init(3) { arr =>
        arr(0) = "zero"
        arr(1) = "one"
        arr(2) = "two"
      }
      iarray.to(Vector)
    }.assert(_ == Vector("zero", "one", "two"))
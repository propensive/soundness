package merino

import probably.*
import gossamer.*
import joviality.*, filesystems.unix
import anticipation.*, integration.jovialityPath
import eucalyptus.*
import serpentine.*
import rudiments.*
import turbulence.*
import parasitism.*, monitors.global, threading.platform

import unsafeExceptions.canThrowAny
import environments.system

import LogFormat.standardAnsi

val StdoutSink = SystemOut.sink
given Log({ case _ => StdoutSink })

object Tests extends Suite(t"Merino tests"):
  def run(using Runner): Unit =
    val tests = (env.pwd / p"tests" / p"test_parsing").directory(Expect)
    val tests2 = (env.pwd / p"tests" / p"test_transform").directory(Expect)
    
    val file: Bytes = test(t"Read file"):
      (env.pwd / p"huge.json").file(Expect).read[DataStream]().slurp()
    .check(_ => true)
    
    val file2: Bytes = test(t"Read file 2"):
      (env.pwd / p"huge2.json").file(Expect).read[DataStream]().slurp()
    .check(_ => true)
    
    // (tests.files ++ tests2.files).foreach: file =>
    //     if file.name.startsWith(t"n_")
    //     then
    //       test(t"Negative test: ${file.name.drop(2).drop(5, Rtl)}"):
    //         try
    //           Json.parse(file.read[DataStream]())
    //           Left(t"success")
    //         catch
    //           case err: JsonParseError => err match
    //             case JsonParseError(_, msg) => Right(msg)
    //           case err: Throwable  =>
    //             err.printStackTrace()
    //             Right(err.toString.show)
    //       .check(_.isRight)
    //     else
    //       test(t"Positive test: ${file.name.drop(5, Rtl)}"):
    //         try
    //           val data = file.read[DataStream]()
    //           val j = Json.parse(file.read[DataStream]())
    //           if data.head.length < 100 then
    //             val a = data.head.uString.s
    //             val b = j.toString
    //             if a.replaceAll(" ", "") != b.replaceAll(" ", "") then
    //               println(t"${a} != ${b}")
              
    //           Right(j.toString)
    //         catch
    //           case err: JsonParseError => err match
    //             case JsonParseError(_, msg) => Left(msg)
    //           case err: Throwable =>
    //             err.printStackTrace()
    //             Left(err.toString.show)
    //       .check(_.isRight)
      
    for i <- 1 to 5 do
      test(t"Parse with Jawn"):
        import org.typelevel.jawn.*, ast.*
        JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file.mutable(using Unsafe)).nn)
      .assert(_ => true)
      
      test(t"Parse with Merino"):
        Json.parse(LazyList(file))
      .assert(_ => true)
      
      test(t"Parse with Jawn 2"):
        import org.typelevel.jawn.*, ast.*
        JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file2.mutable(using Unsafe)).nn)
      .assert(_ => true)
    
      test(t"Parse with Merino 2"):
        Json.parse(LazyList(file2))
      .assert(_ => true)

    suite(t"Number tests"):
      test(t"Parse 0e+1"):
        Json.parse(LazyList(t"0e+1".bytes))
      .assert(_ == Json(0L))
      
      test(t"Parse 0e1"):
        Json.parse(LazyList(t"0e1".bytes))
      .assert(_ == Json(0L))
      
      test(t"Parse ' 4'"):
        Json.parse(LazyList(t" 4".bytes))
      .assert(_ == Json(4L))
      
      test(t"Parse small negative number"):
        Json.parse(LazyList(t"-0.000000000000000000000000000000000000000000000000000000000000000000000000000001".bytes))
      .assert(_ == Json(-1.0e-78))
      
      test(t"Parse 20e1"):
        Json.parse(LazyList(t"20e1".bytes))
      .assert(_ == Json(200L))
      
      test(t"Parse 123e65"):
        Json.parse(LazyList(t"123e65".bytes))
      .assert(_ == Json(1.23e67))
      
      test(t"Parse -0"):
        Json.parse(LazyList(t"-0".bytes))
      .assert(_ == Json(-0.0))
      
      test(t"Parse -123"):
        Json.parse(LazyList(t"-123".bytes))
      .assert(_ == Json(-123L))
      
      test(t"Parse -1"):
        Json.parse(LazyList(t"-1".bytes))
      .assert(_ == Json(-1L))
      
      test(t"Parse 1E22"):
        Json.parse(LazyList(t"1E22".bytes))
      .assert(_ == Json(1.0E22))
      
      test(t"Parse 1E-2"):
        Json.parse(LazyList(t"1E-2".bytes))
      .assert(_ == Json(1.0E-2))
      
      test(t"Parse 1E+2"):
        Json.parse(LazyList(t"1E+2".bytes))
      .assert(_ == Json(1.0E2))
      
      test(t"Parse 123e45"):
        Json.parse(LazyList(t"123e45".bytes))
      .assert(_ == Json(1.23E47))
      
      test(t"Parse 123.456e78"):
        Json.parse(LazyList(t"123.456e78".bytes))
      .assert(_ == Json(1.23456E80))
      
      test(t"Parse 1e-2"):
        Json.parse(LazyList(t"1e-2".bytes))
      .assert(_ == Json(1.0E-2))
      
      test(t"Parse 1e+2"):
        Json.parse(LazyList(t"1e+2".bytes))
      .assert(_ == Json(1.0E2))
      
      test(t"Parse 123"):
        Json.parse(LazyList(t"123".bytes))
      .assert(_ == Json(123L))
      
      test(t"Parse 123.456789"):
        Json.parse(LazyList(t"123.456789".bytes))
      .assert(_ == Json(123.456789))
      
      test(t"Parse \"Hello World\""):
        Json.parse(LazyList(t"\"Hello World\"".bytes))
      .assert(_ == Json("Hello World"))
      
      test(t"Parse \"\""):
        Json.parse(LazyList(t"\"\"".bytes))
      .assert(_ == Json(""))



given realm: Realm = Realm(t"tests")
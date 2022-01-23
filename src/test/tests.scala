package merino

import probably.*
import gossamer.*
import jovian.*
import eucalyptus.*
import rudiments.*

import unsafeExceptions.canThrowAny
given Log(Everything |-> SystemOut)

object Tests extends Suite(t"Merino tests"):
  def run(using Runner): Unit =
    val tests = (Unix.Pwd / t"tests" / t"test_parsing").directory(Expect)
    tests.files.foreach:
      file =>
        if file.name.startsWith(t"n_")
        then
          test(t"Negative test: ${file.name.drop(2).drop(5, Rtl)}"):
            try
              Json.parse(file.read[DataStream](10.kb))
              Left(t"success")
            catch
              case err: JsonParseError => err match
                case JsonParseError(_, msg) => Right(msg)
              case err: Throwable  =>
                err.printStackTrace()
                Right(err.toString.show)
          .check(_.isRight)
        else if file.name.startsWith(t"y_")
        then
          test(t"Positive test: ${file.name.drop(2).drop(5, Rtl)}"):
            try
              val j = Json.parse(file.read[DataStream](1.mb))
              Right(j.show)
            catch
              case err: JsonParseError => err match
                case JsonParseError(_, msg) => Left(msg)
              case err: Throwable        =>
                err.printStackTrace()
                Left(err.toString.show)
          .check(_.isRight)
        else Log.info(t"Skipping test: ${file.name}")

given realm: Realm = Realm(t"tests")

@main def run(): Unit =
  println("STARTING")
  Tests.main(IArray[Text]())
  println("Finishing")
  Tests.main(IArray[Text]())
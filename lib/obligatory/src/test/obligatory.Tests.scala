package obligatory

import soundness.*

import strategies.throwUnsafely
import autopsies.contrastExpectations

object Tests extends Suite(m"Obligatory Tests"):
  def run(): Unit =
    suite(m"Unframing tests"):
      test(m"Unframe by carriage-return lines"):
        import breakables.crDelimited
        Stream(t"one\rtwo\r", t"three").iterator.break().to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by carriage-return lines, without terminal line"):
        import breakables.crDelimited
        Stream(t"one\rtwo", t"\rthree\r").iterator.break().to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines"):
        import breakables.lfDelimited
        Stream(t"one\ntwo\nth", t"ree").iterator.break().to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines, without terminal line"):
        import breakables.lfDelimited
        Stream(t"one\ntwo\nthree\n").iterator.break().to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines"):
        import breakables.crLfDelimited
        Stream(t"""one\r\ntwo\r\nthree""").iterator.break().to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines, without terminal line"):
        import breakables.crLfDelimited
        Stream(t"""one\r\ntwo\r\nthree\r\n""").iterator.break().to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Length-prefixed chunks"):
        import breakables.lengthPrefixed
        Stream(Data(0, 0, 0, 3, 50, 100, -100, 0, 0, 0, 1, -128, 0, 0, 0, 5, 5, 4, 3, 2, 1))
        . iterator
        . break()
        . to(List)
        . map(_.to(List))
      . assert(_ == List(List(50, 100, -100), List(-128), List(5, 4, 3, 2, 1)))

      test(m"Content-Length-prefixed chunks"):
        import breakables.contentLength
        val input =
          t"Content-Type: x\r\nContent-Length: 5\r\n\r\n12345Content-Length: 3\r\n\r\nabc"

        Iterator(input).break().to(List)
      . assert(_ == List("12345", "abc"))

      test(m"Server-side events"):
        import breakables.serverSideEvents
        val input = t"data: foobar\ndata: baz\n\ndata: hello world\n\n"

        Iterator(input).break().to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Server-side events without terminal newlines"):
        import breakables.serverSideEvents
        val input = t"data: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).break().to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

package obligatory

import soundness.*

import strategies.throwUnsafely
import autopsies.contrastExpectations
import servables.jsonIsServable
import charEncoders.utf8
import charDecoders.utf8
import textSanitizers.skip
import jsonPrinters.minimal

object Tests extends Suite(m"Obligatory Tests"):
  def run(): Unit =
    suite(m"Unframing tests"):
      test(m"Unframe by carriage-return lines"):
        Stream(t"one\rtwo\r", t"three").iterator.break[CarriageReturn].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by carriage-return lines, without terminal line"):
        Stream(t"one\rtwo", t"\rthree\r").iterator.break[CarriageReturn].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines"):
        Stream(t"one\ntwo\nth", t"ree").iterator.break[Linefeed].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines, without terminal line"):
        Stream(t"one\ntwo\nthree\n").iterator.break[Linefeed].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines"):
        Stream(t"""one\r\ntwo\r\nthree""").iterator.break[CrLf].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines, without terminal line"):
        Stream(t"""one\r\ntwo\r\nthree\r\n""").iterator.break[CrLf].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Length-prefixed chunks"):
        Stream(Data(0, 0, 0, 3, 50, 100, -100, 0, 0, 0, 1, -128, 0, 0, 0, 5, 5, 4, 3, 2, 1))
        . iterator
        . break[LengthPrefix]
        . to(List)
        . map(_.to(List))
      . assert(_ == List(List(50, 100, -100), List(-128), List(5, 4, 3, 2, 1)))

      test(m"Content-Length-prefixed chunks"):
        val input =
          t"Content-Type: x\r\nContent-Length: 5\r\n\r\n12345Content-Length: 3\r\n\r\nabc"

        Iterator(input).break[ContentLength].to(List)
      . assert(_ == List("12345", "abc"))

      test(m"Server-side events"):
        val input = t"data: foobar\ndata: baz\n\ndata: hello world\n\n"

        Iterator(input).break[Sse].to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Server-side events without terminal newlines"):
        val input = t"data: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).break[Sse].to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Typed server-side events"):
        val input = t"event: one\ndata: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).break[Sse].map(_.decode[Sse]).to(List)
      . assert(_ == List(Sse("one", List("foobar", "baz")), Sse("message", List("hello world"))))

      test(m"Typed server-side events with more fields"):
        val input = t"event: one\nid: 123\ndata: foobar\ndata: baz\n\ndata: hello world\nretry: 54321"

        Iterator(input).break[Sse].map(_.decode[Sse]).to(List)
      . assert(_ == List(Sse("one", List("foobar", "baz"), "123"), Sse("message", List("hello world"), Unset, 54321L)))

      test(m"Remote server"):
        import supervisors.global
        import codicils.cancel
        import internetAccess.enabled
        import Mcp.*
        val server = remote[McpApi](url"http://localhost:8080/")

        import supervisors.global
        import codicils.cancel
        import httpServers.stdlib
        import logging.silent
        import webserverErrorPages.stackTraces
        import classloaders.threadContext


        tcp"8080".serve:
          request.path match
            case % => t"Nothing here"
            case % /: t"favicon.ico" => t"Nothing here"
            case % /: t"favicon.png" => t"Nothing here"
            case % /: t"favicon.svg" => t"Nothing here"
            case % /: t"mcp" =>
              JsonRpc.server[McpServer]

        Thread.sleep(1000000)

      . assert()

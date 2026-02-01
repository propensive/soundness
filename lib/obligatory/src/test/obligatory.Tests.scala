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
        val server = remote[McpServer](url"http://localhost:8080/")

        object Server extends McpServer:
          import Mcp.*
          @rpc
          def ping(): Unit = ???

          @rpc
          def initialize
            ( protocolVersion: Text,
              capabilities:    ClientCapabilities,
              clientInfo:      Implementation,
              _meta:           Optional[Json] )
          : Mcp.Initialize =

              println(s"protocol version = $protocolVersion")
              println(s"capabilities = $capabilities")
              println(s"client info = $clientInfo")
              println(capabilities.experimental.or(Map()).mapValues(_.show).toMap)

              Mcp.Initialize("2025-11-25", ServerCapabilities(), Implementation("pyrus", version = "1.0.0"), "This is just a test MCP implementation")

          @rpc
          def `completion/complete`
                (ref:      Reference,
                  argument: Argument,
                  context:  Optional[Context],
                  _meta:    Optional[Json] )
          : Complete =

              ???


          @rpc
          def `logging/setLevel`(level: LoggingLevel, _meta: Optional[Json]): Unit = ???

          @rpc
          def `prompts/get`(name: Text, arguments: Optional[Map[Text, Text]], _meta: Optional[Json]): Unit = ???

          @rpc
          def `prompts/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListPrompts = ???

          @rpc
          def `resources/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResources = ListResources(Nil)

          @rpc
          def `resources/templates/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResourceTemplates = ???

          @rpc
          def `resources/read`(uri: Text, _meta: Optional[Json]): ReadResource = ???

          @rpc
          def `resources/subscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

          @rpc
          def `resources/unsubscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

          @rpc
          def `tools/call`(name: Text, arguments: Optional[Map[Text, Json]], _meta: Optional[Json]): CallTool = ???

          @rpc
          def `tools/list`(_meta: Optional[Json]): ListTools = ListTools(Nil)

          @rpc
          def `tasks/get`(taskId: Text, _meta: Optional[Json]): Task = ???

          @rpc
          def `tasks/result`(taskId: Text, _meta: Optional[Json]): Map[Text, Json] = ???

          @rpc
          def `tasks/list`(_meta: Optional[Json]): ListTasks = ???

          @rpc
          def `notifications/cancelled`(request: Optional[TextInt], reason: Optional[Text], _meta: Optional[Json])
          : Unit =

              ???


          @rpc
          def `notifications/progress`
            ( progressToken: TextInt,
              progress:      Double,
              total:         Optional[Double],
              message:       Optional[Text],
              _meta:         Optional[Json] )
          : Unit =

              ???


          @rpc
          def `notifications/initialized`(_meta: Optional[Json]): Unit =
            println("MCP Server Initialized")

          @rpc
          def `notifications/resources/list_changed`(_meta: Optional[Json]): Unit = ???

          @rpc
          def `notifications/resources/updated`(uri: Text, _meta: Optional[Json]): Unit = ???

          @rpc
          def `tasks/cancel`(taskId: Text, _meta: Optional[Json]): Task = ???

          @rpc
          def `notifications/roots/list_changed`(_meta: Optional[Json]): Unit = ???

          @rpc
          def `notifications/tasks/status`
            ( taskId:        Text,
              status:        TaskStatus,
              statusMessage: Optional[Text],
              createdAt:     Text,
              lastUpdatedAt: Text,
              ttl:           Int,
              pollInterval:  Optional[Int],
              _meta:         Optional[Json] )
          : Unit = ???


        import supervisors.global
        import codicils.cancel
        import httpServers.stdlib
        import logging.silent
        import webserverErrorPages.stackTraces
        import classloaders.threadContext


        tcp"8080".serve:
          request.path match
            case % => t"Nothing here"
            case % /: t"mcp" =>
              val input = request.body().read[Json]
              println()
              request.textHeaders.each: header =>
                println(header.key + ": " + header.value)

              given mcpSessionId: ("mcpSessionId" is Directive of Text) = identity(_)
              val session: Text = request.headers.mcpSessionId.prim.or(Uuid().encode)
              println(input.show)
              recover:
                case error: ParseError =>
                  Http.Response(Http.Ok, mcpSessionId = session):
                    JsonRpc.error(-32700, t"Parse error: ${error.message}".show).json
                case error: JsonError =>
                  Http.Response(Http.Ok, mcpSessionId = session):
                    JsonRpc.error(-32600, t"Invalid request: ${error.message}".show).json
              . within:
                  Http.Response(Http.Ok, mcpSessionId = session)(JsonRpc.serve(Server)(input)
                  . or(Json(Map())))

        Thread.sleep(1000000)

      . assert()

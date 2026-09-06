                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package exegesis

import scala.caps
import scala.collection.mutable as scm

import java.io as ji
import java.util.concurrent as juc

import soundness.*

import charEncoders.utf8Encoder
import errorDiagnostics.stackTracesDiagnostics
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import threading.virtualThreading
import workingDirectories.javaBaseWorkingDirectory

// Kept as a top-level object (its own class) rather than nested in `Tests` so the LSP codecs the
// dispatcher inlines do not add to the `Tests` class, which would otherwise exceed the JVM
// per-class size limit. The session and dispatcher are built once and sealed pure: a live
// capability may not inhabit an object field, but this fixture drives dispatch synchronously and
// retains nothing beyond the tests.
object TestServer:
  import Lsp.*

  // Built once and stored behind an erased rim: a live session may not inhabit an object field,
  // and these tests drive the dispatcher synchronously, confining nothing.
  private val fixture: AnyRef =
    val registry: Lsp.Registry^ = Lsp.Registry()
    given registry0: (Lsp.Registry^{registry}) = registry

    opened:
      client.publishDiagnostics
        ( document.uri,
          List
            ( Diagnostic
                ( range    = Range(Position(0, 0), Position(0, 1)),
                  severity = DiagnosticSeverity.Error,
                  message  = t"oops" ) ) )

    hover(Hover(MarkupContent(value = document.text)))
    saved(client.progress(t"token".in[Json], t"begin".in[Json]))

    documentHighlights:
      List(DocumentHighlight(Range(position, position), DocumentHighlightKind.Write))

    foldingRanges(List(FoldingRange(startLine = 0, endLine = 4, kind = t"region")))
    prepareRename(Range(position, position))

    callHierarchy(Nil)
      ( List
          ( CallHierarchyIncomingCall
              (from = item, fromRanges = List(item[CallHierarchyItem].range)) ),
        Nil )

    inlayHints(List(InlayHint(range.start, label = t": Int", kind = InlayHintKind.Type)))
    command(t"do.thing")(t"do.thing".in[Json])

    command(t"test.fail"):
      raise(Lsp.Error(Lsp.Error.Reason.RequestFailed, t"deliberate"))
      Unset

    resolveCompletion(item[CompletionItem].copy(detail = t"resolved"))
    LspSession(registry, t"Test", t"1.0").asInstanceOf[AnyRef]

  private val dispatch0: AnyRef = Lsp.Dispatch(fixture.asInstanceOf[Lsp]).asInstanceOf[AnyRef]

  def session: LspSession^ = fixture.asInstanceOf[LspSession]

  def dispatch(json: Json): Optional[Json] =
    dispatch0.asInstanceOf[Json => Optional[Json]](json)

  // Dispatch plus the session's fault-aware conclusion, as `Lsp.listen` performs it.
  def roundtrip(json: Json): Optional[Json] = session.conclude(json, dispatch(json))

// Drives `Lsp.listen` end-to-end over an in-memory transport, capturing what an `Lsp.Observer`
// sees. Top-level for the same reason as `TestServer`: `listen` inlines the LSP codecs, which
// would otherwise count against the `Tests` class's size.
object TrafficFixture:
  import Lsp.*

  val request: Text =
    t"""{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"capabilities":{}}}"""

  // Every message the observer saw, as (direction, body) pairs in the order they were observed.
  lazy val observed: List[(Text, Text)] =
    val log: scm.ArrayBuffer[(Text, Text)] = scm.ArrayBuffer()

    // Released once the response has been observed. The reader loop cancels the writer when
    // standard input runs dry, so an input that ended with the request could interrupt the
    // writer before it drained the response; instead the stream stays open until `sent` fires.
    val answered: juc.CountDownLatch = juc.CountDownLatch(1)

    val observer = new Observer:
      def received(message: Text): Unit = log.synchronized(log.append((t"recv", message)))

      def sent(message: Text): Unit =
        log.synchronized(log.append((t"send", message)))
        answered.countDown()

    val framed: Text = t"Content-Length: ${request.in[Data].readable.length}\r\n\r\n"+request

    val in: ji.InputStream = new ji.ByteArrayInputStream(framed.s.getBytes("UTF-8").nn):
      override def read(array: scala.Array[Byte] | Null, offset: Int, length: Int): Int =
        val count = super.read(array, offset, length)
        if count == -1 then answered.await(10, juc.TimeUnit.SECONDS)
        count

    val out: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()
    given Stdio = Stdio(ji.PrintStream(out, true), null, in, termcapDefinitions.basicTermcap)

    supervise:
      Lsp.listen(t"Test", t"1.0", observer):
        hover(Hover(MarkupContent(value = document.text)))

    log.synchronized(log.toList).to(List)

// Runs a real `Lsp.listen` server on its own task, joined to a client by a pair of pipes: an
// exchange that crosses the framing and the codecs in both directions, exactly as it would between
// an editor and a language server it launched. Top-level for the same reason as `TestServer`: both
// halves inline the LSP codecs, which would otherwise count against the `Tests` class's size.
object LoopbackFixture:
  import Lsp.*

  // A listener that records what the server said unbidden.
  class Record() extends Lsp.Listener:
    private val log: scm.ArrayBuffer[Text] = scm.ArrayBuffer()
    def notes: List[Text] = log.synchronized(log.toList.to(List))

    override def diagnostics(uri: Text, version: Optional[Int], reports: List[Diagnostic]): Unit =
      val messages = reports.stdlib.map(_.message.s).mkString(",")
      log.synchronized(log.append(t"diagnostics:$uri:$messages"))

    override def message(kind: MessageType, text: Text): Unit =
      log.synchronized(log.append(t"message:$text"))

  def connect[result](using listener: Lsp.Listener^)(lambda: (session: Lsp.Connection^) ?=> result)
  :   result =

    val toServer: ji.PipedOutputStream = ji.PipedOutputStream()
    val serverIn: ji.PipedInputStream = ji.PipedInputStream(toServer, 65536)
    val toClient: ji.PipedOutputStream = ji.PipedOutputStream()
    val clientIn: ji.PipedInputStream = ji.PipedInputStream(toClient, 65536)

    given stdio: Stdio =
      Stdio(ji.PrintStream(toClient, true), null, serverIn, termcapDefinitions.basicTermcap)

    supervise:
      async:
        Lsp.listen(t"Loopback", t"1.0"):
          opened:
            client.publishDiagnostics
              ( document.uri,
                List
                  ( Diagnostic
                      ( range    = document.fullRange,
                        severity = DiagnosticSeverity.Warning,
                        message  = t"opened" ) ) )

          hover(Hover(MarkupContent(value = document.text)))

          complete():
            CompletionList(items = List(CompletionItem(label = t"loopback")))

          command(t"test.fail"):
            raise(Lsp.Error(Lsp.Error.Reason.RequestFailed, t"deliberate"))
            Unset

      Lsp.Server.streams(clientIn, toServer).session: connection ?=>
        lambda

// Editor, proxy and server, all three in this process, joined by two pairs of pipes: the editor
// speaks to the proxy over the proxy's own standard input and output, and the proxy holds a session
// with the server it forwards to. Nothing here knows it is not talking to a real language server
// over a real subprocess pipe.
object ProxyFixture:
  import Lsp.*

  // The command a `workspace/executeCommand` request names, or nothing for any other message.
  // Decoded in a `try` rather than with `safely`, here and below: a decodable cannot thread the
  // boundary tactic under separation checking.
  def commandName(message: Json): Text =
    import dynamicAccess.dynamicJson
    try Lsp.params(message).command.as[Text] catch case _: Exception => t""

  def connect[result](using listener: Lsp.Listener^)
     ( lambda: (session: Lsp.Connection^) ?=> result )
  :   result =

    val toServer: ji.PipedOutputStream = ji.PipedOutputStream()
    val serverIn: ji.PipedInputStream = ji.PipedInputStream(toServer, 65536)
    val fromServer: ji.PipedOutputStream = ji.PipedOutputStream()
    val serverOut: ji.PipedInputStream = ji.PipedInputStream(fromServer, 65536)
    val toProxy: ji.PipedOutputStream = ji.PipedOutputStream()
    val proxyIn: ji.PipedInputStream = ji.PipedInputStream(toProxy, 65536)
    val fromProxy: ji.PipedOutputStream = ji.PipedOutputStream()
    val proxyOut: ji.PipedInputStream = ji.PipedInputStream(fromProxy, 65536)

    // What the proxy learned from the server before the editor asked it anything.
    val startup: Promise[Json] = Promise()

    supervise:
      async:
        given stdio: Stdio =
          Stdio(ji.PrintStream(fromServer, true), null, serverIn, termcapDefinitions.basicTermcap)

        Lsp.listen(t"Upstream", t"1.0"):
          opened:
            client.publishDiagnostics
              ( document.uri,
                List
                  ( Diagnostic
                      ( range    = document.fullRange,
                        severity = DiagnosticSeverity.Warning,
                        message  = t"upstream" ) ) )

          hover(Hover(MarkupContent(value = document.text)))

          complete():
            CompletionList(items = List(CompletionItem(label = t"upstream")))

          command(t"test.classpath")(t"a.jar".in[Json])

      async:
        given stdio: Stdio =
          Stdio(ji.PrintStream(fromProxy, true), null, proxyIn, termcapDefinitions.basicTermcap)

        Lsp.proxy(Lsp.Server.streams(serverOut, toServer)):
          rewrite.capabilities(_.copy(renameProvider = true))
          rewrite.hover: hover =>
            hover.copy(contents = MarkupContent(value = t"[${hover.contents.value}]"))

          rewrite.diagnostics(_.map(_.copy(message = t"proxied")))

          // Asked as soon as the proxy has a server to ask, and kept for whoever wants it.
          rewrite.connected:
            val classpath: Json =
              try upstream.execute(t"test.classpath").or(t"none".in[Json])
              catch case _: Exception => t"none".in[Json]

            startup.offer(classpath)

          // Two commands the server upstream knows nothing about, answered by the proxy out of
          // what it asked the server itself: one asked here and now, one asked at startup.
          rewrite.outbound: (method, message) =>
            val name: Text = ProxyFixture.commandName(message)

            if name == t"proxy.now" then
              val classpath: Json =
                try upstream.execute(t"test.classpath").or(t"none".in[Json])
                catch case _: Exception => t"none".in[Json]

              Transit.Answer(classpath)

            else if name == t"proxy.startup"
            then Transit.Answer(safely(startup.await()).or(t"none".in[Json]))
            else Transit.Forward

      Lsp.Server.streams(proxyOut, toProxy).session: connection ?=>
        lambda

object Tests extends Suite(m"Exegesis Tests"):
  import Lsp.*

  def run(): Unit =
    suite(m"Integer enum codecs"):
      test(m"DiagnosticSeverity encodes to its protocol number"):
        val number: Text = DiagnosticSeverity.Warning.in[Json].encode
        number
      . assert(_ == t"2")

      test(m"DiagnosticSeverity decodes from its protocol number"):
        2.in[Json].as[DiagnosticSeverity]
      . assert(_ == DiagnosticSeverity.Warning)

      test(m"TextDocumentSyncKind is numbered from zero"):
        val number: Text = TextDocumentSyncKind.Full.in[Json].encode
        number
      . assert(_ == t"1")

    suite(m"Content-Length framing"):
      test(m"a framed message is unframed to its body"):
        Iterator(t"Content-Length: 5\r\n\r\n12345".in[Data]).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"12345"))

      test(m"a multi-byte UTF-8 body is framed by byte length, not character count"):
        val body = t"""{"k":"café"}"""
        val message = t"Content-Length: ${body.in[Data].readable.length}\r\n\r\n"+body
        Iterator(message.in[Data]).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"""{"k":"café"}"""))

    suite(m"Dispatch"):
      test(m"initialize answers with the derived capabilities"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"capabilities":{}}}"""
          . as[Json]

        TestServer.dispatch(request).let: response =>
          val capabilities = response.as[JsonRpc.Response].result.as[InitializeResult].capabilities

          capabilities.hoverProvider == true
          && capabilities.textDocumentSync == TextDocumentSyncKind.Incremental
          && capabilities.completionProvider.absent
      . assert(_ == true)

      test(m"opening a document publishes a diagnostic to the client"):
        val request: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///x","languageId":"text","version":1,"text":"hello world"}}}"""
          . as[Json]

        TestServer.dispatch(request)
        TestServer.session.outgoing.stdlib.iterator.next().as[JsonRpc.Request].method
      . assert(_ == t"textDocument/publishDiagnostics")

      test(m"a hover request is answered from the ambient document"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":1,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///x"},"position":{"line":0,"character":0}}}"""
          . as[Json]

        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[Hover].contents.value)
      . assert(_ == t"hello world")

      test(m"an incremental change splices at UTF-16 offsets"):
        val change: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///x","version":2},"contentChanges":[{"range":{"start":{"line":0,"character":6},"end":{"line":0,"character":11}},"text":"scala"}]}}"""
          . as[Json]

        val request: Json =
          t"""{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///x"},"position":{"line":0,"character":0}}}"""
          . as[Json]

        TestServer.dispatch(change)
        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[Hover].contents.value)
      . assert(_ == t"hello scala")

      test(m"a batch of changes applies in order"):
        val change: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///x","version":3},"contentChanges":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":5}},"text":"café"},{"range":{"start":{"line":0,"character":5},"end":{"line":0,"character":10}},"text":"lsp"}]}}"""
          . as[Json]

        val request: Json =
          t"""{"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///x"},"position":{"line":0,"character":0}}}"""
          . as[Json]

        TestServer.dispatch(change)
        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[Hover].contents.value)
      . assert(_ == t"café lsp")

      test(m"a document-highlight request encodes its kind as a protocol number"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":4,"method":"textDocument/documentHighlight","params":{"textDocument":{"uri":"file:///x"},"position":{"line":1,"character":2}}}"""
          . as[Json]

        TestServer.dispatch(request).let: response =>
          response.as[JsonRpc.Response].result.as[List[DocumentHighlight]].stdlib.head.kind
      . assert(_ == DocumentHighlightKind.Write)

      test(m"a folding-range request is answered with the folding ranges"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":5,"method":"textDocument/foldingRange","params":{"textDocument":{"uri":"file:///x"}}}"""
          . as[Json]

        TestServer.dispatch(request).let: response =>
          response.as[JsonRpc.Response].result.as[List[FoldingRange]].stdlib.head.endLine
      . assert(_ == 4)

      test(m"a prepareRename request decodes a position and returns a range"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":6,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///x"},"position":{"line":3,"character":5}}}"""
          . as[Json]

        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[Range].start.line)
      . assert(_ == 3)

      test(m"an incomingCalls request decodes a CallHierarchyItem param and echoes it"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":7,"method":"callHierarchy/incomingCalls","params":{"item":{"name":"foo","kind":12,"uri":"file:///x","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":3}},"selectionRange":{"start":{"line":0,"character":0},"end":{"line":0,"character":3}}}}}"""
          . as[Json]

        TestServer.dispatch(request).let: response =>
          response.as[JsonRpc.Response].result.as[List[CallHierarchyIncomingCall]].stdlib.head.from.name
      . assert(_ == t"foo")

      test(m"an inlayHint request encodes its kind as a protocol number"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":8,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":"file:///x"},"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}}}"""
          . as[Json]

        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[List[InlayHint]].stdlib.head.kind)
      . assert(_ == InlayHintKind.Type)

      test(m"a registered command is dispatched by name"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":9,"method":"workspace/executeCommand","params":{"command":"do.thing","arguments":[]}}"""
          . as[Json]

        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[Text])
      . assert(_ == t"do.thing")

      test(m"a $$/setTrace notification is dispatched without a response"):
        val request: Json =
          t"""{"jsonrpc":"2.0","method":"$$/setTrace","params":{"value":"verbose"}}"""
          . as[Json]

        TestServer.dispatch(request)
      . assert(_ == Unset)

      test(m"saving a document sends a progress notification to the client"):
        val request: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didSave","params":{"textDocument":{"uri":"file:///x"}}}"""
          . as[Json]

        TestServer.dispatch(request)
        TestServer.session.outgoing.stdlib.iterator.next().as[JsonRpc.Request].method
      . assert(_ == t"$$/progress")

      test(m"a completionItem/resolve request decodes a bare item and resolves it"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":10,"method":"completionItem/resolve","params":{"label":"foo"}}"""
          . as[Json]

        TestServer.dispatch(request).let(_.as[JsonRpc.Response].result.as[CompletionItem].detail)
      . assert(_ == t"resolved")

    suite(m"Error responses"):
      import dynamicAccess.dynamicJson

      test(m"a handler fault becomes an error response with its wire code and the request id"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":11,"method":"workspace/executeCommand","params":{"command":"test.fail","arguments":[]}}"""
          . as[Json]

        TestServer.roundtrip(request).let: response =>
          (response.error.code.as[Int], response.id.as[Int])
      . assert(_ == (-32803, 11))

      test(m"an unregistered command yields an InvalidParams error response"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":12,"method":"workspace/executeCommand","params":{"command":"no.such","arguments":[]}}"""
          . as[Json]

        TestServer.roundtrip(request).let(_.error.code.as[Int])
      . assert(_ == -32602)

      test(m"a document-scoped request for an unopened document yields an error response"):
        val request: Json =
          t"""{"jsonrpc":"2.0","id":13,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///nope"},"position":{"line":0,"character":0}}}"""
          . as[Json]

        TestServer.roundtrip(request).let(_.error.code.as[Int])
      . assert(_ == -32602)

      test(m"a notification fault is reported through window/logMessage"):
        val request: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didSave","params":{"textDocument":{"uri":"file:///gone"}}}"""
          . as[Json]

        TestServer.roundtrip(request)
        TestServer.session.outgoing.stdlib.iterator.next().as[JsonRpc.Request].method
      . assert(_ == t"window/logMessage")

    suite(m"Traffic observation"):
      val traffic: List[(Text, Text)] = TrafficFixture.observed

      test(m"the observer sees one message in each direction"):
        traffic.stdlib.map(_._1).mkString(",")
      . assert(_ == "recv,send")

      test(m"an inbound message is observed as its body, without the framing"):
        traffic.stdlib.head._2
      . assert(_ == TrafficFixture.request)

      test(m"an outbound message is observed as the response to the request"):
        traffic.stdlib.last._2.as[Json].as[JsonRpc.Response].id.let(_.as[Int])
      . assert(_ == 0)

    suite(m"Client sessions"):
      test(m"initialize reports the capabilities the server derived"):
        val record = LoopbackFixture.Record()
        given listener: (LoopbackFixture.Record^) = record

        LoopbackFixture.connect: server ?=>
          val result = server.initialize(root = t"file:///project")
          server.initialized()
          (result.serverInfo.let(_.name), result.capabilities.hoverProvider)

      . assert(_ == (t"Loopback", true))

      test(m"a request round-trips through the framing and the codecs"):
        val record = LoopbackFixture.Record()
        given listener: (LoopbackFixture.Record^) = record

        LoopbackFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.open(t"file:///a.scala", t"scala", t"hello")
          server.hover(t"file:///a.scala", Position(0, 0)).let(_.contents.value)

      . assert(_ == t"hello")

      test(m"a completion request decodes the server's list"):
        val record = LoopbackFixture.Record()
        given listener: (LoopbackFixture.Record^) = record

        LoopbackFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.open(t"file:///a.scala", t"scala", t"hello")
          server.complete(t"file:///a.scala", Position(0, 0)).items.stdlib.map(_.label.s)

      . assert(_ == List("loopback"))

      test(m"a server notification reaches the listener"):
        val record = LoopbackFixture.Record()
        given listener: (LoopbackFixture.Record^) = record

        LoopbackFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.open(t"file:///a.scala", t"scala", t"hello")
          // The notification is unsolicited, so a request is used to establish that it has been
          // read: the server publishes before it answers.
          server.hover(t"file:///a.scala", Position(0, 0))

        record.notes.stdlib.map(_.s)

      . assert(_ == List("diagnostics:file:///a.scala:opened"))

      test(m"an error response is raised as an Lsp.Error, not awaited forever"):
        val record = LoopbackFixture.Record()
        given listener: (LoopbackFixture.Record^) = record

        LoopbackFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          capture[Lsp.Error](server.execute(t"test.fail")).reason

      . assert(_ == Lsp.Error.Reason.RequestFailed)

    suite(m"Proxying"):
      test(m"the proxy amends the capabilities the upstream server advertised"):
        ProxyFixture.connect: server ?=>
          val result = server.initialize()
          (result.serverInfo.let(_.name), result.capabilities.renameProvider)

      . assert(_ == (t"Upstream", true))

      test(m"a registered rewriter amends the upstream response"):
        ProxyFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.open(t"file:///a.scala", t"scala", t"hello")
          server.hover(t"file:///a.scala", Position(0, 0)).let(_.contents.value)

      . assert(_ == t"[hello]")

      test(m"a rewriter amends a notification the server sent unbidden"):
        val record = LoopbackFixture.Record()
        given listener: (LoopbackFixture.Record^) = record

        ProxyFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.open(t"file:///a.scala", t"scala", t"hello")
          // A request establishes that the notification preceding it has been read.
          server.hover(t"file:///a.scala", Position(0, 0))

        record.notes.stdlib.map(_.s)

      . assert(_ == List("diagnostics:file:///a.scala:proxied"))

      test(m"a hook asks the server upstream a question of its own"):
        ProxyFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.execute(t"proxy.now").let(_.as[Text])

      . assert(_ == t"a.jar")

      test(m"a proxy asks the server a question as soon as it connects"):
        ProxyFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.execute(t"proxy.startup").let(_.as[Text])

      . assert(_ == t"a.jar")

      test(m"an unregistered method passes through untouched"):
        ProxyFixture.connect: server ?=>
          server.initialize()
          server.initialized()
          server.open(t"file:///a.scala", t"scala", t"hello")
          server.complete(t"file:///a.scala", Position(0, 0)).items.stdlib.map(_.label.s)

      . assert(_ == List("upstream"))


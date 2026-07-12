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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import soundness.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely

// Kept as a top-level object (its own class) rather than nested in `Tests` so the
// LSP codecs it inlines do not add to the `Tests` class, which — now that each
// derived JSON codec also carries its `Shape` — would otherwise exceed the JVM
// per-class size limit.
object TestServer extends LspServer():
  def name: Text = t"Test"
  override def version: Optional[Text] = t"1.0"
  def capabilities: Lsp.ServerCapabilities = Lsp.ServerCapabilities(hoverProvider = true)

  override def hover(uri: Text, position: Lsp.Position): Optional[Lsp.Hover] =
    Lsp.Hover(Lsp.MarkupContent(value = t"hi"))

  override def onOpen(document: Lsp.TextDocumentItem)(using LspClient): Unit =
    summon[LspClient].publishDiagnostics
      ( document.uri,
        List
          ( Lsp.Diagnostic
              ( range    = Lsp.Range(Lsp.Position(0, 0), Lsp.Position(0, 1)),
                severity = Lsp.DiagnosticSeverity.Error,
                message  = t"oops" ) ) )

  override def documentHighlights(uri: Text, position: Lsp.Position): List[Lsp.DocumentHighlight] =
    List(Lsp.DocumentHighlight(Lsp.Range(position, position), Lsp.DocumentHighlightKind.Write))

  override def foldingRanges(uri: Text): List[Lsp.FoldingRange] =
    List(Lsp.FoldingRange(startLine = 0, endLine = 4, kind = t"region"))

  override def prepareRename(uri: Text, position: Lsp.Position): Optional[Lsp.Range] =
    Lsp.Range(position, position)

  override def incomingCalls(item: Lsp.CallHierarchyItem): List[Lsp.CallHierarchyIncomingCall] =
    List(Lsp.CallHierarchyIncomingCall(from = item, fromRanges = List(item.range)))

  override def inlayHints(uri: Text, range: Lsp.Range): List[Lsp.InlayHint] =
    List(Lsp.InlayHint(range.start, label = t": Int", kind = Lsp.InlayHintKind.Type))

  override def executeCommand(command: Text, arguments: Optional[List[Json]]): Optional[Json] =
    command.in[Json]

  override def onSave(document: Lsp.TextDocumentIdentifier, text: Optional[Text])(using LspClient)
  :   Unit =

    summon[LspClient].progress(t"token".in[Json], t"begin".in[Json])

  override def resolveCompletion(item: Lsp.CompletionItem): Lsp.CompletionItem =
    item.copy(detail = t"resolved")

  // Build the dispatcher once, here, rather than at each test call site: it expands a
  // schema-carrying codec for every LSP method, which repeated would exceed the JVM
  // per-class size limit in `Tests`.
  lazy val dispatch: Json => Optional[Json] = LspServer.dispatcher(this)

object Tests extends Suite(m"Exegesis Tests"):
  def run(): Unit =
    suite(m"Integer enum codecs"):
      test(m"DiagnosticSeverity encodes to its protocol number"):
        val number: Text = Lsp.DiagnosticSeverity.Warning.in[Json].encode
        number
      . assert(_ == t"2")

      test(m"DiagnosticSeverity decodes from its protocol number"):
        2.in[Json].as[Lsp.DiagnosticSeverity]
      . assert(_ == Lsp.DiagnosticSeverity.Warning)

      test(m"TextDocumentSyncKind is numbered from zero"):
        val number: Text = Lsp.TextDocumentSyncKind.Full.in[Json].encode
        number
      . assert(_ == t"1")

    suite(m"Content-Length framing"):
      test(m"a framed message is unframed to its body"):
        Iterator(t"Content-Length: 5\r\n\r\n12345".in[Data]).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"12345"))

      test(m"a multi-byte UTF-8 body is framed by byte length, not character count"):
        val body = t"""{"k":"café"}"""
        val message = t"Content-Length: ${body.in[Data].length}\r\n\r\n"+body
        Iterator(message.in[Data]).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"""{"k":"café"}"""))

    suite(m"Dispatch"):
      test(m"a hover request is answered with the hover result"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":1,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///x"},"position":{"line":0,"character":0}}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[Lsp.Hover].contents.value)
      . assert(_ == t"hi")

      test(m"opening a document publishes a diagnostic to the client"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///x","languageId":"text","version":1,"text":"hello"}}}"""
          . as[Json]

        dispatch(request)
        TestServer.outgoing.iterator.next().as[JsonRpc.Request].method
      . assert(_ == t"textDocument/publishDiagnostics")

      test(m"a document-highlight request encodes its kind as a protocol number"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{"textDocument":{"uri":"file:///x"},"position":{"line":1,"character":2}}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[List[Lsp.DocumentHighlight]].head.kind)
      . assert(_ == Lsp.DocumentHighlightKind.Write)

      test(m"a folding-range request is answered with the folding ranges"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":3,"method":"textDocument/foldingRange","params":{"textDocument":{"uri":"file:///x"}}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[List[Lsp.FoldingRange]].head.endLine)
      . assert(_ == 4)

      test(m"a prepareRename request decodes a position and returns a range"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":4,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///x"},"position":{"line":3,"character":5}}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[Lsp.Range].start.line)
      . assert(_ == 3)

      test(m"an incomingCalls request decodes a CallHierarchyItem param and echoes it"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":5,"method":"callHierarchy/incomingCalls","params":{"item":{"name":"foo","kind":12,"uri":"file:///x","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":3}},"selectionRange":{"start":{"line":0,"character":0},"end":{"line":0,"character":3}}}}}"""
          . as[Json]

        dispatch(request).let: response =>
          response.as[JsonRpc.Response].result.as[List[Lsp.CallHierarchyIncomingCall]].head.from.name
      . assert(_ == t"foo")

      test(m"an inlayHint request encodes its kind as a protocol number"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":6,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":"file:///x"},"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[List[Lsp.InlayHint]].head.kind)
      . assert(_ == Lsp.InlayHintKind.Type)

      test(m"a workspace/executeCommand request decodes its command name"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":7,"method":"workspace/executeCommand","params":{"command":"do.thing","arguments":[]}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[Text])
      . assert(_ == t"do.thing")

      test(m"a $$/setTrace notification is dispatched without a response"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","method":"$$/setTrace","params":{"value":"verbose"}}"""
          . as[Json]

        dispatch(request)
      . assert(_ == Unset)

      test(m"saving a document sends a progress notification to the client"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didSave","params":{"textDocument":{"uri":"file:///x"}}}"""
          . as[Json]

        dispatch(request)
        TestServer.outgoing.iterator.next().as[JsonRpc.Request].method
      . assert(_ == t"$$/progress")

      test(m"a completionItem/resolve request decodes a bare item and resolves it"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":8,"method":"completionItem/resolve","params":{"label":"foo"}}"""
          . as[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[Lsp.CompletionItem].detail)
      . assert(_ == t"resolved")

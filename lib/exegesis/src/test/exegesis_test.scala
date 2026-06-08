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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import charEncoders.utf8
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

  // Expand the `serve` dispatch macro once, here, rather than at each test call
  // site: each expansion inlines a codec (now schema-carrying) for every LSP
  // method, which repeated would exceed the JVM per-class size limit in `Tests`.
  lazy val dispatch: Json => Optional[Json] = JsonRpc.serve[Lsp](this)

object Tests extends Suite(m"Exegesis Tests"):
  def run(): Unit =
    suite(m"Integer enum codecs"):
      test(m"DiagnosticSeverity encodes to its protocol number"):
        val number: Text = Lsp.DiagnosticSeverity.Warning.json.encode
        number
      . assert(_ == t"2")

      test(m"DiagnosticSeverity decodes from its protocol number"):
        2.json.as[Lsp.DiagnosticSeverity]
      . assert(_ == Lsp.DiagnosticSeverity.Warning)

      test(m"TextDocumentSyncKind is numbered from zero"):
        val number: Text = Lsp.TextDocumentSyncKind.Full.json.encode
        number
      . assert(_ == t"1")

    suite(m"Content-Length framing"):
      test(m"a framed message is unframed to its body"):
        Iterator(t"Content-Length: 5\r\n\r\n12345".data).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"12345"))

      test(m"a multi-byte UTF-8 body is framed by byte length, not character count"):
        val body = t"""{"k":"café"}"""
        val message = t"Content-Length: ${body.data.length}\r\n\r\n"+body
        Iterator(message.data).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"""{"k":"café"}"""))

    suite(m"Dispatch"):
      test(m"a hover request is answered with the hover result"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","id":1,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///x"},"position":{"line":0,"character":0}}}"""
          . decode[Json]

        dispatch(request).let(_.as[JsonRpc.Response].result.as[Lsp.Hover].contents.value)
      . assert(_ == t"hi")

      test(m"opening a document publishes a diagnostic to the client"):
        val dispatch = TestServer.dispatch

        val request: Json =
          t"""{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///x","languageId":"text","version":1,"text":"hello"}}}"""
          . decode[Json]

        dispatch(request)
        TestServer.outgoing.iterator.next().as[JsonRpc.Request].method
      . assert(_ == t"textDocument/publishDiagnostics")

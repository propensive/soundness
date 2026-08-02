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

import soundness.*

import backstops.stackTraceBackstop
import executives.completions
import interpreters.posixInterpreter
import probates.awaitProbate
import strategies.throwUnsafely
import threading.virtualThreading

// A minimal example Language Server, demonstrating a few features: a hover message showing the
// word under the cursor, a fixed completion list, a command, and a diagnostic published whenever
// a document is opened. The server's capabilities are derived from these registrations. `main` is
// a plain (non-inline) method, so the object's static `main([Ljava/lang/String;)V` forwarder is a
// valid JVM entry point, running as an Ethereal resident daemon over the stdio transport.
object DemoLspServer:
  import Lsp.*

  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.listen(t"Exegesis Demo", t"0.1.0"):
          opened:
            client.publishDiagnostics
              ( document.uri,
                List
                  ( Diagnostic
                      ( range    = document.fullRange,
                        severity = DiagnosticSeverity.Warning,
                        message  = t"This is a demo diagnostic from the Exegesis LSP server." ) ) )

          hover:
            document.word(position).let: word =>
              Hover(MarkupContent(value = t"**$word** — ${document.uri}"))

          complete():
            CompletionList
              ( items = List
                  ( CompletionItem(label = t"exegesis", kind = CompletionItemKind.Keyword),
                    CompletionItem(label = t"soundness", kind = CompletionItemKind.Keyword) ) )

          command(t"demo.reverse"):
            arguments.let(_.prim).let: argument =>
              val text: Text = argument.as[Text]
              Text(StringBuilder(text.s).reverse.toString).in[Json]

      Exit.Ok

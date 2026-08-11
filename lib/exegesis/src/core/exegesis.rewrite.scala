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

import anticipation.*
import contingency.*
import gossamer.*
import jacinta.*

import Lsp.*

// The proxy's registration combinators, in a namespace of their own: `Lsp.hover` registers a
// handler for a server this process implements, while `rewrite.hover` amends one a server upstream
// returned, and a lambda at the call site would not tell the two overloads apart. Each is keyed by
// method, decodes the payload with the same `Lsp` types the server side uses, applies the
// function, and re-encodes; a method with no rewriter registered is forwarded untouched.
object rewrite:
  // The upstream server's advertised capabilities, amended before the editor sees them: a proxy
  // that adds a feature must say so, and one that removes a feature must stop claiming it.
  transparent inline def capabilities
     ( inline adjust: ServerCapabilities => ServerCapabilities )
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"initialize") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      val result = json.as[InitializeResult]

      result.copy(capabilities = adjust(result.capabilities)).in[Json]

  // Applied only when the server had something to say: a result that does not decode as a hover
  // is the `null` a server sends when it has none, and is passed on as it stands, so a rewriter
  // never has to spell out the empty case.
  transparent inline def hover(inline lambda: Hover => Hover)(using proxy: Lsp.Proxy^): Unit =
    proxy.results(t"textDocument/hover") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      try lambda(json.as[Hover]).in[Json] catch case _: Exception => json

  transparent inline def completions(inline lambda: CompletionList => CompletionList)
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/completion") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[CompletionList]).in[Json]

  transparent inline def definitions(inline lambda: List[Location] => List[Location])
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/definition") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[Location]]).in[Json]

  transparent inline def symbols(inline lambda: List[DocumentSymbol] => List[DocumentSymbol])
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/documentSymbol") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[DocumentSymbol]]).in[Json]

  transparent inline def codeActions(inline lambda: List[CodeAction] => List[CodeAction])
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/codeAction") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[CodeAction]]).in[Json]

  transparent inline def codeLenses(inline lambda: List[CodeLens] => List[CodeLens])
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/codeLens") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[CodeLens]]).in[Json]

  transparent inline def inlayHints(inline lambda: List[InlayHint] => List[InlayHint])
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/inlayHint") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[InlayHint]]).in[Json]

  transparent inline def signatureHelp(inline lambda: SignatureHelp => SignatureHelp)
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.results(t"textDocument/signatureHelp") = Lsp.Registry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      try lambda(json.as[SignatureHelp]).in[Json] catch case _: Exception => json

  // The diagnostics a server publishes unbidden. Unlike the rest, this is a notification, so the
  // `params` are rewritten rather than a result: the `uri` and `version` the server reported are
  // kept, and only the reports are the proxy's to change.
  transparent inline def diagnostics(inline lambda: List[Diagnostic] => List[Diagnostic])
     ( using proxy: Lsp.Proxy^ )
  :   Unit =

    proxy.notices(t"textDocument/publishDiagnostics") = Lsp.Registry.Slot[Json => Json]: params =>
      import dynamicJsonAccess.enabled
      import strategies.throwUnsafely

      Map
       ( t"uri"         -> params.uri,
         t"version"     -> params.version,
         t"diagnostics" -> lambda(params.diagnostics.as[List[Diagnostic]]).in[Json] )

      . in[Json]

  // The escape hatches, for methods this library does not model, or for a payload a proxy would
  // rather treat as JSON: `result` amends the answer to a request, `notification` the parameters
  // of a message the server sends unbidden.
  transparent inline def result(method: Text)(inline lambda: Json => Json)
     ( using proxy: Lsp.Proxy^ )
  :   Unit =
    proxy.results(method) = Lsp.Registry.Slot[Json => Json](lambda)

  transparent inline def notification(method: Text)(inline lambda: Json => Json)
     ( using proxy: Lsp.Proxy^ )
  :   Unit =
    proxy.notices(method) = Lsp.Registry.Slot[Json => Json](lambda)

  // The whole-message hooks: every message the editor sends, and every message the server sends
  // back, with the chance to forward it, amend it, drop it, or answer it here. Each may also ask
  // the server something of its own, through `upstream` — mind which thread the hook runs on, as
  // `upstream` documents.
  transparent inline def outbound(inline hook: Lsp.Proxy.OutboundHook)(using proxy: Lsp.Proxy^)
  :   Unit =
    proxy.outbound0 = Lsp.Registry.Slot[Lsp.Proxy.OutboundHook](hook)

  transparent inline def inbound(inline hook: Lsp.Proxy.InboundHook)(using proxy: Lsp.Proxy^): Unit =
    proxy.inbound0 = Lsp.Registry.Slot[Lsp.Proxy.InboundHook](hook)

  // Run once, on a task of its own, as soon as the proxy has a session with the server upstream:
  // the place for what a proxy must ask before it can amend anything, and the only hook free to
  // await an answer.
  transparent inline def connected(inline block: => Unit)(using proxy: Lsp.Proxy^): Unit =
    proxy.connected0 = Lsp.Registry.Slot[() => Unit](() => block)

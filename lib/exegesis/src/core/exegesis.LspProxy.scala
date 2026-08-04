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

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

import Lsp.*

// The target of the proxy's registration combinators, lent to the block given to `Lsp.proxy` for
// its duration. An exclusive capability, so it cannot escape the block; its slots are public and
// untracked, as `LspRegistry`'s are, because the combinators are inline and assign from their
// expansion sites.
//
// Two layers of interception live here. The typed rewriters — `hover`, `capabilities`, and the
// rest — are keyed by method and act on a decoded payload; the raw hooks act on whole messages,
// including methods this library does not model. What neither covers is forwarded byte for byte.
class LspProxy private[exegesis] () extends caps.ExclusiveCapability:
  // The rewriters, by method: an erased rim like `LspRegistry`'s slots, for the same reason — a
  // function value in an invariant container freshens its capture set at every adaptation.
  @scala.caps.unsafe.untrackedCaptures
  val results: scm.HashMap[Text, AnyRef] = scm.HashMap()

  @scala.caps.unsafe.untrackedCaptures
  val notices: scm.HashMap[Text, AnyRef] = scm.HashMap()

  @scala.caps.unsafe.untrackedCaptures
  var outbound0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var inbound0: AnyRef | Null = null

object LspProxy:
  // Runs the proxy: this process serves an editor over its own standard input and output while
  // holding a session with the server it forwards to.
  //
  // The relay is asynchronous in both directions — a message is forwarded as it arrives, and
  // nothing waits for a response — so requests may be in flight in any number and answered in any
  // order, exactly as they would be without a proxy in the middle. Request ids pass through
  // unchanged, so a response returns under the id the editor chose; the id-to-method map exists
  // only so that a response can be given back its type on the way out.
  def run
     ( upstream: Lsp.Server, observer: Lsp.Observer = Lsp.Observer.Silent )
     ( register: (proxy: LspProxy^) ?=> Unit )
     ( using stdio: Stdio^, monitor: Monitor, probate: Probate, working: WorkingDirectory,
             diagnostics: Diagnostics )
  :   Unit =

    import strategies.throwUnsafely
    import Json.jsonEncodableInText

    val proxy: LspProxy^ = LspProxy()
    register(using proxy)

    // Registration is over, so the rules are read out once, here: everything below closes over
    // these, and not over the registry, which may not be touched again once a value hiding it
    // exists.
    val results: scm.HashMap[Text, AnyRef] = proxy.results
    val notices: scm.HashMap[Text, AnyRef] = proxy.notices
    val inbound0: Optional[(Optional[Text], Json) => Transit] = inbound(proxy.inbound0)
    val outbound0: Optional[(Text, Json) => Transit] = outbound(proxy.outbound0)

    // The method each in-flight request named, by the text of its id. An id is matched by its
    // encoded form because the protocol allows either a number or a string, and the proxy neither
    // rewrites nor interprets it.
    val pending: scm.HashMap[Text, Text] = scm.HashMap()

    def downstream(json: Json): Unit =
      val body: Text = json.encode
      observer.sent(body)
      stdio.write(LspTransport.frame(body))
      stdio.out.flush()

    // Everything the server sends is intercepted before the session routes it: a proxy relays
    // messages rather than acting on them, and that includes the responses that no named handler
    // covers. Sealed because the listener writes downstream, and so captures this process's own
    // standard output, which an honest capability type would then hide from the loop below —
    // which writes there too. It is a local of this method, and lives as long as the session.
    given listener: Lsp.Listener = caps.unsafe.unsafeAssumePure(new Lsp.Listener:
      override def intercept(json: Json): Boolean =
        val method: Optional[Text] = Lsp.method(json)

        // A response is retyped by the method it answers, a notification by its own name.
        val answered: Optional[Text] =
          if method.absent
          then Lsp.identifier(json).let { id => pending.remove(id.encode).getOrElse(Unset) }
          else Unset

        val retyped: Json =
          val typed: Json = answered.lay(json): method =>
            rule(results.get(method)).lay(json)(rewriteResult(json, _))

          method.lay(typed): method =>
            rule(notices.get(method)).lay(typed)(rewriteParams(typed, _))

        inbound0.lay(downstream(retyped)): hook =>
          hook(method.or(answered), retyped) match
            case Transit.Forward       => downstream(retyped)
            case Transit.Rewrite(json) => downstream(json)
            case Transit.Drop          => ()
            case Transit.Answer(_)     => ()

        true )

    upstream.session: server ?=>
      LspTransport.pump(stdio.in.source[Data], observer): message =>
        safely(message.as[Json]).let: json =>
          Lsp.method(json).let: method =>
            val id: Optional[Json] = Lsp.identifier(json)

            def forward(message: Json): Unit =
              id.let { id => pending(id.encode) = method }
              server.send(message)

            outbound0.lay(forward(json)): hook =>
              hook(method, json) match
                case Transit.Forward       => forward(json)
                case Transit.Rewrite(json) => forward(json)
                case Transit.Drop          => ()

                // Answered here, so the server never sees the request and never answers it.
                case Transit.Answer(result) =>
                  id.let { id => downstream(JsonRpc.Response("2.0", result, id).in[Json]) }

  // The `result` member of a response, rewritten in place. A response carrying an `error` is left
  // alone: a fault is not a payload, and a rewriter typed for the payload could not read it.
  private def rewriteResult(json: Json, rewrite: Json => Json): Json =
    import dynamicJsonAccess.enabled
    import strategies.throwUnsafely

    try
      val result = json.result
      Map(t"jsonrpc" -> t"2.0".in[Json], t"result" -> rewrite(result), t"id" -> json.id).in[Json]

    catch case _: Exception => json

  // The `params` of a notification or of a request the server made of its client, rewritten.
  private def rewriteParams(json: Json, rewrite: Json => Json): Json =
    import dynamicJsonAccess.enabled
    import strategies.throwUnsafely

    try
      val method: Json = json.method
      val params: Json = rewrite(json.params)

      // Rebuilt rather than amended in place: a notification carries no id, and a request the
      // server made of its client must keep the one it chose.
      Lsp.identifier(json).lay:
        Map(t"jsonrpc" -> t"2.0".in[Json], t"method" -> method, t"params" -> params).in[Json]
      . apply: id =>
          Map
           ( t"jsonrpc" -> t"2.0".in[Json],
             t"method"  -> method,
             t"params"  -> params,
             t"id"      -> id )

          . in[Json]

    catch case _: Exception => json

  // The slots are an erased rim, as `LspRegistry`'s are, and each is restored by a single
  // annotated cast at the one place that reads it.
  // A rewriter read back out of the erased rim, restored by a single annotated cast.
  private def rule(slot: scala.Option[AnyRef]): Optional[Json => Json] =
    slot.getOrElse(null) match
      case null => Unset

      case value: AnyRef =>
        val lambda: Json => Json = value.asInstanceOf[LspRegistry.Slot[Json => Json]].value
        lambda

  private def outbound(hook: AnyRef | Null): Optional[(Text, Json) => Transit] =
    if hook == null then Unset else
      val relay: (Text, Json) => Transit =
        hook.asInstanceOf[LspRegistry.Slot[(Text, Json) => Transit]].value

      relay

  private def inbound(hook: AnyRef | Null): Optional[(Optional[Text], Json) => Transit] =
    if hook == null then Unset else
      val relay: (Optional[Text], Json) => Transit =
        hook.asInstanceOf[LspRegistry.Slot[(Optional[Text], Json) => Transit]].value

      relay

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
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"initialize") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      val result = json.as[InitializeResult]

      result.copy(capabilities = adjust(result.capabilities)).in[Json]

  // Applied only when the server had something to say: a result that does not decode as a hover
  // is the `null` a server sends when it has none, and is passed on as it stands, so a rewriter
  // never has to spell out the empty case.
  transparent inline def hover(inline lambda: Hover => Hover)(using proxy: LspProxy^): Unit =
    proxy.results(t"textDocument/hover") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      try lambda(json.as[Hover]).in[Json] catch case _: Exception => json

  transparent inline def completions(inline lambda: CompletionList => CompletionList)
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/completion") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[CompletionList]).in[Json]

  transparent inline def definitions(inline lambda: List[Location] => List[Location])
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/definition") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[Location]]).in[Json]

  transparent inline def symbols(inline lambda: List[DocumentSymbol] => List[DocumentSymbol])
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/documentSymbol") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[DocumentSymbol]]).in[Json]

  transparent inline def codeActions(inline lambda: List[CodeAction] => List[CodeAction])
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/codeAction") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[CodeAction]]).in[Json]

  transparent inline def codeLenses(inline lambda: List[CodeLens] => List[CodeLens])
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/codeLens") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[CodeLens]]).in[Json]

  transparent inline def inlayHints(inline lambda: List[InlayHint] => List[InlayHint])
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/inlayHint") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      lambda(json.as[List[InlayHint]]).in[Json]

  transparent inline def signatureHelp(inline lambda: SignatureHelp => SignatureHelp)
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.results(t"textDocument/signatureHelp") = LspRegistry.Slot[Json => Json]: json =>
      import strategies.throwUnsafely
      try lambda(json.as[SignatureHelp]).in[Json] catch case _: Exception => json

  // The diagnostics a server publishes unbidden. Unlike the rest, this is a notification, so the
  // `params` are rewritten rather than a result: the `uri` and `version` the server reported are
  // kept, and only the reports are the proxy's to change.
  transparent inline def diagnostics(inline lambda: List[Diagnostic] => List[Diagnostic])
     ( using proxy: LspProxy^ )
  :   Unit =

    proxy.notices(t"textDocument/publishDiagnostics") = LspRegistry.Slot[Json => Json]: params =>
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
     ( using proxy: LspProxy^ )
  :   Unit =
    proxy.results(method) = LspRegistry.Slot[Json => Json](lambda)

  transparent inline def notification(method: Text)(inline lambda: Json => Json)
     ( using proxy: LspProxy^ )
  :   Unit =
    proxy.notices(method) = LspRegistry.Slot[Json => Json](lambda)

  // The whole-message hooks: every message the editor sends, and every message the server sends
  // back, with the chance to forward it, amend it, drop it, or answer it here.
  transparent inline def outbound(inline hook: (Text, Json) => Transit)(using proxy: LspProxy^)
  :   Unit =
    proxy.outbound0 = LspRegistry.Slot[(Text, Json) => Transit](hook)

  transparent inline def inbound(inline hook: (Optional[Text], Json) => Transit)
     ( using proxy: LspProxy^ )
  :   Unit =
    proxy.inbound0 = LspRegistry.Slot[(Optional[Text], Json) => Transit](hook)

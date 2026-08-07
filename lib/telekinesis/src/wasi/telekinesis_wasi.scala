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
package telekinesis

import scala.annotation.nowarn
import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import hellenism.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import distillate.*
import soundness.{call, dispose}
import urticose.*
import xenophile.*
import zephyrine.*

// The WIT definitions the navigation below is typechecked against, and which the `call`
// materializer consults (at its downstream expansion site) for module ids, resource methods and
// parameter types.
type WasiHttpApi = Interface in Wit at "/telekinesis/http.wit"
given wasiHttpApi: WasiHttpApi = Interface[Wit](cp"/telekinesis/http.wit")

package httpBackends:
  // An `Http.Backend` over `wasi:http/outgoing-handler`: the request is assembled from
  // `wasi:http/types` resources (`fields` for the headers, then an `outgoing-request` with its
  // method, scheme, authority and path, plus an `outgoing-body` when the method carries one),
  // handed to the host, and its response awaited through the pollable and read from the body's
  // `input-stream`. `inline`, so the `call`s expand at the downstream summoning site: the Wasm
  // Component imports only materialize in code compiled for a Wasm target. Summoning it requires
  // `wasiHttpApi` (and this module's WIT resource) to be visible at that site.
  //
  // The per-site duplication the compiler warns about is the point: the instance must materialize
  // at the downstream summoning site, and a WASI-linked application summons it once.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasi: Http.Backend = new Http.Backend:
    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    Spring[Data] )
      ( using Tactic[ConnectError] )
    :   Http.Response =

      // The URL arrives fully resolved; split it into the scheme, authority and path-with-query
      // the `outgoing-request` setters take.
      val (scheme: Text, afterScheme: Text) = url.cut(t"://", 2) match
        case List(scheme, rest) => (scheme, rest)
        case _                  => (t"http", url)

      val (authority: Text, target: Text) = afterScheme.cut(t"/", 2) match
        case List(host, path) => (host, t"/$path")
        case _                => (afterScheme, t"/")

      def bytes(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("UTF-8").nn)

      // Request headers travel in a `fields` resource, whose ownership passes into the
      // `outgoing-request`, whose ownership in turn passes into `handle` — so neither is
      // explicitly dropped here.
      val fieldsHandle = Foreign["fields", Wit].constructor.call[WitHandle of "fields"]()
      val fields: Foreign of "fields" from Wit = fieldsHandle

      headers.each: (header: Http.Header) =>
        fields.append(header.key, bytes(header.value)).call[Unit]()

      // Applied calls need stable receivers with visible `Origin`s, so the argument conversions
      // can resolve their ecosystem; roots invoked with arguments are bound to `val`s first. (The
      // cast supplies the refinement `Foreign.apply` would carry, whose transparent-inline
      // expansion is deferred inside this `inline given`.)
      val outgoingRequest =
        Foreign["outgoing-request", Wit].asInstanceOf[Foreign of "outgoing-request" from Wit]

      val requestHandle =
        outgoingRequest.constructor(fieldsHandle).call[WitHandle of "outgoing-request"]()

      // The method and scheme are WIT `variant` cases, selected by their lower-kebab-case names;
      // the other setters take `option<string>`s, which a plain `string` argument subsumes (and
      // crosses the boundary wrapped as a present option).
      val request: Foreign of "outgoing-request" from Wit = requestHandle
      request.`set-method`(WitCase["method"](method.show.lower)).call[Unit]()
      request.`set-scheme`(WitCase["scheme"](scheme.lower)).call[Unit]()
      request.`set-authority`(authority).call[Unit]()
      request.`set-path-with-query`(target).call[Unit]()

      // A method that carries a payload streams it through the request's `outgoing-body`, which
      // must then be `finish`ed (a static function) for the request to be complete.
      val payload: Data = if method.payload then body().memoize else Array.empty[Byte]

      val bodyHandles =
        if payload.isEmpty then Unset else
          val bodyHandle = request.body.call[WitHandle of "outgoing-body"]()
          val outgoingBody: Foreign of "outgoing-body" from Wit = bodyHandle
          val writeHandle = outgoingBody.write.call[WitHandle of "output-stream"]()
          (bodyHandle, writeHandle)

      val outgoingHandler =
        Foreign["outgoing-handler", Wit].asInstanceOf[Foreign of "outgoing-handler" from Wit]

      val futureHandle =
        outgoingHandler.handle(requestHandle, Unset).call[WitHandle of "future-incoming-response"]()

      // The payload is written after the request is handed off (the host consumes it as it
      // arrives) and before blocking on the response.
      bodyHandles.let: (bodyHandle, writeHandle) =>
        val outStream: Foreign of "output-stream" from Wit = writeHandle

        outStream.`blocking-write-and-flush`(payload).call[Unit]()

        writeHandle.dispose()

        val outgoingBody =
          Foreign["outgoing-body", Wit].asInstanceOf[Foreign of "outgoing-body" from Wit]

        outgoingBody.finish(bodyHandle, Unset).call[Unit]()

      val future: Foreign of "future-incoming-response" from Wit = futureHandle

      // The response arrives asynchronously: block on the future's pollable, after which `get`
      // yields it (or an `error-code`, raised by the decoder).
      val pollableHandle = future.subscribe.call[WitHandle of "pollable"]()
      val pollable: Foreign of "pollable" from Wit = pollableHandle
      pollable.block.call[Unit]()
      pollableHandle.dispose()

      val responseHandle =
        try future.get.call[Optional[WitHandle of "incoming-response"]]().or:
          abort(ConnectError(ConnectError.Reason.Unknown))
        catch case error: WitError => abort(ConnectError(ConnectError.Reason.Unknown))

      futureHandle.dispose()
      val response: Foreign of "incoming-response" from Wit = responseHandle

      val status: Http.Status = Http.Status.unapply(response.status.call[U16]().int).getOrElse:
        abort(ConnectError(ConnectError.Reason.Unknown))

      val headersHandle = response.headers.call[WitHandle of "fields"]()
      val responseFields: Foreign of "fields" from Wit = headersHandle

      val textHeaders: List[Http.Header] =
        responseFields.entries.call[List[(Text, Data)]]().map: (key: Text, value: Data) =>
          Http.Header(key, value.utf8)

      headersHandle.dispose()

      // Read the body to exhaustion: `blocking-read`'s `Err(closed)` arm (raised by the decoder)
      // is end-of-stream.
      val bodyHandle = response.consume.call[WitHandle of "incoming-body"]()
      val incomingBody: Foreign of "incoming-body" from Wit = bodyHandle
      val streamHandle = incomingBody.selectDynamic("stream").call[WitHandle of "input-stream"]()
      val stream: Foreign of "input-stream" from Wit = streamHandle

      var chunks: List[Data] = Nil

      try
        while true do chunks = stream.`blocking-read`(U64(65536L.bits)).call[Data]() :: chunks
      catch case error: WitError => ()

      streamHandle.dispose()
      bodyHandle.dispose()
      responseHandle.dispose()

      val content: Data = chunks.stdlib.reverse.foldLeft(Array.empty[Byte])(_ ++ _)
      status(textHeaders, Http.Body.Fixed(content))

// Serves HTTP from a Wasm Component: the bridge from `wasi:http/incoming-handler`'s exported
// `handle` function to a Soundness handler. The application supplies a small `@WitExport` shim
// (the one piece that must exist, annotated, in the Wasm-compiled application itself) which
// passes the two facade instances through untyped — they are Wasm-only classes this module never
// names — and everything else happens here: unmarshalling the request, dispatching to the
// handler, marshalling the response, and setting the outparam. `inline`, so the `call`s expand
// at the downstream summoning site, exactly as for the backends above.
object WasiHttpServer:
  inline def handle(request: Any, responseOut: Any)
    ( inline handler: Http.Request => Http.Response )
  :   Unit =

    def bytes(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("UTF-8").nn)

    val requestHandle = new WitHandle(request).asInstanceOf[WitHandle of "incoming-request"]
    val incoming: Foreign of "incoming-request" from Wit = requestHandle

    val method: Http.Method =
      unsafely(incoming.method.call[WitCase of "method"]().name.upper.as[Http.Method])

    val target: Text = incoming.`path-with-query`.call[Optional[Text]]().or(t"/")

    val headersHandle = incoming.headers.call[WitHandle of "fields"]()
    val requestFields: Foreign of "fields" from Wit = headersHandle

    val textHeaders: List[Http.Header] =
      requestFields.entries.call[List[(Text, Data)]]().map: (key: Text, value: Data) =>
        Http.Header(key, value.utf8)

    headersHandle.dispose()

    val bodyHandle = incoming.consume.call[WitHandle of "incoming-body"]()
    val incomingBody: Foreign of "incoming-body" from Wit = bodyHandle
    val streamHandle = incomingBody.selectDynamic("stream").call[WitHandle of "input-stream"]()
    val inputStream: Foreign of "input-stream" from Wit = streamHandle

    var chunks: List[Data] = Nil

    try
      while true do chunks = inputStream.`blocking-read`(U64(65536L.bits)).call[Data]() :: chunks
    catch case error: WitError => ()

    streamHandle.dispose()
    bodyHandle.dispose()
    requestHandle.dispose()

    val content: Data = chunks.stdlib.reverse.foldLeft(Array.empty[Byte])(_ ++ _)

    // The request's host is the server's own; a component behind `wasi:http` is not addressed by
    // hostname, so `Localhost` stands in (and avoids parsing the authority, which would reach the
    // wasm javalib's trapping regex engine).
    val httpRequest =
      Http.Request
        ( method,
          1.1,
          Localhost,
          target,
          textHeaders,
          () => Stream(Iterator(content)) )

    val response = handler(httpRequest)

    // The response's headers travel in a `fields` whose ownership passes into the
    // `outgoing-response`, whose ownership passes into `set` — so neither is dropped here.
    val fieldsHandle = Foreign["fields", Wit].constructor.call[WitHandle of "fields"]()
    val fields: Foreign of "fields" from Wit = fieldsHandle

    response.textHeaders.each: (header: Http.Header) =>
      fields.append(header.key, bytes(header.value)).call[Unit]()

    val outgoingResponse =
      Foreign["outgoing-response", Wit].asInstanceOf[Foreign of "outgoing-response" from Wit]

    val responseHandle =
      outgoingResponse.constructor(fieldsHandle).call[WitHandle of "outgoing-response"]()

    val outgoing: Foreign of "outgoing-response" from Wit = responseHandle
    outgoing.`set-status-code`(U16(response.status.code.toShort.bits)).call[Unit]()

    val outBodyHandle = outgoing.body.call[WitHandle of "outgoing-body"]()

    // The outparam is set before the body is written, so the host can stream the response; the
    // response's ownership passes into `set` (wrapped as the `ok` arm of its `result` parameter).
    val outparamHandle = new WitHandle(responseOut).asInstanceOf[WitHandle of "response-outparam"]

    val responseOutparam =
      Foreign["response-outparam", Wit].asInstanceOf[Foreign of "response-outparam" from Wit]

    responseOutparam.set(outparamHandle, responseHandle).call[Unit]()

    val payload: Data = response.body match
      case Http.Body.Fixed(data) => data
      case Http.Body.Empty       => Array.empty[Byte]
      case body                  => body.stream.memoize

    val outBody: Foreign of "outgoing-body" from Wit = outBodyHandle

    if !payload.isEmpty then
      val writeHandle = outBody.write.call[WitHandle of "output-stream"]()
      val outputStream: Foreign of "output-stream" from Wit = writeHandle
      outputStream.`blocking-write-and-flush`(payload).call[Unit]()
      writeHandle.dispose()

    val outgoingBody =
      Foreign["outgoing-body", Wit].asInstanceOf[Foreign of "outgoing-body" from Wit]

    outgoingBody.finish(outBodyHandle, Unset).call[Unit]()

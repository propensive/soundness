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
package telekinesis

import scala.annotation.nowarn

import anticipation.*
import contingency.*
import gossamer.*
import hellenism.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*
import soundness.{invoke, dispose}
import xenophile.*

// The WIT definitions the navigation below is typechecked against, and which the `invoke`
// materializer consults (at its downstream expansion site) for module ids, resource methods and
// parameter types.
type WasiHttpApi = Interface in Wit at "/telekinesis/http.wit"
given wasiHttpApi: WasiHttpApi = Interface[Wit](cp"/telekinesis/http.wit")

package httpBackends:
  // An `Http.Backend` over `wasi:http/outgoing-handler`: the request is assembled from
  // `wasi:http/types` resources (`fields` for the headers, then an `outgoing-request`), handed to
  // the host, and its response awaited through the pollable and read from the body's
  // `input-stream`. `inline`, so the `invoke`s expand at the downstream summoning site: the Wasm
  // Component imports only materialize in code compiled for a Wasm target. Summoning it requires
  // `wasiHttpApi` (and this module's WIT resource) to be visible at that site.
  //
  // Only `GET` requests are supported so far: setting the method or scheme on an
  // `outgoing-request` takes a WIT `variant` argument, which `invoke` cannot yet marshal, so the
  // request is sent with its defaults (`GET`, and a scheme of the host's choosing).
  //
  // The per-site duplication the compiler warns about is the point: the instance must materialize
  // at the downstream summoning site, and a WASI-linked application summons it once.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasi: Http.Backend = new Http.Backend:
    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    () => LazyList[Data] )
      ( using Tactic[ConnectError] )
    :   Http.Response =

      if method != Http.Get then abort(ConnectError(ConnectError.Reason.Unknown))

      // The URL arrives fully resolved; split it into the authority and the path-with-query the
      // `outgoing-request` setters take. (The scheme is currently left unset — see above.)
      val afterScheme: Text = url.cut(t"://", 2) match
        case List(_, rest) => rest
        case _             => url

      val (authority: Text, target: Text) = afterScheme.cut(t"/", 2) match
        case List(host, path) => (host, t"/$path")
        case _                => (afterScheme, t"/")

      def bytes(text: Text): Data = text.s.getBytes("UTF-8").nn.immutable(using Unsafe)

      // Request headers travel in a `fields` resource, whose ownership passes into the
      // `outgoing-request`, whose ownership in turn passes into `handle` — so neither is
      // explicitly dropped here.
      val fieldsHandle = Foreign["fields", Wit].constructor.invoke[WitHandle of "fields"]
      val fields: Foreign of "fields" from Wit = fieldsHandle

      headers.each: header =>
        fields.append(header.key, bytes(header.value)).invoke[Unit]

      // Applied calls need stable receivers with visible `Origin`s, so the argument conversions
      // can resolve their ecosystem; roots invoked with arguments are bound to `val`s first. (The
      // cast supplies the refinement `Foreign.apply` would carry, whose transparent-inline
      // expansion is deferred inside this `inline given`.)
      val outgoingRequest =
        Foreign["outgoing-request", Wit].asInstanceOf[Foreign of "outgoing-request" from Wit]

      val requestHandle =
        outgoingRequest.constructor(fieldsHandle).invoke[WitHandle of "outgoing-request"]

      // The setters take `option<string>`s; a plain `string` argument subsumes (and crosses the
      // boundary wrapped as a present option).
      val request: Foreign of "outgoing-request" from Wit = requestHandle
      request.`set-authority`(authority).invoke[Unit]
      request.`set-path-with-query`(target).invoke[Unit]

      val outgoingHandler =
        Foreign["outgoing-handler", Wit].asInstanceOf[Foreign of "outgoing-handler" from Wit]

      val futureHandle =
        outgoingHandler.handle(requestHandle, Unset).invoke[WitHandle of "future-incoming-response"]

      val future: Foreign of "future-incoming-response" from Wit = futureHandle

      // The response arrives asynchronously: block on the future's pollable, after which `get`
      // yields it (or an `error-code`, raised by the decoder).
      val pollableHandle = future.subscribe.invoke[WitHandle of "pollable"]
      val pollable: Foreign of "pollable" from Wit = pollableHandle
      pollable.block.invoke[Unit]
      pollableHandle.dispose()

      val responseHandle =
        try future.get.invoke[Optional[WitHandle of "incoming-response"]].or:
          abort(ConnectError(ConnectError.Reason.Unknown))
        catch case error: RuntimeException => abort(ConnectError(ConnectError.Reason.Unknown))

      futureHandle.dispose()
      val response: Foreign of "incoming-response" from Wit = responseHandle

      val status: Http.Status = Http.Status.unapply(response.status.invoke[U16].int).getOrElse:
        abort(ConnectError(ConnectError.Reason.Unknown))

      val headersHandle = response.headers.invoke[WitHandle of "fields"]
      val responseFields: Foreign of "fields" from Wit = headersHandle

      val textHeaders: List[Http.Header] =
        responseFields.entries.invoke[List[(Text, Data)]].map: (key, value) =>
          Http.Header(key, value.utf8)

      headersHandle.dispose()

      // Read the body to exhaustion: `blocking-read`'s `Err(closed)` arm (raised by the decoder)
      // is end-of-stream.
      val bodyHandle = response.consume.invoke[WitHandle of "incoming-body"]
      val incomingBody: Foreign of "incoming-body" from Wit = bodyHandle
      val streamHandle = incomingBody.stream.invoke[WitHandle of "input-stream"]
      val stream: Foreign of "input-stream" from Wit = streamHandle

      var chunks: List[Data] = Nil

      try
        while true do
          chunks = stream.`blocking-read`(U64(65536L.bits)).invoke[Data] :: chunks
      catch case error: RuntimeException => ()

      streamHandle.dispose()
      bodyHandle.dispose()
      responseHandle.dispose()

      status(textHeaders, Http.Body.Streaming(chunks.reverse.to(LazyList)))

package e2ehttp

import scala.scalajs.wit.annotation.*
import componentmodel.exports.wasi.http.IncomingHandler
import scala.scalajs.wasi.http.types.{IncomingRequest, ResponseOutparam}

import anticipation.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import prepositional.*
import spectacular.*
import telekinesis.*, telekinesis.wasiHttpApi
import vacuous.*
import zephyrine.*

// The HTTP service half of the wasm e2e harness, linked against the `http` world and driven by
// `etc/ci/wasm-e2e.sh` under `wasmtime serve`. Where `e2e`'s component exercises the seven WASI
// backends as a client, this one exercises the only piece of the stack that nothing else links:
// `WasiHttpServer`, the incoming half of telekinesis' WASI HTTP backend, which bridges an exported
// `wasi:http/incoming-handler` call to an ordinary Soundness request-to-response function.
//
// The response echoes the request line and body, so the scenario can assert that the method,
// target and payload all survived the round trip through the Component Model ABI.
@WitImplementation
object Service extends IncomingHandler:
  @WitExport("wasi:http/incoming-handler@0.2.0", "handle")
  def handle(request: IncomingRequest, responseOut: ResponseOutparam): Unit =
    WasiHttpServer.handle(request, responseOut): request =>
      val body = request.body().memoize.utf8
      val echo = t"${request.method} ${request.target}\n${body}"

      Http.Response(Http.Ok, contentType = media"text/plain")(echo)

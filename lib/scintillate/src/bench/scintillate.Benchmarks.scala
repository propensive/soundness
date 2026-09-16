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
package scintillate

import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import eucalyptus.*, logging.silentLogging
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charEncoders.utf8Encoder
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import telekinesis.*
import parasite.*, threading.virtualThreading
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*
import zephyrine.memoize
import webserverErrorPages.minimalErrorPage

// Benchmarks for the raw-TCP HTTP/1.1 server. The wire-codec benchmarks (parse,
// serialize) and the full-pipeline benchmark all run entirely in memory — no
// socket and no threads — so they measure the per-request CPU cost of the HTTP
// machinery, isolated from OS-socket and Loom-scheduling overhead.
object Benchmarks extends Suite(m"Scintillate socket-server benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga))

  // A representative GET request with a handful of headers.
  lazy val getRequest: Data =
    List
      ( t"GET /path/to/resource HTTP/1.1",
        t"Host: example.com",
        t"Accept: text/html",
        t"User-Agent: bench" )

    . join(t"", t"\r\n", t"\r\n\r\n")
    . in[Data]

  lazy val getRequestBytes: scala.Array[Byte] = Array.unsafeJvm(getRequest)

  lazy val okResponse: Http.Response = Http.Response(Http.Ok)(t"Hello, World!")

  val handler: Http.Connection ?=> Http.Response = okResponse

  // 1000 GET requests back-to-back, built once; each pipeline run wraps this in a
  // fresh (O(1)) `ByteArrayInputStream`, so the measurement excludes buffer setup.
  val pipelineCount: Int = 1000

  lazy val pipelineBuffer: scala.Array[Byte] =
    val unit = getRequestBytes
    val total = new scala.Array[Byte](unit.length*pipelineCount)
    var index = 0

    while index < pipelineCount do
      _root_.java.lang.System.arraycopy(unit, 0, total, index*unit.length, unit.length)
      index += 1

    total

  // Parse the request-line and headers of a single request.
  def parseRequest(bytes: Data): Http.Method = Http.Request.parse(Chain(bytes)).method

  // Serialise a fixed response to bytes, forcing the whole stream.
  def serializeResponse(response: Http.Response): Int =
    Http.Response.serialize(response).memoize.length

  // Drive every pipelined request through the full connection loop into a null
  // sink: parse, frame body, dispatch, serialize, write, keep-alive bookkeeping.
  def drivePipeline(): Int =
    val in = _root_.java.io.ByteArrayInputStream(pipelineBuffer)
    val out = _root_.java.io.OutputStream.nullOutputStream.nn
    SocketServer(0).serveConnection(handler)(in, out)
    pipelineCount

  // Every server in the socket comparison, by row name. The rows are generated from this
  // table, so each suite holds exactly one row per server; `HttpRivals.ensure` starts the
  // named server inside the measurement JVM.
  val servers: List[(Message, Int)] = List
    ( m"Scintillate  Reactor"                -> HttpRivals.Reactor,
      m"Scintillate  SocketServer (virtual)"  -> HttpRivals.SocketServerVirtual,
      m"Scintillate  SocketServer (platform)" -> HttpRivals.SocketServerPlatform,
      m"http4s  EmberServer"                 -> HttpRivals.Ember,
      m"ZIO  zio-http Server"                -> HttpRivals.ZioHttp,
      m"Netty  raw HTTP codec"               -> HttpRivals.Netty,
      m"Undertow  RoutingHandler"            -> HttpRivals.Undertow,
      m"Jetty  core Handler (virtual)"       -> HttpRivals.Jetty,
      m"Helidon  SE WebServer"               -> HttpRivals.Helidon,
      m"JDK  HttpServer (virtual)"           -> HttpRivals.JdkHttpServer,
      m"Pekko  pekko-http"                   -> HttpRivals.PekkoHttp,
      m"Cask  cask.Routes"                   -> HttpRivals.Cask,
      m"Vert.x  vertx-web Router"            -> HttpRivals.Vertx )

  // One round-trip of `workload` against `server`, staged for the measurement JVM.
  private def roundtrip(server: Int, workload: Int)(using Quotes): Expr[Any] =
    '{
        scintillate.HttpRivals.ensure(${Expr(server)})
        scintillate.HttpRivals.roundtrip(${Expr(server)}, ${Expr(workload)})
    }

  // Kept out of `run` as separate methods: two iterations in one method body trip a
  // compiler crash (wildApprox).
  private def sweep(stress: Stress)(using Testable): Unit =
    servers.each: (name, server) =>
      stress(name)(target = 1*Second, sweep = 256, refine = true):
        roundtrip(server, HttpWorkload.Plaintext)

  private def capacity(stress: Stress, workload: Int, slo: Int)(using Testable): Unit =
    servers.each: (name, server) =>
      stress(name)
        ( target = 1*Second, threshold = slo*Milli(Second), compliance = 99, refine = true ):
        roundtrip(server, workload)

  def run(): Unit =
    val bench = Bench()
    // The real-socket rows: 2 GB heap, all cores (the machine itself is the resource under
    // test), and G1 rather than the harness's default Serial collector, whose single-threaded
    // stop-the-world pauses would dominate p99 latency in a saturated server workload.
    val stress = Stress(heap = t"2g", gc = t"G1")
    val profile = Profile(heap = t"2g")

    val requestSize  = getRequest.length*Byte
    val responseSize = serializeResponse(okResponse)*Byte
    val pipelineSize = getRequest.length*pipelineCount*Byte

    // The rival rows run each codec's own parser or encoder over the same bytes; see
    // `RivalCodecs`.
    suite(m"Wire codec (no socket, no threads)"):
      bench(m"Parse a request head")(target = 1*Second, operationSize = requestSize):
        '{ scintillate.Benchmarks.parseRequest(scintillate.Benchmarks.getRequest) }

      bench(m"Netty  parse a request head")(target = 1*Second, operationSize = requestSize):
        '{ scintillate.RivalCodecs.nettyParse() }

      bench(m"Jetty  parse a request head")(target = 1*Second, operationSize = requestSize):
        '{ scintillate.RivalCodecs.jettyParse() }

      bench(m"Serialize a fixed response")(target = 1*Second, operationSize = responseSize):
        '{ scintillate.Benchmarks.serializeResponse(scintillate.Benchmarks.okResponse) }

      bench(m"Netty  serialize a fixed response")
        ( target = 1*Second, operationSize = responseSize ):
        '{ scintillate.RivalCodecs.nettySerialize() }

      bench(m"Jetty  serialize a fixed response")
        ( target = 1*Second, operationSize = responseSize ):
        '{ scintillate.RivalCodecs.jettySerialize() }

    suite(m"Full pipeline (in-process, no socket)"):
      bench(m"1000 pipelined GETs through serveConnection")
        ( target = 1*Second, operationSize = pipelineSize ):
        '{ scintillate.Benchmarks.drivePipeline() }

    // Real-socket requests per second: see `HttpRivals` for the client, the servers,
    // the workloads and the colocation caveats. The harness workers (the clients) run on
    // virtual threads in every row — one cheap persistent connection each, identical
    // across servers — while scintillate's two `SocketServer` rows toggle the kind of
    // thread the *server* handles connections on. The sweep suite doubles the
    // client-connection count from 1 to 256, then refines around the fastest count, reading
    // as each server's throughput-vs-N curve with its peak flagged `sustained`; each capacity
    // suite searches for the highest sustained rate with 99% of requests answered within the
    // SLO — each server's headline requests/sec figure for that workload — at whichever
    // connection count achieves it, which need not be the largest compliant one. On loopback, an uncontended round-trip is
    // tens of microseconds, so a 5 ms SLO measures queuing, scheduling and GC under
    // load, not network noise; the two megabyte-scale workloads get 10 ms, since copying
    // their payloads alone approaches the tighter bound.
    suite(m"HTTP over real sockets: scaling sweep (plaintext, N ≤ 256)"):
      sweep(stress)

    suite(m"HTTP over real sockets: capacity search (plaintext, 99% ≤ 5 ms)"):
      capacity(stress, HttpWorkload.Plaintext, 5)

    suite(m"HTTP over real sockets: capacity search (JSON, 99% ≤ 5 ms)"):
      capacity(stress, HttpWorkload.Json, 5)

    suite(m"HTTP over real sockets: capacity search (path and header, 99% ≤ 5 ms)"):
      capacity(stress, HttpWorkload.Route, 5)

    suite(m"HTTP over real sockets: capacity search (1 MiB response, 99% ≤ 10 ms)"):
      capacity(stress, HttpWorkload.Large, 10)

    suite(m"HTTP over real sockets: capacity search (64 KiB POST echo, 99% ≤ 10 ms)"):
      capacity(stress, HttpWorkload.Echo, 10)

    // Where the round-trip's CPU goes, for scintillate and (as the reference) zio-http.
    // The single profiled client thread's frames are identical in both rows, so any
    // difference between the histograms is server-side. CPU-only: parked time (the
    // client awaiting the response, the server awaiting a request) is invisible.
    suite(m"Profile: HTTP round-trip hotspots"):
      // The connection loop with no sockets under it: where the socket rows
      // below spend most of their samples parked in `kqueue`/`poll` — which is
      // honest for a round-trip, but drowns everything else — this one puts
      // every sample inside the HTTP stack itself. The right instrument for
      // asking where a request's CPU and allocation actually go.
      profile(m"1000 pipelined GETs through serveConnection")(target = 5*Second):
        '{ scintillate.Benchmarks.drivePipeline() }

      profile(m"Scintillate  socket round-trip")(target = 5*Second):
        '{
            scintillate.HttpRivals.ensure(scintillate.HttpRivals.SocketServerVirtual)
            scintillate.HttpRivals.roundtrip
              ( scintillate.HttpRivals.SocketServerVirtual, scintillate.HttpWorkload.Plaintext )
        }

      profile(m"ZIO  zio-http socket round-trip")(target = 5*Second):
        '{
            scintillate.HttpRivals.ensure(scintillate.HttpRivals.ZioHttp)
            scintillate.HttpRivals.roundtrip
              ( scintillate.HttpRivals.ZioHttp, scintillate.HttpWorkload.Plaintext )
        }

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
package scintillate

import scala.quoted.*

import ambience.*, environments.javaEnvironment, systems.javaSystem
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
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*
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
    . data

  lazy val getRequestBytes: Array[Byte] = getRequest.mutable(using Unsafe)

  lazy val okResponse: Http.Response = Http.Response(Http.Ok)(t"Hello, World!")

  val handler: HttpConnection ?=> Http.Response = okResponse

  // 1000 GET requests back-to-back, built once; each pipeline run wraps this in a
  // fresh (O(1)) `ByteArrayInputStream`, so the measurement excludes buffer setup.
  val pipelineCount: Int = 1000

  lazy val pipelineBuffer: Array[Byte] =
    val unit = getRequestBytes
    val total = new Array[Byte](unit.length*pipelineCount)
    var index = 0

    while index < pipelineCount do
      _root_.java.lang.System.arraycopy(unit, 0, total, index*unit.length, unit.length)
      index += 1

    total

  // Parse the request-line and headers of a single request.
  def parseRequest(bytes: Data): Http.Method = Http.Request.parse(LazyList(bytes)).method

  // Serialise a fixed response to bytes, forcing the whole stream.
  def serializeResponse(response: Http.Response): Int =
    Http.Response.serialize(response).read[Data].length

  // Drive every pipelined request through the full connection loop into a null
  // sink: parse, frame body, dispatch, serialize, write, keep-alive bookkeeping.
  def drivePipeline(): Int =
    val in = _root_.java.io.ByteArrayInputStream(pipelineBuffer)
    val out = _root_.java.io.OutputStream.nullOutputStream.nn
    SocketServer(0).serveConnection(handler)(in, out)
    pipelineCount

  def run(): Unit =
    val bench = Bench()

    val requestSize  = getRequest.length*Byte
    val responseSize = serializeResponse(okResponse)*Byte
    val pipelineSize = getRequest.length*pipelineCount*Byte

    suite(m"Wire codec (no socket, no threads)"):
      bench(m"Parse a request head")(target = 1*Second, operationSize = requestSize):
        '{ scintillate.Benchmarks.parseRequest(scintillate.Benchmarks.getRequest) }

      bench(m"Serialize a fixed response")(target = 1*Second, operationSize = responseSize):
        '{ scintillate.Benchmarks.serializeResponse(scintillate.Benchmarks.okResponse) }

    suite(m"Full pipeline (in-process, no socket)"):
      bench(m"1000 pipelined GETs through serveConnection")
        ( target = 1*Second, operationSize = pipelineSize ):
        '{ scintillate.Benchmarks.drivePipeline() }

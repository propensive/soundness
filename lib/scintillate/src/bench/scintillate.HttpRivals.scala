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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package scintillate

import contingency.*, strategies.throwUnsafely
import eucalyptus.*, logging.silentLogging
import gossamer.*
import hieroglyph.charEncoders.utf8Encoder
import parasite.*, probates.awaitProbate
import proscenium.*
import telekinesis.*
import webserverErrorPages.minimalErrorPage

// The real-socket HTTP requests-per-second comparison: scintillate's `SocketServer`
// against the FS2 ecosystem's server (http4s ember, built directly on `fs2.io.net`)
// and zio-http, with an in-process load generator. One stress operation is one
// request/response round-trip on a per-worker persistent keep-alive socket, so the
// harness's latency histogram and capacity search read directly as per-request
// latency and sustained requests per second.
//
// Server and clients share the measurement JVM: the client work steals server CPU,
// but steals it identically for every server, so the figures are relative — a
// two-machine harness would report higher absolute rates. Each stress row runs in
// its own measurement JVM, so each server lives in a `lazy val` forced by the
// warmup (outside every timed window) and is torn down by JVM exit: no stop logic,
// no cross-row port conflicts, and never two servers alive at once. Every server
// answers `GET /bench` with an identical 13-byte `text/plain` body over keep-alive,
// with logging silenced.
//
// The client is deliberately dumber than any HTTP client library (which would be a
// fourth variable): it writes pre-serialized request bytes and scans the response
// only for `Content-Length`, allocation-free, so the server under test remains the
// bottleneck. A response without `Content-Length` (e.g. chunked) fails the run
// rather than letting a rival serve cheaper framing. At high worker counts the
// window-start connection storm can overflow the accept backlog (macOS
// `kern.ipc.somaxconn` defaults to 128); the connect retry loop absorbs it. With
// keep-alive, concurrent sockets number the worker count, not the request rate, so
// the default `ulimit -n` of 10240 is ample and the ephemeral-port range is never
// stressed.
object HttpRivals:
  // The client deliberately fails fast on any protocol violation (truncation, missing
  // `Content-Length`) by throwing: a failed operation should crash the row, not skew it.
  import unsafeExceptions.canThrowAny

  val scintillateVirtualPort: Int = 18080
  val scintillatePlatformPort: Int = 18081
  val emberPort: Int = 18082
  val zioPort: Int = 18083

  // ── The client ─────────────────────────────────────────────────────────────

  val requestBytes: scala.Array[Byte] =
    "GET /bench HTTP/1.1\r\nHost: localhost\r\nAccept: text/plain\r\nUser-Agent: bench\r\n\r\n"
    . getBytes("US-ASCII").nn

  private val contentLengthHeader: scala.Array[Byte] = "content-length:".getBytes("US-ASCII").nn

  final class Connection(val owner: Thread, val socket: java.net.Socket):
    val out: java.io.OutputStream = socket.getOutputStream.nn
    val in: java.io.BufferedInputStream =
      new java.io.BufferedInputStream(socket.getInputStream.nn, 8192)
    val line: scala.Array[Byte] = new scala.Array[Byte](1024)

  // Each stress window spawns fresh worker threads, so a plain `ThreadLocal` would
  // strand its sockets when a window's threads die; the registry is swept of
  // dead-owner connections on every connect, bounding open descriptors to about two
  // windows' worth.
  private val registry = new java.util.concurrent.ConcurrentLinkedQueue[Connection]()
  private val local = new java.lang.ThreadLocal[Connection]()

  private def connect(port: Int): Connection =
    registry.removeIf: connection =>
      if connection.owner.isAlive then false else
        try connection.socket.close() catch case _: java.io.IOException => ()
        true

    var connection: Connection | Null = null
    var attempts = 0

    while connection == null do
      try
        val socket = new java.net.Socket()
        socket.connect(new java.net.InetSocketAddress("localhost", port), 1000)
        socket.setTcpNoDelay(true)
        connection = new Connection(Thread.currentThread.nn, socket)
      catch case _: java.io.IOException =>
        attempts += 1
        if attempts > 40
        then throw new java.lang.IllegalStateException(s"cannot connect to port $port")
        Thread.sleep(25)

    val result = connection.nn
    local.set(result)
    registry.add(result)
    result

  // One operation: write the request, read the response. A worker's first operation
  // (per window) additionally pays for its connect, one inflated sample per worker
  // per window; a broken connection is re-established once.
  def roundtrip(port: Int): Int =
    val connection = local.get() match
      case null             => connect(port)
      case held: Connection => held

    try exchange(connection) catch case _: java.io.IOException =>
      try connection.socket.close() catch case _: java.io.IOException => ()
      exchange(connect(port))

  private def exchange(connection: Connection): Int =
    connection.out.write(requestBytes)
    connection.out.flush()

    var contentLength = -1
    var headerEnd = false

    while !headerEnd do
      val length = readLine(connection)
      if length == 0 then headerEnd = true
      else if contentLength < 0 then contentLength = contentLengthOf(connection.line, length)

    if contentLength < 0
    then throw new java.io.IOException("response without Content-Length")

    var remaining: Long = contentLength

    while remaining > 0 do
      val skipped = connection.in.skip(remaining)

      if skipped > 0 then remaining -= skipped
      else if connection.in.read() < 0
      then throw new java.io.IOException("truncated response body")
      else remaining -= 1

    contentLength

  // Read one CRLF-terminated line into the connection's scratch buffer, returning
  // its length without the terminator; zero is the blank line ending the headers.
  private def readLine(connection: Connection): Int =
    var index = 0
    var byte = connection.in.read()

    while byte >= 0 && byte != '\n' do
      if byte != '\r' && index < connection.line.length then
        connection.line(index) = byte.toByte
        index += 1
      byte = connection.in.read()

    if byte < 0 then throw new java.io.IOException("connection closed mid-response")
    index

  // The line's `Content-Length` value, or -1 if it is some other header;
  // case-insensitive, allocation-free.
  private def contentLengthOf(line: scala.Array[Byte], length: Int): Int =
    var matches = length > contentLengthHeader.length
    var index = 0

    while matches && index < contentLengthHeader.length do
      val byte = line(index)
      val lower = if byte >= 'A' && byte <= 'Z' then (byte + 32).toByte else byte
      if lower != contentLengthHeader(index) then matches = false
      index += 1

    if !matches then -1 else
      var value = 0

      while index < length do
        val byte = line(index)
        if byte >= '0' && byte <= '9' then value = value*10 + (byte - '0')
        index += 1

      value

  // ── The servers ────────────────────────────────────────────────────────────

  // Counted down never: parked on to hold a server's `supervise` scope (and so its
  // Monitor and daemons) open for the lifetime of the measurement JVM.
  private val forever = new java.util.concurrent.CountDownLatch(1)

  private def awaitReady(port: Int): Unit =
    var attempts = 0
    var ready = false

    while !ready do
      try
        val socket = new java.net.Socket("localhost", port)
        socket.close()
        ready = true
      catch case _: java.io.IOException =>
        attempts += 1
        if attempts > 200
        then throw new java.lang.IllegalStateException(s"server on port $port never became ready")
        Thread.sleep(25)

  val okHandler: Http.Connection ?=> Http.Response = Http.Response(Http.Ok)(t"Hello, World!")

  // The `Threading` in force here selects the kind of thread `SocketServer`'s
  // per-connection daemons run on — independent of the harness workers' threading.
  // The launcher thread is virtual, hence a daemon: it never obstructs JVM exit.
  private def startScintillate(port: Int)(using Threading): Unit =
    Thread.ofVirtual.nn.start: () =>
      supervise:
        val service = SocketServer(port).handle(okHandler)
        forever.await()
        service.cancel()

    awaitReady(port)

  lazy val scintillateVirtual: Unit =
    startScintillate(scintillateVirtualPort)(using threading.virtualThreading)

  lazy val scintillatePlatform: Unit =
    startScintillate(scintillatePlatformPort)(using threading.platformThreading)

  // Ember runs on the global `IORuntime`, exactly as its own users run it; the
  // `Resource` finalizer is deliberately discarded, since JVM exit is teardown.
  lazy val emberServer: Unit =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    import org.http4s.implicits.*
    import org.http4s.dsl.io.*
    import com.comcast.ip4s.*

    val app = org.http4s.HttpRoutes.of[IO]:
      case GET -> Root / "bench" => Ok("Hello, World!")

    val server =
      org.http4s.ember.server.EmberServerBuilder.default[IO]
      . withHost(host"127.0.0.1")
      . withPort(port"18082")
      . withHttpApp(app.orNotFound)
      . build.allocated.unsafeRunSync()

    awaitReady(emberPort)

  // zio-http's Netty event-loop threads are non-daemon (ZIO's own `ZScheduler` workers
  // are daemon), so once the measurement JVM's `main` returns, the process would never
  // exit and the harness would wait on it forever. The watchdog joins `main` from a
  // daemon thread and halts the JVM the moment it finishes — by which point the
  // results are already on stdout.
  private def haltWhenMainExits(): Unit =
    Thread.getAllStackTraces.nn.keySet.nn.forEach: thread =>
      if thread.getName == "main" then
        Thread.ofVirtual.nn.start: () =>
          thread.join()
          java.lang.Runtime.getRuntime.nn.halt(0)

  // zio-http on the default ZIO runtime, forked as a fiber; like ember, torn down
  // only by JVM exit (which the watchdog above must force past Netty's non-daemon
  // event loops).
  lazy val zioServer: Unit =
    import zio.http.*

    haltWhenMainExits()

    val routes = Routes(Method.GET / "bench" -> handler(Response.text("Hello, World!")))
    val program = Server.serve(routes).provide(Server.defaultWithPort(zioPort))

    zio.Unsafe.unsafe: (unsafe: zio.Unsafe) ?=>
      zio.Runtime.default.unsafe.fork(program)

    awaitReady(zioPort)

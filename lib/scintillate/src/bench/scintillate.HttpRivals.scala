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

// The real-socket HTTP comparison: scintillate's servers against the JVM's mainstream
// (raw Netty, Undertow, Jetty), its virtual-thread servers (Helidon SE, the JDK's own
// `HttpServer`) and the Scala ecosystems' (http4s ember, zio-http, Pekko HTTP, Cask,
// Vert.x), with an in-process load generator. One stress operation is one
// request/response round-trip on a per-worker persistent keep-alive socket, so the
// harness's latency histogram and capacity search read directly as per-request
// latency and sustained requests per second.
//
// Every server answers the five `HttpWorkload`s on its own port, written the way its
// own users would write it: default runtime, keep-alive, logging silenced, and the JSON
// library of its own ecosystem (circe for http4s, zio-json for zio-http, spray-json for
// Pekko, upickle for Cask, jacinta for scintillate, Jackson for the Java servers). The
// JSON row is therefore a comparison of stacks, not of servers alone — deliberately so,
// since that is what a user of each stack would pay. Before a server's first row is
// measured, `verify` sends each workload once and compares the whole response body
// byte-for-byte with the expected one, so a server that answers wrongly fails the row
// rather than skewing it.
//
// Server and clients share the measurement JVM: the client work steals server CPU,
// but steals it identically for every server, so the figures are relative — a
// two-machine harness would report higher absolute rates. Each stress row runs in
// its own measurement JVM, so each server lives in a `lazy val` forced by the
// warmup (outside every timed window) and is torn down by JVM exit: no stop logic,
// no cross-row port conflicts, and never two servers alive at once.
//
// The client is deliberately dumber than any HTTP client library (which would be a
// further variable): it writes pre-serialized request bytes and scans the response
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
  // `Content-Length`, a wrong body) by throwing: a failed operation should crash the row,
  // not skew it.
  import unsafeExceptions.canThrowAny

  // Server identifiers; each server listens on `port(id)`.
  val Reactor: Int = 0
  val SocketServerVirtual: Int = 1
  val SocketServerPlatform: Int = 2
  val Ember: Int = 3
  val ZioHttp: Int = 4
  val Netty: Int = 5
  val Undertow: Int = 6
  val Jetty: Int = 7
  val Helidon: Int = 8
  val JdkHttpServer: Int = 9
  val PekkoHttp: Int = 10
  val Cask: Int = 11
  val Vertx: Int = 12

  def port(server: Int): Int = 18080 + server

  // Force the server's `lazy val`: the first call in a measurement JVM starts and verifies
  // it (during warmup); every later call is a field read.
  def ensure(server: Int): Unit =
    watchdog

    server match
      case Reactor              => RivalScintillate.reactor
      case SocketServerVirtual  => RivalScintillate.virtual
      case SocketServerPlatform => RivalScintillate.platform
      case Ember                => RivalEmber.server
      case ZioHttp              => RivalZio.server
      case Netty                => RivalNetty.server
      case Undertow             => RivalUndertow.server
      case Jetty                => RivalJetty.server
      case Helidon              => RivalHelidon.server
      case JdkHttpServer        => RivalJdk.server
      case PekkoHttp            => RivalPekko.server
      case Cask                 => RivalCask.server
      case Vertx                => RivalVertx.server

      case _ =>
        throw new java.lang.IllegalArgumentException(s"no server numbered $server")

  // ── The client ─────────────────────────────────────────────────────────────

  private val contentLengthHeader: scala.Array[Byte] = "content-length:".getBytes("US-ASCII").nn

  final class Connection(val owner: Thread, val socket: java.net.Socket):
    val out: java.io.OutputStream = socket.getOutputStream.nn
    val in: java.io.BufferedInputStream =
      new java.io.BufferedInputStream(socket.getInputStream.nn, 65536)
    val line: scala.Array[Byte] = new scala.Array[Byte](1024)

  // Each stress window spawns fresh worker threads, so a plain `ThreadLocal` would
  // strand its sockets when a window's threads die; the registry is swept of
  // dead-owner connections on every connect, bounding open descriptors to about two
  // windows' worth.
  private val registry = new java.util.concurrent.ConcurrentLinkedQueue[Connection]()
  private val local = new java.lang.ThreadLocal[Connection]()

  private def open(port: Int): Connection =
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

    connection.nn

  private def connect(port: Int): Connection =
    registry.removeIf: connection =>
      if connection.owner.isAlive then false else
        try connection.socket.close() catch case _: java.io.IOException => ()
        true

    val result = open(port)
    local.set(result)
    registry.add(result)
    result

  // One operation: write the workload's request, read the response. A worker's first
  // operation (per window) additionally pays for its connect, one inflated sample per
  // worker per window; a broken connection is re-established once.
  def roundtrip(server: Int, workload: Int): Int =
    val connection = local.get() match
      case null             => connect(port(server))
      case held: Connection => held

    try exchange(connection, workload, null) catch case _: java.io.IOException =>
      try connection.socket.close() catch case _: java.io.IOException => ()
      exchange(connect(port(server)), workload, null)

  // Send every workload once over a fresh connection and compare each body in full with
  // the expected one.
  def verify(server: Int): Unit =
    val connection = open(port(server))

    try
      var workload = 0

      while workload < HttpWorkload.count do
        val expected = HttpWorkload.expected(workload)
        val body = new scala.Array[Byte](expected.length.max(1024))
        val length = exchange(connection, workload, body)

        if length != expected.length
            || !java.util.Arrays.equals(body, 0, length, expected, 0, length)
        then
          val name = HttpWorkload.names(workload)
          val text = if length > 1024 then "" else String(body, 0, length, "UTF-8")
          throw new java.lang.IllegalStateException
            ( s"server $server answered $name with a wrong $length-byte body: $text" )

        workload += 1

    finally connection.socket.close()

  // Read the response; its body is skipped, or copied into `capture` when one is given and
  // the body fits.
  private def exchange(connection: Connection, workload: Int, capture: scala.Array[Byte] | Null)
  :   Int =

    connection.out.write(HttpWorkload.requests(workload))
    connection.out.flush()

    var contentLength = -1
    var headerEnd = false

    while !headerEnd do
      val length = readLine(connection)
      if length == 0 then headerEnd = true
      else if contentLength < 0 then contentLength = contentLengthOf(connection.line, length)

    if contentLength < 0
    then throw new java.io.IOException("response without Content-Length")

    if capture != null && contentLength <= capture.length
    then connection.in.readNBytes(capture, 0, contentLength)
    else
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

  // ── Server plumbing ────────────────────────────────────────────────────────

  // Counted down never: parked on to hold a server's scope open for the lifetime of the
  // measurement JVM.
  val forever = new java.util.concurrent.CountDownLatch(1)

  // Wait for the server to accept, then check its answers.
  def ready(server: Int): Unit =
    var attempts = 0
    var accepting = false

    while !accepting do
      try
        val socket = new java.net.Socket("localhost", port(server))
        socket.close()
        accepting = true
      catch case _: java.io.IOException =>
        attempts += 1
        if attempts > 400
        then throw new java.lang.IllegalStateException(s"server $server never became ready")
        Thread.sleep(25)

    verify(server)

  // Several rivals (zio-http's and Vert.x's Netty event loops, Pekko's dispatchers,
  // Undertow's XNIO workers) run non-daemon threads, so once the measurement JVM's `main`
  // returns, the process would never exit and the harness would wait on it forever. The
  // watchdog joins `main` from a daemon thread and halts the JVM the moment it finishes —
  // by which point the results are already on stdout.
  private lazy val watchdog: Unit =
    Thread.getAllStackTraces.nn.keySet.nn.forEach: thread =>
      if thread.getName == "main" then
        Thread.ofVirtual.nn.start: () =>
          thread.join()
          java.lang.Runtime.getRuntime.nn.halt(0)

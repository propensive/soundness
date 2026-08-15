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

import java.net as jn
import java.nio as jnio
import java.nio.channels as jnc
import java.util.concurrent as juc

import anticipation.*
import contingency.*, strategies.throwUnsafely
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

// The event-loop front-end: a fixed fleet of selector lanes, each a long-lived platform
// thread owning a `Selector` and every connection registered with it. Where the
// thread-per-connection front-end spawns a daemon per accepted socket — paying a
// continuation mount and unmount per request, and forcing every capability across the
// thread boundary through `AnyRef` rims — a lane services a *batch* of ready connections
// per `select()` wake-up on one hot thread, with no per-request thread transition at all.
//
// A request is accumulated until its head (and any fixed body within the inline limit)
// is complete, then parsed with the ordinary `Http.Request.parseHead` over an in-memory
// cursor that provably cannot block, and its handler runs *inline on the lane*: the
// reactive front-end's contract is a non-blocking handler. A response of known extent
// coalesces into a single channel write. Requests the fast path cannot serve inline —
// chunked or oversized bodies, `Expect: 100-continue`, protocol upgrades — leave the
// reactor for the thread-per-connection path (the fallback hand-off; TLS listeners
// bypass the reactor wholesale, taking ALPN-negotiated h2 with them).
//
// Separation design, after `zephyrine.Conduit` (which passes the checker with no
// `unsafeAssume*`): `Connection` is the one tracked capability — exclusive, stateful,
// every mutator an `update def` — minted by its lane, attached to its `SelectionKey`,
// and only ever touched by that lane's thread. The lanes, mailboxes and the reactor
// itself stay deliberately plain: their only mutable state is untracked JDK objects
// (selectors, channels, queues, an `AtomicBoolean`), and classifying them would force
// capture sets through arrays and thread lambdas for no proof value. The single audited
// cast is the attachment recovery at the top of each readiness event
// (`SelectionKey.attachment` is `AnyRef` by JDK signature): the exclusivity it
// re-asserts is structural — no reference to the connection ever leaves its lane.
//
// The lane threads are *platform* threads, created directly rather than through the
// ambient `Threading` given: `Selector.select()` is not a Loom-rewired blocking point,
// so a virtual thread would pin its carrier for the duration of every select.
object Reactor:
  // A fixed body up to this size is accumulated and served inline; anything larger
  // takes the fallback path. Matches `SocketServer`'s `drainLimit`.
  private val inlineBodyLimit: Int = 65536

  // The head must complete within the request-line and header caps that
  // `Http.Request.parseHead` enforces once parsing begins; until the terminator has
  // been seen, this bound stops a trickling client growing the accumulator unbounded.
  private val headLimit: Int = 8192 + 65536

  private val closeHeader: Http.Header = Http.Header(t"connection", t"close")

  // One registration queue per lane: a channel accepted by the boss loop is posted here
  // and registered by the owning lane on its next wake-up (`SocketChannel.register`
  // blocks against an in-progress `select()` on another thread). Deliberately a plain
  // class over an untracked JDK queue, not a `SharedCapability`: its safety argument is
  // the queue's JMM publication (as `Conduit`'s hand-off), and it imposes no capture
  // obligation, so `Lane` can live in a plain array.
  private final class Mailbox:
    private val queue: juc.ConcurrentLinkedQueue[AnyRef] = juc.ConcurrentLinkedQueue()

    def post(item: AnyRef): Unit = queue.add(item)
    def drain(): AnyRef | Null = queue.poll()

  // Per-connection state, confined to its owning lane: the one class here whose
  // confinement the checker is asked to prove. Exclusive and stateful, like the
  // zephyrine kernel's stage types; the lane is the only thread that ever calls these
  // members, so the exclusivity is genuinely single-owner.
  private final class Connection(val channel: jnc.SocketChannel, val key: jnc.SelectionKey)
  extends scala.caps.ExclusiveCapability, scala.caps.Stateful:

    // The in-flight request bytes: everything read but not yet consumed. `end` bounds
    // the valid bytes; `scanned` is the end-of-head scan's progress (so arriving bytes
    // are scanned once); `headEnd` is the offset just past CRLFCRLF once seen;
    // `pendingHead` and `needed` hold the parsed head and the request's total extent
    // while a fixed body is still arriving. Plain data throughout — the parsed `Head`
    // is an untracked case class — so no capability is ever stored in a field.
    private var accumulator: scala.Array[Byte] = new scala.Array[Byte](1024)
    private var end: Int = 0
    private var scanned: Int = 0
    private var headEnd: Int = -1
    private var pendingHead: Http.Request.Head | Null = null
    private var needed: Int = -1
    private var contentLength: Int = 0
    private var keep: Boolean = true
    private var closing: Boolean = false

    // Serialized responses awaiting the channel: normally drained immediately after
    // serving; a partial write leaves the remainder here with `OP_WRITE` armed.
    private val outbound: java.util.ArrayDeque[jnio.ByteBuffer] = java.util.ArrayDeque()

    private update def ensure(capacity: Int): Unit =
      if accumulator.length < capacity then
        var size = accumulator.length*2
        while size < capacity do size *= 2
        val grown = new scala.Array[Byte](size)
        java.lang.System.arraycopy(accumulator, 0, grown, 0, end)
        accumulator = grown

    update def readable(buffer: jnio.ByteBuffer, reactor: Reactor^): Unit =
      buffer.clear()

      val count = try channel.read(buffer) catch case _: java.io.IOException => -1

      if count < 0 then
        // Half-close: everything complete was served as it arrived, so any remainder
        // is an unfinishable fragment. Let queued responses drain, then close.
        closing = true
        if outbound.isEmpty then close()
      else if count > 0 then
        buffer.flip()
        ensure(end + count)
        buffer.get(accumulator, end, count)
        end += count
        process(reactor)

    // Serve every complete request in the accumulator; pipelined requests are served
    // back-to-back, their responses queueing in order.
    private update def process(reactor: Reactor^): Unit =
      var continue = !closing

      while continue do
        continue = false

        if headEnd < 0 then
          scanHead()

          if headEnd < 0 && end > headLimit then
            refuse(Http.RequestHeaderFieldsTooLarge)

        if headEnd >= 0 && pendingHead == null && !closing then parseHead(reactor)

        val head = pendingHead

        if head != null && !closing && end >= needed then
          serve(head, reactor)

          // Shift any pipelined remainder to the front and reset per-request state.
          val remainder = end - needed
          if remainder > 0
          then java.lang.System.arraycopy(accumulator, needed, accumulator, 0, remainder)
          end = remainder
          scanned = 0
          headEnd = -1
          pendingHead = null
          needed = -1

          if keep then continue = remainder > 0 else closing = true

    // Scan newly-arrived bytes for the CRLFCRLF head terminator, resuming where the
    // previous scan stopped (backing up three bytes in case the terminator straddles
    // two reads).
    private update def scanHead(): Unit =
      var index = if scanned > 3 then scanned - 3 else 0

      while headEnd < 0 && index + 3 < end do
        if accumulator(index) == 13 && accumulator(index + 1) == 10
            && accumulator(index + 2) == 13 && accumulator(index + 3) == 10
        then headEnd = index + 4
        else index += 1

      scanned = end

    // Parse the completed head with the ordinary parser over a preset in-memory cursor,
    // which provably cannot block: a preset cursor is `static` — its `refill` never
    // pulls. Requests the fast path cannot serve inline leave for the fallback.
    private update def parseHead(reactor: Reactor^): Unit =
      // A `Data` is a frozen byte array; the cast freezes the freshly-copied range.
      val headData: Data =
        java.util.Arrays.copyOfRange(accumulator, 0, headEnd).nn.asInstanceOf[Data]

      recover:
        case error: Http.Request.Error =>
          refuse(SocketServer.errorStatus(error.reason))

      . protect:
          val cursor = Cursor[Data](headData)
          val head = Http.Request.parseHead(cursor)
          val facts = SocketServer.factsOf(head)

          if facts.chunked || SocketServer.expectsContinue(head, facts)
              || SocketServer.isUpgrade(facts)
              || facts.contentLength.or(0) > inlineBodyLimit
          then
            // TODO(fallback): hand the channel and buffered bytes to the
            // thread-per-connection path. Until then, refuse politely.
            refuse(Http.NotImplemented)
          else
            keep = SocketServer.keepAlive(head, facts)
            contentLength = facts.contentLength.or(0)
            needed = headEnd + contentLength
            pendingHead = head

    // Serve one complete request inline: mint the body stream over the accumulated
    // bytes (zero-copy for the empty case; one bounded copy for a fixed body), run the
    // handler, and queue the serialized response.
    private update def serve(head: Http.Request.Head, reactor: Reactor^): Unit =
      val bodyRef: AnyRef =
        if contentLength == 0 then Http.emptyBody().asInstanceOf[AnyRef]
        else
          val bodyData: Data =
            java.util.Arrays.copyOfRange(accumulator, headEnd, needed).nn.asInstanceOf[Data]

          bodyData.stream.asInstanceOf[AnyRef]

      val keep0 = keep

      val respond: Http.Connection.Respond^{this} = new Http.Connection.Respond:
        def apply(response: Http.Response^)(using Tactic[StreamError]): Unit =
          val response2 = if keep0 then response else response + closeHeader
          // `memoize` forces the whole serialized response, including a `Flowing`
          // body, to completion on the lane: the reactive front-end's non-blocking
          // handler contract. The result is one buffer, hence one channel write.
          val bytes: Data =
            Http.Response.serialize(response2, head.method != Http.Head, head.version)
            . memoize

          enqueue(bytes)

      val request =
        Http.Request
          ( head.method,
            head.version,
            head.host,
            head.target,
            head.headers,
            () => bodyRef.asInstanceOf[Stream[Data] over Credit] )

      val connection = new Http.Connection(request, false, reactor.port, respond)

      // The handler's `Response^{connection}` is delivered over the same single-owner
      // connection it captures — self-aliasing by design, which the checker reads as a
      // clash. The one rim in this file, inherited from the `Respond` API shape; a
      // `consume`-typed `respond` (the planned follow-up) retires it.
      scala.caps.unsafe.unsafeAssumeSeparate:
        connection.respond:
          try reactor.invoke(connection)
          catch case throwable: Throwable => reactor.errors.handle(throwable, connection)

    // Queue an error response and close once it drains.
    private update def refuse(status: Http.Status): Unit =
      val response = Http.Response(status)() + closeHeader
      enqueue(Http.Response.serialize(response).memoize)
      closing = true

    private update def enqueue(bytes: Data): Unit =
      outbound.add(jnio.ByteBuffer.wrap(bytes.asInstanceOf[scala.Array[Byte]]).nn)
      write()

    update def writable(): Unit = write()

    private update def write(): Unit =
      var blocked = false

      try
        while !blocked && !outbound.isEmpty do
          val pending = outbound.peek.nn
          channel.write(pending)
          if pending.hasRemaining then blocked = true else outbound.poll()
      catch case _: java.io.IOException =>
        outbound.clear()
        close()

      if key.isValid then
        if blocked
        then key.interestOps(jnc.SelectionKey.OP_READ | jnc.SelectionKey.OP_WRITE)
        else
          key.interestOps(jnc.SelectionKey.OP_READ)
          if closing then close()

    update def close(): Unit =
      key.cancel()
      try channel.close() catch case _: java.io.IOException => ()

  // One selector lane: a `Selector`, its registration mailbox, and a reusable read
  // buffer. The buffer is lane-confined, so one per lane suffices.
  private final class Lane(val selector: jnc.Selector):
    val mailbox: Mailbox = Mailbox()
    val buffer: jnio.ByteBuffer = jnio.ByteBuffer.allocateDirect(65536).nn

    // Hand a freshly-accepted channel to this lane and wake its selector.
    def adopt(channel: jnc.SocketChannel): Unit =
      mailbox.post(channel)
      selector.wakeup()

    def register(): Unit =
      var item = mailbox.drain()

      while item != null do
        val channel = item.asInstanceOf[jnc.SocketChannel]

        try
          channel.configureBlocking(false)
          channel.socket.nn.setTcpNoDelay(true)
          val key = channel.register(selector, jnc.SelectionKey.OP_READ).nn
          key.attach(Connection(channel, key))
        catch case _: java.io.IOException => try channel.close() catch case _: Exception => ()

        item = mailbox.drain()

    def iterate(reactor: Reactor^): Unit =
      selector.select()
      register()

      val ready = selector.selectedKeys.nn
      val iterator = ready.iterator.nn

      while iterator.hasNext do
        val key = iterator.next().nn
        iterator.remove()

        if key.isValid then
          // The audited attachment recovery: `attachment` is `AnyRef` by JDK signature;
          // the connection was attached by this lane and is only ever recovered here.
          val connection = key.attachment.asInstanceOf[Connection^]

          if key.isReadable then connection.readable(buffer, reactor)
          if key.isValid && key.isWritable then connection.writable()

// The reactor itself: `loops` selector lanes plus a boss accept thread, started eagerly
// at construction; `stop()` closes the listener, wakes every lane, and joins the
// threads. Serves cleartext HTTP/1.1 with the handler run inline on the lane.
final class Reactor
  ( val port: Int, local: Boolean = true, loops: Int = 0 )
  ( handler: (connection: Http.Connection) ?=> Http.Response^{connection} )
  ( using errorPage: WebserverErrorPage ):

  import Reactor.*

  private[Reactor] def invoke(connection: Http.Connection): Http.Response^{connection} =
    handler(using connection)

  private[Reactor] def errors: WebserverErrorPage = errorPage

  private val count: Int =
    if loops > 0 then loops else Runtime.getRuntime.nn.availableProcessors

  // An untracked JDK atomic rather than a `var` needing a `Stateful` classification:
  // the flag is read by every lane thread and written once by `stop()`.
  private val running: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(true)

  private val listener: jnc.ServerSocketChannel =
    val channel = jnc.ServerSocketChannel.open().nn
    channel.configureBlocking(true)
    val address = jn.InetAddress.getByName(if local then "localhost" else "0.0.0.0").nn
    channel.bind(jn.InetSocketAddress(address, port), 128)
    channel

  private val fleet: scala.IArray[Lane] =
    scala.IArray.tabulate(count): index =>
      Lane(jnc.Selector.open().nn)

  private val threads: scala.IArray[Thread] =
    scala.IArray.tabulate(count): index =>
      val lane = fleet(index)

      Thread.ofPlatform.nn.name(s"scintillate-lane-$index").nn.start: () =>
        while running.get do lane.iterate(this)
      . nn

  // The boss thread: a blocking accept loop distributing connections round-robin. A
  // platform thread too: it spends its life in `accept()`.
  private val boss: Thread =
    Thread.ofPlatform.nn.name("scintillate-accept").nn.start: () =>
      var next = 0

      while running.get do
        try
          val channel = listener.accept().nn
          fleet(next).adopt(channel)
          next = (next + 1)%count
        catch case _: java.io.IOException => ()
    . nn

  def stop(): Unit =
    running.set(false)
    try listener.close() catch case _: java.io.IOException => ()

    var index = 0
    while index < count do
      fleet(index).selector.wakeup()
      index += 1

    boss.join()
    index = 0
    while index < count do
      threads(index).join()
      index += 1

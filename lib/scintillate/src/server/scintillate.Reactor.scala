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
import proscenium.*
import rudiments.*
import vacuous.*

// The event-loop front-end: a fixed fleet of selector loops, each a long-lived platform
// thread owning a `Selector` and every connection registered with it. Where the
// thread-per-connection front-end spawns a daemon per accepted socket — paying a
// continuation mount and unmount per request, and forcing every capability across the
// thread boundary through `AnyRef` rims — a loop services a *batch* of ready connections
// per `select()` wake-up on one hot thread, with no per-request thread transition at all.
//
// Separation design, after `zephyrine.Conduit` (which passes the checker with no
// `unsafeAssume*`): the only shared structure is each loop's `Mailbox`, a
// `SharedCapability`-classified queue whose safety argument is JMM publication (the
// carrier type is `AnyRef`; capture sets are erased crossing it, exactly as `Conduit`'s
// blocks cross its hand-off). Everything else is confined: a `Connection` is minted by
// its loop, attached to its `SelectionKey`, and only ever touched by that loop's thread.
// The single audited cast is the attachment recovery at the top of each readiness event
// (`SelectionKey.attachment` is `AnyRef` by JDK signature): the exclusivity it re-asserts
// is structural — no reference to the connection ever leaves its loop.
//
// The loop threads are *platform* threads, created directly rather than through the
// ambient `Threading` given: `Selector.select()` is not a Loom-rewired blocking point, so
// a virtual thread would pin its carrier for the duration of every select.
object Reactor:

  // One registration queue per lane: a channel accepted by the boss loop is posted here
  // and registered by the owning loop on its next wake-up (`SocketChannel.register`
  // blocks against an in-progress `select()` on another thread). Deliberately a plain
  // class over an untracked JDK queue, not a `SharedCapability`: its safety argument is
  // the queue's JMM publication (as `Conduit`'s hand-off), and it imposes no capture
  // obligation, so `Lane` can live in a plain array.
  private final class Mailbox:
    private val queue: juc.ConcurrentLinkedQueue[AnyRef] = juc.ConcurrentLinkedQueue()

    def post(item: AnyRef): Unit = queue.add(item)
    def drain(): AnyRef | Null = queue.poll()

  // Per-connection state, confined to its owning loop: the one class here whose
  // confinement the checker is asked to prove. Exclusive and stateful, like the
  // zephyrine kernel's stage types; the loop is the only thread that ever calls these
  // members, so the exclusivity is genuinely single-owner. (`Lane` and `Reactor`, by
  // contrast, are deliberately plain classes: their only mutable state is untracked JDK
  // objects — selectors, channels, an `AtomicBoolean` — and classifying them would force
  // capture sets through arrays and thread lambdas for no proof value.) Echo-level for
  // now: the HTTP accumulator and outbound queue arrive with the fast path.
  private final class Connection(val channel: jnc.SocketChannel, val key: jnc.SelectionKey)
  extends scala.caps.ExclusiveCapability, scala.caps.Stateful:

    // Pending outbound bytes: written when the channel signals writability. Echo-level
    // placeholder for the write queue; replaced by the response queue in the fast path.
    private var outbound: jnio.ByteBuffer | Null = null

    update def readable(buffer: jnio.ByteBuffer): Unit =
      buffer.clear()

      val count = try channel.read(buffer) catch case _: java.io.IOException => -1

      if count < 0 then close()
      else if count > 0 then
        buffer.flip()
        val copy = jnio.ByteBuffer.allocate(buffer.remaining).nn
        copy.put(buffer)
        copy.flip()
        outbound = copy
        write()

    update def writable(): Unit = write()

    private update def write(): Unit =
      val pending = outbound

      if pending != null then
        try
          channel.write(pending)

          if pending.hasRemaining
          then key.interestOps(key.interestOps | jnc.SelectionKey.OP_WRITE)
          else
            outbound = null
            key.interestOps(jnc.SelectionKey.OP_READ)
        catch case _: java.io.IOException => close()

    update def close(): Unit =
      key.cancel()
      try channel.close() catch case _: java.io.IOException => ()

  // One selector loop: a `Selector`, its registration mailbox, and a reusable read
  // buffer. The buffer is loop-confined, so one per loop suffices for now; the pooled
  // `Freelist`/`Blockpool` read buffers arrive with the fast path.
  private final class Lane(val selector: jnc.Selector):
    val mailbox: Mailbox = Mailbox()
    val buffer: jnio.ByteBuffer = jnio.ByteBuffer.allocateDirect(65536).nn

    // Hand a freshly-accepted channel to this loop and wake its selector.
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

    def iterate(): Unit =
      selector.select()
      register()

      val ready = selector.selectedKeys.nn
      val iterator = ready.iterator.nn

      while iterator.hasNext do
        val key = iterator.next().nn
        iterator.remove()

        if key.isValid then
          // The audited attachment recovery: `attachment` is `AnyRef` by JDK signature;
          // the connection was attached by this loop and is only ever recovered here.
          val connection = key.attachment.asInstanceOf[Connection^]

          if key.isReadable then connection.readable(buffer)
          if key.isValid && key.isWritable then connection.writable()

// The reactor itself: `loops` selector loops plus a boss accept loop, started eagerly at
// construction; `stop()` closes the listener, wakes every loop, and joins the threads.
// Echo-level protocol for the skeleton: bytes read are written straight back.
final class Reactor(port: Int, local: Boolean = true, loops: Int = 0):
  import Reactor.*

  private val count: Int =
    if loops > 0 then loops else Runtime.getRuntime.nn.availableProcessors

  // An untracked JDK atomic rather than a `var` needing a `Stateful` classification:
  // the flag is read by every loop thread and written once by `stop()`.
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
      val loop = fleet(index)

      Thread.ofPlatform.nn.name(s"scintillate-loop-$index").nn.start: () =>
        while running.get do loop.iterate()
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

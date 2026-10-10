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
package ethereal

import scala.caps
import scala.unsafeExceptions.canThrowAny

import java.io as ji
import java.lang as jl
import java.nio.file as jnf
import java.util as ju
import scala.collection.concurrent as scc

import anticipation.*
import coaxial.*
import contingency.*
import digression.*
import distillate.*
import eucalyptus.*
import turbulence.*
import zephyrine.*
import fulminate.*
import galilei.*
import gossamer.*
import prepositional.*
import profanity.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.emptyDiagnostics
import galilei.Io.Error.Reason

object Session:
  // The credit each stream opens with, in each direction, and the most bytes one `data`
  // document carries: the launcher's figures (xek's `spec/launcher.md`, *Flow control*).
  val window: Long = 65536L
  val chunk: Int = Launcher.maximumChunk

  // Which of the client's descriptors a path names, where it names one: the forms by which a
  // process refers to its own descriptor table. Any such path is the table's to answer
  // for, whether or not the client holds the descriptor, so that the daemon's own table is
  // never reached through it.
  def governed(path: Text): Optional[Int] = path.s match
    case "/dev/stdin"  => 0
    case "/dev/stdout" => 1
    case "/dev/stderr" => 2

    case other =>
      val prefix =
        if other.startsWith("/dev/fd/") then "/dev/fd/"
        else if other.startsWith("/proc/self/fd/") then "/proc/self/fd/"
        else ""

      if prefix.isEmpty then Unset else
        val number = other.substring(prefix.length).nn

        if number.nonEmpty && number.forall(_.isDigit) && number.length < 8 then number.toInt
        else Unset

  // A daemon thread, named.
  private[ethereal] def thread(name: Text)(body: -> Unit): jl.Thread =
    val runnable: jl.Runnable = () => body
    jl.Thread.ofPlatform().nn.daemon(true).nn.name(name.s).nn.start(runnable).nn

// The chunks of one stream the launcher sends, between the session's reader thread and
// whatever reads the invocation's stdin, or a descriptor opened to read. Bounded by the
// credit this side has granted, which is only ever as much as it has consumed; `credit` says
// so to the launcher, in halves of the window rather than per chunk, to halve the chatter.
class SessionInput(stream: Text, credit: Long -> Unit) extends ji.InputStream:
  private val chunks: ju.ArrayDeque[Data] = ju.ArrayDeque()
  // [synchronized] offset var in InputStream subclass, not Stateful
  @caps.unsafe.untrackedCaptures private var offset: Int = 0
  // [synchronized] ended flag in InputStream subclass, not Stateful
  @caps.unsafe.untrackedCaptures private var ended: Boolean = false
  // [synchronized] consumed counter in InputStream subclass, not Stateful
  @caps.unsafe.untrackedCaptures private var consumed: Long = 0L

  def push(bytes: Data): Unit = synchronized:
    if !ended then chunks.addLast(bytes)
    notifyAll()

  def end(): Unit = synchronized:
    ended = true
    notifyAll()

  def isEnded: Boolean = synchronized(ended)

  override def read(): Int =
    val one: scala.Array[Byte] = new scala.Array[Byte](1)
    if read(one, 0, 1) < 0 then -1 else one(0) & 0xff

  override def read(buffer: scala.Array[Byte] | Null, start: Int, length: Int): Int = synchronized:
    if length == 0 then 0 else
      while chunks.isEmpty && !ended do wait()

      if chunks.isEmpty then -1 else
        val head: Data = chunks.peekFirst().nn
        val count = length.min(head.length - offset)
        jl.System.arraycopy(Array.unsafeJvm(head), offset, buffer, start, count)
        offset += count

        if offset == head.length then
          chunks.pollFirst()
          offset = 0

        consumed += count

        if consumed >= Session.window/2 then
          credit(consumed)
          consumed = 0L

        count

  override def available(): Int = synchronized:
    var total: Int = 0
    val iterator = chunks.iterator().nn
    while iterator.hasNext do total = total + iterator.next().nn.length
    (total - offset).max(0)

// A stream the daemon sends: buffered up to a chunk, and sent on `flush` — which the
// invocation's `PrintStream`s do after every write — or when full, each chunk once
// `allowance` grants it credit. Once the launcher reports the stream closed, or is gone, a
// write raises `Outlet.Error`, as writing a broken pipe would end a process.
class SessionOutput(stream: Text, allowance: Int -> Int, emit: Data -> Unit)
extends ji.OutputStream:
  private val buffer: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream(Session.chunk)
  val severed: Atomic.Bool = Atomic(false)

  override def write(byte: Int): Unit = synchronized:
    buffer.write(byte)
    if buffer.size >= Session.chunk then flush()

  override def write(bytes: scala.Array[Byte] | Null, start: Int, length: Int): Unit = synchronized:
    var position = start
    val end = start + length

    while position < end do
      val space = Session.chunk - buffer.size
      val count = space.min(end - position)
      buffer.write(bytes, position, count)
      position += count
      if buffer.size >= Session.chunk then flush()

  override def flush(): Unit = synchronized:
    if buffer.size > 0 then
      val bytes: scala.Array[Byte] = buffer.toByteArray.nn
      buffer.reset()
      var position = 0

      while position < bytes.length do
        val allowed = allowance(bytes.length - position)

        if allowed == 0 then
          import strategies.throwUnsafely
          severed() = true
          abort(Outlet.Error(stream))

        emit(Array.unsafeFrozen(ju.Arrays.copyOfRange(bytes, position, position + allowed).nn))
        position += allowed

  override def close(): Unit = flush()

// The daemon's half of an invocation's session: one connection, read by one thread and
// written by another, carrying every stream of the invocation as `data` documents under
// per-stream credit, with the control traffic in between. The launcher's half is `session.rs`
// in xek's runner, and the rules both follow are in its `spec/launcher.md`.
//
// The shape that keeps the one connection from blocking itself: the reader never waits on
// anything but the socket — it deposits each chunk with its stream and acts on each control
// document at once, handing a signal to a thread of its own, since a trap may sleep — and the
// writer is the only caller of `write`. A stream's producer waits for credit before it queues
// a chunk, outside any lock, so a stream the client is slow to drain stalls only its own
// producer: `mytool | less`, paused, still receives a signal.
class Session
  ( connection:  Connection,
    in:          ji.InputStream,
    descriptors: List[Launcher.Descriptor],
    dispatch:    Signal -> SignalResponse,
    log:         DaemonLogEvent -> Unit ):

  import Launcher.Message

  private val out: ji.OutputStream = connection.writer

  // ── The writer's queue: control documents go first ───────────────────────────────────────

  private val outbox: Object = Object()
  private val control: ju.ArrayDeque[Data] = ju.ArrayDeque()
  private val data: ju.ArrayDeque[Data] = ju.ArrayDeque()
  // [synchronized] closing flag in non-Stateful Session
  @caps.unsafe.untrackedCaptures private var closing: Boolean = false
  // [synchronized] dead flag in non-Stateful Session
  @caps.unsafe.untrackedCaptures private var dead: Boolean = false

  def send(message: Message): Unit = enqueue(Launcher.encode(message), priority = true)

  private def enqueue(frame: Data, priority: Boolean): Unit = outbox.synchronized:
    if !dead && !closing then
      (if priority then control else data).addLast(frame)
      outbox.notifyAll()

  // Ends the session from this side: `exit-status` behind everything queued before it, then
  // the connection closes once it has been written.
  def finish(code: Int): Unit = outbox.synchronized:
    if !dead && !closing then
      data.addLast(Launcher.encode(Message.ExitStatus(code)))
      closing = true
      outbox.notifyAll()

  private lazy val writer: jl.Thread = Session.thread(t"session-writer"):
    var running = true

    while running do
      val frame: Optional[Data] = outbox.synchronized:
        while control.isEmpty && data.isEmpty && !closing && !dead do outbox.wait()

        if dead then Unset
        else if !control.isEmpty then control.pollFirst().nn
        else if !data.isEmpty then data.pollFirst().nn
        else Unset

      frame match
        case frame: Data =>
          try
            out.write(Array.unsafeJvm(frame))
            out.flush()
          catch case _: ji.IOException =>
            gone()
            running = false

        case _ => running = false

  // Waits for the queue to drain after `finish`, so the exit status reaches the launcher
  // before the connection closes under it.
  def awaitWritten(): Unit = safely(writer.join())

  // ── Credit for the streams the daemon sends ─────────────────────────────────────────────

  private val credits: ju.HashMap[Text, Long] = ju.HashMap()
  // [synchronized] launcherGone flag in non-Stateful Session
  @caps.unsafe.untrackedCaptures private var launcherGone: Boolean = false

  private def openCredit(stream: Text): Unit = credits.synchronized:
    credits.put(stream, Session.window)

  private def grant(stream: Text, bytes: Long): Unit = credits.synchronized:
    if credits.containsKey(stream) then credits.put(stream, credits.get(stream).nn + bytes)
    credits.notifyAll()

  // Waits until the stream has credit, then takes up to `want` of it; zero once the launcher
  // is gone, or the stream is not one this side sends.
  private def allowance(stream: Text, want: Int): Int = credits.synchronized:
    var allowed = 0
    var waiting = true

    while waiting do
      if launcherGone || !credits.containsKey(stream) then waiting = false
      else
        val credit: Long = credits.get(stream).nn

        if credit > 0 then
          allowed = want.min(credit.toInt).min(Session.chunk)
          credits.put(stream, credit - allowed)
          waiting = false
        else
          credits.wait()

    allowed

  // ── The streams ──────────────────────────────────────────────────────────────────────────

  private val inputs: scc.TrieMap[Text, SessionInput] = scc.TrieMap()
  private val outputs: scc.TrieMap[Text, SessionOutput] = scc.TrieMap()

  private def input(stream: Text): SessionInput =
    // [field-purity] credit callback over session stored in stream's field
    val credit: Long -> Unit =
      caps.unsafe.unsafeAssumePure: count => send(Message.Credit(stream, count))

    val stream0 = SessionInput(stream, credit)
    inputs(stream) = stream0
    stream0

  private def output(stream: Text): SessionOutput =
    openCredit(stream)
    // [field-purity] allowance callback over session stored in stream's field
    val allowance0: Int -> Int = caps.unsafe.unsafeAssumePure: want => allowance(stream, want)
    val emit: Data -> Unit =
      // [field-purity] emit callback over session stored in stream's field
      caps.unsafe.unsafeAssumePure: chunk =>
        enqueue(Launcher.encode(Message.Data(stream, chunk)), priority = false)

    val stream0 = SessionOutput(stream, allowance0, emit)
    outputs(stream) = stream0
    stream0

  val stdin: SessionInput = input(t"stdin")
  val stdout: SessionOutput = output(t"stdout")
  val stderr: SessionOutput = output(t"stderr")

  // The client terminal's size, as the launcher last measured it: at connection, and again
  // with every `WINCH` and `CONT`.
  val windowSize: Atomic.Ref[Optional[(Int, Int)]] = Atomic.Ref(Unset)

  // ── A command on the client's terminal ──────────────────────────────────────────────────

  // One at a time: `run`, then the launcher's `exited`, which the reader delivers here. A
  // launcher that is gone, or that never answers because it has gone, yields 127.
  private val terminalLock: Object = Object()
  private val exited: ju.ArrayDeque[Int] = ju.ArrayDeque()

  def terminal(command: Text, arguments: List[Text], pwd: Optional[Text]): Int =
    terminalLock.synchronized:
      exited.synchronized(exited.clear())
      send(Message.Run(command, arguments, pwd))

      exited.synchronized:
        while exited.isEmpty && !launcherGone do exited.wait(250L)
        if exited.isEmpty then 127 else exited.pollFirst().nn

  private def exitedWith(code: Int): Unit = exited.synchronized:
    exited.addLast(code)
    exited.notifyAll()

  // ── The reader ───────────────────────────────────────────────────────────────────────────

  // The launcher closed the connection, or it failed: every stream from the client ends,
  // every stream to it is closed, and nothing waits for credit any longer.
  private def gone(): Unit =
    credits.synchronized:
      launcherGone = true
      credits.notifyAll()

    outbox.synchronized:
      dead = true
      outbox.notifyAll()

    inputs.values.foreach(_.end())
    outputs.values.foreach(_.severed() = true)
    exited.synchronized(exited.notifyAll())

  def start(): Unit =
    writer
    Session.thread(t"session-reader")(read())

  private def read(): Unit =
    var running = true

    // The connection's close — by the writer after `exit-status`, or by the launcher — ends
    // the read, however the stream reports it.
    def next(): Optional[Launcher.Message] =
      try Launcher.readDocument(in).let(Launcher.decode(_))
      catch case _: ji.IOException => Unset

    while running do
      next() match
        case Message.Data(stream, bytes) =>
          inputs.get(stream).foreach(_.push(bytes))

        case Message.End(stream) =>
          inputs.get(stream).foreach(_.end())

        case Message.Credit(stream, bytes) =>
          grant(stream, bytes)

        case Message.Exited(code) =>
          exitedWith(code)

        case Message.Closed(stream) =>
          log(DaemonLogEvent.Closed(stream))
          outputs.get(stream).foreach(_.severed() = true)

        case Message.Signal(name, columns, rows, deadline) =>
          val interrupt: UnixSignal | WindowsSignal =
            safely(name.as[UnixSignal]).or(name.as[WindowsSignal])

          log(DaemonLogEvent.ReceivedSignal(interrupt))

          // A `WINCH` or `CONT` carries the terminal's size, recorded before the signal is
          // dispatched so that a trap, or the termcap, reads the new size and not the old.
          columns.let: columns => rows.let: rows => windowSize() = (columns, rows)

          val signal: Signal =
            Signal(interrupt, columns, rows, deadline.let(_.toDouble*Milli(Second)))

          // Off the reader: a trap may take its time, and the launcher's next document must
          // not wait on it. One signal is outstanding at a time; the launcher sees to that.
          Session.thread(t"session-signal"):
            val response = dispatch(signal)
            send(Message.SignalAck(response == SignalResponse.Accept))

        case Unset =>
          running = false

        case other =>
          log(DaemonLogEvent.UnrecognizedMessage)

    gone()

  // ── The client's descriptors ─────────────────────────────────────────────────────────────

  // The client's view of the paths that name its own descriptors, for galilei to consult
  // before it opens anything: `/dev/stdin` is the session's stdin, `/dev/fd/63` from
  // `mytool <(…)` is carried as a stream once the launcher is asked for it, and a regular
  // file behind a descriptor is opened by its real path. A descriptor the client did not
  // advertise names nothing, whatever the daemon's own table holds at that number.
  val fdtable: Fdtable = path => Session.governed(path).let(descriptor(_))

  private def descriptor(fd: Int): Fdtable.Descriptor = new Fdtable.Descriptor:
    def open[result](flags: List[OpenFlag])(lambda: Handle => result): result =
      val advertised: Launcher.Descriptor =
        descriptors.seek(_.fd == fd).or(throw Fdtable.Refusal(Reason.Nonexistent))

      val reading = flags.has(OpenFlag.Read)
      val writing = flags.has(OpenFlag.Write) || flags.has(OpenFlag.Append)

      if reading && !advertised.direction.contains('r')
      then throw Fdtable.Refusal(Reason.PermissionDenied)

      if writing && !advertised.direction.contains('w')
      then throw Fdtable.Refusal(Reason.PermissionDenied)

      advertised.path match
        case real: Text if advertised.kind == t"file" => file(real)(lambda)

        case _ => fd match
          case 0 => lambda(handle(stdin, Unset))
          case 1 => lambda(handle(Unset, stdout))
          case 2 => lambda(handle(Unset, stderr))
          case _ => stream(fd, reading, writing)(lambda)

  // A regular file the client holds open: its real path, in this process. Opened for
  // writing only on the first write, so a file the client holds read-only is never touched.
  private def file[result](real: Text)(lambda: Handle => result): result =
    val path = jnf.Path.of(real.s).nn
    val reader: () -> Chain[Data] = () => Chain(Array.unsafeFrozen(jnf.Files.readAllBytes(path).nn))

    val out: ji.OutputStream = new ji.OutputStream:
      private lazy val target: ji.OutputStream =
        jnf.Files
        . newOutputStream(path, jnf.StandardOpenOption.WRITE, jnf.StandardOpenOption.CREATE)
        . nn

      def write(byte: Int): Unit = target.write(byte)

      override def write(bytes: scala.Array[Byte] | Null, start: Int, length: Int): Unit =
        target.write(bytes, start, length)

      override def flush(): Unit = target.flush()
      override def close(): Unit = target.close()

    try lambda(handleOf(reader, out)) finally out.close()

  // A descriptor carried as a stream: asked of the launcher, read to its end or written and
  // ended, and — if the invocation stops reading before the end — closed, so the launcher
  // stops carrying it.
  private def stream[result](fd: Int, reading: Boolean, writing: Boolean)
    ( lambda: Handle => result )
  :   result =

    val name = fd.show
    val source: Optional[SessionInput] = if reading then input(name) else Unset
    val sink: Optional[SessionOutput] = if writing then output(name) else Unset
    send(Message.Open(name))

    try lambda(handle(source, sink))
    finally
      source.let: source =>
        if !source.isEnded then send(Message.Closed(name))
        inputs.remove(name)

      sink.let: sink =>
        sink.flush()
        send(Message.End(name))
        outputs.remove(name)

  private def handle(source: Optional[SessionInput], sink: Optional[SessionOutput]): Handle =
    val reader: () -> Chain[Data] = () => source match
      case input: SessionInput => Chain(Array.unsafeFrozen(input.readAllBytes().nn))
      case _                   => Chain[Data]()

    val out: ji.OutputStream = sink match
      case output: SessionOutput => output
      case _                     => ji.OutputStream.nullOutputStream().nn

    handleOf(reader, out)

  private def handleOf(reader: () -> Chain[Data], out: ji.OutputStream): Handle =
    val writer: Chain[Data] -> Unit = chain =>
      chain.each: data => out.write(Array.unsafeJvm(data))
      out.flush()

    Handle.whole(reader, writer)

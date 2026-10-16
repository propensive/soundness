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
package susurration

import java.util as ju

import scala.caps

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

import Ssh.Error.Reason

// A session channel of the connection protocol (RFC 4254 §5–6): a remote process, with its
// standard streams carried as `CHANNEL_DATA` (stdout) and `CHANNEL_EXTENDED_DATA` (stderr), its
// exit status as a request, and flow control in both directions by windows. The channel is
// driven by the transport's single thread: writing pumps the connection until the server has
// opened enough window, and reading pumps it until the next event, with anything else that
// arrives meanwhile queued for its turn.
//
// Not a `caps.Mutable`: the channel shares the session's transport rather than owning it, and
// a mutable handle over a shared capability is what the separation checker rightly refuses, so
// — as vivisection's JDWP connection does — the transport and the channel's state are held
// behind untracked fields, and the channel is driven, like the transport, by one thread.
object Channel:
  // What the server may send before this client adjusts, and what it may send in one packet.
  val InitialWindow: Int = 2 << 20
  val MaxPacket: Int = 32768

  enum Event:
    case Stdout(data: Data)
    case Stderr(data: Data)
    case Exit(status: Int)
    case Signal(name: Text)
    case Reply(success: Boolean)
    case Eof
    case Closed

class Channel private[susurration]
  ( transport:  Transport,
    local:      Int,
    remote:     Int,
    window0:    Int,
    maxPacket:  Int ):

  import Channel.*

  // [field-purity] flow-control and lifecycle state, driven by the one thread
  @caps.unsafe.untrackedCaptures
  private var remoteWindow: Int = window0

  // [field-purity] as above
  @caps.unsafe.untrackedCaptures
  private var localWindow: Int = InitialWindow

  // Events received while waiting for a different one; a JDK deque, which needs no tracking.
  private val pending: ju.ArrayDeque[Event] = ju.ArrayDeque()

  // [field-purity] as above
  @caps.unsafe.untrackedCaptures
  private var eofSent: Boolean = false

  // [field-purity] as above
  @caps.unsafe.untrackedCaptures
  private var closeSent: Boolean = false

  // [field-purity] as above
  @caps.unsafe.untrackedCaptures
  private var closed0: Boolean = false

  // [field-purity] as above
  @caps.unsafe.untrackedCaptures
  private var exit0: Optional[Int] = Unset

  // [field-purity] as above
  @caps.unsafe.untrackedCaptures
  private var stderr0: List[Data] = Nil

  def closed: Boolean = closed0
  def exitStatus: Optional[Int] = exit0

  // Sends a channel request; with `reply`, pumps until the server answers it.
  def request(kind: Text, reply: Boolean)(fields: Wire.Writer^ => Unit)
    ( using Tactic[Ssh.Error], Diagnostics )
  :   Boolean =

    val writer = Wire.Writer()
    writer.byte(Ssh.Message.ChannelRequest)
    writer.uint32(remote)
    writer.text(kind)
    writer.boolean(reply)
    fields(writer)
    transport.send(writer.data)

    if !reply then true else
      def recur(): Boolean = take { case Event.Reply(success) => success } match
        case success: Boolean => success
        case _                => if closed0 then false else { pump(); recur() }

      recur()

  // An environment variable for the process; the server applies it only if its `AcceptEnv`
  // allows the name, and says nothing either way.
  def env(name: Text, value: Text)(using Tactic[Ssh.Error], Diagnostics): Unit =
    request(t"env", false): writer =>
      writer.text(name)
      writer.text(value)

  // Starts the command; the server must accept it.
  def exec(command: Text)(using Tactic[Ssh.Error], Diagnostics): Unit =
    if !request(t"exec", true)(_.text(command))
    then abort(Ssh.Error(Reason.RequestRefused(t"exec")))

  // Writes to the process's standard input, in packets the server's window and packet size
  // allow, waiting for window when there is none.
  def write(data: Data)(using Tactic[Ssh.Error], Diagnostics): Unit =
    def recur(offset: Int): Unit =
      if offset < data.length && !closed0 then
        if remoteWindow <= 0 then
          pump()
          recur(offset)
        else
          val count = Math.min(Math.min(remoteWindow, maxPacket), data.length - offset)
          val writer = Wire.Writer()
          writer.byte(Ssh.Message.ChannelData)
          writer.uint32(remote)
          writer.string(data.segment(offset.z till (offset + count).z))
          transport.send(writer.data)
          remoteWindow -= count
          recur(offset + count)

    recur(0)

  // Ends the process's standard input.
  def eof()(using Tactic[Ssh.Error], Diagnostics): Unit =
    if !eofSent && !closed0 then
      eofSent = true
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.ChannelEof)
      writer.uint32(remote)
      transport.send(writer.data)

  // The next event from the process: queued ones first, then whatever the connection delivers.
  // `Unset` once the channel is closed and nothing remains.
  def next()(using Tactic[Ssh.Error], Diagnostics): Optional[Event] =
    take { case event => event } match
      case event: Event => event
      case _            => if closed0 then Unset else { pump(); next() }

  // The process's standard output as a lazy sequence of chunks, ending when the process does.
  // Stderr chunks are kept in `stderr`, and the exit status is available afterwards.
  def stdout()(using Tactic[Ssh.Error], Diagnostics): Chain[Data] =
    def recur(): Chain[Data] = next() match
      case Event.Stdout(data) => data #:: Chain.defer(recur())
      case Event.Stderr(data) => stderr0 = data :: stderr0; recur()
      case Event.Eof          => recur()
      case Event.Closed       => Chain()
      case Unset              => Chain()
      case _                  => recur()

    Chain.defer(recur())

  def stderr: List[Data] = stderr0.reverse

  // Closes the channel, draining the server's events until it has closed its side too.
  def close()(using Tactic[Ssh.Error], Diagnostics): Unit =
    if !closeSent then
      closeSent = true
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.ChannelClose)
      writer.uint32(remote)
      transport.send(writer.data)

    def recur(): Unit = if !closed0 then { pump(); recur() }
    recur()

  // Removes and returns the first queued event the partial function accepts.
  private def take[result](select: PartialFunction[Event, result]): Optional[result] =
    val iterator = pending.iterator.nn

    def recur(): Optional[result] =
      if !iterator.hasNext then Unset else
        val event = iterator.next.nn
        if select.isDefinedAt(event) then { iterator.remove(); select(event) } else recur()

    recur()

  // Reads one packet and dispatches it: this channel's events are queued; the connection's
  // housekeeping is answered.
  private def pump()(using Tactic[Ssh.Error], Diagnostics): Unit =
    given wire: Tactic[Wire.Error] = summon[Tactic[Ssh.Error]].contramap: error =>
      Ssh.Error(Reason.Malformed(error.message.text))

    val payload = transport.receive()
    val reader = Wire.Reader(payload)
    val kind = reader.byte()

    def recipient(): Unit =
      val id = reader.uint32()
      if id != local then abort(Ssh.Error(Reason.Malformed(t"a message for an unknown channel")))

    kind match
      case Ssh.Message.ChannelData =>
        recipient()
        val data = reader.string()
        pending.add(Event.Stdout(data))
        consumed(data.length)

      case Ssh.Message.ChannelExtendedData =>
        recipient()
        reader.uint32()
        val data = reader.string()
        pending.add(Event.Stderr(data))
        consumed(data.length)

      case Ssh.Message.ChannelWindowAdjust =>
        recipient()
        remoteWindow += reader.uint32()

      case Ssh.Message.ChannelEof =>
        recipient()
        pending.add(Event.Eof)

      case Ssh.Message.ChannelClose =>
        recipient()
        closed0 = true
        pending.add(Event.Closed)

        if !closeSent then
          closeSent = true
          val writer = Wire.Writer()
          writer.byte(Ssh.Message.ChannelClose)
          writer.uint32(remote)
          transport.send(writer.data)

      case Ssh.Message.ChannelRequest =>
        recipient()
        val request = reader.text()
        val wantReply = reader.boolean()

        request match
          case t"exit-status" =>
            val status = reader.uint32()
            exit0 = status
            pending.add(Event.Exit(status))

          case t"exit-signal" =>
            pending.add(Event.Signal(reader.text()))

          case _ =>
            ()

        if wantReply then
          val writer = Wire.Writer()
          writer.byte(Ssh.Message.ChannelFailure)
          writer.uint32(remote)
          transport.send(writer.data)

      case Ssh.Message.ChannelSuccess =>
        recipient()
        pending.add(Event.Reply(true))

      case Ssh.Message.ChannelFailure =>
        recipient()
        pending.add(Event.Reply(false))

      // A global request from the server (OpenSSH announces its host keys this way) is declined
      // if it wants an answer, and otherwise ignored.
      case Ssh.Message.GlobalRequest =>
        reader.text()
        if reader.boolean() then transport.send(Array(Ssh.Message.RequestFailure))

      case other =>
        abort(Ssh.Error(Reason.Unexpected(other & 0xff)))

  // Grants the server more window once half of the initial one has been used.
  private def consumed(count: Int)(using Tactic[Ssh.Error], Diagnostics): Unit =
    localWindow -= count

    if localWindow < InitialWindow / 2 then
      val grant = InitialWindow - localWindow
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.ChannelWindowAdjust)
      writer.uint32(remote)
      writer.uint32(grant)
      transport.send(writer.data)
      localWindow += grant

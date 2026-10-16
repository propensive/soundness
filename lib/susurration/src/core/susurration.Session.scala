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

import scala.caps

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

import Ssh.Error.Reason

// An authenticated SSH connection, lent by a session: it runs commands on the server. `exec`
// is the whole exchange — input written, output and status collected — while `channel` lends
// the channel itself to a block, for a protocol that interleaves reading and writing, such as
// git's.
object Session:
  case class Result(stdout: Data, stderr: Data, status: Optional[Int])

  private def join(chunks: List[Data]): Data =
    val writer = Wire.Writer()
    chunks.each(writer.bytes(_))
    writer.data

// Like the transport it drives, a plain class with its one counter untracked.
class Session(transport: Transport):
  // [field-purity] the next local channel id, advanced by the one driving thread
  @caps.unsafe.untrackedCaptures
  private var counter: Int = 0

  // Opens a session channel (RFC 4254 §5.1 and §6.1).
  def open()(using Tactic[Ssh.Error], Diagnostics): Channel =
    given wire: Tactic[Wire.Error] = summon[Tactic[Ssh.Error]].contramap: error =>
      Ssh.Error(Reason.Malformed(error.message.text))

    val local = counter
    counter += 1
    val writer = Wire.Writer()
    writer.byte(Ssh.Message.ChannelOpen)
    writer.text(t"session")
    writer.uint32(local)
    writer.uint32(Channel.InitialWindow)
    writer.uint32(Channel.MaxPacket)
    transport.send(writer.data)

    // The confirmation's remote id, window and packet size.
    def recur(): (Int, Int, Int) =
      val payload = transport.receive()
      val reader = Wire.Reader(payload)

      reader.byte() match
        case Ssh.Message.ChannelOpenConfirmation =>
          reader.uint32()
          (reader.uint32(), reader.uint32(), reader.uint32())

        case Ssh.Message.ChannelOpenFailure =>
          reader.uint32()
          reader.uint32()
          abort(Ssh.Error(Reason.ChannelRefused(reader.text())))

        // The server may announce its host keys first.
        case Ssh.Message.GlobalRequest =>
          reader.text()
          if reader.boolean() then transport.send(Array(Ssh.Message.RequestFailure))
          recur()

        case other =>
          abort(Ssh.Error(Reason.Unexpected(other & 0xff)))

    val (remote, window, maxPacket) = recur()
    Channel(transport, local, remote, window, maxPacket)

  // Runs a command with the given standard input and environment, and collects what it wrote
  // and how it exited.
  def exec(command: Text, input: Data = Data(), environment: List[(Text, Text)] = Nil)
    ( using Tactic[Ssh.Error], Diagnostics )
  :   Session.Result =

    val channel = open()

    try
      environment.each: (name, value) => channel.env(name, value)
      channel.exec(command)
      channel.write(input)
      channel.eof()
      val stdout = Session.join(channel.stdout().to[List])
      Session.Result(stdout, Session.join(channel.stderr), channel.exitStatus)
    finally channel.close()

  // Lends an open session channel to the block, closing it afterwards.
  def channel[result](block: Channel => result)(using Tactic[Ssh.Error], Diagnostics)
  :   result =

    val channel = open()
    try block(channel) finally channel.close()

  def close()(using Tactic[Ssh.Error], Diagnostics): Unit = transport.disconnect()

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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package perihelion

import anticipation.*
import denominative.*
import fulminate.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import monotonous.*
import parasite.*
import prepositional.*
import rudiments.*
import scintillate.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

import alphabets.base64.standard
import charEncoders.utf8

def websocket(using request: HttpConnection): Http.Response =
  Websocket(request).response


object Frame:
  def apply(bytes: Bytes, offset: Int = 0): Frame = ???



enum Frame(payload: Bytes):
  case Continuation(fin: Boolean, payload: Bytes) extends Frame(payload)
  case Text(fin: Boolean, payload: Bytes) extends Frame(payload)
  case Binary(fin: Boolean, payload: Bytes) extends Frame(payload)
  case Ping(payload: Bytes) extends Frame(payload)
  case Pong(payload: Bytes) extends Frame(payload)
  case Close(code: Int) extends Frame(Bytes())

  def mask: Optional[Bytes] = Unset

  val length = payload.length

  val byte0: Byte = this match
    case Continuation(fin, _) => if fin then bin"10000000" else bin"00000000"
    case Text(fin, _)         => if fin then bin"10000001" else bin"00000001"
    case Binary(fin, _)       => if fin then bin"10000010" else bin"00000010"
    case Close(_)             => bin"00001000"
    case Ping(_)              => bin"00001001"
    case Pong(_)              => bin"00001010"

  val lengthByte: Byte = payload.length match
    case length if length <= 125   => length.toByte
    case length if length <= 65535 => 126
    case _                         => 127

  val headerLength = lengthByte match
    case 126 => 4
    case 127 => 10
    case _   => 2

  val byte1: Byte = ((if mask.present then bin"10000000" else bin"00000000") | lengthByte).toByte

  val header: Bytes = headerLength match
    case 2  => Bytes(byte0, byte1)
    case 4  => Bytes(byte0, byte1, (length >> 8).toByte, length.toByte)
    case 10 => val byte6 = (length >> 24).toByte
               val byte7 = (length >> 16).toByte
               val byte8 = (length >> 8).toByte
               val byte9 = length.toByte
               Bytes(byte0, byte1, 0, 0, 0, 0, byte6, byte7, byte8, byte9)

object Websocket:
  val magic: Text = t"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  enum Opcode:
    case Continuation, Text, Binary, Reserved0, Reserved1, Reserved2, Reserved3, Reserved4, Close,
         Ping, Pong

class Websocket(request: Http.Request):
  private given prefix: ("secWebsocketAccept" is Prefixable of Text) = identity(_)
  private given prefix2: ("secWebsocketKey" is Prefixable of Text) = identity(_)
  private val key: Text = request.headers.secWebsocketKey.prim.or(panic(m"Missing header"))
  private val spool: Spool[Frame] = Spool()
  private val conduit: Conduit = Conduit(request.body)

  def unmask(bytes: Bytes, mask: Bytes): Bytes =
    Bytes.fill(bytes.length): index =>
      array

  def start()(using Monitor, Codicil): Task[Unit] = async:
    var continue = true

    while continue do
      val head1 = conduit.datum
      val fin = (head1 & 128) == 128
      val opcode = Websocket.Opcode.fromOrdinal(head1 & 16)
      conduit.next()

      val length = conduit.datum match
        case 127   => S64(conduit.take(8)).long.toInt
        case 126   => S16(conduit.take(2)).int
        case count => count

      val payload = conduit.take(length)

      val frame = opcode match
        case Websocket.Opcode.Continuation =>
        case Websocket.Opcode.Text         =>
        case Websocket.Opcode.Binary       =>
        case Websocket.Opcode.Ping         =>
          spool.put(Frame.Pong(payload))
        case Websocket.Opcode.Pong         =>
        case Websocket.Opcode.Close        =>
          continue = false

  def response: Http.Response =
    Http.Response
     (Http.SwitchingProtocols,
      secWebsocketAccept = (key+Websocket.magic).digest[Sha1].serialize[Base64],
      connection         = t"Upgrade",
      upgrade            = t"websocket")
     (spool.stream.map(_.pack()))

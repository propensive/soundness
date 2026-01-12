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
┃    Soundness, version 0.49.0.                                                                    ┃
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
import hypotenuse.*
import vacuous.*


object Frame:
  def apply(bytes: Data, offset: Int = 0): Frame = ???

enum Frame(payload: Data):
  case Continuation(fin: Boolean, payload: Data) extends Frame(payload)
  case Text(fin: Boolean, payload: Data) extends Frame(payload)
  case Binary(fin: Boolean, payload: Data) extends Frame(payload)
  case Ping(payload: Data) extends Frame(payload)
  case Pong(payload: Data) extends Frame(payload)
  case Close(code: Int) extends Frame(Data())

  private def mask: Optional[Data] = Unset

  def length = payload.length

  private def byte0: Byte = this match
    case Continuation(fin, _) => if fin then bin"10000000" else bin"00000000"
    case Text(fin, _)         => if fin then bin"10000001" else bin"00000001"
    case Binary(fin, _)       => if fin then bin"10000010" else bin"00000010"
    case Close(_)             => bin"00001000"
    case Ping(_)              => bin"00001001"
    case Pong(_)              => bin"00001010"

  private def lengthByte: Byte = payload.length match
    case length if length <= 125   => length.toByte
    case length if length <= 65535 => 126
    case _                         => 127

  private def headerLength = lengthByte match
    case 126 => 4
    case 127 => 10
    case _   => 2

  private def byte1: Byte =
    ((if mask.present then bin"10000000" else bin"00000000") | lengthByte).toByte

  def header: Data = headerLength match
    case 2 => Data(byte0, byte1)
    case 4 => Data(byte0, byte1, (length >> 8).toByte, length.toByte)
    case _ =>
      val byte6 = (length >> 24).toByte
      val byte7 = (length >> 16).toByte
      val byte8 = (length >> 8).toByte
      val byte9 = length.toByte
      Data(byte0, byte1, 0, 0, 0, 0, byte6, byte7, byte8, byte9)

  def bytes: Data = header ++ payload

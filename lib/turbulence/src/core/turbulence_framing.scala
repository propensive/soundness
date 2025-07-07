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
package turbulence

import java.lang as jl

import anticipation.*
import contingency.*
import hypotenuse.*
import rudiments.*
import vacuous.*

extension (stream: LazyList[Data])
  inline def framed[width <: U16 | U32](terminator: Optional[width] = Unset)
    ( using Tactic[StreamError]^ )
  :   LazyList[Data] =

    inline compiletime.erasedValue[width] match
      case _: U16 =>
        framingImpl(stream, 2, terminator.asInstanceOf[Optional[U16]].let(_.long).or(-1L))

      case _: U32 =>
        framingImpl(stream, 4, terminator.asInstanceOf[Optional[U32]].let(_.long).or(-1L))

private[turbulence] def framingImpl
  ( stream: LazyList[Data], width: Int, terminator: Long )
  ( using Tactic[StreamError]^ )
:   LazyList[Data] =

  def take
    ( n: Int, buffer: IArray[Byte], offset: Int, tail: LazyList[Data], totalSoFar: Long )
  :   (IArray[Byte], IArray[Byte], Int, LazyList[Data]) =

    val out = new Array[Byte](n)
    var taken = 0
    var buf = buffer
    var off = offset
    var rest = tail

    while taken < n do
      val avail = buf.length - off
      val need = n - taken

      if avail >= need then
        jl.System.arraycopy(buf, off, out, taken, need)
        off += need
        taken = n
      else
        if avail > 0 then
          jl.System.arraycopy(buf, off, out, taken, avail)
          taken += avail

        rest match
          case head #:: more =>
            buf = head
            off = 0
            rest = more

          case _ =>
            abort(StreamError((totalSoFar + taken).b))

    (out.immutable(using Unsafe), buf, off, rest)

  def decodeBE(bytes: IArray[Byte]): Long =
    var acc = 0L
    var index = 0

    while index < bytes.length do
      acc = (acc << 8) | (bytes(index) & 0xFF)
      index += 1

    acc

  def loop(buffer: IArray[Byte], offset: Int, tail: LazyList[Data], totalSoFar: Long)
  :   LazyList[Data] =

    val nonEmpty: Optional[(IArray[Byte], Int, LazyList[Data])] =
      if buffer.length - offset > 0 then (buffer, offset, tail)
      else tail match
        case head #:: more => (head, 0, more)
        case _             => Unset

    nonEmpty.let: (buf0, off0, rest0) =>
      val (prefix, buf1, off1, rest1) = take(width, buf0, off0, rest0, totalSoFar)
      val length = decodeBE(prefix)

      if length == terminator then LazyList()
      else if length < 0 || length > Int.MaxValue then
        abort(StreamError((totalSoFar + width).b))
      else
        val (frame, buf2, off2, rest2) =
          take(length.toInt, buf1, off1, rest1, totalSoFar + width)

        frame #:: loop(buf2, off2, rest2, totalSoFar + width + length)

    . or(LazyList())

  loop(IArray.empty[Byte], 0, stream, 0L)

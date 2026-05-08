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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import language.experimental.captureChecking

import java.io as ji

import anticipation.*
import beneficence.*
import rudiments.*
import vacuous.*

object Stdio:
  def apply
    ( out:     ji.PrintStream | Null,
      err:     ji.PrintStream | Null,
      in:      ji.InputStream | Null,
      termcap: Termcap )
  :   Stdio =

    val safeOut: ji.PrintStream = Optional(out).or(MutePrintStream)
    val safeErr: ji.PrintStream = Optional(err).or(MutePrintStream)
    val safeIn: ji.InputStream = Optional(in).or(MuteInputStream)
    val termcap2: Termcap = termcap

    new Stdio:
      val termcap: Termcap = termcap2
      val out: ji.PrintStream = safeOut
      val err: ji.PrintStream = safeErr
      val in: ji.InputStream = safeIn


  object MuteOutputStream extends ji.OutputStream:
    def write(byte: Int): Unit = ()
    override def write(array: Array[Byte] | Null): Unit = ()
    override def write(array: Array[Byte] | Null, offset: Int, length: Int): Unit = ()
    override def close(): Unit = ()

  lazy val MutePrintStream = ji.PrintStream(MuteOutputStream)

  object MuteInputStream extends ji.InputStream:
    def read(): Int = -1
    override def read(array: Array[Byte] | Null): Int = 0
    override def read(array: Array[Byte] | Null, offset: Int, length: Int): Int = 0
    override def reset(): Unit = ()
    override def close(): Unit = ()
    override def available(): Int = 0

trait Stdio extends Io, Findable:
  val termcap: Termcap^
  val out: ji.PrintStream
  val err: ji.PrintStream
  val in: ji.InputStream

  protected[turbulence] lazy val reader: ji.Reader = ji.InputStreamReader(in)

  def write(bytes: Data): Unit = out.write(bytes.mutable(using Unsafe), 0, bytes.length)
  def print(text: Text): Unit = out.print(text.s)
  def writeErr(bytes: Data): Unit = err.write(bytes.mutable(using Unsafe), 0, bytes.length)
  def printErr(text: Text): Unit = err.print(text.s)
  def read(array: Array[Byte]): Int = in.read(array, 0, array.length)
  def read(array: Array[Char]): Int = reader.read(array, 0, array.length)
  def platform: Boolean = System.out == out && System.in == in && System.err == err

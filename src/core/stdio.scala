/*
    Turbulence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import rudiments.*
import vacuous.*
import anticipation.*

import java.io as ji

package stdioSources:
  given virtualMachine: Stdio = Stdio(System.out.nn, System.err.nn, System.in.nn)

import language.experimental.captureChecking

@capability
trait Io:
  def write(bytes: Bytes): Unit
  def print(text: Text): Unit

object Err:
  def write(bytes: Bytes)(using stdio: Stdio): Unit = stdio.writeErr(bytes)
  def print[TextType](text: TextType)(using stdio: Stdio)(using printable: Printable[TextType]): Unit =
    stdio.printErr(printable.print(text))
  
  def println[TextType](text: TextType)(using Stdio, Printable[TextType]): Unit =
    print(text)
    println()
  
  def println()(using Stdio): Unit = print("\n".tt)

object Out:
  def write(bytes: Bytes)(using stdio: Stdio): Unit = stdio.write(bytes)
  def print[TextType](text: TextType)(using stdio: Stdio)(using printable: Printable[TextType]): Unit =
    stdio.print(printable.print(text))
  
  def println()(using Stdio): Unit = print("\n".tt)
  
  def println[TextType](text: TextType)(using Stdio, Printable[TextType]): Unit =
    print(text)
    println()

object In:
  def read(bytes: Array[Byte])(using stdio: Stdio): Int = stdio.read(bytes)

object Stdio:
  def apply(initOut: ji.PrintStream | Null, initErr: ji.PrintStream | Null, initIn: ji.InputStream | Null): Stdio =
    val safeOut: ji.PrintStream = if initOut == null then MutePrintStream else initOut
    val safeErr: ji.PrintStream = if initErr == null then MutePrintStream else initErr
    val safeIn: ji.InputStream = if initIn == null then MuteInputStream else initIn
  
    new Stdio:
      def out: ji.PrintStream = safeOut
      def err: ji.PrintStream = safeErr
      def in: ji.InputStream = safeIn

  object MuteOutputStream extends ji.OutputStream:
    def write(byte: Int): Unit = ()
    override def write(array: Array[Byte]): Unit = ()
    override def write(array: Array[Byte], offset: Int, length: Int): Unit = ()
    override def close(): Unit = ()

  lazy val MutePrintStream = ji.PrintStream(MuteOutputStream)

  object MuteInputStream extends ji.InputStream:
    def read(): Int = -1
    override def read(array: Array[Byte]): Int = 0
    override def read(array: Array[Byte], offset: Int, length: Int): Int = 0
    override def reset(): Unit = ()
    override def close(): Unit = ()
    override def available(): Int = 0

trait Stdio extends Io:
  def out: ji.PrintStream
  def err: ji.PrintStream
  def in: ji.InputStream

  protected[turbulence] lazy val reader: ji.Reader = ji.InputStreamReader(in, "UTF-8")
  
  def write(bytes: Bytes): Unit = out.write(bytes.mutable(using Unsafe), 0, bytes.length)
  def print(text: Text): Unit = out.print(text.s)
  
  def writeErr(bytes: Bytes): Unit = err.write(bytes.mutable(using Unsafe), 0, bytes.length)
  def printErr(text: Text): Unit = err.print(text.s)

  def read(array: Array[Byte]): Int = in.read(array, 0, array.length)
  def read(array: Array[Char]): Int = reader.read(array, 0, array.length)

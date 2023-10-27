/*
    Turbulence, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import anticipation.*
import perforate.*

import java.io as ji

package stdioSources:
  given jvm(using streamCut: Raises[StreamCutError]): Stdio = Stdio(System.out.nn, System.err.nn, System.in.nn)

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

object Stdio:
  def apply(initOut: ji.PrintStream, initErr: ji.PrintStream, initIn: ji.InputStream): Stdio = new Stdio:
    val out: ji.PrintStream = initOut
    val err: ji.PrintStream = initErr
    val in: ji.InputStream = initIn

trait Stdio extends Io:
  val out: ji.PrintStream
  val err: ji.PrintStream
  val in: ji.InputStream

  private lazy val reader: ji.Reader = ji.InputStreamReader(in, "UTF-8")
  
  def write(bytes: Bytes): Unit = out.write(bytes.mutable(using Unsafe), 0, bytes.length)
  def print(text: Text): Unit = out.print(text.s)
  
  def writeErr(bytes: Bytes): Unit = err.write(bytes.mutable(using Unsafe), 0, bytes.length)
  def printErr(text: Text): Unit = err.print(text.s)

  def read(array: Array[Byte]): Int = in.read(array, 0, array.length)
  def read(array: Array[Char]): Int = reader.read(array, 0, array.length)

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
import hieroglyph.*

package basicIo:
  given jvm(using streamCut: Raises[StreamCutError]): Stdio = new Stdio:
    val encoder = CharEncoder.system
    def putOutText(text: Text): Unit = putOutBytes(encoder.encode(text))
    def putErrText(text: Text): Unit = putErrBytes(encoder.encode(text))
    
    def putOutBytes(bytes: Bytes): Unit =
      if System.out == null then raise(StreamCutError(0.b))(())
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
    def putErrBytes(bytes: Bytes): Unit =
      if System.out == null then raise(StreamCutError(0.b))(())
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))

@capability
trait Stdio:
  def putErrBytes(bytes: Bytes): Unit
  def putErrText(text: Text): Unit
  def putOutBytes(bytes: Bytes): Unit
  def putOutText(text: Text): Unit

object Err
object Out

object Io:
  def put(bytes: Bytes)(using io: Stdio): Unit =
    io.putOutBytes(bytes)

  def print[TextType](text: TextType)(using io: Stdio)(using printable: Printable[TextType]): Unit =
    io.putOutText(printable.print(text))
  
  def printErr[TextType](text: TextType)(using io: Stdio)(using printable: Printable[TextType]): Unit =
    io.putErrText(printable.print(text))
  
  def println[TextType](text: TextType)(using io: Stdio, printable: Printable[TextType]): Unit =
    io.putOutText(printable.print(text))
    io.putOutText("\n".tt)

  def println()(using io: Stdio): Unit = io.putOutText("\n".tt)
  def printlnErr()(using io: Stdio): Unit = io.putErrText("\n".tt)
  
  def printlnErr[TextType](text: TextType)(using io: Stdio, printable: Printable[TextType]): Unit =
    io.putErrText(printable.print(text))
    io.putErrText("\n".tt)

object Stdio:
  given default(using Quickstart)(using Raises[StreamCutError]): Stdio = basicIo.jvm

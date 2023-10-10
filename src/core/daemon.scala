/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import serpentine.*, hierarchies.unix

import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    overwritePreexisting, deleteRecursively}

import anticipation.*, fileApi.galileiApi
import rudiments.*, homeDirectories.default
import perforate.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8
import parasite.*
import turbulence.*
import ambience.*
import spectacular.*

import scala.collection.mutable as scm

import java.net as jn

case class Client(pid: Pid, async: Async[Unit])

class Daemon(port: Int) extends Application:
  private val clients: scm.HashMap[Pid, Client] = scm.HashMap()
  private var continue: Boolean = true
  val xdg = Xdg()

  def client(socket: jn.Socket)(using Monitor, Raises[StreamCutError]): Unit =
    val input = Readable.inputStream.read(socket.getInputStream.nn)
    
    val async: Async[Unit] = Async:
      input.map(_.debug).foreach(println)
      clients -= Pid(99)
    
    clients(Pid(99)) = Client(Pid(99), async)

  def invoke(using Invocation): Execution =
    erased given Raises[PathError] = ###
    erased given Raises[StreamCutError] = ###
    erased given Raises[CancelError] = ###
    erased given Raises[IoError] = ###

    val portFile: Path = xdg.runtimeDir.or(xdg.stateHome) / p"fury.port"

    summon[Writable[Path, Text]]

    execute:
      supervise:
        val socket: jn.ServerSocket = jn.ServerSocket(0)
        val port: Int = socket.getLocalPort
        port.show.writeTo(portFile)
        while continue do safely(client(socket.accept().nn))

      ExitStatus.Ok

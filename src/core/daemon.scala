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
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import parasite.*
import gossamer.*
import turbulence.*
import guillotine.*
import eucalyptus.*, logging.silent
import ambience.*
import spectacular.*
import cellulose.*

import scala.collection.mutable as scm

import java.net as jn

case class CliSession(pid: Pid, async: Async[Unit]):
  val signals: Funnel[Signal] = Funnel()

class Daemon() extends Application:
  println("Starting daemon")
  private val clients: scm.HashMap[Pid, CliSession] = scm.HashMap()
  private var continue: Boolean = true
  val xdg = Xdg()

  def client(socket: jn.Socket)(using Monitor, Raises[StreamCutError], Raises[UndecodableCharError], Raises[AggregateError[CodlError]], Raises[CodlReadError], Raises[NumberError]): Unit =
    val input = Readable.inputStream.read(socket.getInputStream.nn)
    val codl = Codl.parse(input)
    println(codl.debug)
    
    safely(codl.as[InitSession]).or(codl.as[SendSignal]) match
      case SendSignal(process, signal) =>
        println(clients(process))
        println("SIGNAL "+signal)

      case InitSession(process, inputType, args, env) =>
        println("forking "+process)
        val pr = java.io.PrintWriter(socket.getOutputStream, true)
        
        val async: Async[Unit] = Async:
          println("Async")
          codl.body.foreach:
            case char: Char =>
              print(char)
          
          println("Closing")
          clients -= process
        
        clients(process) = CliSession(process, async)

  def invoke(using context: CliContext): Execution = context match
    case given Invocation =>
      println("Invoking")
      import errorHandlers.throwUnsafely
      val portFile: Path = xdg.runtimeDir.or(xdg.stateHome) / p"fury.port"
      val startupFile: Path = xdg.runtimeDir.or(xdg.stateHome) / p"fury.start"
      println(portFile)

      execute:
        supervise:
          val lockProcess = sh"flock -u -x -n $portFile cat".fork[Unit]()
          val socket: jn.ServerSocket = jn.ServerSocket(0)
          val port: Int = socket.getLocalPort
          println(port.show)
          port.show.writeTo(portFile)
          startupFile.touch()
          startupFile.wipe()
          
          while continue do
            
            safely(client(socket.accept().nn))

        ExitStatus.Ok

    case _                      => println("Not an invacation"); ???

object Testing extends Daemon()

case class InitSession(process: Pid, input: Text, arg: List[Text], env: List[Text])
case class SendSignal(process: Pid, signal: Text)
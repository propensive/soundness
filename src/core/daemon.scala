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
import surveillance.*
import eucalyptus.*, logging.silent
import ambience.*
import spectacular.*
import cellulose.*

import scala.collection.mutable as scm

import java.net as jn

case class CliSession(pid: Pid, async: Async[Unit], terminate: Promise[Unit], close: () => Unit)(using Monitor):
  val signals: Funnel[Signal] = Funnel()
  
  val terminator: Async[Unit] = Async:
    safely(terminate.await())
    signals.stop()
    close()

class Daemon() extends Application:
  private val clients: scm.HashMap[Pid, CliSession] = scm.HashMap()
  private var continue: Boolean = true
  val xdg = Xdg()

  def client(socket: jn.Socket)(using Monitor, Raises[StreamCutError], Raises[UndecodableCharError], Raises[AggregateError[CodlError]], Raises[CodlReadError], Raises[NumberError]): Unit =
    val input = Readable.inputStream.read(socket.getInputStream.nn)
    val codl = Codl.parse(input)
    println(codl.debug)
    
    safely(codl.as[Initialize]).or(codl.as[Interrupt]) match
      case Interrupt(process, signal) =>
        println(clients(process))
        println("SIGNAL "+signal)

      case Initialize(process, script, inputType, args, env) =>
        println(s"forking $process for $script")
        val pr = java.io.PrintWriter(socket.getOutputStream, true)
        
        val promise: Promise[Unit] = Promise()
        val async: Async[Unit] = Async:
          codl.body.foreach:
            case char: Char =>
              if char.toInt >= 32 then
                pr.print(char)
                pr.flush()

              if char.toInt == 3 then promise.offer(())
          
          clients -= process
        
        clients(process) = CliSession(process, async, promise, () => socket.close())

  def invoke(using context: CliContext): Execution = context match
    case given Invocation =>
      import errorHandlers.throwUnsafely
      val furyDir: Directory = (xdg.runtimeDir.or(xdg.stateHome) / p"fury").as[Directory]
      val portFile: Path = furyDir / p"port"
      val initFile: Path = furyDir / p"init"
      
      def shutdown(): Unit =
        println("Shutdown daemon")
        portFile.wipe()
        System.exit(0)

      execute:
        supervise:
          Async:
            sh"flock -u -x -n $portFile cat".exec[Unit]()
            println("Lock process died")
            shutdown()

          Async:
            safely(furyDir.watch()).mm: watcher =>
              watcher.stream.foreach:
                case Delete(_, t"port") => shutdown()
                case _ => ()
              
          val socket: jn.ServerSocket = jn.ServerSocket(0)
          val port: Int = socket.getLocalPort
          port.show.writeTo(portFile)
          initFile.touch()
          initFile.wipe()
          
          while continue do safely(client(socket.accept().nn))

        ExitStatus.Ok

    case _                      => println("Not an invacation"); ???

object Testing extends Daemon()

case class Initialize(process: Pid, script: Text, input: Text, arg: List[Text], env: List[Text])
case class Interrupt(process: Pid, signal: Signal)
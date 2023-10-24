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
    deleteRecursively}

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
import java.io as ji

case class CliSession(pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit)(using Monitor)
  
object Daemon:
  def listen(block: Monitor ?=> ShellSession ?=> ExitStatus): Unit =
    given context: Invocation = Invocation(IArray(), environments.jvm, workingDirectories.default, LazyList(), _ => (), _ => ())
    Daemon(monitor => session => block(using monitor)(using session)).invoke.execute(context)

class Daemon(block: Monitor => ShellSession => ExitStatus) extends Application:
  daemon =>
  
  import environments.jvm
  import errorHandlers.throwUnsafely

  private val xdg = Xdg()
  private val furyDir: Directory = (xdg.runtimeDir.or(xdg.stateHome) / p"fury").as[Directory]
  private val portFile: Path = furyDir / p"port"
  private val waitFile: Path = furyDir / p"wait"
  private val clients: scm.HashMap[Pid, CliSession] = scm.HashMap()
  private var continue: Boolean = true

  lazy val termination: Unit =
    println("Shutdown daemon")
    portFile.wipe()
    System.exit(0)
  
  def shutdown(): Unit = termination

  def client(socket: jn.Socket)(using Monitor, Raises[StreamCutError], Raises[UndecodableCharError], Raises[AggregateError[CodlError]], Raises[CodlReadError], Raises[NumberError]): Unit =
    val input = Readable.inputStream.read(socket.getInputStream.nn)
    println("about to parse codl")
    val codl = Codl.parse(input)
    println(codl.debug)
    
    safely(codl.as[Initialize]).or(codl.as[Interrupt]) match
      case Interrupt(process, signal) =>
        val client = clients(process)
        client.signals.put(signal)

      case Initialize(process, workDir, scriptName, inputType, args, env) =>
        val promise: Promise[Unit] = Promise()
        val signalFunnel: Funnel[Signal] = Funnel()
        
        val session = new ShellSession(socket.getOutputStream.nn) with WorkingDirectory(workDir):
          def shutdown(): Unit = daemon.shutdown()
          def script: Text = scriptName
          def arguments: IArray[Text] = IArray.from(args)
          def signals: LazyList[Signal] = signalFunnel.stream
          def input = codl.body
          def stdin: Stdin = inputType
        
        val async: Async[Unit] = Async:
          try block(summon[Monitor])(session)
          finally
            clients -= process
            socket.close()
        
        clients(process) = CliSession(process, async, signalFunnel, promise, () => socket.close())

  def invoke(using context: CliContext): Execution = context match
    case given Invocation =>
      Async.onShutdown:
        waitFile.wipe()
        portFile.wipe()
      
      execute:
        supervise:
          Async:
            sh"flock -u -x -n $portFile cat".exec[Unit]()
            shutdown()

          Async:
            safely(furyDir.watch()).mm: watcher =>
              watcher.stream.foreach:
                case Delete(_, t"port") => shutdown()
                case _ => ()
              
          val socket: jn.ServerSocket = jn.ServerSocket(0)
          val port: Int = socket.getLocalPort
          port.show.writeTo(portFile)
          waitFile.touch()
          waitFile.wipe()
          while continue do safely(client(socket.accept().nn))

        ExitStatus.Ok

    case _                      => println("Not an invacation"); ???

case class Initialize(process: Pid, work: Text, script: Text, input: Stdin, arg: List[Text], env: List[Text])
case class Interrupt(process: Pid, signal: Signal)

@main
def fury(): Unit = Daemon.listen:
  Io.println(t"Hello world")
  Io.println(arguments.debug)
  Io.println(shell.script)
  Io.println(shell.stdin.debug)
  Io.println(shell.directory.or(t"unknown"))
  
  shell.input.multiplexWith(shell.signals).takeWhile(_ != 'q').foreach: datum =>
    datum match
      case 'Q' => shell.shutdown()
      case sig: Signal => Io.println(t"sig: ${sig}")
      case char: Char  => Io.println(t"key: ${char.toInt}")
      case text: Text  => Io.println(t"text: $text")

  Io.println(t"Done")
  ExitStatus.Ok

trait ShellSession(out: ji.OutputStream) extends Stdio, WorkingDirectory:
  def shutdown(): Unit
  def arguments: IArray[Text]
  def signals: LazyList[Signal]
  def input: LazyList[Char] | LazyList[Text]
  def stdin: Stdin
  def script: Text

  def putErrBytes(bytes: Bytes): Unit = putOutBytes(bytes)
  def putErrText(text: Text): Unit = putOutText(text)
  
  def putOutBytes(bytes: Bytes): Unit =
    out.write(bytes.mutable(using Unsafe))
    out.flush()
  
  def putOutText(text: Text): Unit =
    out.write(text.bytes.mutable(using Unsafe))
    out.flush()

inline def arguments(using session: ShellSession): IArray[Text] = session.arguments
inline def shell(using shell: ShellSession): ShellSession = shell
//inline def stdin(using shell: ShellSession): LazyList[Bytes] = 
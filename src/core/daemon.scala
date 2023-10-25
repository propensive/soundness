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
import profanity.*
import gossamer.*
import turbulence.*
import guillotine.*
import surveillance.*
import eucalyptus.*, logging.silent
import ambience.*
import spectacular.*
import cellulose.*

import scala.collection.mutable as scm
import scala.compiletime.*

import java.net as jn
import java.io as ji

case class CliSession(pid: Pid, async: Async[Unit], signals: Funnel[Signal], terminate: Promise[Unit], close: () => Unit)(using Monitor)
  
object Daemon:
  def listen(block: ShellSession ?=> ExitStatus): Unit =
    given context: Invocation = Invocation(IArray(), environments.jvm, workingDirectories.default, LazyList(), _ => (), _ => ())
    Daemon(session => block(using session)).invoke.execute(context)

class Daemon(block: ShellSession => ExitStatus) extends Application:
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
    portFile.wipe()
    System.exit(0)
  
  def shutdown()(using Stdio): Unit =
    Io.println(t"Shutdown daemon")
    termination

  def client(socket: jn.Socket)(using Monitor, Stdio, Raises[StreamCutError], Raises[UndecodableCharError], Raises[AggregateError[CodlError]], Raises[CodlReadError], Raises[NumberError]): Unit =
    val input = Readable.inputStream.read(socket.getInputStream.nn)
    val codl = Codl.parse(input)
    
    safely(codl.as[Initialize]).or(codl.as[Interrupt]) match
      case Interrupt(process, signal) =>
        Io.println(t"Received signal $signal")
        clients(process).signals.put(signal)

      case Initialize(process, workDir, scriptName, inputType, args, env) =>
        val promise: Promise[Unit] = Promise()
        val signalFunnel: Funnel[Signal] = Funnel()
        
        val session = new ShellSession(socket.getOutputStream.nn) with WorkingDirectory(workDir):
          def monitor: Monitor = summon[Monitor]
          def shutdown(): Unit = daemon.shutdown()
          def script: Text = scriptName
          def arguments: IArray[Text] = IArray.from(args)
          def signals: LazyList[Signal] = signalFunnel.stream
          def input = codl.body
          def stdin: Stdin = inputType
        
        val async: Async[Unit] = Async:
          try block(session) finally
            clients -= process
            socket.close()
            Io.println(t"Closed connection to PID ${process.value}")
        
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

case class Initialize(process: Pid, work: Text, script: Text, input: Stdin, arg: List[Text], env: List[Text])
case class Interrupt(process: Pid, signal: Signal)

@main
def fury(): Unit =
  import errorHandlers.throwUnsafely
  
  Daemon.listen:
    supervise:
      terminal(shell.input, shell.signals):
        Io.println(t"Hello world")
        Io.println(arguments.debug)
        Io.println(shell.script)
        Io.println(shell.stdin.debug)
        Io.println(shell.directory.or(t"unknown"))
    
        tty.events.takeWhile(_ != Keypress.Printable('Q')).foreach:
          //case Keypress.Printable(ch) => Io.print(ch)
          case other                  => println(other)

        Io.println(t"Rows: ${tty.rows}")
        Io.println(t"Cols: ${tty.columns}")
        Io.println(t"Done")

    ExitStatus.Ok

trait ShellSession(out: ji.OutputStream) extends Stdio, WorkingDirectory:
  def monitor: Monitor
  def shutdown(): Unit
  def arguments: IArray[Text]
  def signals: LazyList[Signal]
  def input: LazyList[Char]
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

inline def arguments: IArray[Text] = shell.arguments
inline def shell: ShellSession = summonInline[ShellSession]
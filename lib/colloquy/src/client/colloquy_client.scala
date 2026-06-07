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
package colloquy

import java.io as ji
import java.net as jn
import java.nio.file as jnf
import java.util.concurrent as juc

import scala.collection.mutable as scm

import soundness.*

import backstops.silent
import charEncoders.utf8
import classloaders.threadContext
import codicils.cancel
import executives.completions
import internetAccess.enabled
import interpreters.posix
import logging.silent
import supervisors.global
import systems.java
import temporaryDirectories.system
import threading.platform

// A front-end for a Colloquy REPL. `colloquy serve <port>` runs a REPL server on
// that TCP port; `colloquy <port>` connects to a server on localhost and drives
// an interactive session — read a line in a Profanity line editor, send it
// (double-newline-terminated), print the verbatim reply — until Ctrl+D or Ctrl+C.
@main
def repl(): Unit = cli:
  arguments match
    case Argument("serve") :: Argument(As[Int](portNumber)) :: Nil =>
      execute(serve(portNumber))

    case Argument(As[Int](portNumber)) :: Nil =>
      execute:
        safely(Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
          connect(port).lay(unreachable(portNumber)): duplex =>
            try converse(duplex) finally duplex.close()

    case _ =>
      execute(Exit.Fail(1))

// Runs a REPL server on the given TCP port and blocks until interrupted.
private def serve(portNumber: Int)(using Stdio, Monitor, Codicil, System): Exit =
  given Scalac[3.8] = Scalac(Nil)
  given Classloader = serverClassloader

  safely(Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
    whereas:
      case BindError(_) => Out.println(t"colloquy: port $portNumber is unavailable"); Exit.Fail(5)
      case error: Error => Out.println(t"colloquy: ${error.message}"); Exit.Fail(6)

    . recover:
        val service = Repl().serve(port)
        Out.println(t"colloquy: serving a REPL on port $portNumber (Ctrl+C to stop)")
        juc.CountDownLatch(1).await()
        service.stop()
        Exit.Ok

// Builds the classloader the REPL compiles against inside the Ethereal daemon.
// The launch classloader is not a `URLClassLoader`, so it exposes no compile
// classpath, and `java.class.path` is this executable — a shebang-prefixed jar
// with no `.jar` suffix, which dotc refuses to read as a library. We symlink it
// to a `.jar` path (a `ZipFile` reads the appended archive past the shebang) and
// hand the REPL a *parent-first* `URLClassLoader` over it: the jar lands on
// dotc's compile classpath so it finds the bundled scala libraries, while
// parent-first delegation keeps runtime classes (notably `ReplBridge`, whose
// session registry must be shared) identical to the daemon's. Outside the daemon
// (e.g. `ethereal.script` unset) we fall back to the thread-context loader.
private def serverClassloader(using System): Classloader =
  try
    val executable: Text = unsafely(System.properties.ethereal.script[Text]())
    val link: jnf.Path = jnf.Files.createTempDirectory("colloquy").nn.resolve("colloquy.jar").nn
    jnf.Files.createSymbolicLink(link, jnf.Path.of(executable.s)).nn
    val url: jn.URL = ji.File(link.toString).toURI.nn.toURL.nn
    new Classloader(jn.URLClassLoader(Array(url), threadContext.java))
  catch case _: Throwable => threadContext

private def invalidPort(portNumber: Int)(using Stdio): Exit =
  Out.println(t"colloquy: $portNumber is not a valid TCP port")
  Exit.Fail(2)

private def unreachable(portNumber: Int)(using Stdio): Exit =
  Out.println(t"colloquy: could not connect to localhost:$portNumber")
  Exit.Fail(3)

// Opens a TCP connection to the server, or `Unset` if it is refused.
private def connect(port: Port over Tcp): Optional[Duplex] =
  try (ip"127.0.0.1" via port).duplex() catch case _: Exception => Unset

// The read/edit/print loop. The server's reply is printed verbatim. Ctrl+C/Ctrl+D
// dismiss the line editor (`DismissError`) and end the session.
private def converse(duplex: Duplex)(using Stdio, Monitor, Codicil, Console, Environment): Exit =
  // `duplex.stream` blocks on its first socket read, so force the iterator lazily
  // — only after a line has been sent — otherwise it would deadlock here before
  // the editor starts (the server sends nothing until it receives a message).
  lazy val chunks: Iterator[Data] = duplex.stream.iterator

  whereas:
    case TerminalError() =>
      Out.println(t"colloquy: the terminal could not be initialised")
      Exit.Fail(4)

  . recover:
      interactive: terminal ?=>
        var running = true

        while running do
          Out.print(t"> ")

          whereas:
            case DismissError() => running = false

          . recover:
              LineEditor().ask: line =>
                duplex.send(Stream((line+t"\n\n").data))
                Out.print(reply(chunks))

        Exit.Ok

// Pulls chunks from the (persistent) socket stream, buffering until the `"\n\n"`
// message delimiter, and decodes the buffered bytes verbatim.
private def reply(chunks: Iterator[Data]): Text =
  val buffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  var done = false

  while !done && chunks.hasNext do
    chunks.next().each(buffer += _)

    done =
      buffer.length >= 2 && buffer(buffer.length - 1) == '\n'.toByte
      && buffer(buffer.length - 2) == '\n'.toByte

  IArray.from(buffer).utf8

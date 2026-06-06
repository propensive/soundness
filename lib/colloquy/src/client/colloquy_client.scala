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

import scala.collection.mutable as scm

import soundness.*

import backstops.silent
import charEncoders.utf8
import codicils.cancel
import executives.completions
import internetAccess.enabled
import interpreters.posix
import supervisors.global
import threading.platform

// An interactive front-end for a Colloquy REPL server. It connects to a server's
// TCP port on localhost, then loops: read a line in a Profanity line editor, send
// it (double-newline-terminated) to the server, print the verbatim reply, and
// repeat until the user presses Ctrl+D or Ctrl+C.
@main
def repl(): Unit = cli:
  arguments match
    case Argument(As[Int](portNumber)) :: Nil =>
      execute:
        safely(Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
          connect(port).lay(unreachable(portNumber)): duplex =>
            try converse(duplex) finally duplex.close()

    case _ =>
      execute(Exit.Fail(1))

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
  val chunks: Iterator[Data] = duplex.stream.iterator

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

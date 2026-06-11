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
package flame

import _root_.java.io as ji
import _root_.java.net as jn
import _root_.java.nio.channels as jnc
import _root_.java.nio.file as jnf

import soundness.*

import classloaders.threadContext
import probates.await
import logging.silent
import strategies.throwUnsafely
import systems.java
import temporaryDirectories.system
import threading.platform

// Sends `request` to `output` as a length-prefixed BinTEL frame — the on-wire protocol
// the server speaks (a 4-byte big-endian length, then that many BinTEL body bytes).
private def send(output: ji.OutputStream, request: Repl.Request): Unit =
  val body = request.bintel
  val out  = ji.DataOutputStream(output)
  out.writeInt(body.length)
  out.write(body.mutable(using Unsafe))
  out.flush()

private def send(socket: jn.Socket, request: Repl.Request): Unit =
  send(socket.getOutputStream.nn, request)

// Sends `request` and reads the framed BinTEL reply, decoding it to a typed `Reply`.
private def exchange(input: ji.InputStream, output: ji.OutputStream, request: Repl.Request)
:   Repl.Reply =
  send(output, request)
  val in     = ji.DataInputStream(input)
  val length = in.readInt()
  val bytes  = new Array[Byte](length)
  in.readFully(bytes)
  Bintel.read[Repl.Reply](bytes.immutable(using Unsafe))

private def exchange(socket: jn.Socket, request: Repl.Request): Repl.Reply =
  exchange(socket.getInputStream.nn, socket.getOutputStream.nn, request)

// Mimics a standard-REPL session: `size` references `greeting`, a field of the
// enclosing object, by simple name (as `var name = …` would be in the Scala REPL).
// `session` reads `size` once, mutates `greeting`, then reads it again — the
// second read should track the change (a live binding), not the captured value.
object ReplFixture:
  var greeting: String = "hello"

  def session(using Scalac[3.8], Classloader, TemporaryDirectory, Monitor, System, Probate)
  :   (Repl.Outcome, Repl.Outcome) =
    val repl = Repl[3.8]:
      def size: Int = greeting.length

    val before: Repl.Outcome = repl.interpret(t"size")
    greeting = "changed"
    (before, repl.interpret(t"size"))

object Tests extends Suite(m"Flame Tests"):
  def run(): Unit =
    suite(m"REPL tests"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a definition is visible on a later line"):
        supervise:
          val repl = Repl()
          repl.interpret(t"val x = 40")
          repl.interpret(t"println(x + 2)")
      . assert:
          case Repl.Outcome.Ran(_, _, _) => true
          case _                         => false

      test(m"a type error is reported as Rejected with notices"):
        supervise:
          Repl().interpret(t"val n: Int = \"forty\"")
      . assert:
          case Repl.Outcome.Rejected(notices) => notices.nonEmpty
          case _                              => false

      test(m"a runtime exception is reported as Threw"):
        supervise:
          Repl().interpret(t"throw new RuntimeException(\"boom\")")
      . assert:
          case Repl.Outcome.Threw(_, _, _) => true
          case _                           => false

    suite(m"REPL binding tests"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"captured values and a lifted definition are usable in the REPL"):
        supervise:
          val greeting: String = "hello"
          var counter:  Int    = 5

          val repl = Repl[3.8]:
            val text  = greeting
            val count = counter
            def total: Int = text.length + count

          repl.interpret(t"println(total)")     // "hello".length + 5
      . assert:
          case Repl.Outcome.Ran(_, _, _) => true
          case _                         => false

      test(m"a lifted import is in scope for REPL lines"):
        supervise:
          // the lifted import is consumed by the macro, so it reads as unused here
          @annotation.nowarn val repl = Repl[3.8]:
            import scala.collection.mutable.ListBuffer

          repl.interpret(t"println(ListBuffer(1, 2, 3).sum)")
      . assert:
          case Repl.Outcome.Ran(_, _, _) => true
          case _                         => false

      test(m"a captured value persists across several lines"):
        supervise:
          val secret: Int = 42

          val repl = Repl[3.8]:
            val seed = secret

          repl.interpret(t"val doubled = seed*2")
          repl.interpret(t"println(doubled + seed)")
      . assert:
          case Repl.Outcome.Ran(_, _, _) => true
          case _                         => false

    suite(m"REPL result rendering"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"an expression's value is rendered via Inspectable"):
        supervise:
          Repl().interpret(t"21 * 2")
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"42").or(false)
          case _                             => false

      test(m"a definition renders no value"):
        supervise:
          Repl().interpret(t"val x = 5")
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.absent
          case _                             => false

      test(m"a rendered result is bound to res0 for later lines"):
        supervise:
          val repl = Repl()
          repl.interpret(t"40 + 2")
          repl.interpret(t"res0 + 1")
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"43").or(false)
          case _                             => false

      test(m"stdout printed while a line runs is captured"):
        supervise:
          Repl().interpret(t"println(7)")
      . assert:
          case Repl.Outcome.Ran(_, _, output) => output.contains(t"7")
          case _                              => false

      test(m"multi-line code keeps its newlines when tokenized"):
        Repl.tokenize(t"val x = 1\nval y = 2").map(_.text).join
      . assert(_.contains(t"\n"))

    suite(m"REPL TCP server"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a reply carries the value, type and highlighting"):
        supervise:
          val tcpPort = Port[Tcp]()
          val service = Repl().serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try exchange(socket, Repl.Request.Submit(1, t"1 + 1"))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _) =>
            value.let(_ == t"2").or(false) && tpe.let(_.contains(t"Int")).or(false)

          case _ =>
            false

      test(m"a quit request fulfils the server's quit signal"):
        supervise:
          val tcpPort = Port[Tcp]()
          val repl    = Repl()
          val service = repl.serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try
            send(socket, Repl.Request.Quit(0))

            // Wait (bounded) for the quit signal rather than blocking forever.
            val runnable: Runnable = () => repl.awaitQuit()
            val waiter = Thread(runnable)
            waiter.start()
            waiter.join(5000L)
            !waiter.isAlive
          finally
            socket.close()
            service.stop()
      . assert(_ == true)

      test(m"a message sent over a UNIX domain socket is answered"):
        supervise:
          val directory    = jnf.Files.createTempDirectory("flame-test").nn
          val socketPath   = directory.resolve("repl.sock").nn.toString.tt
          val service      = Repl().serve(socketPath)
          val address      = jn.UnixDomainSocketAddress.of(socketPath.s).nn
          val channel      = jnc.SocketChannel.open(address).nn

          try
            exchange
             (jnc.Channels.newInputStream(channel).nn, jnc.Channels.newOutputStream(channel).nn,
              Repl.Request.Submit(3, t"6 * 7"))
          finally
            channel.close()
            service.stop()
      . assert:
          case Repl.Reply.Ran(_, value, _, _, _, _) => value.let(_ == t"42").or(false)
          case _                                    => false

      test(m"a completion request returns matching completions"):
        supervise:
          val tcpPort = Port[Tcp]()
          val service = Repl().serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try exchange(socket, Repl.Request.Complete(1, t"List(1, 2, 3).m", 15))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Completed(_, items) => items.exists(_.name.contains(t"map"))
          case _                              => false

    suite(m"REPL block captures outside references"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a lifted def can reference a value from the enclosing scope"):
        supervise:
          val base = 100

          val repl = Repl[3.8]:
            def shifted(n: Int): Int = n + base

          repl.interpret(t"shifted(5)")
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"105").or(false)
          case _                             => false

      test(m"a lifted def captures an enclosing method parameter"):
        def session(base: Int): Repl.Outcome = supervise:
          val repl = Repl[3.8]:
            def plus(n: Int): Int = n + base

          repl.interpret(t"plus(2)")

        session(40)
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"42").or(false)
          case _                             => false

      test(m"a lifted def can reference both a block binding and an outside value"):
        supervise:
          val base = 100

          val repl = Repl[3.8]:
            val offset = 5
            def total: Int = offset + base

          repl.interpret(t"total")
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"105").or(false)
          case _                             => false

      test(m"a lifted def references a field of an enclosing object, tracking changes"):
        supervise:
          ReplFixture.greeting = "hi"     // length 2; then mutated to "changed" (7)
          ReplFixture.session
      . assert:
          case (Repl.Outcome.Ran(_, before, _), Repl.Outcome.Ran(_, after, _)) =>
            before.let(_ == t"2").or(false) && after.let(_ == t"7").or(false)
          case _ =>
            false

      test(m"a lifted def can write back to a host var"):
        supervise:
          var tally = 1

          val repl = Repl[3.8]:
            def bump(): Unit = tally = tally + 10

          repl.interpret(t"bump()")
          repl.interpret(t"bump()")
          tally
      . assert(_ == 21)

      test(m"a lifted import is in scope for a lifted def and for later lines"):
        supervise:
          val repl = Repl[3.8]:
            import scala.collection.mutable.ListBuffer
            def make: ListBuffer[Int] = ListBuffer(1, 2, 3)

          repl.interpret(t"make.sum")               // lifted def uses the import
          repl.interpret(t"ListBuffer(9, 9).sum")   // a later line uses it directly
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"18").or(false)
          case _                             => false

      test(m"a block-local var can be reassigned from a REPL line"):
        supervise:
          val repl = Repl[3.8]:
            var counter = 10

          repl.interpret(t"counter = counter + 5")
          repl.interpret(t"counter")
      . assert:
          case Repl.Outcome.Ran(_, value, _) => value.let(_ == t"15").or(false)
          case _                             => false

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

import _root_.java.io as ji
import _root_.java.net as jn

import soundness.*

import classloaders.threadContext
import codicils.await
import logging.silent
import strategies.throwUnsafely
import systems.java
import temporaryDirectories.system
import threading.platform

// Mimics a standard-REPL session: `size` references `greeting`, a field of the
// enclosing object, by simple name (as `var name = …` would be in the Scala REPL).
// `session` reads `size` once, mutates `greeting`, then reads it again — the
// second read should track the change (a live binding), not the captured value.
object ReplFixture:
  var greeting: String = "hello"

  def session(using Scalac[3.8], Classloader, TemporaryDirectory, Monitor, System, Codicil)
  :   (Repl.Outcome, Repl.Outcome) =
    val repl = Repl[3.8]:
      def size: Int = greeting.length

    val before: Repl.Outcome = repl.interpret(t"size")
    greeting = "changed"
    (before, repl.interpret(t"size"))

object Tests extends Suite(m"Colloquy Tests"):
  def run(): Unit =
    suite(m"REPL tests"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a definition is visible on a later line"):
        supervise:
          val repl = Repl()
          repl.interpret(t"val x = 40")
          repl.interpret(t"println(x + 2)")
      . assert:
          case Repl.Outcome.Ran(_, _) => true
          case _                      => false

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
          case Repl.Outcome.Threw(_, _) => true
          case _                        => false

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
          case Repl.Outcome.Ran(_, _) => true
          case _                      => false

      test(m"a lifted import is in scope for REPL lines"):
        supervise:
          // the lifted import is consumed by the macro, so it reads as unused here
          @annotation.nowarn val repl = Repl[3.8]:
            import scala.collection.mutable.ListBuffer

          repl.interpret(t"println(ListBuffer(1, 2, 3).sum)")
      . assert:
          case Repl.Outcome.Ran(_, _) => true
          case _                      => false

      test(m"a captured value persists across several lines"):
        supervise:
          val secret: Int = 42

          val repl = Repl[3.8]:
            val seed = secret

          repl.interpret(t"val doubled = seed*2")
          repl.interpret(t"println(doubled + seed)")
      . assert:
          case Repl.Outcome.Ran(_, _) => true
          case _                      => false

    suite(m"REPL result rendering"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"an expression's value is rendered via Inspectable"):
        supervise:
          Repl().interpret(t"21 * 2")
      . assert:
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"42").or(false)
          case _                          => false

      test(m"a definition renders no value"):
        supervise:
          Repl().interpret(t"val x = 5")
      . assert:
          case Repl.Outcome.Ran(_, value) => value.absent
          case _                          => false

      test(m"a rendered result is bound to res0 for later lines"):
        supervise:
          val repl = Repl()
          repl.interpret(t"40 + 2")
          repl.interpret(t"res0 + 1")
      . assert:
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"43").or(false)
          case _                          => false

    suite(m"REPL TCP server"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a message sent over TCP is answered with a JSON reply"):
        supervise:
          val tcpPort = Port[Tcp]()
          val service = Repl().serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try
            val output = socket.getOutputStream.nn
            output.write("{\"id\":1,\"code\":\"1 + 1\",\"kind\":\"Submit\"}\n\n".getBytes("UTF-8").nn)
            output.flush()

            val input   = ji.BufferedReader(ji.InputStreamReader(socket.getInputStream.nn, "UTF-8"))
            val builder = StringBuilder()
            var line: String | Null = input.readLine()

            while line != null && !line.nn.isEmpty do
              builder.append(line.nn).append("\n")
              line = input.readLine()

            builder.toString
          finally
            socket.close()
            service.stop()
      . assert: reply =>
          reply.contains("Ran") && reply.contains("2")

    suite(m"REPL block captures outside references"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a lifted def can reference a value from the enclosing scope"):
        supervise:
          val base = 100

          val repl = Repl[3.8]:
            def shifted(n: Int): Int = n + base

          repl.interpret(t"shifted(5)")
      . assert:
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"105").or(false)
          case _                          => false

      test(m"a lifted def captures an enclosing method parameter"):
        def session(base: Int): Repl.Outcome = supervise:
          val repl = Repl[3.8]:
            def plus(n: Int): Int = n + base

          repl.interpret(t"plus(2)")

        session(40)
      . assert:
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"42").or(false)
          case _                          => false

      test(m"a lifted def can reference both a block binding and an outside value"):
        supervise:
          val base = 100

          val repl = Repl[3.8]:
            val offset = 5
            def total: Int = offset + base

          repl.interpret(t"total")
      . assert:
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"105").or(false)
          case _                          => false

      test(m"a lifted def references a field of an enclosing object, tracking changes"):
        supervise:
          ReplFixture.greeting = "hi"     // length 2; then mutated to "changed" (7)
          ReplFixture.session
      . assert:
          case (Repl.Outcome.Ran(_, before), Repl.Outcome.Ran(_, after)) =>
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
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"18").or(false)
          case _                          => false

      test(m"a block-local var can be reassigned from a REPL line"):
        supervise:
          val repl = Repl[3.8]:
            var counter = 10

          repl.interpret(t"counter = counter + 5")
          repl.interpret(t"counter")
      . assert:
          case Repl.Outcome.Ran(_, value) => value.let(_ == t"15").or(false)
          case _                          => false

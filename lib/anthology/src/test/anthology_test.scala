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
package anthology

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

object Tests extends Suite(m"Anthology Tests"):
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

      test(m"a message sent over TCP is evaluated and answered"):
        supervise:
          val tcpPort = Port[Tcp]()
          val service = Repl().serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try
            val output = socket.getOutputStream.nn
            output.write("1 + 1\n\n".getBytes("UTF-8").nn)
            output.flush()

            val input = ji.BufferedReader(ji.InputStreamReader(socket.getInputStream.nn, "UTF-8"))
            input.readLine()
          finally
            socket.close()
            service.stop()
      . assert(_ == "2")

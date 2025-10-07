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
┃    Soundness, version 0.43.0.                                                                    ┃
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

import soundness.*

import workingDirectories.jre
import systemProperties.jre
import supervisors.global
import asyncTermination.cancel
import logging.silent
import stdioSources.virtualMachine.ansi

import strategies.throwUnsafely

object Tests extends Suite(m"Anthology Tests"):
  def run(): Unit =

    suite(m"Scala compiler tests"):

      val helloWorld = t"""
        package foo

        @main
        def run(): Unit = println("Hello world")
      """

      val sources = Map(t"hello.scala" -> helloWorld)

      test(m"Compile hello world"):
        val classpath = LocalClasspath.system()
        val dir: Path on Linux = workingDirectory[Path on Linux] / Name[Linux](t"tmp")

        recover:
          case CompilerError() =>
            println(t"Compiler error")

          case AsyncError(_) =>
            println(t"Async error")

        . within:
            val process = Scalac[3.6](Nil)(classpath)(sources, dir)
            val progress = async(process.progress.each(println(_)))

            process.complete()
            process.notices.each(println(_))
            progress.await()

      . assert()

    suite(m"Scala.js compiler tests"):

      val helloWorld = t"""
        package foo

        object Hello:
          def mainxyz(): Unit =
            def recur(n: Int, m: Int, c: Int): Int =
              if c == 0 then n else recur(m, n + m, c - 1)

            val result = recur(1, 2, 8)
            println(result)

      """


      val sources = Map(t"hello.scala" -> helloWorld)

      test(m"Compile hello world"):
        val classpath = LocalClasspath.system()
        val dir: Path on Linux = workingDirectory[Path on Linux] / Name[Linux](t"tmp")

        recover:
          case CompilerError() =>
            println(t"Compiler error")

          case AsyncError(_) =>
            println(t"Async error")

        . within:
            val process = Scalac[3.6](List(scalacOptions.scalaJs))(classpath)(sources, dir)
            val progress = async(process.progress.each(println(_)))

            process.complete()
            process.notices.each(println(_))
            progress.await()


      . assert()

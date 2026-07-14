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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package superlunary

import java.lang as jl
import java.nio.file as jnf
import java.util as ju

import scala.quoted.*

import ambience.*
import anticipation.*
import contingency.*
import prepositional.*
import interfaces.paths.pathOnLinux
import systems.javaSystem

object embeddings:
  inline given automatic: [value]
  =>  ( refs: References )
  =>  ( stageable: Stageable over refs.Transport )
  =>  Quotes
  =>  Conversion[value, Expr[value]] =

    value =>
      val encoded: stageable.Transport = stageable.embed[value](value)
      val allocation: Int = refs.allocate(encoded)

      ' {
          import strategies.throwUnsafely
          stageable.extract[value](${refs.array}(${Expr(allocation)}).asInstanceOf[refs.Transport])
        }


// The staging directories awaiting deletion at shutdown. A single shared hook drains them
// sequentially: one hook per directory would have the JVM start them all concurrently at
// exit, each walking a directory tree with open directory streams — enough, after a run
// with many dispatches, to exhaust the process's file-descriptor limit at the very moment
// cleanup runs.
private val doomed: ju.concurrent.ConcurrentLinkedQueue[jnf.Path] =
  ju.concurrent.ConcurrentLinkedQueue()

private lazy val shutdownHook: Unit =
  val runnable: Runnable = () =>
    var directory = doomed.poll()

    while directory != null do
      if jnf.Files.exists(directory) then
        val stream = jnf.Files.walk(directory).nn

        try stream.sorted(ju.Comparator.reverseOrder).nn.forEach: path =>
          try jnf.Files.deleteIfExists(path.nn) catch case _: Throwable => ()
        catch case _: Throwable => ()
        finally stream.close()

      directory = doomed.poll()

  jl.Runtime.getRuntime.nn.addShutdownHook(jl.Thread(runnable))

def deleteOnShutdown(directory: jnf.Path): Unit =
  doomed.add(directory)
  shutdownHook

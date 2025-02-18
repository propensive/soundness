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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import galilei.*
import hellenism.*
import inimitable.*
import jacinta.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*

import scala.quoted.*

object Dispatcher:
  private var cache: Map[Codepoint, (Path, Text => Text)] = Map()

trait Dispatcher:
  type Result[OutputType]
  protected def scalac: Scalac[?]
  protected def invoke[OutputType](dispatch: Dispatch[OutputType]): Result[OutputType]

  inline def dispatch[OutputType: Decodable in Json]
     (body: References ?=> Quotes ?=> Expr[OutputType])
     [ScalacVersionType <: Scalac.All]
     (using codepoint:   Codepoint,
            classloader: Classloader,
            properties:  SystemProperties,
            directory:   TemporaryDirectory)
  :     Result[OutputType] raises CompilerError =
    try
      import strategies.throwUnsafely
      val uuid = Uuid()

      val references = new References()

      val (out, fn): (Path, Text => Text) =
        if Dispatcher.cache.contains(codepoint) then
          val settings: staging.Compiler.Settings =
            staging.Compiler.Settings.make(None, scalac.commandLineArguments.map(_.s))

          given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

          staging.withQuotes:
            '{ (array: List[Json]) =>
                ${  references.setRef('array)
                    body(using references)  } }

          Dispatcher.cache(codepoint)

        else
          val out: Path on Linux = temporaryDirectory[Path on Linux] / Name(uuid.show)
          val settings: staging.Compiler.Settings =
            staging.Compiler.Settings.make(Some(out.encode.s), scalac.commandLineArguments.map(_.s))

          given compiler: staging.Compiler = staging.Compiler.make(classloader.java)(using settings)

          val fn: Text => Text = staging.run:
            val fromList: Expr[List[Json] => Text] = '{ (array: List[Json]) =>
              import Json.jsonEncodableInText
              ${
                references.setRef('array)
                body(using references)
              }.json.encode
            }

            '{ text => $fromList(text.decode[Json].as[List[Json]]) }

          Dispatcher.cache = Dispatcher.cache.updated(codepoint, (out, fn))
          (out, fn)

      val classpath = classloaders.threadContext.classpath match
        case classpath: LocalClasspath =>
          LocalClasspath(classpath.entries :+ ClasspathEntry.Directory(out.encode))

        case _ =>
          val systemClasspath = Properties.java.`class`.path()
          LocalClasspath:
            ClasspathEntry.Directory(out.encode) :: systemClasspath.decode[LocalClasspath].entries

      invoke[OutputType]
       (Dispatch
         (out,
          classpath,
          () => fn(references()).decode[Json].as[OutputType],
          (fn: Text => Text) => fn(references()).decode[Json].as[OutputType]))

    catch case throwable: Throwable =>
      println("Failed, somehow")
      println(throwable)
      ???

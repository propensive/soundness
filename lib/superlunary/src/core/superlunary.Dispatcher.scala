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
┃    Soundness, version 0.34.0.                                                                    ┃
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
import symbolism.*

import interfaces.paths.pathOnMacOs

import scala.quoted.*


trait Dispatcher:
  println("Initializing dispatcher")
  type Result[output]
  type Format
  protected def scalac: Scalac[?]
  protected def invoke[output](dispatch: Dispatch[output, Format]): Result[output]

  private var cache: Map[Codepoint, (Path on MacOs, Format => Format)] = Map()


  inline def dispatch[output: Decodable in Json, carrier]
              (body: References[carrier] ?=> Quotes ?=> Expr[output])
              [version <: Scalac.All]
              (using codepoint:    Codepoint,
                     classloader:  Classloader,
                     properties:   SystemProperties,
                     directory:    TemporaryDirectory,
                     dispatchable: Dispatchable over carrier in Format)
  : Result[output] raises CompilerError =

      try
        import strategies.throwUnsafely
        val uuid = Uuid()

        val references: References[carrier] = new References()

        val (out, function): (Path on MacOs, Format => Format) =
          if cache.contains(codepoint) then
            val settings: staging.Compiler.Settings =
              staging.Compiler.Settings.make(None, scalac.commandLineArguments.map(_.s))

            given compiler: staging.Compiler =
              staging.Compiler.make(classloader.java)(using settings)

            staging.withQuotes:
              '{  (array: List[carrier]) =>
                    ${references.setRef('array) yet body(using references)}  }

            cache(codepoint)

          else
            val out: Path on MacOs = (temporaryDirectory / uuid).on[MacOs]

            val settings: staging.Compiler.Settings =
              staging.Compiler.Settings.make
               (Some(out.encode.s), scalac.commandLineArguments.map(_.s))

            given compiler: staging.Compiler =
              staging.Compiler.make(classloader.java)(using settings)

            val function: Format => Format = staging.run:
              val fromList: Expr[List[carrier] => Format] =
                '{ (array: List[carrier]) =>
                      ${dispatchable.encoder[output]}(${references.setRef('array) yet body(using references)})  }

              '{ format => $fromList(${dispatchable.decoder}(format)) }

            cache = cache.updated(codepoint, (out, function))

            (out, function)

        val classpath = classloaders.threadContext.classpath match
          case classpath: LocalClasspath =>
            LocalClasspath(classpath.entries :+ Classpath.Directory(out))

          case _ =>
            val systemClasspath = Properties.java.`class`.path()
            LocalClasspath:
              Classpath.Directory(out) :: systemClasspath.decode[LocalClasspath].entries

        invoke[output]
         (Dispatch
           (out,
            classpath,
            () => dispatchable.decode[output](function(dispatchable.encode(references()))),
            (function: Format => Format) => dispatchable.decode[output](function(dispatchable.encode(references())))))
      catch case throwable: Throwable =>
        println("Failed, somehow")
        println(throwable)
        ???

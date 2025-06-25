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
┃    Soundness, version 0.36.0.                                                                    ┃
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

import interfaces.paths.pathOnLinux

import scala.quoted.*


trait Dispatcher(using classloader: Classloader) extends Targetable, Formal:
  type Result[output]
  protected val scalac: Scalac[?]
  protected def invoke[output](dispatch: Dispatch[output, Form, Target]): Result[output]
  private var cache: Map[Codepoint, (Target, Form => Form)] = Map()

  lazy val settings2: staging.Compiler.Settings =
    staging.Compiler.Settings.make(None, scalac.commandLineArguments.map(_.s))

  lazy val compiler2: staging.Compiler = staging.Compiler.make(classloader.java)(using settings2)

  def deploy(path: Path on Linux): Target

  inline def dispatch[output, transport]
              (body: References[transport] ?=> Quotes ?=> Expr[output])
              [version <: Scalac.All]
              (using codepoint:    Codepoint,
                     properties:   SystemProperties,
                     directory:    TemporaryDirectory,
                     dispatchable: Dispatchable over transport in Form)
  : Result[output] raises CompilerError =

      try
        import strategies.throwUnsafely
        val references: References[transport] = new References()

        val (target, function): (Target, Form => Form) =
          if cache.contains(codepoint) then
            // This is necessary to allocate references as a side effect
            given staging.Compiler = compiler2

            staging.withQuotes:
              '{  (array: List[transport]) =>
                    ${  references() = 'array
                        body(using references)  }  }

            cache(codepoint)

          else
            val uuid = Uuid()
            val out = (temporaryDirectory / uuid).on[Linux]

            val settings: staging.Compiler.Settings =
              staging.Compiler.Settings.make
               (Some(out.encode.s), scalac.commandLineArguments.map(_.s))

            given compiler: staging.Compiler =
              staging.Compiler.make(classloader.java)(using settings)

            val function: Form => Form = staging.run:
              '{  format =>
                    ${dispatchable.encoder[output]}
                       (${  references() = '{${dispatchable.decoder}(format)}
                            body(using references)  })  }

            val target = deploy(out)
            cache = cache.updated(codepoint, (target, function))

            (target, function)

        invoke[output]
         (Dispatch
           (target,
            function => dispatchable.decode[output](function(dispatchable.encode(references())))))

      catch case throwable: Throwable =>
        println(throwable)
        throwable.printStackTrace()
        abort(CompilerError())

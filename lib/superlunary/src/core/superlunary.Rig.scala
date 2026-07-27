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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import proscenium.compat.*

import java.nio.file as jnf
import java.util.function as juf

import galilei.*
import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import hellenism.*
import inimitable.*
import prepositional.*
import serpentine.*

import interfaces.paths.pathOnLinux
import systems.javaSystem


trait Rig(using classloader0: Classloader) extends Targetable, Formal, Transportive:
  type Result[output]
  type Transport <: Object

  protected val scalac: Scalac[?, ?]

  protected def invoke[output](stage: Stage[output, Form, Target]): Result[output]

  // Keyed by call site AND the staged tree's printed form: one call site may legitimately
  // dispatch different trees (an axial benchmark stages one tree per implementation), and
  // trees which differ only in `References`-transported data share one compilation.
  private var cache: Map[(Codepoint, Text), (Target, juf.Function[Form, Form])] = Map()

  protected val classloader = classloader0

  def classpath(out: Path on Linux): LocalClasspath =
    val entries = Classpath.Directory(out) :: (classloaders.threadContextClassloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().as[LocalClasspath]).entries)

    LocalClasspath(entries*)

  lazy val settings2: staging.Compiler.Settings =
    staging.Compiler.Settings.make(None, scalac.commandLineArguments.stdlib.map(_.s))

  lazy val compiler2: staging.Compiler = staging.Compiler.make(classloader.java)(using settings2)

  def stage(path: Path on Linux): Target


  inline def dispatch[output]
    ( body: (References over Transport) ?=> Quotes ?=> Expr[output] )
    ( using codepoint:  Codepoint,
            properties: System,
            directory:  TemporaryDirectory,
            stageable:  Stageable over Transport in Form )
  :   Result[output] raises CompilerError raises RemoteError =

    val references: References over Transport = References[Transport]()

    // A throwaway probe pass constructs the tree once to fingerprint it, before deciding
    // whether it is already compiled; its `References` instance is discarded so that the
    // real allocation happens exactly once on either path below.
    val fingerprint: Text =
      val probe: References over Transport = References[Transport]()
      given staging.Compiler = compiler2

      staging.withQuotes:
        ' {
            (array: Array[Object]) =>
              $ {
                  // As with `incoming` below: launder the reach `.rd` capability off
                  // the quoted array reference.
                  probe() = ('array).asInstanceOf[Expr[Array[Object]]]
                  body(using probe)
                }
          }
        . show.tt

    val key: (Codepoint, Text) = (codepoint, fingerprint)

    val (target, function): (Target, juf.Function[Form, Form]) =
      if cache.stdlib.contains(key) then
        given staging.Compiler = compiler2

        // This is necessary to allocate references as a side effect
        staging.withQuotes:
          ' {
              (array: Array[Object]) =>
                $ {
                    // As with `incoming` below.
                    references() = ('array).asInstanceOf[Expr[Array[Object]]]
                    body(using references)
                  }
            }

        cache.stdlib(key)

      else
        val uuid = Uuid()

        val out =
          import strategies.throwUnsafely
          (temporaryDirectory / uuid).on[Linux]

        deleteOnShutdown(jnf.Paths.get(out.encode.s).nn)

        val settings: staging.Compiler.Settings =
          staging.Compiler.Settings.make
            ( Some(out.encode.s), scalac.commandLineArguments.stdlib.map(_.s) )

        given compiler: staging.Compiler =
          staging.Compiler.make(classloader.java)(using settings)

        val function: juf.Function[Form, Form] = staging.run:
          ' {
              form =>
                // Bound once: splices read transported values from this array, so it must
                // not be re-deserialized per evaluation — a spliced value inside a hot loop
                // would otherwise decode the whole transport on every iteration.
                val incoming: Array[Object] =
                  import strategies.throwUnsafely
                  stageable.deserialize(form)

                stageable.serialize:

                  val array = new Array[Object](1)

                  array(0) =
                    stageable.embed[output]:
                      $ {
                          // A quoted reference to the `incoming` val is charged that val's
                          // reach `.rd` capability at inline expansion sites, which the pure
                          // `Expr[Array[Object]]` slot cannot hold; the staged program never
                          // mutates the array through this alias.
                          references() = ('incoming).asInstanceOf[Expr[Array[Object]]]
                          body(using references)
                        }

                  array
            }

        val target = stage(out)
        cache = Map.of(cache.stdlib.updated(key, (target, function)))

        (target, function)

    invoke[output]
      ( Stage
          ( target,
            function =>
              stageable.extract[output]:
                stageable.deserialize(function(stageable.serialize(references())))
                . head.asInstanceOf[Transport] ) )

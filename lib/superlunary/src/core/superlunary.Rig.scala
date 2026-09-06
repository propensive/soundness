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

import java.nio.file as jnf
import java.util.function as juf

import fulminate.*
import galilei.*
import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import rudiments.defines
import contingency.*
import digression.*
import distillate.*
import gossamer.*
import hellenism.*
import inimitable.*
import prepositional.*
import serpentine.*
import spectacular.*

import pathInterfaces.pathOnLinux
import systems.javaBaseSystem

object Rig:
  // RemoteError → Rig.Error
  // The failure of the marshalling boundary, not of the staged code: a value could not be
  // written into the rig's `Transport`, or read back out of it.
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Serialization   extends Reason(1)
      case Deserialization extends Reason(2)
      case Unknown         extends Reason(3)

    given Reason is Showable =
      case Reason.Serialization   => t"the output could not be serialized"
      case Reason.Deserialization => t"the input could not be deserialized"
      case Reason.Unknown         => t"of an unknown reason"

  case class Error(reason: Rig.Error.Reason)(using Diagnostics)
  extends fulminate.Error(306, reason.number)
    ( m"failed to perform a remote operation because $reason" )

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
    val entries =
      Classpath.Directory(out) :: LocalClasspath.of(classloaders.threadContextClassloader).entries

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
  :   Result[output] raises Compiler.Error raises Rig.Error =

    val references: References over Transport = References[Transport]()

    // A throwaway probe pass constructs the tree once to fingerprint it, before deciding
    // whether it is already compiled; its `References` instance is discarded so that the
    // real allocation happens exactly once on either path below.
    val fingerprint: Text =
      val probe: References over Transport = References[Transport]()
      given staging.Compiler = compiler2

      staging.withQuotes:
        ' {
            (array: scala.Array[Object]) =>
              $ {
                  // As with `incoming` below: launder the reach `.rd` capability off
                  // the quoted array reference.
                  probe() = ('array).asInstanceOf[Expr[scala.Array[Object]]]
                  body(using probe)
                }
          }
        . show.tt

    val key: (Codepoint, Text) = (codepoint, fingerprint)

    val (target, function): (Target, juf.Function[Form, Form]) =
      if cache.defines(key) then
        given staging.Compiler = compiler2

        // This is necessary to allocate references as a side effect
        staging.withQuotes:
          ' {
              (array: scala.Array[Object]) =>
                $ {
                    // As with `incoming` below.
                    references() = ('array).asInstanceOf[Expr[scala.Array[Object]]]
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
                val incoming: scala.Array[Object] =
                  import strategies.throwUnsafely
                  stageable.deserialize(form)

                stageable.serialize:

                  val array = new scala.Array[Object](1)

                  array(0) =
                    stageable.embed[output]:
                      $ {
                          // A quoted reference to the `incoming` val is charged that val's
                          // reach `.rd` capability at inline expansion sites, which the pure
                          // `Expr[Array[Object]]` slot cannot hold; the staged program never
                          // mutates the array through this alias.
                          references() = ('incoming).asInstanceOf[Expr[scala.Array[Object]]]
                          body(using references)
                        }

                  array
            }

        val target = stage(out)
        cache = (cache.stdlib.updated(key, (target, function))).to(Map)

        (target, function)

    invoke[output]
      ( Stage
          ( target,
            function =>
              stageable.extract[output]:
                stageable.deserialize(function(stageable.serialize(references())))
                . head.asInstanceOf[Transport] ) )

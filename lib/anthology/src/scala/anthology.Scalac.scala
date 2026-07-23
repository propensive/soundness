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
package anthology

import proscenium.compat.*

import scala.language.adhocExtensions

import scala.annotation.targetName
import scala.util.control as suc

import dotty.tools.dotc as dtd
import dotty.tools.dotc.core as dtdc
import dotty.tools.dotc.interfaces as dtdi
import dotty.tools.dotc.reporting.*
import dotty.tools.dotc.sbt.interfaces as dtdsi
import dotty.tools.dotc.util as dtdu

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import gossamer.*
import hellenism.*
import nomenclature.n
import parasite.*, Async.nominative
import prepositional.*
import rudiments.*

object Scalac:
  type Versions = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5 | 3.6 | 3.7 | 3.8 | 3.9

  case class Option[-version <: Versions](flags: Text*)

  private val mutex: Mutex = Mutex()
  private var Scala3: dtd.Compiler = new dtd.Compiler()

  def refresh(): Unit = mutex { Scala3 = new dtd.Compiler() }
  def compiler(): dtd.Compiler = Scala3

  // Preserves the single-type-argument call form, `Scalac[3.6](options)`, which targets the
  // classfile universe.
  @targetName("applyClassfile")
  def apply[version <: Versions](options: List[Option[version]])
  :   Scalac[version, Universe.Classfile] =

    new Scalac(options)

case class Scalac[version <: Scalac.Versions, universe <: Universe] private
  ( options: List[Scalac.Option[version]] ):

  def commandLineArguments: List[Text] = options.bind(_.flags)

  def targeting[universe2 <: Universe]: Scalac[version, universe2] = new Scalac(options)

  // Retargets this configuration at the universe from which the given artifact is produced,
  // for callers who think in terms of the end product rather than its intermediate form.
  def producing[artifact <: Artifact](using provenance: Provenance[artifact])
  :   Scalac[version, provenance.Origin] =

    new Scalac(options)


  def apply
    ( classpath: LocalClasspath )
    [ path: Abstractable across Paths to Text ]
    ( sources: Map[Text, Text], out: path )
    ( using System, Monitor, Probate, Universe.Emission[universe] )
  :   CompileProcess logs CompileEvent raises CompilerError =

    val scalacProcess: CompileProcess = CompileProcess()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: Diagnostic)(using dtdc.Contexts.Context): Unit =
        Log.fine(CompileEvent.Notice(diagnostic.toString.tt))
        scalacProcess.put(notice(diagnostic))

    val callbackApi = new dtdi.CompilerCallback {}

    object ProgressApi extends dtdsi.ProgressCallback:
      private var last: Int = -1


      override def informUnitStarting(stage: String | Null, unit: dtd.CompilationUnit | Null)
      :   Unit =

        ()


      override def progress
        ( current:      Int,
          total:        Int,
          currentStage: String | Null,
          nextStage:    String | Null )
      :   Boolean =

        val int = (100.0*current/total).toInt

        if int > last then
          last = int

          scalacProcess.put
            ( CompileProgress
              ( last/100.0, if currentStage == null then t"null" else currentStage.tt ) )

        scalacProcess.continue

    object driver extends dtd.Driver:
      val currentContext =
        val context = initCtx.fresh
        //val pluginParams = plugins
        //val jsParams =

        val arguments: List[Text] =
          summon[Universe.Emission[universe]].flags :::
            List(t"-d", out.generic, t"-classpath", classpath()) :::
            commandLineArguments :::
            List(t"")

        Log.info(CompileEvent.Running(arguments))
        setup(arguments.stdlib.map(_.s).toArray, context).map(_(1)).get

      def run(): CompileProcess =
        given dtdc.Contexts.Context = currentContext.fresh.pipe: context =>
          context
          . setReporter(reporter)
          . setCompilerCallback(callbackApi)
          . setProgressCallback(ProgressApi)

        val sourceFiles: scala.collection.immutable.List[dtdu.SourceFile] =
          sources.stdlib.toList.map: (name, content) =>
            dtdu.SourceFile.virtual(name.s, content.s)

        scalacProcess.put:
          task(n"scalac"):
            try
              Scalac.compiler().newRun.tap: run =>
                run.compileSources(sourceFiles)
                if !reporter.hasErrors then finish(Scalac.Scala3, run)

              scalacProcess.put
                ( if reporter.hasErrors then CompileResult.Failure else CompileResult.Success )

            catch case suc.NonFatal(error) =>
              scalacProcess.put(CompileResult.Crash(error.stackTrace))
              Scalac.refresh()

        scalacProcess

    driver.run()

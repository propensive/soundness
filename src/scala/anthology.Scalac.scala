/*
    Anthology, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anthology

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import gossamer.*
import hellenism.*
import parasite.*
import rudiments.*

import dotty.tools.dotc as dtd
import dotty.tools.dotc.reporting.*
import dotty.tools.dotc.interfaces as dtdi
import dotty.tools.dotc.util as dtdu
import dotty.tools.dotc.core as dtdc
import dotty.tools.dotc.sbt.interfaces as dtdsi

import scala.util.control as suc

import language.adhocExtensions

object Scalac:
  type All = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5 | 3.6
  private var Scala3: dtd.Compiler = new dtd.Compiler()
  def refresh(): Unit = Scala3 = new dtd.Compiler()
  def compiler(): dtd.Compiler = Scala3

case class Scalac[VersionType <: Scalac.All](options: List[ScalacOption[VersionType]]):

  def commandLineArguments: List[Text] = options.flatMap(_.flags)

  def apply(classpath: LocalClasspath)[PathType: GenericPath]
     (sources: Map[Text, Text], out: PathType)
     (using SystemProperties, Monitor, Codicil)
  :     CompileProcess logs CompileEvent raises CompilerError =

    val scalacProcess: CompileProcess = CompileProcess()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: Diagnostic)(using dtdc.Contexts.Context): Unit =
        Log.fine(CompileEvent.Notice(diagnostic.toString.tt))
        scalacProcess.put(Notice(diagnostic))

    val callbackApi = new dtdi.CompilerCallback {}

    object ProgressApi extends dtdsi.ProgressCallback:
      private var last: Int = -1
      override def informUnitStarting(stage: String, unit: dtd.CompilationUnit): Unit = ()

      override def progress(current: Int, total: Int, currentStage: String, nextStage: String)
      :     Boolean =

        val int = (100.0*current/total).toInt

        if int > last then
          last = int
          scalacProcess.put(CompileProgress(last/100.0, currentStage.tt))

        scalacProcess.continue

    object driver extends dtd.Driver:
      val currentCtx =
        val ctx = initCtx.fresh
        //val pluginParams = plugins
        //val jsParams =
        val args: List[Text] =
          List(t"-d", out.pathText, t"-classpath", classpath())
          ::: commandLineArguments
          ::: List(t"")

        Log.info(CompileEvent.Running(args))
        setup(args.map(_.s).to(Array), ctx).map(_(1)).get

      def run(): CompileProcess =
        given dtdc.Contexts.Context = currentCtx.fresh.pipe: ctx =>
          ctx
          . setReporter(reporter)
          . setCompilerCallback(callbackApi)
          . setProgressCallback(ProgressApi)

        val sourceFiles: List[dtdu.SourceFile] = sources.to(List).map: (name, content) =>
          dtdu.SourceFile.virtual(name.s, content.s)

        scalacProcess.put:
          task(t"scalac"):
            try
              Scalac.compiler().newRun.tap: run =>
                run.compileSources(sourceFiles)
                if !reporter.hasErrors then finish(Scalac.Scala3, run)

              scalacProcess.put
               (if reporter.hasErrors then CompileResult.Failure else CompileResult.Success)

            catch case suc.NonFatal(error) =>
              scalacProcess.put(CompileResult.Crash(error.stackTrace))
              Scalac.refresh()

        scalacProcess

    driver.run()

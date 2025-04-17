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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import gossamer.*
import hellenism.*
import parasite.*
import prepositional.*
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
  type Versions = 3.0 | 3.1 | 3.2 | 3.3 | 3.4 | 3.5 | 3.6 | 3.7

  case class Option[-version <: Versions](flags: Text*)

  private var Scala3: dtd.Compiler = new dtd.Compiler()
  def refresh(): Unit = synchronized { Scala3 = new dtd.Compiler() }
  def compiler(): dtd.Compiler = Scala3

case class Scalac[version <: Scalac.All](options: List[Scalac.Option[version]]):
  def commandLineArguments: List[Text] = options.flatMap(_.flags)

  def apply(classpath: LocalClasspath)[path: Abstractable across Paths to Text]
       (sources: Map[Text, Text], out: path)
       (using System, Monitor, Codicil)
  : CompileProcess logs CompileEvent raises CompilerError =

      val scalacProcess: CompileProcess = CompileProcess()

      object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
        def doReport(diagnostic: Diagnostic)(using dtdc.Contexts.Context): Unit =
          Log.fine(CompileEvent.Notice(diagnostic.toString.tt))
          scalacProcess.put(Notice(diagnostic))

      val callbackApi = new dtdi.CompilerCallback {}

      object ProgressApi extends dtdsi.ProgressCallback:
        private var last: Int = -1

        override def informUnitStarting(stage: String | Null, unit: dtd.CompilationUnit | Null)
        : Unit =

            ()

        override def progress
                      (current:      Int,
                       total:        Int,
                       currentStage: String | Null,
                       nextStage:    String | Null)
        : Boolean =

          val int = (100.0*current/total).toInt

          if int > last then
            last = int
            scalacProcess.put
             (CompileProgress
               (last/100.0, if currentStage == null then t"null" else currentStage.tt))

          scalacProcess.continue

      object driver extends dtd.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          //val pluginParams = plugins
          //val jsParams =
          val args: List[Text] =
            List(t"-d", out.generic, t"-classpath", classpath())
            ::: commandLineArguments
            ::: List(t"")

          Log.info(CompileEvent.Running(args))

          setup(args.map(_.s).to(Array), ctx).map(_(1)).getOrElse:
            abort(CompilerError())

        def run(): CompileProcess =
          println("running")
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
                println("compiling")
                Scalac.compiler().newRun.tap: run =>
                  run.compileSources(sourceFiles)
                  println("and")
                  if !reporter.hasErrors then finish(Scalac.Scala3, run)

                scalacProcess.put
                 (if reporter.hasErrors then CompileResult.Failure else CompileResult.Success)

                println(options)
                if options.has(scalacOptions.scalaJs) then
                  println("has Scala.JS")
                  import org.scalajs.linker.*
                  import org.scalajs.linker.interface.*
                  import org.scalajs.logging.*
                  import scala.concurrent.*, duration.*

                  val logger = new Logger:
                    def log(level: Level, message: => String): Unit = println(message)
                    def trace(exception: => Throwable): Unit = exception.printStackTrace()

                  println("logger = "+logger)

                  val config = StandardConfig().withExperimentalUseWebAssembly(true).withOptimizer(true).withMinify(true).withModuleKind(ModuleKind.ESModule)
                  println("config = "+config)

                  val linker = try StandardImpl.linker(config)
                    catch case error: Throwable =>
                      error.printStackTrace()
                      ???

                  println("linker = "+linker)
                  val irFileCache: IRFileCache = StandardImpl.irFileCache()
                  val cache = irFileCache.newCache
                  import scala.concurrent.ExecutionContext.Implicits.global
                  val scalalib: Seq[IRContainer] = Await.result(PathIRContainer.fromClasspath(List(java.nio.file.Paths.get("/Users/propensive/work/soundness/scalajs-library_2.13-1.18.2.jar").nn)), 5.minutes)(0)
                  val scalalib2: Seq[IRContainer] = Await.result(PathIRContainer.fromClasspath(List(java.nio.file.Paths.get("/Users/propensive/work/soundness/scalajs-scalalib_2.13-2.13.16+1.18.2.jar").nn)), 5.minutes)(0)
                  val javalib: Seq[IRContainer] = Await.result(PathIRContainer.fromClasspath(List(java.nio.file.Paths.get("/Users/propensive/work/soundness/scalajs-javalib-1.18.2.jar").nn)), 5.minutes)(0)
                  val scalalibFiles: Seq[IRFile] = Await.result(cache.cached(scalalib), 5.minutes)
                  val scalalibFiles2: Seq[IRFile] = Await.result(cache.cached(scalalib2), 5.minutes)
                  val javalibFiles: Seq[IRFile] = Await.result(cache.cached(javalib), 5.minutes)

                  println("linking")
                  val output = PathOutputDirectory(java.nio.file.Paths.get("js").nn)
                  println(out.generic.s+"/foo/Hello$.sjsir")
                  val irFile1: IRFile = Await.result(PathIRFile(java.nio.file.Paths.get(out.generic.s+"/foo/Hello$.sjsir").nn), 5.minutes)
                  val irFile2: IRFile = Await.result(PathIRFile(java.nio.file.Paths.get(out.generic.s+"/foo/Hello.sjsir").nn), 5.minutes)
                  val mainMethod = ModuleInitializer.mainMethod("foo.Hello", "mainxyz")
                  println(mainMethod)
                  try println("Result: "+Await.result(linker.link(List(irFile1, irFile2) ++ scalalibFiles ++ scalalibFiles2 ++ javalibFiles, mainMethod :: Nil, output, logger), 1.minutes))
                  catch case throwable: Throwable =>
                    println(throwable)
                    throwable.printStackTrace()

              catch case suc.NonFatal(error) =>
                scalacProcess.put(CompileResult.Crash(error.stackTrace))
                Scalac.refresh()

          scalacProcess

      driver.run()

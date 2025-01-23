/*
    Larceny, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package larceny

import dotty.tools.*, dotc.*, util.*, reporting.*, core.*, Contexts.*

import scala.util.chaining.*
import scala.collection.mutable as scm

import language.adhocExtensions

object Subcompiler:
  val Scala3: Compiler = new Compiler()

  class CustomReporter() extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
    val errors: scm.ListBuffer[CompileError] = scm.ListBuffer()

    def doReport(diagnostic: Diagnostic)(using Context): Unit =
      try
        val pos = diagnostic.pos
        val code = String(ctx.compilationUnit.source.content.slice(pos.start, pos.end))
        val offset = pos.point - pos.start
        val ordinal = diagnostic.msg.errorId.ordinal

        errors += CompileError(ordinal, diagnostic.msg.message, code, pos.start, offset)

      catch case err: Throwable => ()

  def compile(classpath: String, source: String): List[CompileError] =
    compile(classpath, source, Set((0, source.length)))

  def compile(classpath: String, source: String, regions: Set[(Int, Int)]): List[CompileError] =
    object driver extends Driver:
      val currentCtx: Context =
        val ctx = initCtx.fresh
        val ctx2 = ctx.setSetting(ctx.settings.classpath, classpath)
        setup(Array[String](""), ctx2).map(_(1)).get

      def run(source: String, regions: Set[(Int, Int)], errors: List[CompileError])
      :     List[CompileError] =

        if regions.isEmpty then errors else
          val reporter: CustomReporter = CustomReporter()
          val sourceFile: SourceFile = SourceFile.virtual("<subcompilation>", source)
          val ctx = currentCtx.fresh

          given Context =
            ctx
            . setReporter(reporter)
            . setSetting(ctx.settings.classpath, classpath)
            . setSetting(ctx.settings.YstopBefore, List("genSJSIR"))
            . setSetting(ctx.settings.color, "never")

          Scala3.newRun.tap: run =>
            run.compileSources(List(sourceFile))
            if !reporter.hasErrors then finish(Scala3, run)

          val newErrors = reporter.errors.to(List)

          def recompile(todo: List[CompileError], done: Set[(Int, Int)], source: String)
          :     List[CompileError] =

            todo match
              case Nil =>
                if done.isEmpty then errors ::: newErrors
                else run(source, regions -- done, errors ::: newErrors)

              case error :: tail =>
                regions.find { (start, end) => error.point >= start && error.point <= end }.match
                  case None =>
                    recompile(tail, done, source)

                  case Some(region@(from, to)) =>
                    if done.contains(region) then recompile(tail, done, source) else
                      val newSource = source.take(from)+"{}"+(" "*(to - from - 2))+source.drop(to)
                      recompile(tail, done + region, newSource)

          recompile(newErrors, Set(), source)

    driver.run(source, regions, Nil)

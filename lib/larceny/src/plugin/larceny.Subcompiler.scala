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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package larceny

import language.adhocExtensions

import scala.collection.mutable as scm
import scala.util.chaining.*

import dotty.tools.*, dotc.*, util.*, reporting.*, core.*, config.Settings, Contexts.*

object Subcompiler:
  val Scala3: Compiler = new Compiler()

  class CustomReporter() extends Reporter, HideNonSensicalMessages:
    val errors: scm.ListBuffer[CompileError] = scm.ListBuffer()

    def doReport(diagnostic: Diagnostic)(using context: Context): Unit =
      try
        var position = diagnostic.pos
        while position.outer != NoSourcePosition do position = position.outer

        val code =
          String(context.compilationUnit.source.content.slice(position.start, position.end))

        val offset = position.point - position.start
        val ordinal = diagnostic.msg.errorId.ordinal

        errors += CompileError(ordinal, diagnostic.msg.message, code, position.start, offset)

      catch case error: Throwable => ()


  def compile
    ( language: List[Settings.Setting.ChoiceWithHelp[String]], classpath: String, source: String )
  :   List[CompileError] =

    compile(language, classpath, source, Set((0, source.length)))


  def compile
    ( language:  List[Settings.Setting.ChoiceWithHelp[String]],
      classpath: String,
      source:    String,
      regions:   Set[(Int, Int)] )
  :   List[CompileError] =

    object driver extends Driver:
      val currentContext: Context =
        val context = initCtx.fresh
        val context2 = context.setSetting(context.settings.classpath, classpath)
        setup(Array[String](""), context2).map(_(1)).get


      def run(source: String, regions: Set[(Int, Int)], errors: List[CompileError])
      :   List[CompileError] =

        if regions.isEmpty then errors else
          val reporter: CustomReporter = CustomReporter()
          val sourceFile: SourceFile = SourceFile.virtual("<subcompilation>", source)
          val context = currentContext.fresh

          given context0: Context =
            context
            . setReporter(reporter)
            . setSetting(context.settings.language, language)
            . setSetting(context.settings.classpath, classpath)
            . setSetting(context.settings.YstopBefore, List("genSJSIR"))
            . setSetting(context.settings.color, "never")

          Scala3.newRun.tap: run =>
            run.compileSources(List(sourceFile))
            if !reporter.hasErrors then finish(Scala3, run)

          val newErrors = reporter.errors.to(List)

          def recompile(todo: List[CompileError], done: Set[(Int, Int)], source: String)
          :   List[CompileError] =

            todo match
              case Nil =>
                if done.isEmpty then errors ::: newErrors
                else run(source, regions -- done, errors ::: newErrors)

              case error :: tail =>
                regions.find: (start, end) =>
                  error.point >= start && error.point <= end

                . match
                    case None =>
                      recompile(tail, done, source)

                    case Some(region@(from, to)) =>
                      if done.contains(region) then recompile(tail, done, source) else

                        val newSource =
                          source.take(from)+"{}"+(" "*(to - from - 2))+source.drop(to)

                        recompile(tail, done + region, newSource)

          recompile(newErrors, Set(), source)


    driver.run(source, regions, Nil)

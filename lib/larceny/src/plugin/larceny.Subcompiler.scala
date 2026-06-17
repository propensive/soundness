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
        // Find the position to report. Inline / macro position chains have a
        // delicate structure:
        //   - the innermost link is in the macro's *definition* source; the
        //     outermost is the user's call site. We want a position in the
        //     user's source.
        //   - macros that synthesise a Position(sourceFile, start, end)
        //     (e.g. hypotenuse's bin/hex macros) emit numbers computed in
        //     their *own* source's coordinate space; dotty may relabel that
        //     position's source as the compilation unit's, which makes the
        //     numbers look user-source-shaped while pointing at unrelated
        //     content.
        //
        // Strategy: first walk to the outermost cuSource position — that's
        // the user's call-site span and therefore a trustworthy outer bound.
        // Then walk the chain again from the innermost end, preferring the
        // narrowest cuSource position that fits *inside* that bound. That
        // gives us the precise span when one exists (e.g. the splice in
        // x"<a>$bad</a>") while rejecting bogus inner positions whose
        // numbers don't fall inside the call site's span.
        val cuSource = context.compilationUnit.source

        var outermostInCu: SourcePosition | Null = null
        var cur = diagnostic.pos

        while cur.exists do
          if cur.source == cuSource then outermostInCu = cur
          val outer = cur.outer
          cur = if outer == null || outer == NoSourcePosition then NoSourcePosition else outer.nn

        var position = diagnostic.pos

        outermostInCu match
          case bound: SourcePosition =>
            var found: SourcePosition = bound
            var c = diagnostic.pos

            while c.exists do
              if
                c.source == cuSource &&
                  c.start >= bound.start &&
                  c.end <= bound.end &&
                  (c.end - c.start) < (found.end - found.start)
              then found = c

              val outer = c.outer
              c = if outer == null || outer == NoSourcePosition then NoSourcePosition else outer.nn

            position = found

          case null =>
            var outer = position.outer
            while outer != null && outer != NoSourcePosition do
              position = outer.nn
              outer = position.outer

        val content = context.compilationUnit.source.content
        val focus = String(content.slice(position.start, position.end))


        val offset = position.point - position.start
        val ordinal = diagnostic.msg.errorId.ordinal

        errors += CompileError(ordinal, diagnostic.msg.message, focus, position.start, offset)

      catch case error: Throwable => ()


  def compile
    ( language: List[Settings.Setting.ChoiceWithHelp[String]], classpath: String, source: String )
  :   List[CompileError] =

    compile(language, classpath, source, Set((0, source.length)), Nil)


  def compile
    ( language:  List[Settings.Setting.ChoiceWithHelp[String]],
      classpath: String,
      source:    String,
      regions:   Set[(Int, Int)] )
  :   List[CompileError] =

    compile(language, classpath, source, regions, Nil)


  // `plugins` is a list of `-Xplugin:` paths (jar files or directories) that
  // should be loaded into the sub-compilation. The outer LarcenyTransformer
  // computes this from the parent compilation's `-Xplugin` settings minus
  // larceny itself, so the inner compile sees the same plugin pipeline that
  // production code would see — except for larceny, whose recursion would
  // re-fire on any `demilitarize(...)` calls in the sub-source.
  def compile
    ( language:  List[Settings.Setting.ChoiceWithHelp[String]],
      classpath: String,
      source:    String,
      regions:   Set[(Int, Int)],
      plugins:   List[String] )
  :   List[CompileError] =

    object driver extends Driver:
      val currentContext: Context =
        val context = initCtx.fresh
        val context2 = context.setSetting(context.settings.classpath, classpath)
        val args = Array[String]("") ++ plugins.map: p => s"-Xplugin:$p"
        setup(args, context2).map(_(1)).get


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

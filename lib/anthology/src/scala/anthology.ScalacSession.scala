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

import java.nio.file as jnf

import scala.caps
import scala.language.adhocExtensions
import scala.util.control as suc

import dotty.tools.dotc as dtd
import dotty.tools.dotc.core as dtdc
import dotty.tools.dotc.interfaces as dtdi
import dotty.tools.dotc.util as dtdu
import dotty.tools.io as dtio

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import digression.*
import distillate.*
import gossamer.*
import hellenism.*
import prepositional.*
import proscenium.compat.*
import rudiments.*
import serpentine.*
import spectacular.*

object ScalacSession:
  // The process of a compile within a session. Compilation output is kept in memory (the
  // session's front-end names no output directory): `classfiles` exposes the emitted
  // artifacts, and `save` materializes them under a directory, only if the caller wants
  // them on disk. Since session compiles run synchronously, the result is already complete
  // by the time the process is returned.
  class Process private[anthology] (output: dtio.AbstractFile) extends CompileProcess():
    // Every artifact the compile emitted — classfiles, TASTy, `.sjsir`, `.nir` — keyed by
    // its location on the classpath this output constitutes. Empty if the compile failed
    // before the back-end ran.
    def classfiles: Map[Path on Classpath, Data] =
      def walk(dir: dtio.AbstractFile, prefix: Text): scala.List[(Path on Classpath, Data)] =
        dir.iterator.toList.flatMap: file =>
          val path: Text = t"$prefix/${file.name}"

          if file.isDirectory then walk(file, path)
          // The compiler emits names which are valid classpath elements, so decoding
          // cannot fail; the bytes are fresh from `toByteArray`, so no writer is retained.
          else scala.List((unsafely(path.as[Path on Classpath]), Array.unsafeFrozen(file.toByteArray)))

      Map.from(walk(output, t""))

    def save[path: Abstractable across Paths to Text](directory: path): Unit =
      val root = jnf.Paths.get(directory.generic.s).nn

      classfiles.stdlib.foreach: (path, data) =>
        // The classpath root renders as empty text, so `encode` is already the
        // `directory`-relative form, `pkg/Name.class`.
        val target = root.resolve(path.encode.s).nn
        jnf.Files.createDirectories(target.getParent.nn)
        // `Files.write` only reads its array argument.
        jnf.Files.write(target, Array.unsafeJvm(data))
        ()

// A warm compiler session: one base context (whose symbol table persists from run to run,
// as in the REPL) and one `Compiler`, reused across compiles. `compile` is an `update`
// method and its process borrows the session, so separation checking serializes compiles;
// runs are synchronous on the caller's thread because dotc pins a context base to the
// thread of its first run (`Run.compileUnits`'s `checkSingleThreaded`).
//
// Two limitations follow from the REPL-style retained symbol table: redefining a top-level
// name compiled earlier in the same session is unsupported (distinct source and class
// names per compile, as a REPL generates, are safe); and a macro defined by one compile
// loads at its next use from the *classpath*, not the retained symbols, so callers wanting
// same-session macro expansion must `save` to a directory on the session's classpath.
class ScalacSession private[anthology] (arguments: List[Text])
  ( using loggable: (CompileEvent is Loggable)^, tactic: Tactic[Compiler.Error] )
extends caps.ExclusiveCapability, caps.Stateful:

  private val driver: ScalacDriver = ScalacDriver()

  @scala.caps.unsafe.untrackedCaptures
  private var compiler: dtd.Compiler = new dtd.Compiler()

  @scala.caps.unsafe.untrackedCaptures
  private var baseContext: dtdc.Contexts.Context =
    driver.baseContext(arguments).getOrElse(abort(Compiler.Error()))

  // After a crash the whole warm state is rebuilt together: replacing only the compiler
  // would restart its run ids against a context whose denotation validity periods already
  // used them.
  private update def rebuild(): Unit =
    compiler = new dtd.Compiler()
    baseContext = driver.baseContext(arguments).getOrElse(abort(Compiler.Error()))

  update def compile(sources: Map[Text, Text]): ScalacSession.Process^{this, caps.any} =
    val output = dtio.virtualDirectory("<session output>")
    val process = ScalacSession.Process(output)
    val reporter = processReporter(process)

    val context =
      baseContext.fresh
      . setReporter(reporter)
      . setCompilerCallback(new dtdi.CompilerCallback {})
      . setProgressCallback(progressCallback(process))

    context.setSetting(context.settings.outputDir, output)

    val sourceFiles: scala.collection.immutable.List[dtdu.SourceFile] =
      sources.stdlib.toList.map: (name, content) => dtdu.SourceFile.virtual(name.s, content.s)

    try
      given dtdc.Contexts.Context = context

      compiler.newRun.tap: run =>
        run.compileSources(sourceFiles)
        if !reporter.hasErrors then driver.finishRun(compiler, run)

      process.put
        ( if reporter.hasErrors then CompileResult.Failure else CompileResult.Success )

    catch case suc.NonFatal(error) =>
      process.put(CompileResult.Crash(error.stackTrace))
      rebuild()

    process

// A named instance class rather than an anonymous given: an anonymous subclass would
// freshen the capability types in its inferred `Result` member.
class ScalacSessional[version <: Scalac.Versions, universe <: Universe]
  ( using system:   System,
          emission: Universe.Emission[universe],
          tactic:   Tactic[Compiler.Error],
          loggable: (CompileEvent is Loggable)^ )
extends Sessional:
  type Self = Scalac.Setup[version, universe]

  // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
  // existential, so returning it (or anything capturing it) from the block is a level
  // violation the capture checker rejects.
  type Result = ScalacSession^

  def session[result](target: Self)(lambda: (session: Result) ?=> result): result =
    val arguments: List[Text] =
      emission.flags :::
        List(t"-classpath", target.classpath()) :::
        target.scalac.commandLineArguments :::
        List(t"")

    // Nothing OS-level needs tearing down when the scope ends: the warm context is only
    // memory, reclaimed with the handle, which capture checking confines to the block.
    lambda(using ScalacSession(arguments))

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

import scala.util.control as suc

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import digression.*
import galilei.*
import gossamer.*
import hellenism.*
import org.jetbrains.kotlin.cli.common.ExitCode
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSourceLocation
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.jetbrains.kotlin.config.KotlinCompilerVersion
import org.jetbrains.kotlin.config.Services
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import vacuous.*

object Kotlinc:
  type Versions = 1.9 | 2.0 | 2.1 | 2.2 | 2.3 | 2.4

  case class Option[-version <: Versions](flags: Text*)

  // The version of the embedded compiler, as it reports itself: `2.4.10`.
  def version: Text = KotlinCompilerVersion.VERSION.nn.tt

// Drives the Kotlin compiler in-process, presenting the same interface as `Scalac` and `Javac`:
// named sources compile against an explicit classpath into an output directory, and the
// compiler's messages become `Compiler.Notice`s on the resulting `Compiler.Process`. Kotlin
// reads its sources from disk, so they are written to a scratch directory, beneath the temporary
// directory of the `System`, for the duration of the compilation, and diagnostics are mapped back
// onto the names they were given.
case class Kotlinc[version <: Kotlinc.Versions](options: List[Kotlinc.Option[version]]):
  def commandLineArguments: List[Text] = options.flatMap(_.flags)
  def version: Text = Kotlinc.version

  def apply(classpath: LocalClasspath)[path: Abstractable across Paths to Text]
    ( sources: Map[Text, Text], out: path )
    ( using System, Monitor, Probate )
  :   Compiler.Process logs Compiler.Event raises Compiler.Error =

    Log.info(Compiler.Event.Start)
    val process: Compiler.Process = Compiler.Process(version)

    // The scratch directory belongs to the task which compiles: it is made there, and deleted
    // there when the compilation ends, however it ends. Failing to make it is a crash.
    async:
      import filesystemBackends.javaBaseFilesystem
      import temporaryDirectories.systemTemporaryDirectory

      def crash(error: Throwable): Unit =
        Log.fail(Compiler.Event.CompilerCrash)
        process.put(Compiler.Result.Crash(error.stackTrace))

      try
        recover:
          case error: Io.Error => crash(error)

        . protect:
            temporaryDirectory[Path on Local].open[Scratch](Read & Write): handle ?=>
              val scratch: jnf.Path = jnf.Path.of(handle.stem.encode.s).nn
              compile(classpath, sources, out.generic, scratch, process)

      catch case suc.NonFatal(error) => crash(error)

    process

  // Writes `sources` beneath `scratch` and compiles them into `out`, reporting to `process`.
  private def compile
    ( classpath: LocalClasspath,
      sources:   Map[Text, Text],
      out:       Text,
      scratch:   jnf.Path,
      process:   Compiler.Process )
    ( using Compiler.Event is Loggable, System )
  :   Unit =

    // The name each source was given, keyed by the canonical path it was written to, so that a
    // diagnostic reads as if the compiler had seen the name rather than the scratch file.
    val names: Map[Text, Text] =
      sources.to[List].map: (name, code) =>
        val file = scratch.resolve(name.s).nn
        jnf.Files.createDirectories(file.getParent.nn)
        jnf.Files.writeString(file, code.s)

        (file.toRealPath().nn.toString.tt, name)

      . to[Map]

    val collector = new MessageCollector:
      def clear(): Unit = ()
      def hasErrors(): Boolean = process.errors > 0

      def report
        ( severity: CompilerMessageSeverity | Null,
          message:  String | Null,
          location: CompilerMessageSourceLocation | Null )
      :   Unit =

        val importance =
          if severity == null then Importance.Info
          else if severity.isError then Importance.Error
          else if severity.isWarning then Importance.Warning
          else Importance.Info

        // Only what the compiler says about the sources becomes a notice; its logging output,
        // which `-verbose` makes copious, would otherwise flood the stream.
        if importance != Importance.Info || location != null then
          val text: Text = if message == null then t"" else message.tt
          Log.fine(Compiler.Event.Notice(text))

          val file: Text =
            if location == null then t"unknown"
            else location.getPath.nn.tt.pipe: path => names(path).or(path)

          val span: Optional[Span] =
            if location == null || location.getLine < 1 then Unset else
              val line = location.getLine
              val column = location.getColumn
              val endLine = if location.getLineEnd < 1 then line else location.getLineEnd
              val endColumn = if location.getColumnEnd < 1 then column else location.getColumnEnd

              Span.area((line - 1).z, (column - 1).z, (endLine - 1).z, (endColumn - 1).z)

          process.put(Compiler.Notice(importance, file, text, span))

    // The Kotlin standard library is never implied: like every other classpath entry it is the
    // caller's to provide, which is what `-no-stdlib` makes so.
    val arguments: List[Text] =
      List(t"-no-stdlib", t"-classpath", classpath(), t"-d", out) +
        commandLineArguments +
        names.keys.to[List]

    Log.info(Compiler.Event.Running(t"kotlinc" :: arguments))

    process.put(Compiler.Progress(0.1, t"kotlinc"))
    jnf.Files.createDirectories(jnf.Paths.get(out.s))
    val compiler = K2JVMCompiler()
    val parsed = compiler.createArguments().nn
    // stdlib bridge: `parseArguments` takes a Java array of *nullable* `String`, and no
    // `ClassTag` witnesses a union element, so the native `to[Array]` cannot build it.
    compiler.parseArguments(arguments.map(_.s).stdlib.toArray, parsed)
    val exit = compiler.exec(collector, Services.EMPTY.nn, parsed).nn
    val success = exit == ExitCode.OK

    if success then process.put(Compiler.Progress(1.0, t"kotlinc"))
    process.put(if success then Compiler.Result.Success else Compiler.Result.Failure)

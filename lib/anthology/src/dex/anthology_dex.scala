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

import scala.collection.immutable as sci
import scala.jdk.CollectionConverters.*
import scala.util.control as suc

import com.android.tools.r8.{CompilationMode, D8, D8Command, Diagnostic, DiagnosticsHandler,
    OutputMode}

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import galilei.*
import gossamer.*
import hellenism.*
import parasite.*
import prepositional.*
import serpentine.*
import vacuous.*

// The dexing configuration `dexEdges`' tool folds settings into: the lowest Android API level
// the output must run on (lower levels oblige D8 to desugar more), D8's compilation mode, and
// optionally the Android platform stubs (`android.jar`) that ground desugaring decisions in
// the platform's real API surface.
private[anthology] case class DexConfiguration
  ( minApi:   Int,
    mode:     CompilationMode,
    platform: Optional[Path on Linux] )

object dexOptions:
  private def dex(edit: DexConfiguration => DexConfiguration): Setting =
    Setting[DexConfiguration](_ == Dex)(edit)

  // The lowest Android API level the artifact must run on. Levels below 24 lack native default
  // interface methods (which Scala 3 emits for trait members), so they additionally demand the
  // `platform` stubs to desugar against.
  def minApi(level: Int): Setting = dex(_.copy(minApi = level))

  // An Android SDK's platform stubs (`platforms/android-<n>/android.jar`), needed only below
  // API level 24 or when the program references platform types.
  def platform(jar: Path on Linux): Setting = dex(_.copy(platform = jar))

  object mode:
    val debug: Setting = dex(_.copy(mode = CompilationMode.DEBUG))
    val release: Setting = dex(_.copy(mode = CompilationMode.RELEASE))

// The dexing edge of a toolchain: `Classfile` to `Dex`, driving R8's D8 dexer (a JVM library)
// through its API to translate a compilation's classfiles, and its whole classpath with them,
// into an archive of `classes*.dex` files—the container ART and `DexClassLoader` consume, and
// the form the dexes of an APK are zipped from.
object dexEdges:
  def apply(): List[Edge] = List(Edge(Universe.Classfile, Dex, DexTool))

  // API level 26 (Android 8.0) is the earliest with native support for everything Scala 3
  // bytecode relies on, so dexing needs no platform stubs by default.
  private[anthology] def configuration: DexConfiguration =
    DexConfiguration(26, CompilationMode.DEBUG, Unset)

  private object DexTool extends Tool:
    type Settings = DexConfiguration

    def name: Text = t"dex"
    def initial: DexConfiguration = configuration

    def run
      ( settings:    DexConfiguration,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[LinkError], LinkEvent is Loggable )
    :   Deliverable =

      val (directory, classpath) = input.emission(Dex)
      Deliverable.Product(link0(settings, directory, classpath, out))

  // The dexing pipeline itself. DEX carries no entry-point metadata: ART is told its main
  // class at invocation, by the manifest of a hosting APK or the command line of `dalvikvm`,
  // so entry points—like a library JAR's—do not apply.
  private def link0
    ( form:      DexConfiguration,
      directory: Path on Linux,
      classpath: LocalClasspath,
      out:       Path on Linux )
  :   Path on Linux logs LinkEvent raises LinkError =

    val roots: sci.List[jnf.Path] =
      jnf.Paths.get(directory.encode.s).nn ::
        classpath.entries.stdlib.flatMap:
          case ClasspathEntry.Directory(directory) => sci.List(jnf.Paths.get(directory.s).nn)
          case ClasspathEntry.Jar(jar)             => sci.List(jnf.Paths.get(jar.s).nn)
          case _                                   => sci.Nil

    val (archives, directories) = roots.partition(jnf.Files.isRegularFile(_))

    // D8 accepts archives wholesale but not directories, whose classfiles are enumerated
    // individually; `module-info` classfiles are not program code and are excluded.
    val classfiles: sci.List[jnf.Path] = directories.flatMap: directory =>
      val walk = jnf.Files.walk(directory).nn

      try walk.iterator.nn.asScala.toList.filter: path =>
        path.toString.endsWith(".class") && !path.toString.endsWith("module-info.class")
      finally walk.close()

    object handler extends DiagnosticsHandler:
      override def error(diagnostic: Diagnostic | Null): Unit =
        Log.warn(LinkEvent.Message(diagnostic.nn.getDiagnosticMessage.nn.tt))

      override def warning(diagnostic: Diagnostic | Null): Unit =
        Log.warn(LinkEvent.Message(diagnostic.nn.getDiagnosticMessage.nn.tt))

      override def info(diagnostic: Diagnostic | Null): Unit =
        Log.info(LinkEvent.Message(diagnostic.nn.getDiagnosticMessage.nn.tt))

    try
      val outPath = jnf.Paths.get(out.encode.s).nn
      jnf.Files.createDirectories(outPath)

      val builder = D8Command.builder(handler).nn
      builder.addProgramFiles((archives ++ classfiles).asJava)
      builder.setMinApiLevel(form.minApi)
      builder.setMode(form.mode)
      builder.setOutput(outPath.resolve("main.dex.jar").nn, OutputMode.DexIndexed)

      form.platform.let: platform => builder.addLibraryFiles(jnf.Paths.get(platform.encode.s))

      D8.run(builder.build().nn)
      out / "main.dex.jar"

    catch case suc.NonFatal(error) =>
      abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))

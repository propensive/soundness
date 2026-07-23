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
package androidsample

import soundness.*

import ambience.systems.javaSystem
import logging.silentLogging
import stdios.virtualMachineStdio
import termcapDefinitions.basicTermcap
import probates.cancelProbate
import strategies.throwUnsafely
import threading.platformThreading

import java.nio.file.{Files, Paths}

// The build driver for the Android sample: it compiles `dice.scala` against the Android platform
// stubs and the facade runtime with anthology's `Scalac`, then links the compilation either as a
// bare `Artifact.Dex` (for the SDK-tool packaging path) or — with no Android SDK tool whatsoever
// — as a complete, signed `Artifact.Apk`.
//
// Arguments: <android.jar> <output directory> <app source file> <classpath file (one entry per
// line: the app's compile-and-dex runtime, excluding the platform)>.
object build:
  // The runtime classpath (facade machinery + Scala/Kotlin standard libraries) the app both
  // compiles against and is dexed with; the platform stubs are added for compilation only.
  def runtime(classpathFile: String): List[ClasspathEntry.Directory | ClasspathEntry.Jar] =
    Files.readAllLines(Paths.get(classpathFile)).nn.toArray.nn.to(List).map(_.toString).map:
      entry =>
        if entry.endsWith(".jar") then ClasspathEntry.Jar(entry.tt)
        else ClasspathEntry.Directory(entry.tt)

  // Compiles every `.scala` source in `sourceDir` and returns the compilation to link. The
  // platform stubs join the compile classpath but must not be dexed: on the device, the
  // operating system provides the platform.
  def compilation
    ( androidJar: String, outDir: String, sourceDir: String, classpathFile: String )
  :   Compilation[Universe.Classfile] =

    val sources: Map[Text, Text] =
      Files.list(Paths.get(sourceDir)).nn.toArray.nn.to(List).map(_.toString)
      . filter(_.endsWith(".scala")).map: path =>
          val name = Paths.get(path).nn.getFileName.nn.toString.tt
          name -> Files.readString(Paths.get(path)).nn.tt

      . to(Map)

    val entries = runtime(classpathFile)
    val platform: ClasspathEntry.Jar = ClasspathEntry.Jar(androidJar.tt)
    val compileClasspath = LocalClasspath((platform :: entries)*)

    val classes = Paths.get(outDir, "classes").nn
    Files.createDirectories(classes)
    val classesPath: Path on Linux = unsafely(classes.toString.tt.as[Path on Linux])

    supervise:
      val process =
        Scalac[3.8](List(scalacOptions.experimental))(compileClasspath)(sources, classesPath)

      process.complete() match
        case CompileResult.Success =>
          Out.println(t"compiled ${sources.size} sources")

        case other =>
          process.notices.each { notice => Out.println(notice.message) }
          Out.println(t"compilation failed: $other")
          sys.exit(1)

    Compilation(classesPath, LocalClasspath(entries*))

// Links the compilation as a bare DEX archive, packaged into an APK afterwards by the SDK tools.
@main
def buildDice(androidJar: String, outDir: String, sourceDir: String, classpathFile: String)
:   Unit =

  import dexLinkages.given
  val compiled = build.compilation(androidJar, outDir, sourceDir, classpathFile)
  val out: Path on Linux = unsafely(outDir.tt.as[Path on Linux])

  val artifact =
    Linker[Artifact.Dex](List(dexOptions.minApi(26), dexOptions.mode.release)).link(compiled, out)

  Out.println(t"linked $artifact")

// Links the compilation as a complete, signed APK — dexed, binary-manifested, zip-aligned and
// v2-signed entirely by Soundness, with no `aapt2`, `zipalign` or `apksigner`.
@main
def buildApk(androidJar: String, outDir: String, sourceDir: String, classpathFile: String)
:   Unit =

  import apkLinkages.given
  val compiled = build.compilation(androidJar, outDir, sourceDir, classpathFile)
  val out: Path on Linux = unsafely(outDir.tt.as[Path on Linux])

  val artifact =
    Linker[Artifact.Apk]
      ( List
          ( apkOptions.minApi(26),
            apkOptions.targetApi(36),
            apkOptions.packageName(t"dev.soundness.dice"),
            apkOptions.label(t"Dice Roller"),
            apkOptions.version(1, t"1.0"),
            apkOptions.permission(t"android.permission.VIBRATE") ),
        List(Linker.EntryPoint(Fqcn(t"dice.DiceActivity"))) )
    . link(compiled, out)

  Out.println(t"linked $artifact")

// A richer showcase app (`sketch.SketchActivity`) linked as a signed APK, exercising more of the
// Android API surface through the facades.
@main
def buildSketch(androidJar: String, outDir: String, sourceDir: String, classpathFile: String)
:   Unit =

  import apkLinkages.given
  val compiled = build.compilation(androidJar, outDir, sourceDir, classpathFile)
  val out: Path on Linux = unsafely(outDir.tt.as[Path on Linux])

  val artifact =
    Linker[Artifact.Apk]
      ( List
          ( apkOptions.minApi(26),
            apkOptions.targetApi(36),
            apkOptions.packageName(t"dev.soundness.sketch"),
            apkOptions.label(t"Sketch"),
            apkOptions.version(1, t"1.0") ),
        List(Linker.EntryPoint(Fqcn(t"sketch.SketchActivity"))) )
    . link(compiled, out)

  Out.println(t"linked $artifact")

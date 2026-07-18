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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import java.nio.file.{Files, Paths}

import org.scalajs.linker.interface.{ModuleKind, StandardConfig}

import soundness.*

import galilei.Linux.pathOnLinux
import logging.silentLogging
import probates.cancelProbate
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading
import workingDirectories.javaWorkingDirectory

private type JnfPath = java.nio.file.Path

// Unexecuted definitions whose successful compilation asserts the variance properties of
// `Compilation`: a portable compilation may be linked as any concrete portable backend.
def portableLinksAsJs(compilation: Compilation[Backend.Portable]): Compilation[Backend.Js] =
  compilation

def portableLinksAsWasi(compilation: Compilation[Backend.Portable]): Compilation[Backend.Wasi] =
  compilation

object Tests extends Suite(m"Anthology Tests"):
  def run(): Unit =
    test(m"A single-type-argument Scalac targets the JVM"):
      val scalac: Scalac[3.6, Backend.Jvm] = Scalac[3.6](Nil)
      scalac.commandLineArguments
    . assert(_ == Nil)

    test(m"Retargeting a Scalac preserves its options"):
      Scalac[3.8](List(scalacOptions.experimental)).targeting[Backend.Portable]
      . commandLineArguments
    . assert(_ == List(t"-experimental"))

    test(m"The JVM backend adds no compiler flags"):
      summon[Backend.Emission[Backend.Jvm]].flags
    . assert(_ == Nil)

    test(m"Every portable backend adds the -scalajs flag"):
      List
        ( summon[Backend.Emission[Backend.Js]].flags,
          summon[Backend.Emission[Backend.Wasm]].flags,
          summon[Backend.Emission[Backend.Wasi]].flags,
          summon[Backend.Emission[Backend.Portable]].flags )
    . assert(_.forall(_ == List(t"-scalajs")))

    test(m"Module-kind options configure the linker"):
      linkerOptions.moduleKind.commonJs.edit(StandardConfig()).asInstanceOf[StandardConfig]
      . moduleKind
    . assert(_ == ModuleKind.CommonJSModule)

    test(m"The JavaScript linkage produces an ES module"):
      summon[Linkage[Backend.Js]].initial.asInstanceOf[StandardConfig].moduleKind
    . assert(_ == ModuleKind.ESModule)

    test(m"The browser Wasm linkage enables the WebAssembly backend"):
      summon[Linkage[Backend.Wasm]].initial.asInstanceOf[StandardConfig]
      . esFeatures.useWebAssembly
    . assert(_ == true)

    test(m"A module-kind option is not applicable to a Wasm linker"):
      demilitarize:
        Linker[Backend.Wasm](List(linkerOptions.moduleKind.esModule))
    . assert(_.nonEmpty)

    test(m"A WASI linkage is not available without toolchain and WIT world"):
      demilitarize:
        summon[Linkage[Backend.Wasi]]
    . assert(_.nonEmpty)

    test(m"A JVM compilation cannot be linked"):
      demilitarize:
        val compilation: Compilation[Backend.Jvm] = ???
        portableLinksAsJs(compilation)
    . assert(_.nonEmpty)

    test(m"A portable compilation is not a native compilation"):
      demilitarize:
        val compilation: Compilation[Backend.Portable] = ???
        val native: Compilation[Backend.Native] = compilation
    . assert(_.nonEmpty)

    test(m"A native option is not applicable to a JavaScript linker"):
      demilitarize:
        Linker[Backend.Js](List(nativeOptions.gc.immix))
    . assert(_.nonEmpty)

    test(m"An sjsir option is not applicable to a native linker"):
      demilitarize:
        Linker[Backend.Native](List(linkerOptions.optimize.fast))
    . assert(_.nonEmpty)

    test(m"A native linkage is not available without probing the C toolchain"):
      demilitarize:
        summon[Linkage[Backend.Native]]
    . assert(_.nonEmpty)

    test(m"Compiling for the native backend requires plugin evidence"):
      demilitarize:
        summon[Backend.Emission[Backend.Native]]
    . assert(_.nonEmpty)

    test(m"Triples render as LLVM target triples"):
      Triple.Arm64MacOs.text
    . assert(_ == t"arm64-apple-darwin")

    test(m"A portable compilation cannot be bundled as an executable JAR"):
      demilitarize:
        val compilation: Compilation[Backend.Portable] = ???
        Bundler.bundle(compilation, Unset, Unset)
    . assert(_.nonEmpty)

    test(m"A JVM compilation cannot be bundled as an sjsir library"):
      demilitarize:
        val compilation: Compilation[Backend.Jvm] = ???
        Bundler.library(compilation, Unset)
    . assert(_.nonEmpty)

    // An end-to-end exercise of the portable pipeline—compile with `-scalajs`, then link as
    // JavaScript—which runs only when a cached proscala toolchain (whose distribution includes
    // the Scala.js runtime JARs) can be found.
    val source: Text =
      t"""|object Main:
          |  def main(args: Array[String]): Unit = println("hello")
          |""".s.stripMargin.tt

    sjsClasspath().let: classpath =>
      supervise:
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val process =
          Scalac[3.8](Nil).targeting[Backend.Portable]
            (classpath)(Map(t"hello.scala" -> source), out)

        test(m"A portable compilation succeeds"):
          process.complete()
        . assert(_ == CompileResult.Success)

        test(m"A portable compilation emits sjsir"):
          Files.list(Paths.get(out.encode.s)).nn.iterator.nn.asScala
          . exists(_.getFileName.nn.toString.endsWith(".sjsir"))
        . assert(_ == true)

        val linked: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

        test(m"Linking as JavaScript produces a nonempty main.js"):
          Linker[Backend.Js]
            ( List(linkerOptions.moduleKind.esModule),
              List(Linker.EntryPoint(Fqcn(t"Main"))) )
          . link(Compilation(out, classpath), linked)
          . pipe: artifact =>
              Files.size(Paths.get(artifact.encode.s))
        . assert(_ > 100L)

    // The native counterpart—compile with the Scala Native plugin, link with clang, and run the
    // binary—which runs only when the plugin and runtime JARs are cached and clang is present.
    nativeSetup().let: (plugin, classpath) =>
      supervise:
        given NirPlugin = plugin
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val process =
          Scalac[3.8](Nil).targeting[Backend.Native]
            (classpath)(Map(t"hello.scala" -> source), out)

        test(m"A native compilation succeeds"):
          process.complete()
        . assert(_ == CompileResult.Success)

        test(m"A native compilation emits nir"):
          Files.list(Paths.get(out.encode.s)).nn.iterator.nn.asScala
          . exists(_.getFileName.nn.toString.endsWith(".nir"))
        . assert(_ == true)

        safely(NativeLinkage()).let: linkage =>
          given Linkage[Backend.Native] = linkage
          val linked: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

          test(m"Linking natively produces a runnable binary"):
            Linker[Backend.Native](Nil, List(Linker.EntryPoint(Fqcn(t"Main"))))
            . link(Compilation(out, classpath), linked)
            . pipe: artifact =>
                mute[ExecEvent](sh"$artifact".exec[Text]()).trim
          . assert(_ == t"hello")

  // Locates a cached proscala release's `lib` directory, which carries the fork standard
  // library and the Scala.js runtime JARs.
  def proscalaLibrary(): Optional[JnfPath] =
    val home = java.lang.System.getProperty("user.home").nn
    val root = Paths.get(home, ".cache", "soundness", "proscala").nn

    if !Files.isDirectory(root) then Unset else
      Files.list(root).nn.iterator.nn.asScala.to(List).sortBy(_.toString).reverse
      . map(_.resolve("lib").nn)
      . find { lib => Files.isDirectory(lib) && Files.exists(lib.resolve("scala3-library.jar")) }
      . getOrElse(Unset)

  // Yields the runtime JARs a portable compilation needs as a classpath, when cached.
  def sjsClasspath(): Optional[LocalClasspath] =
    val fixed: List[Text] =
      List
        ( t"scala-library.jar",
          t"scala3-library.jar",
          t"scala3-library_sjs1.jar",
          t"scalajs-scalalib_2.13.jar" )

    proscalaLibrary().let: lib =>
      def contents = Files.list(lib).nn.iterator.nn.asScala

      val complete =
        fixed.forall { name => Files.exists(lib.resolve(name.s)) }
        && contents.exists(_.getFileName.nn.toString.startsWith("scalajs-javalib"))
        && contents.exists(_.getFileName.nn.toString.startsWith("scalajs-library_2.13"))

      if !complete then Unset else
        val globbed = Files.list(lib).nn.iterator.nn.asScala.to(List).filter: jar =>
          val name = jar.getFileName.nn.toString
          name.startsWith("scalajs-javalib") || name.startsWith("scalajs-library_2.13")

        val jars = fixed.map { name => lib.resolve(name.s).nn } ++ globbed
        LocalClasspath(jars.map { jar => ClasspathEntry.Jar(jar.toString.tt) }*)

  // Yields the Scala Native compiler plugin and the runtime JARs a native compilation needs
  // (from the coursier cache, alongside the fork standard library), when all are present.
  def nativeSetup(): Optional[(NirPlugin, LocalClasspath)] =
    val home = java.lang.System.getProperty("user.home").nn

    val caches =
      List
        ( Paths.get(home, "Library", "Caches", "Coursier", "v1").nn,
          Paths.get(home, ".cache", "coursier", "v1").nn )

    val artifacts: List[Text] =
      List
        ( t"nscplugin_3.9.0-RC1/0.5.12/nscplugin_3.9.0-RC1-0.5.12.jar",
          t"scala3lib_native0.5_3/3.9.0-RC1%2B0.5.12/scala3lib_native0.5_3-3.9.0-RC1%2B0.5.12.jar",
          t"scalalib_native0.5_3/3.9.0-RC1%2B0.5.12/scalalib_native0.5_3-3.9.0-RC1%2B0.5.12.jar",
          t"javalib_native0.5_3/0.5.12/javalib_native0.5_3-0.5.12.jar",
          t"auxlib_native0.5_3/0.5.12/auxlib_native0.5_3-0.5.12.jar",
          t"clib_native0.5_3/0.5.12/clib_native0.5_3-0.5.12.jar",
          t"posixlib_native0.5_3/0.5.12/posixlib_native0.5_3-0.5.12.jar",
          t"nativelib_native0.5_3/0.5.12/nativelib_native0.5_3-0.5.12.jar" )

    proscalaLibrary().let: proscala =>
      caches
      . map(_.resolve("https/repo1.maven.org/maven2/org/scala-native").nn)
      . find { base => artifacts.forall { jar => Files.exists(base.resolve(jar.s)) } }
      . map: base =>
          val jars = artifacts.map { jar => base.resolve(jar.s).nn }
          val stdlib = List("scala-library.jar", "scala3-library.jar").map(proscala.resolve(_).nn)
          val plugin = unsafely(jars.head.toString.tt.as[soundness.Path on Linux])

          val classpath =
            LocalClasspath
              ((jars.tail ++ stdlib).map { jar => ClasspathEntry.Jar(jar.toString.tt) }*)

          (NirPlugin(plugin), classpath)

      . getOrElse(Unset)

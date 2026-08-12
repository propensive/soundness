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

import java.nio.file.{Files, Paths}

import org.scalajs.linker.interface.{ModuleKind, StandardConfig}

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import galilei.Linux.pathOnLinux
import logging.silentLogging
import probates.cancelProbate
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading
import workingDirectories.javaWorkingDirectory

private type JnfPath = java.nio.file.Path

// Inert graph fixtures for toolchain tests: parameterized nodes and pass-through tools which
// record their executions in `executionLog` and never touch the filesystem.
case class TestIr(id: Text) extends anthology.Format.Ir
case class TestApp(id: Text) extends anthology.Format.Application

object executionLog:
  @scala.caps.unsafe.untrackedCaptures
  var entries: List[Text] = Nil

def passTool(label: Text): Tool = new Tool:
  type Settings = Unit

  def name: Text = label
  def initial: Unit = ()

  def run
    ( settings:    Unit,
      input:       Deliverable,
      entryPoints: List[EntryPoint],
      out:         soundness.Path on Linux )
    ( using Monitor, System, WorkingDirectory )
    ( using Tactic[LinkError], (LinkEvent is Loggable)^ )
  :   Deliverable =

    executionLog.entries = label :: executionLog.entries
    input

// The default settings of the tool on the edge producing `target`, for assertions on the
// defaults each edge provider declares.
def initialOf(edges: List[Edge], target: anthology.Format): Any =
  edges.stdlib.find(_.target == target).get.tool.initial

object Tests extends Suite(m"Anthology Tests"):
  def run(): Unit =
    test(m"A single-type-argument Scalac targets the classfile universe"):
      val scalac: Scalac[3.6, Universe.Classfile] = Scalac[3.6](Nil)
      scalac.commandLineArguments
    . assert(_ == Nil)

    test(m"Retargeting a Scalac preserves its options"):
      Scalac[3.8](List(scalacOptions.experimental)).targeting[Universe.Sjsir]
      . commandLineArguments
    . assert(_ == List(t"-experimental"))

    test(m"The classfile universe adds no compiler flags"):
      summon[Universe.Emission[Universe.Classfile]].flags
    . assert(_ == Nil)

    test(m"The sjsir universe adds the -scalajs flag"):
      summon[Universe.Emission[Universe.Sjsir]].flags
    . assert(_ == List(t"-scalajs"))

    test(m"The module system is part of the JavaScript node's identity"):
      initialOf(sjsEdges(), anthology.Js(anthology.Js.Module.CommonJs))
      . asInstanceOf[StandardConfig].moduleKind
    . assert(_ == ModuleKind.CommonJSModule)

    test(m"The ES-module edge produces an ES module"):
      initialOf(sjsEdges(), anthology.Js(anthology.Js.Module.Es))
      . asInstanceOf[StandardConfig].moduleKind
    . assert(_ == ModuleKind.ESModule)

    test(m"The browser Wasm edge enables the WebAssembly backend"):
      initialOf(sjsEdges(), anthology.Wasm).asInstanceOf[StandardConfig]
      . esFeatures.useWebAssembly
    . assert(_ == true)

    test(m"The dex edge defaults to API level 26"):
      initialOf(dexEdges(), Dex).asInstanceOf[DexConfiguration].minApi
    . assert(_ == 26)

    // The toolchain DAG: path search and validation, checked without invoking any tool.
    val android = Toolchain(jarEdges(), dexEdges(), apkEdges())

    test(m"The classfile-to-APK path routes through dex"):
      android.path(Universe.Classfile, Apk).map(_.target.id)
    . assert(_ == List(t"dex", t"apk"))

    test(m"An executable JAR links directly from classfiles"):
      android.path(Universe.Classfile, anthology.Jar).map(_.tool.name)
    . assert(_ == List(t"jar"))

    test(m"A format is a zero-length path from itself"):
      android.path(Dex, Dex)
    . assert(_ == Nil)

    test(m"No path exists against the direction of the edges"):
      capture[LinkError](android.path(Apk, Universe.Classfile)).reason
    . assert(_ == LinkError.Reason.NoPath(t"apk", t"classfile"))

    test(m"An unregistered edge leaves its format unreachable"):
      capture[LinkError](Toolchain(jarEdges()).path(Universe.Classfile, Dex)).reason
    . assert(_ == LinkError.Reason.NoPath(t"classfile", t"dex"))

    test(m"Duplicate edges between the same formats are rejected"):
      capture[LinkError](Toolchain(dexEdges(), dexEdges())).reason
    . assert(_ == LinkError.Reason.DuplicateEdge(t"classfile", t"dex"))

    test(m"A cyclic toolchain is rejected"):
      val a = TestIr(t"a")
      val b = TestIr(t"b")
      val cycle = List(Edge(a, b, passTool(t"ab")), Edge(b, a, passTool(t"ba")))
      capture[LinkError](Toolchain(cycle)).reason
    . assert(_ == LinkError.Reason.CyclicToolchain)

    test(m"Two shortest paths between formats are ambiguous"):
      val a = TestIr(t"a")
      val b1 = TestIr(t"b1")
      val b2 = TestIr(t"b2")
      val c = TestApp(t"c")

      val diamond =
        Toolchain
          ( List
              ( Edge(a, b1, passTool(t"a-b1")),
                Edge(a, b2, passTool(t"a-b2")),
                Edge(b1, c, passTool(t"b1-c")),
                Edge(b2, c, passTool(t"b2-c")) ) )

      capture[LinkError](diamond.path(a, c)).reason
    . assert(_ == LinkError.Reason.AmbiguousPath(t"a", t"c"))

    test(m"Native binaries for different triples are distinct formats"):
      Binary(Triple.Arm64MacOs) == Binary(Triple.X64Linux)
    . assert(_ == false)

    supervise:
      val a = TestIr(t"a")
      val b = TestIr(t"b")
      val c = TestApp(t"c")

      val chain =
        Toolchain(List(Edge(a, b, passTool(t"first")), Edge(b, c, passTool(t"second"))))

      val scratch: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

      test(m"Producing a format runs each path edge's tool in order"):
        executionLog.entries = Nil
        chain.produce(Deliverable.Product(scratch), a, c, scratch)
        executionLog.entries
      . assert(_ == List(t"second", t"first"))

      test(m"A setting applying to no path format is rejected"):
        val inapplicable = Setting[Unit](_ => false)(settings => settings)
        val production = Deliverable.Product(scratch)
        capture[LinkError](chain.produce(production, a, c, scratch, List(inapplicable))).reason
      . assert(_ == LinkError.Reason.InapplicableSetting)

      // Cross-family settings are rejected before any tool runs, so these paths are checkable
      // without invoking D8, the Scala.js linker or the bundler.
      val emission = Deliverable.Emission(scratch, LocalClasspath())

      test(m"A dex setting is not applicable on a JAR path"):
        val settings = List(dexOptions.minApi(24))
        val toolchain = Toolchain(jarEdges())

        capture[LinkError]
          ( toolchain.produce(emission, Universe.Classfile, anthology.Jar, scratch, settings) )
        . reason
      . assert(_ == LinkError.Reason.InapplicableSetting)

      test(m"An sjs setting is not applicable on a dex path"):
        val settings = List(linkerOptions.optimize.fast)
        val toolchain = Toolchain(dexEdges())

        capture[LinkError](toolchain.produce(emission, Universe.Classfile, Dex, scratch, settings))
        . reason
      . assert(_ == LinkError.Reason.InapplicableSetting)

      test(m"A native setting is not applicable on a JavaScript path"):
        val settings = List(nativeOptions.gc.immix)
        val target = anthology.Js(anthology.Js.Module.Es)
        val toolchain = Toolchain(sjsEdges())

        capture[LinkError](toolchain.produce(emission, Universe.Sjsir, target, scratch, settings))
        . reason
      . assert(_ == LinkError.Reason.InapplicableSetting)

      // The xeq packaging edges, exercised up to (but never through) `Packager.pack`'s
      // filesystem and network work: every case below aborts before any stub is read.
      val bundles = Toolchain(xeqEdges())
      val jarInput = Deliverable.Product(scratch)

      test(m"An xeq bundle requires a runner source"):
        val target = anthology.Xeq(ziggurat.Packaging.Delivery.EmbedAll)
        capture[LinkError](bundles.produce(jarInput, anthology.Jar, target, scratch)).reason
      . assert(_ == LinkError.Reason.MissingSetting(t"runners"))

      test(m"A local runner source cannot imply the targets"):
        val target = anthology.Xeq(ziggurat.Packaging.Delivery.EmbedAll)
        val settings = List(xeqOptions.runners.local(scratch))

        capture[LinkError](bundles.produce(jarInput, anthology.Jar, target, scratch, settings))
        . reason
      . assert(_ == LinkError.Reason.MissingSetting(t"targets"))

      test(m"Native delivery requires exactly one target"):
        val target = anthology.Xeq(ziggurat.Packaging.Delivery.Native)
        val settings = List(xeqOptions.runners.standard)

        capture[LinkError](bundles.produce(jarInput, anthology.Jar, target, scratch, settings))
        . reason match
            case LinkError.Reason.Packaging(_) => true
            case _                             => false
      . assert(_ == true)

    test(m"JAR and library packaging edges need no evidence"):
      jarEdges().map(_.target.id)
    . assert(_ == List(t"jar", t"library-classfile", t"library-sjsir", t"library-nir"))

    test(m"A library JAR's universe must match its compilation's"):
      capture[LinkError](Toolchain(jarEdges()).path(Universe.Sjsir, Library(Universe.Nir))).reason
    . assert(_ == LinkError.Reason.NoPath(t"sjsir", t"library-nir"))

    test(m"An sjsir compilation cannot be packaged as an executable JAR"):
      capture[LinkError](Toolchain(jarEdges()).path(Universe.Sjsir, anthology.Jar)).reason
    . assert(_ == LinkError.Reason.NoPath(t"sjsir", t"jar"))

    test(m"A classfile compilation cannot be linked as JavaScript"):
      val target = anthology.Js(anthology.Js.Module.Es)
      capture[LinkError](Toolchain(sjsEdges()).path(Universe.Classfile, target)).reason
    . assert(_ == LinkError.Reason.NoPath(t"classfile", t"js-es"))

    test(m"WASI 0.3 is unreachable without an edge producing it"):
      val target = Wasi(Wasi.Version.Wasip3)
      capture[LinkError](Toolchain(sjsEdges()).path(Universe.Sjsir, target)).reason
    . assert(_ == LinkError.Reason.NoPath(t"sjsir", t"wasip3"))

    // The `component` and `wasm-object` universes are nodes without edges: LIRA's registry
    // names them, and composition tools will register into them, but no Soundness tool yet
    // produces or consumes either.
    test(m"The component universe is a node awaiting edges"):
      capture[LinkError](Toolchain(sjsEdges()).path(Universe.Sjsir, Component)).reason
    . assert(_ == LinkError.Reason.NoPath(t"sjsir", t"component"))

    test(m"The wasm-object universe is a node awaiting edges"):
      capture[LinkError](Toolchain(sjsEdges()).path(WasmObject, Wasi(Wasi.Version.Wasip1))).reason
    . assert(_ == LinkError.Reason.NoPath(t"wasm-object", t"wasip1"))

    test(m"The AXML encoder emits the binary-XML chunk header"):
      val axml = Axml.encode(Axml.Element(t"manifest", Nil, Nil))
      List(axml.readable(0), axml.readable(1), axml.readable(2), axml.readable(3)).map(_.toInt & 0xff)
    . assert(_ == List(0x03, 0x00, 0x08, 0x00))

    test(m"The AXML total-size field equals the encoded length"):
      val axml = Axml.encode(Axml.Element(t"manifest", Nil, Nil))
      def u8(index: Int): Int = axml.readable(index).toInt & 0xff
      val declared = u8(4) | (u8(5) << 8) | (u8(6) << 16) | (u8(7) << 24)
      declared == axml.readable.length
    . assert(_ == true)

    test(m"The APK edge defaults to API level 26"):
      initialOf(apkEdges(), Apk).asInstanceOf[Apk.Configuration].minApi
    . assert(_ == 26)

    test(m"The APK API level configures both dexing and packaging"):
      val setting = apkOptions.minApi(24)

      ( setting.edit(Dex, initialOf(dexEdges(), Dex)).asInstanceOf[DexConfiguration].minApi,
        setting.edit(Apk, initialOf(apkEdges(), Apk)).asInstanceOf[Apk.Configuration].minApi )
    . assert(_ == (24, 24))

    test(m"The WASI component edge is not available without toolchain and WIT world"):
      demilitarize:
        sjsEdges.wasi()
    . assert(_.nonEmpty)

    test(m"A well-formed compiler session block compiles cleanly"):
      demilitarize:
        val classpath: LocalClasspath = ???

        Scalac[3.8](Nil).on(classpath).session:
          val process = compilation.compile(Map(t"a.scala" -> t"class A"))
          process.errors

        ()
    . assert(_ == Nil)

    // Aspirational: the handle's fresh capability and the borrow expressed in `compile`'s
    // result type record the intent, but rejecting these statically awaits further
    // separation-checker support (as telekinesis's `HttpSession.fetch` notes for its own
    // borrow).
    test(m"A compiler session handle cannot escape its scope"):
      demilitarize:
        val classpath: LocalClasspath = ???
        val leaked = Scalac[3.8](Nil).on(classpath).session(compilation)
      . map(_.message)
    . aspire(_.nonEmpty)

    test(m"A live compile process cannot span a further compile"):
      demilitarize:
        val classpath: LocalClasspath = ???

        Scalac[3.8](Nil).on(classpath).session:
          val process1 = compilation.compile(Map(t"a.scala" -> t"class A"))
          val process2 = compilation.compile(Map(t"b.scala" -> t"class B"))
          process1.errors
    . aspire(_.nonEmpty)

    // The OCI edge itself needs only a WIT world (the component is linked by the preceding
    // edge, whose provider probes the WASI toolchain), so its graph shape is checkable without
    // `wasm-tools` installed.
    val ociToolchain =
      val world = WitWorld(unsafely(temporaryDirectory / Uuid()), t"main")
      Toolchain(ociEdges()(using world))

    test(m"An OCI image is unreachable from a classfile compilation"):
      capture[LinkError](ociToolchain.path(Universe.Classfile, OciImage)).reason
    . assert(_ == LinkError.Reason.NoPath(t"classfile", t"oci"))

    test(m"An OCI image is unreachable without the component edge"):
      capture[LinkError](ociToolchain.path(Universe.Sjsir, OciImage)).reason
    . assert(_ == LinkError.Reason.NoPath(t"sjsir", t"oci"))

    test(m"An OCI image config defaults to the wasm/wasip2 platform"):
      val config = OciConfiguration()
      (config.architecture, config.os)
    . assert(_ == (t"wasm", t"wasip2"))

    test(m"An sjsir compilation is not a native compilation"):
      demilitarize:
        val compilation: Compilation[Universe.Sjsir] = ???
        val native: Compilation[Universe.Nir] = compilation
    . assert(_.nonEmpty)

    test(m"Compiling into the nir universe requires plugin evidence"):
      demilitarize:
        summon[Universe.Emission[Universe.Nir]]
    . assert(_.nonEmpty)

    test(m"Triples render as LLVM target triples"):
      Triple.Arm64MacOs.text
    . assert(_ == t"arm64-apple-darwin")

    // An end-to-end exercise of the portable pipeline—compile with `-scalajs`, then link as
    // JavaScript—which runs only when a cached proscala toolchain (whose distribution includes
    // the Scala.js runtime JARs) can be found.
    val source: Text =
      t"""|object Main:
          |  def main(args: scala.Array[String]): Unit = println("hello")
          |""".s.stripMargin.tt

    sjsClasspath().let: classpath =>
      supervise:
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val process =
          Scalac[3.8](Nil).targeting[Universe.Sjsir]
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
          Toolchain(sjsEdges()).produce
            ( Deliverable.Emission(out, classpath),
              Universe.Sjsir,
              anthology.Js(anthology.Js.Module.Es),
              linked,
              Nil,
              List(EntryPoint(Fqcn(t"Main"))) )
          . pipe: artifact =>
              Files.size(Paths.get(artifact.encode.s))
        . assert(_ > 100L)

        test(m"Packaging an sjsir library JAR produces a nonempty archive"):
          Toolchain(jarEdges()).produce
            ( Deliverable.Emission(out, classpath),
              Universe.Sjsir,
              Library(Universe.Sjsir),
              linked )
          . pipe: artifact =>
              Files.size(Paths.get(artifact.encode.s))
        . assert(_ > 100L)

    // The packaging pipeline: compile against the fork standard library alone, link an
    // executable JAR, and run it under `java -jar`.
    proscalaLibrary().let: lib =>
      supervise:
        val jars = List("scala-library.jar", "scala3-library.jar").map(lib.resolve(_).nn)
        val classpath = LocalClasspath(jars.map { jar => Classpath.Entry.Jar(jar.toString.tt) }*)
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val process = Scalac[3.8](Nil)(classpath)(Map(t"hello.scala" -> source), out)

        test(m"A classfile compilation succeeds"):
          process.complete()
        . assert(_ == CompileResult.Success)

        val linked: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

        test(m"Linking a JAR produces a runnable artifact"):
          Toolchain(jarEdges()).produce
            ( Deliverable.Emission(out, classpath),
              Universe.Classfile,
              anthology.Jar,
              linked,
              List(jarOptions.name(t"app.jar")),
              List(EntryPoint(Fqcn(t"Main"))) )
          . pipe: artifact =>
              mute[Exec.Event](sh"java -jar $artifact".exec[Text]()).trim
        . assert(_ == t"hello")

        // The whole point of source nodes: one path from `.scala` text to a runnable JAR, with
        // the compiler and the bundler both selected by the path rather than named by the caller.
        test(m"One path compiles Scala source and runs the JAR it produces"):
          val toolchain = Toolchain(List(scalacEdges.classfile(Scalac[3.8](Nil))), jarEdges())
          val staged: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

          toolchain.produce
            ( Deliverable.Sources(Map(t"hello.scala" -> source), classpath),
              Language.Scala,
              anthology.Jar,
              staged,
              List(jarOptions.name(t"whole.jar")),
              List(EntryPoint(Fqcn(t"Main"))) )

          . pipe: artifact =>
              mute[Exec.Event](sh"java -jar $artifact".exec[Text]()).trim
        . assert(_ == t"hello")

        test(m"A compile edge reports a failing compilation as an error count"):
          val toolchain = Toolchain(List(scalacEdges.classfile(Scalac[3.8](Nil))))
          val staged: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
          val bad = Map(t"bad.scala" -> t"class Bad:\n  def x: Int = \"nope\"\n")

          capture[LinkError]
            ( toolchain.produce
                ( Deliverable.Sources(bad, classpath),
                  Language.Scala,
                  Universe.Classfile,
                  staged ) )

          . reason match
              case LinkError.Reason.CompilationFailed(errors) => errors > 0
              case _                                          => false
        . assert(_ == true)

        test(m"Linking as DEX produces an archive containing classes.dex"):
          Toolchain(dexEdges()).produce
            ( Deliverable.Emission(out, classpath), Universe.Classfile, Dex, linked )
          . pipe: artifact =>
              val zipfile = java.util.zip.ZipFile(artifact.encode.s)

              try zipfile.entries.nn.asScala.exists(_.getName == "classes.dex")
              finally zipfile.close()
        . assert(_ == true)

        // Warm-session compilations: one retained compiler context across several compiles.
        val alpha = Map(t"alpha.scala" -> t"class Alpha:\n  def x: Int = 42\n")
        val beta = Map(t"beta.scala" -> t"class Beta:\n  def alpha: Alpha = Alpha()\n")

        test(m"A session's second compile sees the first compile's symbols"):
          Scalac[3.8](Nil).on(classpath).session:
            alpha.compile().complete()
            compilation.compile(beta).complete()
        . assert(_ == CompileResult.Success)

        test(m"A failed compile leaves the session usable"):
          Scalac[3.8](Nil).on(classpath).session:
            val bad = Map(t"gamma.scala" -> t"class Gamma:\n  def x: Int = \"nope\"\n")
            val failure = bad.compile().complete()
            (failure, alpha.compile().complete())
        . assert(_ == (CompileResult.Failure, CompileResult.Success))

        test(m"A session compile exposes its classfiles in memory"):
          Scalac[3.8](Nil).on(classpath).session:
            val process = alpha.compile()
            process.complete()
            process.classfiles.stdlib.contains(t"/Alpha.class".as[Path on Classpath])
        . assert(_ == true)

        test(m"A session compile's updates report progress and completion"):
          Scalac[3.8](Nil).on(classpath).session:
            val process = alpha.compile()
            process.complete()

            var progressed: Int = 0

            process.updates.records.each:
              case CompileProcess.Update.Progressed(_) => progressed += 1
              case CompileProcess.Update.Noticed(_)    => ()

            progressed > 0
        . assert(_ == true)

        val saved: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(saved.encode.s))

        test(m"Saved session output appears on disk"):
          Scalac[3.8](Nil).on(classpath).session:
            alpha.compile().complete()
            val process = beta.compile()
            process.complete()
            process.save(saved)

          Files.exists(Paths.get(saved.encode.s).nn.resolve("Beta.class"))
        . assert(_ == true)

    // The native counterpart—compile with the Scala Native plugin, link with clang, and run the
    // binary—which runs only when the plugin and runtime JARs are cached and clang is present.
    nativeSetup().let: (plugin, classpath) =>
      supervise:
        given NirPlugin = plugin
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val process =
          Scalac[3.8](Nil).targeting[Universe.Nir]
            (classpath)(Map(t"hello.scala" -> source), out)

        test(m"A native compilation succeeds"):
          process.complete()
        . assert(_ == CompileResult.Success)

        test(m"A native compilation emits nir"):
          Files.list(Paths.get(out.encode.s)).nn.iterator.nn.asScala
          . exists(_.getFileName.nn.toString.endsWith(".nir"))
        . assert(_ == true)

        safely(nativeEdges()).let: edges =>
          val linked: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

          test(m"Linking natively produces a runnable binary"):
            val host =
              Triple.host.or(panic(m"the host triple always resolves on a supported platform"))

            Toolchain(edges).produce
              ( Deliverable.Emission(out, classpath),
                Universe.Nir,
                Binary(host),
                linked,
                Nil,
                List(EntryPoint(Fqcn(t"Main"))) )
            . pipe: artifact =>
                mute[Exec.Event](sh"$artifact".exec[Text]()).trim
          . assert(_ == t"hello")

    // `Kotlinc` itself is not constructed here: linking it resolves the compiler classes, which
    // are a compile-only dependency, so the options are checked through the flags they carry.
    test(m"Kotlinc options carry their command-line flags"):
      List(kotlincOptions.warnings.asErrors, kotlincOptions.jvmTarget(17)).flatMap(_.flags)
    . assert(_ == List(t"-Werror", t"-jvm-target", t"17"))

    test(m"A Kotlin 2 option does not apply to a Kotlin 1.9 compiler"):
      demilitarize:
        Kotlinc[1.9](List(kotlincOptions.warnings.extra))
    . assert(_.nonEmpty)

    // An end-to-end Kotlin compilation, which runs only where the compiler it drives is on the
    // classpath. It is a compile-only dependency of the `kotlin` component — the caller supplies
    // the compiler, as with `Scalac` — so this suite skips these tests rather than carrying
    // 28,000 classfiles into every assembly built from it.
    kotlinToolchain().let: stdlib =>
      supervise:
        val classpath = LocalClasspath(List(Classpath.Entry.Jar(stdlib))*)
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())

        val greeting: Text =
          t"""|package demo
              |
              |fun greet(): String = "hello"
              |""".s.stripMargin.tt

        val process = Kotlinc[2.4](Nil)(classpath)(Map(t"demo/Greeting.kt" -> greeting), out)

        test(m"A Kotlin compilation succeeds"):
          process.complete()
        . assert(_ == CompileResult.Success)

        test(m"A Kotlin compilation emits classfiles"):
          Files.list(Paths.get(out.encode.s, "demo")).nn.iterator.nn.asScala
          . exists(_.getFileName.nn.toString == "GreetingKt.class")
        . assert(_ == true)

        val broken: Text =
          t"""|package demo
              |
              |fun broken(): Int = "not an integer"
              |""".s.stripMargin.tt

        val out2: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        val failing = Kotlinc[2.4](Nil)(classpath)(Map(t"demo/Broken.kt" -> broken), out2)

        test(m"A Kotlin compilation with a type error fails"):
          failing.complete()
        . assert(_ == CompileResult.Failure)

        test(m"A Kotlin error is counted"):
          failing.errors
        . assert(_ > 0)

        test(m"A Kotlin notice names the source it was given"):
          failing.notices.map(_.file).to[List]
        . assert(_ == List(t"demo/Broken.kt"))

  // Locates the Kotlin standard library on this suite's classpath, when the Kotlin compiler is
  // there to be driven. The compiler never implies the standard library, so a compilation must be
  // given it explicitly.
  def kotlinToolchain(): Optional[Text] =
    val compiler =
      try Class.forName("org.jetbrains.kotlin.cli.jvm.K2JVMCompiler") != null
      catch case _: ClassNotFoundException => false

    if !compiler then Unset else
      java.lang.System.getProperty("java.class.path").nn.tt
      . cut(java.io.File.pathSeparator.nn.tt)
      . filter(_.contains(t"kotlin-stdlib"))
      . prim

  // Locates a cached proscala release's `lib` directory, which carries the fork standard
  // library and the Scala.js runtime JARs.
  def proscalaLibrary(): Optional[JnfPath] =
    val home = java.lang.System.getProperty("user.home").nn
    val root = Paths.get(home, ".cache", "soundness", "proscala").nn

    if !Files.isDirectory(root) then Unset else
      Files.list(root).nn.iterator.nn.asScala.to(scala.List).sortBy(_.toString).reverse
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
        fixed.stdlib.forall { name => Files.exists(lib.resolve(name.s)) }
        && contents.exists(_.getFileName.nn.toString.startsWith("scalajs-javalib"))
        && contents.exists(_.getFileName.nn.toString.startsWith("scalajs-library_2.13"))

      if !complete then Unset else
        val globbed = Files.list(lib).nn.iterator.nn.asScala.to(scala.List).filter: jar =>
          val name = jar.getFileName.nn.toString
          name.startsWith("scalajs-javalib") || name.startsWith("scalajs-library_2.13")

        val jars = fixed.stdlib.map { name => lib.resolve(name.s).nn } ++ globbed
        LocalClasspath(jars.map { jar => Classpath.Entry.Jar(jar.toString.tt) }*)

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
      . find { base => artifacts.stdlib.forall { jar => Files.exists(base.resolve(jar.s)) } }
      . map: base =>
          val jars = artifacts.stdlib.map { jar => base.resolve(jar.s).nn }
          val stdlib = List("scala-library.jar", "scala3-library.jar").map(proscala.resolve(_).nn)
          val plugin = unsafely(jars.head.toString.tt.as[soundness.Path on Linux])

          val classpath =
            LocalClasspath
              ((jars.tail ++ stdlib).map { jar => Classpath.Entry.Jar(jar.toString.tt) }*)

          (NirPlugin(plugin), classpath)

      . getOrElse(Unset)

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
package abscission

import java.nio.file as jnf

import soundness.*

import filesystemOptions.dereferenceSymlinks
import galilei.Linux.pathOnLinux
import logging.silentLogging
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import filesystemBackends.javaBaseFilesystem
import threads.platformThreads
import workingDirectories.javaBaseWorkingDirectory
import anthology.Toolchain.Universe

// `Globe`'s closed world, shrunk from the classpath the tests run on and loaded by a classloader
// whose parent is the JDK's alone: whatever the world needs, it must hold. Each world is made in
// a directory of its own beneath `scratch`, which the suite lends for as long as its tests run.
object Worlds:
  private val parent: ClassLoader = getClass.getClassLoader.nn
  private val classpath: scala.List[String] = Shrinker.classpath(parent)
  private val entry: String = "abscission.Globe"
  private def running: LocalClasspath =
    LocalClasspath(classpath.map { entry => Classpath.Entry.classify(entry.tt.as[Path on Linux]) }*)


  final class World(val report: Shrinker.Report, root: jnf.Path):
    private val loader: ClassLoader =
      new java.net.URLClassLoader
        ( scala.Array[java.net.URL | Null](root.toUri.nn.toURL.nn),
          ClassLoader.getPlatformClassLoader )

    def has(name: String): Boolean =
      try
        loader.loadClass(name)
        true
      catch case _: ClassNotFoundException => false

    // The names of the methods a class of the world declares.
    def methods(name: String): scala.List[String] =
      scala.Predef.wrapRefArray(loader.loadClass(name).nn.getDeclaredMethods.nn).toList
      . map(_.nn.getName.nn).distinct.sorted

    // The transcript of a performance given by the world's own classes.
    def transcript(): String =
      val globe: AnyRef = loader.loadClass(entry+"$").nn.getField("MODULE$").nn.get(null).nn
      globe.asInstanceOf[Runnable].run()
      java.lang.System.getProperty("abscission.globe").nn

class Worlds(scratch: Path on Linux):
  import Worlds.*

  private def directory(name: Text): Path on Linux =
    val path: Path on Linux = scratch / name
    path.create[Directory]()
    path

  private def shrink(name: Text, keep: scala.List[String], pin: scala.List[String]): World =
    val output: jnf.Path = jnf.Path.of(directory(name).encode.s).nn

    // Every suite of the classpath is a service, and none of them is an entry point here.
    val request: Shrinker.Request =
      Shrinker.Request
        ( classpath, output.toString, scala.Nil, scala.List(entry), keep, pin, false, true )

    World(Shrinker.shrink(request), output)

  lazy val plain: World = shrink(t"plain", scala.Nil, scala.Nil)

  lazy val guarded: World =
    shrink
      ( t"guarded",
        scala.List("abscission.Yorick"),
        scala.List("abscission.Prompter", "abscission.Steward") )

  // The same world, made as a JAR by the edges of a toolchain.
  lazy val jar: World =
    supervise:
      val out: Path on Linux = directory(t"jar")
      val emission = Deliverable.Emission(directory(t"empty"), running)
      val settings = List(shrinkOptions.services(false), shrinkOptions.fine(true))
      val entryPoints = List(EntryPoint(fqcn"abscission.Globe"))
      val toolchain = Toolchain(shrinkEdges(Scalac[3.9](Nil)))

      val file: Path on Linux =
        toolchain.produce(emission, Universe.Classfile, Shrunk.Jar, out, settings, entryPoints)

      val report: Shrinker.Report =
        Shrinker.Report
          ( scala.Nil, 0, 0, 0, 0L, 0, 0L, 0, 0, 0, 0, 0, scala.Nil, scala.Nil, scala.Nil,
            scala.collection.immutable.Map() )

      World(report, jnf.Path.of(file.encode.s).nn)

object Tests extends Suite("shrinking", m"Abscission Tests"):
  def run(): Unit =
    temporaryDirectory[Path on Linux].open[Scratch](Read & Write): handle ?=>
      suites(Worlds(handle.stem))

  // Every test reads a world from the scratch directory, so each is a `check`, run where it
  // stands, rather than an `assert`, which a parallel runner may defer beyond the loan.
  private def suites(worlds: Worlds): Unit =
    suite(m"A closed world"):
      test(m"the classpath is shrunk without problems"):
        worlds.plain.report.problems.map(_.tt).to(List)
      . check(_ == Nil)

      test(m"the closed world gives the performance the classpath gives"):
        worlds.plain.transcript().tt
      . check(_ == Globe.perform().tt)

      test(m"the closed world holds fewer classes than the classpath"):
        worlds.plain.report.kept < worlds.plain.report.classes/4
      . check(_ == true)

      test(m"a unit which nothing reaches is not read"):
        worlds.plain.report.read < worlds.plain.report.units/4
      . check(_ == true)

      test(m"a class which is never instantiated is not kept"):
        worlds.plain.has("abscission.Yorick")
      . check(_ == false)

      test(m"an object which only removed code refers to is not kept"):
        worlds.plain.has("abscission.Wardrobe$")
      . check(_ == false)

      test(m"a method which nothing calls is removed, with its forwarder"):
        worlds.plain.methods("abscission.Player").map(_.tt).to(List)
      . check(_ == List(t"line", t"name", t"speak", t"speak$$"))

      test(m"a method of a subclass which nothing calls is removed"):
        worlds.plain.methods("abscission.Hamlet").map(_.tt).to(List)
      . check(_ == List(t"line", t"name", t"speak"))

      test(m"a method of a value class is removed from its companion"):
        worlds.plain.methods("abscission.Ducats$").map(_.tt).to(List).filter(_.ends(t"$$extension"))
      . check: methods =>
          methods.exists(_ == t"plus$$extension") && !methods.exists(_ == t"minus$$extension")

      test(m"a method of an enumeration which nothing calls is removed"):
        worlds.plain.methods("abscission.Humour").map(_.tt).to(List).filter(_ == t"dry")
      . check(_ == Nil)

      test(m"a lazy value which nothing reads is removed"):
        worlds.plain.methods("abscission.Troupe").map(_.tt).to(List)
      . check(!_.exists(_ == t"rival"))

      test(m"an object nested in another is kept without the other's fields"):
        worlds.plain.methods("abscission.Stage$").map(_.tt).to(List)
      . check(!_.exists(_ == t"boards"))

      test(m"an object which is only somewhere for a class to be has nothing of its own"):
        worlds.plain.methods("abscission.Playhouse$").map(_.tt).to(List)
      . check(_ == Nil)

      test(m"a method called through a structural type is kept by its name"):
        worlds.plain.methods("abscission.Fool").map(_.tt).to(List)
      . check(_ == List(t"jest"))

      test(m"a case class which is only matched against is kept"):
        worlds.plain.has("abscission.Aside")
      . check(_ == true)

      test(m"why a class of the library was kept is a chain back to a recompiled class"):
        worlds.plain.report.why("scala.Option").map(_.tt).to(List)
      . check: chain =>
          chain.exists(_ == t"scala.Option") && chain.exists(_.starts(t"abscission."))

    suite(m"Keeping and pinning"):
      test(m"a class which matches a pattern to keep is kept whole"):
        worlds.guarded.methods("abscission.Yorick").map(_.tt).to(List)
      . check(_ == List(t"jest", t"line"))

      test(m"what a kept class's initialization needs is kept with it"):
        worlds.guarded.methods("abscission.Wardrobe$").map(_.tt).to(List)
      . check(_ == List(t"costume", t"stock"))

      test(m"a unit which matches a pattern to pin is copied as it was"):
        worlds.guarded.methods("abscission.Prompter$").map(_.tt).to(List)
      . check(_.exists(_ == t"interval"))

      test(m"a pinned unit is reported"):
        worlds.guarded.report.pinned.sorted.map(_.tt).to(List)
      . check(_ == List(t"abscission.Prompter", t"abscission.Steward"))

      test(m"what a pinned unit calls is kept, by the name it is compiled with"):
        worlds.guarded.methods("abscission.Ledger$").map(_.tt).to(List)
      . check(_ == List(t"credit"))

      test(m"the world with a kept class and a pinned unit performs as before"):
        worlds.guarded.transcript().tt
      . check(_ == Globe.perform().tt)

    suite(m"A toolchain"):
      test(m"classfiles are shrunk to a JAR which performs as the classpath does"):
        worlds.jar.transcript().tt
      . check(_ == Globe.perform().tt)

      test(m"the JAR holds no class which nothing reaches"):
        worlds.jar.has("abscission.Yorick")
      . check(_ == false)

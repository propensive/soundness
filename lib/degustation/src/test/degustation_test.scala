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
package degustation

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.IteratorHasAsScala

import soundness.*
import galilei.Linux.pathOnLinux

import alphabets.hexLowerCase
import logging.silentLogging
import probates.cancelProbate
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

object Tests extends Suite(m"Degustation Tests"):

  def proscalaLibrary(): Optional[java.nio.file.Path] =
    val home = java.lang.System.getProperty("user.home").nn
    val root = Paths.get(home, ".cache", "soundness", "proscala").nn

    if !Files.isDirectory(root) then Unset else
      Files.list(root).nn.iterator.nn.asScala.to(scala.List).sortBy(_.toString).reverse
      . map(_.resolve("lib").nn)
      . find { lib => Files.isDirectory(lib) && Files.exists(lib.resolve("scala3-library.jar")) }
      . getOrElse(Unset)

  val fixture: Text =
    t"""|package fixture
        |
        |trait Openish:
        |  def abstractOne: Int
        |  def concrete: Int = 1
        |
        |sealed trait Choice
        |case class Alpha(x: Int) extends Choice
        |object Beta extends Choice
        |
        |class Overloads:
        |  def f(x: Int): Int = x
        |  def f(x: String): String = x
        |  private def hidden: Int = 0
        |
        |object Tops:
        |  val value: Int = 3
        |
        |inline def double(n: Int): Int = n * 2
        |""".s.stripMargin.tt

  def run(): Unit = proscalaLibrary().let: lib =>
    val jars = scala.List("scala-library.jar", "scala3-library.jar").map(lib.resolve(_).nn)
    val classpath = LocalClasspath(jars.map { jar => ClasspathEntry.Jar(jar.toString.tt) }*)
    val libraryPaths = jars.map { jar => Text(jar.toString) }

    def compile(source: Text): (List[Text], List[Text]) =
      supervise:
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))
        val process = Scalac[3.9](List())(classpath)(Map(t"fixture.scala" -> source), out)
        process.complete()

        val tastyFiles = Files.walk(Paths.get(out.encode.s)).nn.iterator.nn.asScala
          . to(scala.List)
          . filter { path => path.toString.endsWith(".tasty") }
          . map { path => Text(path.toString) }

        (List.from(tastyFiles), List.from(Text(out.encode.s) :: libraryPaths))

    def listing(source: Text): scala.List[(Text, Text)] =
      val (tastyFiles, classpath0) = compile(source)

      Inspection.atomize(tastyFiles, classpath0).stdlib
      . map { atom => (atom.key, atom.encoding.serialize[Hex]) }
      . sortBy(_(0).s)

    val baseline = listing(fixture)
    val keys = baseline.map(_(0).s).toSet

    test(m"the fixture yields a nonempty atom listing"):
      baseline.size
    . assert(_ > 0)

    test(m"overloads yield distinct keys"):
      keys.count(_.startsWith("fixture.Overloads.f("))
    . assert(_ == 2)

    test(m"private members are not API"):
      keys.exists(_.contains("hidden"))
    . assert(!_)

    test(m"an inline definition yields a replaceable atom"):
      keys.exists { key => key.startsWith("fixture.double(") && key.endsWith("[inline]") }
    . assert(identity)

    test(m"top-level definitions are keyed by package, not carrier"):
      keys.exists(_.startsWith("fixture.double("))
    . assert(identity)

    test(m"recompilation yields an identical listing"):
      listing(fixture) == baseline
    . assert(identity)

    test(m"declaration order does not affect the listing"):
      val reordered: Text =
        t"""|package fixture
            |
            |inline def double(n: Int): Int = n * 2
            |
            |object Tops:
            |  val value: Int = 3
            |
            |class Overloads:
            |  private def hidden: Int = 0
            |  def f(x: String): String = x
            |  def f(x: Int): Int = x
            |
            |sealed trait Choice
            |case class Alpha(x: Int) extends Choice
            |object Beta extends Choice
            |
            |trait Openish:
            |  def concrete: Int = 1
            |  def abstractOne: Int
            |""".s.stripMargin.tt

      listing(reordered) == baseline
    . assert(identity)

    test(m"adding a concrete method leaves other atoms unchanged"):
      val extended = Text(fixture.s.replace("def concrete: Int = 1",
          "def concrete: Int = 1\n  def added: Int = 2").nn)

      val grown = listing(extended).toMap
      val before = baseline.toMap
      val overlap = before.keySet.intersect(grown.keySet)

      // Every atom other than the open template (whose abstract list is unchanged, but whose
      // member `added` is new) must be byte-identical.
      overlap.forall { key => before(key) == grown(key) } && grown.size == before.size + 1
    . assert(identity)

    test(m"adding an abstract member changes the open template's atom"):
      val extended = Text(fixture.s.replace("def abstractOne: Int",
          "def abstractOne: Int\n  def abstractTwo: Int").nn)

      val grown = listing(extended).toMap
      grown(t"fixture.Openish") != baseline.toMap.apply(t"fixture.Openish")
    . assert(identity)

    test(m"adding a sealed child changes the sealed template's atom"):
      val extended = Text(fixture.s.replace("object Beta extends Choice",
          "object Beta extends Choice\ncase class Gamma(y: Int) extends Choice").nn)

      val grown = listing(extended).toMap
      grown(t"fixture.Choice") != baseline.toMap.apply(t"fixture.Choice")
    . assert(identity)

    test(m"the discipline adapter claims tasty and derived binaries"):
      import reliquary.*
      val path = TreePath(t"fixture/Alpha.tasty")

      (ScalaTasty.claims(path, Array.freeze(Array[Byte](0))),
       ScalaTasty.claims(TreePath(t"fixture/Alpha.class"), Array.freeze(Array[Byte](0))),
       ScalaTasty.claims(TreePath(t"readme.md"), Array.freeze(Array[Byte](0))))
    . assert(_ == (true, true, false))

    test(m"the discipline adapter atomizes tasty and holds binaries atomless"):
      import reliquary.*
      val (tastyFiles, _) = compile(fixture)

      val content = tastyFiles.map: file =>
        val name = Text(Paths.get(file.s).nn.getFileName.nn.toString)
        val data = Array.unsafeFrozen(Files.readAllBytes(Paths.get(file.s)).nn)
        (TreePath(t"fixture/$name"), data)

      val binary = (TreePath(t"fixture/Alpha.class"), Array.freeze(Array[Byte](4)))
      val all = List.from(content.stdlib :+ binary)
      val context = Discipline.Context(t"jvm", List.from(libraryPaths))
      val atomization = ScalaTasty.atomize(all, context)

      (atomization.discipline,
       atomization.atoms.stdlib.exists(_.key.s.startsWith("fixture.Overloads.f(")),
       atomization.atoms.stdlib.exists(_.key.s.contains("Alpha.class")))
    . assert(_ == (t"scala-tasty/1", true, false))

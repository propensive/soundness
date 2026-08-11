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
    val classpath = LocalClasspath(jars.map { jar => Classpath.Entry.Jar(jar.toString.tt) }*)
    val libraryPaths = jars.map { jar => Text(jar.toString) }

    def compileWith(source: Text, deps: LocalClasspath, libs: scala.List[Text], sjs: Boolean)
    :   (List[Text], List[Text], Text) =

      supervise:
        val out: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(out.encode.s))

        val process =
          if sjs then
            Scalac[3.9](List()).targeting[Universe.Sjsir]
              (deps)(Map(t"fixture.scala" -> source), out)
          else Scalac[3.9](List())(deps)(Map(t"fixture.scala" -> source), out)

        process.complete()

        val tastyFiles = Files.walk(Paths.get(out.encode.s)).nn.iterator.nn.asScala
          . to(scala.List)
          . filter { path => path.toString.endsWith(".tasty") }
          . map { path => Text(path.toString) }

        (List.from(tastyFiles), List.from(Text(out.encode.s) :: libs), out.encode)

    def compile(source: Text): (List[Text], List[Text]) =
      val (tastyFiles, classpath0, _) = compileWith(source, classpath, libraryPaths, false)
      (tastyFiles, classpath0)

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

    test(m"qualified-private members are conservatively API"):
      val qualified = listing(t"""|package fixture
          |
          |class Scoped:
          |  private[fixture] def limited: Int = 0
          |  private def hidden: Int = 0
          |""".s.stripMargin.tt)

      (qualified.exists(_(0).s.contains("limited")), qualified.exists(_(0).s.contains("hidden")))
    . assert(_ == (true, false))

    test(m"an export forwarder atomizes as its hand-written equivalent"):
      val exported = listing(t"""|package fixture
          |
          |object Impl:
          |  def value: Int = 3
          |
          |object Front:
          |  export Impl.value
          |""".s.stripMargin.tt)

      val written = listing(t"""|package fixture
          |
          |object Impl:
          |  def value: Int = 3
          |
          |object Front:
          |  final def value: Int = Impl.value
          |""".s.stripMargin.tt)

      exported == written
    . assert(identity)

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

    test(m"an inline body change is isolated to the replaceable atom"):
      val changed = Text(fixture.s.replace("inline def double(n: Int): Int = n * 2",
          "inline def double(n: Int): Int = n + n").nn)

      val grown = listing(changed).toMap
      val before = baseline.toMap
      val inlineKey = before.keySet.find(_.s.endsWith("[inline]")).get

      before.keySet == grown.keySet
      && before(inlineKey) != grown(inlineKey)
      && before.keySet.filter(_ != inlineKey).forall { key => before(key) == grown(key) }
    . assert(identity)

    test(m"an import inside an inline body is transparent"):
      val plain: Text =
        t"""|package lexical
            |
            |inline def compute(a: Int): Int =
            |  val b = a + 1
            |  b*2
            |""".s.stripMargin.tt

      val imported: Text =
        t"""|package lexical
            |
            |inline def compute(a: Int): Int =
            |  import scala.math.*
            |  val b = a + 1
            |  b*2
            |""".s.stripMargin.tt

      listing(plain).toMap == listing(imported).toMap
    . assert(identity)

    test(m"an inline body references what it splices"):
      val source: Text =
        t"""|package refs
            |
            |def helper(n: Int): Int = n + 1
            |inline def outer(n: Int): Int = helper(n) * 2
            |inline def nested(n: Int): Int = outer(n) + 1
            |""".s.stripMargin.tt

      val (tastyFiles, classpath0) = compile(source)
      val atoms = Inspection.atomize(tastyFiles, classpath0).stdlib

      def refs(prefix: String): scala.collection.immutable.Set[String] =
        atoms
        . find { atom => atom.key.s.startsWith(prefix) && atom.key.s.endsWith("[inline]") }
        . map:
            _.references.stdlib.map:
              case ScalaReference.Own(key)     => s"own:$key"
              case ScalaReference.Foreign(key) => s"foreign:$key"
            . toSet

        . getOrElse(scala.collection.immutable.Set())

      val outerRefs = refs("refs.outer(")
      val nestedRefs = refs("refs.nested(")

      (outerRefs.exists(_.startsWith("own:refs.helper(")),
       outerRefs.exists(_.startsWith("foreign:")),
       nestedRefs.exists { ref => ref.startsWith("own:refs.outer(") && ref.endsWith("[inline]") })
    . assert(_ == (true, true, true))

    test(m"adding a sealed child changes the sealed template's atom"):
      val extended = Text(fixture.s.replace("object Beta extends Choice",
          "object Beta extends Choice\ncase class Gamma(y: Int) extends Choice").nn)

      val grown = listing(extended).toMap
      grown(t"fixture.Choice") != baseline.toMap.apply(t"fixture.Choice")
    . assert(identity)

    // A corpus soak: the build's own compiled output for a foundational module, atomized twice.
    // Skipped when the build output is not present (e.g. a partial checkout).
    val corpus = Paths.get("out", "vacuous", "core", "compile.dest", "classes").nn

    if Files.isDirectory(corpus) then
      val corpusTasty = Files.walk(corpus).nn.iterator.nn.asScala.to(scala.List)
        . filter { path => path.toString.endsWith(".tasty") }
        . map { path => Text(path.toString) }

      val corpusClasspath = List.from(Text(corpus.toString) :: libraryPaths)

      test(m"a real module's TASTy atomizes without vocabulary gaps"):
        Inspection.atomize(List.from(corpusTasty), corpusClasspath).stdlib.size
      . assert(_ > 0)

      test(m"a real module's atomization is deterministic"):
        def once(): scala.List[(Text, Text)] =
          Inspection.atomize(List.from(corpusTasty), corpusClasspath).stdlib
          . map { atom => (atom.key, atom.encoding.serialize[Hex]) }
          . sortBy(_(0).s)

        once() == once()
      . assert(identity)

    test(m"the discipline adapter claims tasty and derived binaries"):
      import reliquary.*
      val path = TreePath(t"fixture/Alpha.tasty")

      (Tasty.claims(path, Array.freeze(Array[Byte](0))),
       Tasty.claims(TreePath(t"fixture/Alpha.class"), Array.freeze(Array[Byte](0))),
       Tasty.claims(TreePath(t"readme.md"), Array.freeze(Array[Byte](0))))
    . assert(_ == (true, true, false))

    test(m"a jvm-only lira assembles from a real compilation and verifies"):
      import reliquary.*
      val (_, _, out) = compileWith(fixture, classpath, libraryPaths, false)

      val compilation =
        Compilation[Universe.Classfile](unsafely(out.s.tt.as[soundness.Path on Linux]), classpath)

      val input = LiraBundle(compilation)
      val registry = Discipline.Registry(List(Tasty))

      val bytes = LiraAssembler.assemble
        ( t"fixture-core",
          List(input),
          registry,
          toolchain = List(LiraBundle.tool[Universe.Classfile](t"3.9.0")),
          owns      = List(t"fixture"),
          classpath = { _ => List.from(Text(out.s) :: libraryPaths) } )

      val lira = Lira.read(bytes)
      val report = Verification.install(lira)

      (lira.manifest.module,
       lira.manifest.api.stdlib.map(_.discipline),
       lira.manifest.section.stdlib.map(_.realm),
       lira.manifest.section.stdlib.forall(_.derivative.present),
       report.atomizations.stdlib.map(_.discipline))
    . assert(_ == (t"fixture-core", scala.List(t"tasty/1"), scala.List(t"jvm"), true,
        scala.List(t"tasty/1")))

    val sjsJars = scala.List("scala3-library_sjs1.jar", "scalajs-scalalib_2.13.jar")
      . map(lib.resolve(_).nn)
      . filter(Files.exists(_))
      . ++ (Files.list(lib).nn.iterator.nn.asScala.to(scala.List).filter: path =>
          val name = path.getFileName.nn.toString
          name.startsWith("scalajs-library_2.13") || name.startsWith("scalajs-javalib"))

    if sjsJars.size >= 3 then
      val sjsClasspath = LocalClasspath
        ((jars ++ sjsJars).map { jar => Classpath.Entry.Jar(jar.toString.tt) }*)

      val sjsLibraryPaths = (jars ++ sjsJars).map { jar => Text(jar.toString) }

      test(m"a two-universe lira upholds the cross-universe invariant"):
        import reliquary.*
        val (_, _, jvmOut) = compileWith(fixture, classpath, libraryPaths, false)
        val (_, _, sjsOut) = compileWith(fixture, sjsClasspath, sjsLibraryPaths, true)

        val jvmInput = LiraBundle(Compilation[Universe.Classfile]
          (unsafely(jvmOut.s.tt.as[soundness.Path on Linux]), classpath))

        val sjsInput = LiraBundle(Compilation[Universe.Sjsir]
          (unsafely(sjsOut.s.tt.as[soundness.Path on Linux]), sjsClasspath))

        val registry = Discipline.Registry(List(Tasty))

        def contextClasspath(universe: Text): List[Text] =
          if universe == t"sjsir" then List.from(Text(sjsOut.s) :: sjsLibraryPaths)
          else List.from(Text(jvmOut.s) :: libraryPaths)

        val bytes = LiraAssembler.assemble
          ( t"fixture-core",
            List(jvmInput, sjsInput),
            registry,
            toolchain = List(LiraBundle.tool[Universe.Classfile](t"3.9.0")),
            classpath = { input => contextClasspath(input.realm) } )

        val lira = Lira.read(bytes)
        val report = Verification.install(lira)
        val sjsSection = lira.manifest.section.stdlib.find(_.realm == t"sjsir")

        (lira.manifest.section.stdlib.map(_.realm),
         report.materialized.stdlib.map(_(0).realm),
         report.materialized.stdlib.find(_(0).realm == t"sjsir")
           . map(_(1).entries.stdlib.exists(_.path.text.s.endsWith(".sjsir"))))
      . assert(_ == (scala.List(t"jvm", t"sjsir"), scala.List(t"jvm", t"sjsir"),
          scala.Some(true)))

    test(m"the discipline adapter atomizes tasty and holds binaries atomless"):
      import reliquary.*
      val (tastyFiles, _) = compile(fixture)

      val content = tastyFiles.map: file =>
        val name = Text(Paths.get(file.s).nn.getFileName.nn.toString)
        val data = Array.unsafeFrozen(Files.readAllBytes(Paths.get(file.s)).nn)
        (TreePath(t"fixture/$name"), data)

      val binary = (TreePath(t"fixture/Alpha.class"), Array.freeze(Array[Byte](4)))
      val all = List.from(content.stdlib :+ binary)
      val context = Discipline.Context(t"jvm", classpath = List.from(libraryPaths))
      val atomization = Tasty.atomize(all, context)

      (atomization.discipline,
       atomization.atoms.stdlib.exists(_.key.s.startsWith("fixture.Overloads.f(")),
       atomization.atoms.stdlib.exists(_.key.s.contains("Alpha.class")))
    . assert(_ == (t"tasty/1", true, false))

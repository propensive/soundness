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
package probably

import java.util as ju
import java.util.concurrent as juc

import scala.jdk.CollectionConverters.*

import denominative.dysasymptotics.linearSize

import soundness.*

enum Codec:
  case Zip, Tar, Cpio

// A runner in LISTING mode over the given terms: `skip` records rather than runs, and
// nothing is ever reported, so a `Reporter[Unit]` suffices.
def listing(terms: List[Text] = Nil): Runner[Unit] =
  given reporter: Reporter[Unit] = new Reporter[Unit]:
    def report(): Unit = ()
    def fail(report: Unit, error: Throwable, active: Set[Test.Id]): Unit = ()
    def declare(report: Unit, suite: Testable): Unit = ()
    def complete(report: Unit): Unit = ()

  Runner(Selection.parse(terms).copy(listOnly = true))

// Outside any suite, where no `Testable` is in scope but the one a method is given.
object Orphans:
  def refused: scala.List[Boolean] =
    demilitarize:
      def orphan()(using Testable): Test.Id = test(m"orphan")
    . map(_.message.tt.contains(t"a test must be declared for a Suite"))

object Tests extends Suite("probably", m"Probably Tests"):
  // The event sequence of a suite invoked with `terms`, as `label`s, with the exit status.
  def label(event: TestEvent): Text = event match
    case TestEvent.SuiteStarted(ref, _)             => t"suite-started:${ref.name}"
    case TestEvent.SuiteEnded(ref, _)               => t"suite-ended:${ref.name}"
    case TestEvent.TestScheduled(ref, _, _, _, _)   => t"scheduled:${ref.name}"
    case TestEvent.TestStarted(ref, _)              => t"started:${ref.name}"
    case TestEvent.TestEnded(ref, _)                => t"ended:${ref.name}"
    case TestEvent.TestCompleted(ref, _, _, outcome, _, _) => t"completed:${ref.name}:${outcome.outcome}"
    case TestEvent.RunCompleted(passed, _)          => if passed then t"run-completed:true" else t"run-completed:false"
    case TestEvent.RunTerminated(_, active, _)      => t"run-terminated:${active.map(_.name).join(t",")}"
    case _                                          => t"other"

  def invoked(suite: Suite[?], terms: Text): (Int, scala.List[Text]) =
    val events: juc.ConcurrentLinkedQueue[TestEvent] = juc.ConcurrentLinkedQueue()
    val exit = suite.invoke(terms, event => events.add(event))
    val labels = scala.collection.mutable.ListBuffer[Text]()
    events.forEach(event => labels.append(label(event.nn)))
    (exit, labels.toList)

  def index(labels: scala.List[Text], label: Text): Int = labels.indexOf(label)
  def verdicts(labels: scala.List[Text]): scala.collection.immutable.Set[Text] = labels.filter(_.starts(t"completed:")).toSet

  // The lines the beneficence plugin recorded for one of this module's source files, in
  // `META-INF/probably/tests/<source>-<hash>`: read from wherever this class was loaded from,
  // a directory of classes or a jar.
  def indexed(source: Text): List[Text] =
    val location =
      java.io.File(getClass.getProtectionDomain.nn.getCodeSource.nn.getLocation.nn.toURI)

    val directory = "META-INF/probably/tests/"
    def lines(bytes: scala.Array[Byte]): List[Text] = String(bytes, "UTF-8").tt.cut(t"\n")

    if location.isDirectory then
      val files: List[java.io.File] =
        java.io.File(location, directory).listFiles.nn.iterator.map(_.nn).to(List)

      files.filter(_.getName.nn.startsWith(source.s + "-")).flatMap: file =>
        lines(java.nio.file.Files.readAllBytes(file.toPath).nn)
    else
      val zip = java.util.zip.ZipFile(location)

      try
        val entries: List[java.util.zip.ZipEntry] =
          ju.Collections.list(zip.entries()).nn.asScala.iterator.map(_.nn).to(List)

        entries.filter(_.getName.nn.startsWith(directory + source.s + "-")).flatMap: entry =>
          lines(zip.getInputStream(entry).nn.readAllBytes().nn)
      finally zip.close()

  def run(): Unit =
    suite(m"The index of tests"):
      // `Probe`, in `probably.QueuedSuites.scala`, has tests at its root, in a group and in an
      // `aspirationally` block: its index lines, as the name paths and the ids they imply.
      def path(line: Text): List[(Text, Text)] = line.cut(t"\t") match
        case t"test" :: t"probe" :: t"" :: groups :: _ :: name :: _ =>
          val above: List[Text] = t"probe" :: groups.cut(t"/").filter(_ != t"")
          val hash: Int = above.fold(0) { (sum, name) => sum + name.s.hashCode } ^ name.s.hashCode

          List
            ( ( (above :+ name).join(t"/"),
                String.format("%06x", Int.box(hash & 0xffffff)).nn.tt ) )

        case _ =>
          Nil

      def paths(): List[(Text, Text)] = indexed(t"probably.QueuedSuites.scala").flatMap(path)

      test(m"a suite's index agrees with a listing made by running it, ids included"):
        val events: juc.ConcurrentLinkedQueue[TestEvent] = juc.ConcurrentLinkedQueue()
        Probe().invoke(t"--list", event => events.add(event))
        val listed = scala.collection.mutable.ListBuffer[(Text, Text)]()
        val index = paths()

        events.forEach: event =>
          event.nn match
            case TestEvent.TestScheduled(ref, _, _, _, _) =>
              listed.append((ref.path.join(t"/"), ref.id))

            case _ =>
              ()

        (index.size, listed.iterator.to(List) == index)
      . assert(_ == (6, true))

      test(m"the index names a suite object, and leaves out what an impromptu block declares"):
        val lines = indexed(t"probably_test.scala")

        ( lines.exists(_.starts(t"suite\tprobably.Tests\tprobably\tProbably Tests\t")),
          lines.exists(_.starts(t"open\tprobably\t")),
          lines.exists(_.contains(t"\tquick one\t")) )
      . assert(_ == (true, true, false))

      test(m"a test is refused where no Testable says which suite it is for"):
        Orphans.refused
      . assert(_ == scala.List(true))

      test(m"a method declares tests for a named suite, for any suite, or impromptu"):
        demilitarize:
          def named()(using Testable of "probably"): Test.Id = test(m"named")
          def shared[topic <: Label]()(using Testable of topic): Test.Id = test(m"shared")
          def unlisted()(using Testable of Impromptu): Test.Id = test(m"unlisted")
        . map(_.message)
      . assert(_ == Nil)

      test(m"a suite's id is given, or derived from its title"):
        ( Tests.key,
          Probe().key,
          Suite.derive(t"Honeycomb Tests"),
          Suite.derive(t"  y\"…\" interpolator (v2) "),
          Suite.derive(t"…") )
      . assert(_ == (t"probably", t"probe", t"honeycomb-tests", t"y-interpolator-v2", t"suite"))

      test(m"a suite's id heads the paths of its tests, and its title is still its name"):
        val id = impromptu(test(m"probe"))
        val ref = TestEvent.Ref.of(id)
        (ref.path.prim, TestEvent.Ref.of(Tests.id).name, TestEvent.Ref.of(Tests.id).moniker)
      . assert(_ == (t"probably", t"Probably Tests", t"probably"))

      test(m"a suite's id selects its tests, with or without a hyphen in it"):
        val hyphenated = Testable.of[Impromptu](m"Honeycomb Tests", Unset, Unset, t"html-5")
        val inside = Test.Id(m"inside", hyphenated, summon[Codepoint])
        val outside = impromptu(test(m"outside"))

        List(t"html-5", t"probably", t"html-5/**", t"html").map: term =>
          val selection = Selection.parse(List(term))

          ( selection.admits(inside, Entry.Kind.Check, Nil, Nil),
            selection.admits(outside, Entry.Kind.Check, Nil, Nil) )
      . assert(_ == List((true, false), (false, true), (true, false), (false, false)))

      test(m"a test is declared at its suite's position within an impromptu block"):
        impromptu(test(m"probe")).suite.let(_.name.text)
      . assert(_ == t"The index of tests")

      test(m"a test's id is the low 24 bits of its hash over the texts of the names"):
        val hash = (t"Probably Tests".s.hashCode + t"group".s.hashCode) ^ t"a quoted name".s.hashCode
        val group = Testable.of[Impromptu](m"group", Testable.of[Impromptu](m"Probably Tests"))
        Test.Id(m"a `quoted` name", group, summon[Codepoint]).id
        == String.format("%06x", Int.box(hash & 0xffffff)).nn.tt
      . assert(_ == true)

    suite(m"Queued execution"):
      test(m"--workers parses, and defaults to none"):
        scala.List(t"--workers=2", t"--workers=0", t"--workers=-1", t"--workers=many", t"**")
        . map(term => Selection.parse(List(term)).workers)
      . assert(_ == scala.List(2, 0, 0, 0, 0))

      test(m"an inline run queues nothing"):
        invoked(Probe(), t"")(1).filter(_.starts(t"scheduled:"))
      . assert(_ == scala.Nil)

      test(m"queued and inline runs reach the same verdicts and exit status"):
        val direct = invoked(Probe(), t"")
        val queued = invoked(Probe(), t"--workers=1")
        (direct(0), queued(0), verdicts(direct(1)) == verdicts(queued(1)))
      . assert(_ == (1, 1, true))

      test(m"every queued assertion is announced before it starts"):
        val labels = invoked(Probe(), t"--workers=1")(1)
        scala.List(t"one", t"three", t"four", t"five").map: name =>
          index(labels, t"scheduled:$name") < index(labels, t"started:$name")
      . assert(_ == scala.List(true, true, true, true))

      test(m"an assertion within `aspirationally` records an aspiration"):
        invoked(Probe(), t"")(1).filter(_.starts(t"completed:six"))
      . assert(_ == scala.List(t"completed:six:aspire-fail"))

      test(m"a pure aspiration is queued like any other assertion"):
        invoked(Probe(), t"--workers=1")(1).filter(_.starts(t"scheduled:six"))
      . assert(_ == scala.List(t"scheduled:six"))

      test(m"a check runs inline and its value reaches a later assertion"):
        invoked(Probe(), t"--workers=1")(1).filter(_.starts(t"completed:three"))
      . assert(_ == scala.List(t"completed:three:pass"))

      test(m"a suite ends only after its last queued assertion, and the run after every suite"):
        val labels = invoked(Probe(), t"--workers=1")(1)
        ( index(labels, t"suite-ended:inner") > index(labels, t"completed:five:fail"),
          labels.last == t"run-completed:false",
          index(labels, t"suite-ended:probe") > index(labels, t"completed:six:aspire-fail") )
      . assert(_ == (true, true, true))

      test(m"several workers reach the same verdicts"):
        verdicts(invoked(Probe(), t"--workers=4")(1))
      . assert(_ == verdicts(invoked(Probe(), t"")(1)))

      test(m"a suite asking for workers runs its assertions beside one another"):
        verdicts(invoked(Parallel(), t"--workers=1")(1))
      . assert(_ == scala.collection.immutable.Set(t"completed:first:pass", t"completed:second:pass"))

      test(m"a suite's workers count for nothing when the host does not queue"):
        verdicts(invoked(Parallel(), t"")(1))
      . assert(_ == scala.collection.immutable.Set(t"completed:first:fail", t"completed:second:fail"))

      test(m"an error escaping a worker terminates the run, after the traversal"):
        val (exit, labels) = invoked(Escaping(), t"--workers=1")
        (exit, labels.exists(_.starts(t"run-terminated:")), labels.contains(t"scheduled:after"))
      . assert(_ == (2, true, true))

    test(n"square", m"square a number", n"quick")(3*3).assert(_ == 9)

    test(m"double every value").over(Axis(t"n")(1, 2, 3, 4)): n =>
      n*2
    . assert((n, result) => result == n*2)

    test(m"enum companions form axes").over(Codec): codec =>
      codec.ordinal
    . assert((codec, ordinal) => ordinal >= 0 && ordinal < 3)

    test(m"biaxial spread with a gap").over(Axis(t"x")(1, 2, 3), Axis(t"y")(10, 20)):
      case (x, y) if x + y != 23 => x*y
    . assert((x, y, result) => result == x*y)

    test(m"a run's duration multiplier defaults to 1"):
      Selection.all.scale
    . assert(_ == 1.0)

    test(m"--scale sets the duration multiplier"):
      Selection.parse(List(t"--scale=2.5")).scale
    . assert(_ == 2.5)

    test(m"--scale is not mistaken for a name term"):
      Selection.parse(List(t"--scale=2")).terms
    . assert(_ == Nil)

    test(m"a non-positive or unparseable scale leaves the default"):
      List(t"--scale=0", t"--scale=-1", t"--scale=soon").map(term => Selection.parse(List(term)).scale)
    . assert(_ == List(1.0, 1.0, 1.0))

    test(m"tag: terms parse as sets of alternatives which intersect"):
      Selection.parse(List(t"tag:slow,network", t"tag:nightly")).tags
    . assert(_ == List(Set(t"slow", t"network"), Set(t"nightly")))

    test(m"not: terms parse as exclusions; an empty one is ignored"):
      Selection.parse(List(t"not:tag:slow", t"not:kind:bench", t"not:")).exclusions.map(_.trivial)
    . assert(_ == List(false, false))

    test(m"an open-ended range is a one-sided inclusive bound"):
      Selection.parse(List(t"N=4..", t"N=..64")).constraints
    . assert:
        _ == List
          ( Selection.Constraint.Least(t"N", 4.0, true),
            Selection.Constraint.Most(t"N", 64.0, true) )

    test(m"a tag literal is a tag, and a test carries its tags in order"):
      impromptu(test(m"probe", n"slow", n"no-network")).tags.map(_.text)
    . assert(_ == List(t"slow", t"no-network"))

    test(m"a tagged test is admitted by any of a tag: term's alternatives"):
      val id = impromptu(test(m"probe", n"slow", n"network"))

      List(t"tag:slow", t"tag:network,fast", t"tag:fast").map: term =>
        Selection.parse(List(term)).admits(id, Entry.Kind.Check, Nil, id.tags)
    . assert(_ == List(true, true, false))

    test(m"repeated tag: terms intersect"):
      val both = impromptu(test(m"probe", n"slow", n"network"))
      val lone = impromptu(test(m"lone", n"slow"))
      val selection = Selection.parse(List(t"tag:slow", t"tag:network"))

      ( selection.admits(both, Entry.Kind.Check, Nil, both.tags),
        selection.admits(lone, Entry.Kind.Check, Nil, lone.tags) )
    . assert(_ == (true, false))

    test(m"not:tag: excludes the tagged tests and keeps the rest"):
      val slow = impromptu(test(m"slow one", n"slow"))
      val quick = impromptu(test(m"quick one"))
      val selection = Selection.parse(List(t"not:tag:slow"))

      ( selection.admits(slow, Entry.Kind.Check, Nil, slow.tags),
        selection.admits(quick, Entry.Kind.Check, Nil, quick.tags) )
    . assert(_ == (false, true))

    test(m"not: on an axis constraint removes only the matching cells"):
      val id = impromptu(test(m"probe"))
      val axis = Axis(t"N")(4, 8)
      val selection = Selection.parse(List(t"not:N=4"))

      ( selection.admits(id, Entry.Kind.Check, List(axis.coordinate(4)), Nil),
        selection.admits(id, Entry.Kind.Check, List(axis.coordinate(8)), Nil),
        selection.admits(id, Entry.Kind.Check, Nil, Nil) )
    . assert(_ == (false, true, true))

    test(m"not: on a kind removes that kind alone"):
      val id = impromptu(test(m"probe"))
      val selection = Selection.parse(List(t"not:kind:bench"))

      ( selection.admits(id, Entry.Kind.Bench, Nil, Nil),
        selection.admits(id, Entry.Kind.Check, Nil, Nil) )
    . assert(_ == (false, true))

    test(m"not: on an identity removes that test from a wider selection"):
      val kept = impromptu(test(n"kept", m"kept"))
      val dropped = impromptu(test(n"dropped", m"dropped"))
      val selection = Selection.parse(List(t"**", t"not:dropped"))

      ( selection.admits(kept, Entry.Kind.Check, Nil, Nil),
        selection.admits(dropped, Entry.Kind.Check, Nil, Nil) )
    . assert(_ == (true, false))

    test(m"open ranges admit from, and up to, their bounds"):
      val id = impromptu(test(m"probe"))
      val axis = Axis(t"N")(2, 4, 8)
      val least = Selection.parse(List(t"N=4.."))
      val most = Selection.parse(List(t"N=..4"))

      axis.values.map: n =>
        ( least.admits(id, Entry.Kind.Check, List(axis.coordinate(n)), Nil),
          most.admits(id, Entry.Kind.Check, List(axis.coordinate(n)), Nil) )
    . assert(_ == List((false, true), (true, true), (true, false)))

    test(m"a listing retains a spread's axis and its values, as one row"):
      given runner: Runner[Unit] = listing()
      given verdicts: Inclusion[Unit, Verdict] = (_, _, _, _) => ()
      given details: Inclusion[Unit, Verdict.Detail] = (_, _, _, _) => ()

      impromptu:
        test(m"spread", n"axial").over(Axis(t"n")(1, 2, 3)) { n => n }.assert(_ > 0)

      runner.listed.map: row =>
        ( row.id.name.text,
          row.kind,
          row.tags.map(_.text),
          row.axes.map { axis => (axis.spec.label, axis.values.map(_.text)) } )
    . assert(_ == List((t"spread", Entry.Kind.Check, List(t"axial"), List((t"n", List(t"1", t"2", t"3"))))))

    test(m"a listing keeps only the cells the selection admits"):
      given runner: Runner[Unit] = listing(List(t"n=2.."))
      given verdicts: Inclusion[Unit, Verdict] = (_, _, _, _) => ()
      given details: Inclusion[Unit, Verdict.Detail] = (_, _, _, _) => ()

      impromptu(test(m"spread").over(Axis(t"n")(1, 2, 3)) { n => n }.assert(_ > 0))
      runner.listed.map { row => row.axes.map { axis => axis.values.map(_.text) } }
    . assert(_ == List(List(List(t"2", t"3"))))

    test(m"a declared emergent axis is listed with its bounds and no values"):
      val runner = listing()
      val id = impromptu(test(m"stress"))
      val spec = Axis.Spec(t"N", Axis.Domain.Integral, emergent = true)
      runner.declare(id, Entry.Kind.Stress, spec, 1.0, 64.0)
      runner.skip(id, Entry.Kind.Stress, Nil, 100L)

      runner.listed.map: row =>
        val axes = row.axes.map: axis =>
          (axis.spec.label, axis.spec.emergent, axis.values, axis.least, axis.most)

        (row.expected, axes)
    . assert(_ == List((100L, List((t"N", true, Nil, 1.0, 64.0)))))

    test(m"a declaration for an unlisted test is not reported"):
      val runner = listing(List(t"kind:bench"))
      val id = impromptu(test(m"stress"))
      runner.declare(id, Entry.Kind.Stress, Axis.Spec(t"N", Axis.Domain.Integral, true), 1.0, 8.0)
      runner.skip(id, Entry.Kind.Stress, Nil, 100L)
      runner.listed
    . assert(_ == Nil)


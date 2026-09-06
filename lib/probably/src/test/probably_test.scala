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

object Tests extends Suite(m"Probably Tests"):
  def run(): Unit =
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
      test(m"probe", n"slow", n"no-network").tags.map(_.text)
    . assert(_ == List(t"slow", t"no-network"))

    test(m"a tagged test is admitted by any of a tag: term's alternatives"):
      val id = test(m"probe", n"slow", n"network")

      List(t"tag:slow", t"tag:network,fast", t"tag:fast").map: term =>
        Selection.parse(List(term)).admits(id, Entry.Kind.Check, Nil, id.tags)
    . assert(_ == List(true, true, false))

    test(m"repeated tag: terms intersect"):
      val both = test(m"probe", n"slow", n"network")
      val lone = test(m"lone", n"slow")
      val selection = Selection.parse(List(t"tag:slow", t"tag:network"))

      ( selection.admits(both, Entry.Kind.Check, Nil, both.tags),
        selection.admits(lone, Entry.Kind.Check, Nil, lone.tags) )
    . assert(_ == (true, false))

    test(m"not:tag: excludes the tagged tests and keeps the rest"):
      val slow = test(m"slow one", n"slow")
      val quick = test(m"quick one")
      val selection = Selection.parse(List(t"not:tag:slow"))

      ( selection.admits(slow, Entry.Kind.Check, Nil, slow.tags),
        selection.admits(quick, Entry.Kind.Check, Nil, quick.tags) )
    . assert(_ == (false, true))

    test(m"not: on an axis constraint removes only the matching cells"):
      val id = test(m"probe")
      val axis = Axis(t"N")(4, 8)
      val selection = Selection.parse(List(t"not:N=4"))

      ( selection.admits(id, Entry.Kind.Check, List(axis.coordinate(4)), Nil),
        selection.admits(id, Entry.Kind.Check, List(axis.coordinate(8)), Nil),
        selection.admits(id, Entry.Kind.Check, Nil, Nil) )
    . assert(_ == (false, true, true))

    test(m"not: on a kind removes that kind alone"):
      val id = test(m"probe")
      val selection = Selection.parse(List(t"not:kind:bench"))

      ( selection.admits(id, Entry.Kind.Bench, Nil, Nil),
        selection.admits(id, Entry.Kind.Check, Nil, Nil) )
    . assert(_ == (false, true))

    test(m"not: on an identity removes that test from a wider selection"):
      val kept = test(n"kept", m"kept")
      val dropped = test(n"dropped", m"dropped")
      val selection = Selection.parse(List(t"**", t"not:dropped"))

      ( selection.admits(kept, Entry.Kind.Check, Nil, Nil),
        selection.admits(dropped, Entry.Kind.Check, Nil, Nil) )
    . assert(_ == (true, false))

    test(m"open ranges admit from, and up to, their bounds"):
      val id = test(m"probe")
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

      test(m"spread").over(Axis(t"n")(1, 2, 3)) { n => n }.assert(_ > 0)
      runner.listed.map { row => row.axes.map { axis => axis.values.map(_.text) } }
    . assert(_ == List(List(List(t"2", t"3"))))

    test(m"a declared emergent axis is listed with its bounds and no values"):
      val runner = listing()
      val id = test(m"stress")
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
      val id = test(m"stress")
      runner.declare(id, Entry.Kind.Stress, Axis.Spec(t"N", Axis.Domain.Integral, true), 1.0, 8.0)
      runner.skip(id, Entry.Kind.Stress, Nil, 100L)
      runner.listed
    . assert(_ == Nil)


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


import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import rudiments.*
import vacuous.*
import symbolism.*
import denominative.dysasymptotics.linearSize

object Selection:
  enum Term:
    case Identifier(name: Text)   // a stable moniker, or a 6-hex-digit hash
    case Glob(pattern: Text)      // matched against names and slash-joined name paths

  enum Constraint:
    case Membership(axis: Text, values: Set[Text])
    case Least(axis: Text, limit: Double, inclusive: Boolean)
    case Most(axis: Text, limit: Double, inclusive: Boolean)
    case Interval(axis: Text, least: Double, most: Double)

    def axis: Text

  val all: Selection = Selection(Nil, Nil, Nil, Nil, Nil, false, 1.0)

  private def hex(text: Text): Boolean =
    text.length == 6 && text.s.forall: char => char.isDigit || (char >= 'a' && char <= 'f')

  private def identifier(text: Text): Boolean =
    text.length > 0 && Character.isJavaIdentifierStart(text.s.charAt(0)) &&
      text.s.forall(Character.isJavaIdentifierPart(_))

  private def number(text: Text): Optional[Double] =
    if text.s.matches("-?[0-9]+(\\.[0-9]+)?") then text.s.toDouble else Unset

  // Parses command-line selection terms. Identity terms (hashes, monikers, name globs) are
  // unioned; `kind:` terms, `tag:` terms and axis constraints (`parser=jacinta`, `N<32`,
  // `N=4..64`, `N=4..`, `N=..64`) intersect with that union; and `not:<term>` terms are
  // subtracted last, each removing whatever its term would admit. Unrecognized terms are
  // treated as name globs.
  def parse(arguments: List[Text]): Selection =
    arguments.fold(all): (selection, argument) =>
      // An empty argument selects nothing and is not a term (so a bare `not:` is ignored).
      if argument == "" then selection
      else if argument == "--list" then selection.copy(listOnly = true)
      // `--scale=<factor>` is not a selection at all — it changes how long the tests it
      // admits are given to run — but it arrives on the same command line, and a host like
      // fume has no other channel to a suite. A non-positive or unparseable factor is
      // ignored rather than fatal: a mistyped duration should not lose a run's results.
      else if argument.starts("--scale=") then
        number(argument.skip(8)).lay(selection): factor =>
          if factor > 0.0 then selection.copy(scale = factor) else selection
      else if argument.starts("kind:") then
        val kinds = argument.skip(5) match
          case "test"    => List(Entry.Kind.Check)
          case "bench"   => List(Entry.Kind.Bench)
          case "stress"  => List(Entry.Kind.Stress)
          case "profile" => List(Entry.Kind.Profile)
          case _          => Nil

        selection.copy(kinds = selection.kinds + kinds)
      // `tag:a,b` admits a test carrying ANY of the listed tags; a second `tag:` term
      // intersects with the first, so `tag:slow tag:network` means slow AND network.
      else if argument.starts("tag:") then
        val tags: Set[Text] = argument.skip(4).cut(",").filter(_ != "").to[Set]
        if tags.nil then selection else selection.copy(tags = selection.tags :+ tags)
      // `not:<term>` subtracts: the inner term is parsed as a selection of its own, and any
      // cell it would admit is excluded. Each `not:` is independent (they union), so
      // `not:tag:slow not:kind:bench` excludes every slow test AND every benchmark. An inner
      // term that selects nothing in particular (`not:` alone, or `not:--list`) is ignored,
      // as excluding everything could never be what was meant.
      else if argument.starts("not:") then
        val exclusion = parse(List(argument.skip(4)))
        if exclusion.trivial then selection
        else selection.copy(exclusions = selection.exclusions :+ exclusion)
      else constraint(argument).lay(selection.copy(terms = selection.terms :+ term(argument))):
        constraint => selection.copy(constraints = selection.constraints :+ constraint)

  private def term(argument: Text): Term =
    if identifier(argument) || hex(argument) then Term.Identifier(argument) else Term.Glob(argument)

  private def constraint(argument: Text): Optional[Constraint] =
    def split(operator: Text): Optional[(Text, Text)] =
      val index = argument.s.indexOf(operator.s)
      if index <= 0 then Unset else (argument.keep(index), argument.skip(index + operator.length))

    def bound(operator: Text)(make: (Text, Double) => Constraint): Optional[Constraint] =
      split(operator).let: (axis, value) => number(value).let(make(axis, _))

    bound("<=")(Constraint.Most(_, _, true))
    . or(bound(">=")(Constraint.Least(_, _, true)))
    . or(bound("<")(Constraint.Most(_, _, false)))
    . or(bound(">")(Constraint.Least(_, _, false)))
    . or:
        split("=").let: (axis, value) =>
          if value.contains("..") then
            val index = value.s.indexOf("..")
            val least: Text = value.keep(index)
            val most: Text = value.skip(index + 2)

            // A range may be open at either end: `N=4..` means at least 4 and `N=..64` at
            // most 64 (both inclusive), spellings which need no shell quoting, unlike `>=`.
            if least == "" && most == "" then Unset
            else if least == "" then number(most).let(Constraint.Most(axis, _, true))
            else if most == "" then number(least).let(Constraint.Least(axis, _, true))
            else number(least).let { least => number(most).let(Constraint.Interval(axis, least, _)) }
          else Constraint.Membership(axis, value.cut(",").to[Set])


// A subset of a suite's tests, parsed from command-line terms: which tests run (and, for
// axial tests and benchmarks, which of their cells), or — with `--list` — which are only
// enumerated. An empty selection admits everything.
case class Selection
  ( terms:       List[Selection.Term],
    kinds:       List[Entry.Kind],
    constraints: List[Selection.Constraint],
    // Each element is one `tag:` term — a set of alternatives — and the elements intersect.
    tags:        List[Set[Text]],
    // The `not:` terms, each a one-term selection, subtracted after admission.
    exclusions:  List[Selection],
    listOnly:    Boolean,
    // The multiplier applied to every declared target DURATION — `Bench`'s, `Stress`'s and
    // `Profile`'s — so that a whole run can be made proportionally longer (a careful
    // overnight pass) or shorter (a quick check) without editing the suite. Geometric, and
    // 1.0 by default: 2.0 runs each measurement twice as long, 0.25 a quarter as long.
    // Latency thresholds are NOT scaled: they are pass/fail criteria, not durations.
    scale:       Double ):

  def trivial: Boolean =
    terms.nil && kinds.nil && constraints.nil && tags.nil && exclusions.nil

  def admits
    ( id: Test.Id, kind: Entry.Kind, coordinates: List[(Axis.Spec, Value)], tags: List[Tag] )
  :   Boolean =

    admitted(kind) && admitted(id) && admitted(coordinates, false) && admitted(tags)
    && !exclusions.exists(_.excludes(id, kind, coordinates, tags))

  // Whether this selection, as a `not:` term, removes the cell. Identical to admission but
  // for one thing: a constraint on an axis the cell does not have matches NOTHING here,
  // where for admission it admits everything — `not:N=4` must not remove tests without an
  // `N` axis, just as `N=4` must not exclude them.
  private def excludes
    ( id: Test.Id, kind: Entry.Kind, coordinates: List[(Axis.Spec, Value)], tags: List[Tag] )
  :   Boolean =

    admitted(kind) && admitted(id) && admitted(coordinates, true) && admitted(tags)

  private def admitted(kind: Entry.Kind): Boolean = kinds.nil || kinds.has(kind)

  private def admitted(tags: List[Tag]): Boolean =
    this.tags.all { alternatives => tags.exists { tag => alternatives.has(tag.text) } }

  private def ancestry(id: Test.Id): List[Test.Id] =
    id :: id.suite.let { suite => ancestry(suite.id) }.or(Nil)

  private def admitted(id: Test.Id): Boolean = terms.nil || locally:
    val chain = ancestry(id)
    val names = chain.reverse.map(_.name.text)
    val path = names.join("/")

    // A path of monikers where declared, falling back to names, so that mixed selections
    // like `jacinta/parseJson` work even when only some links are named.
    val monikerPath = chain.reverse.map { link => link.moniker.or(link.name.text) }.join("/")

    terms.exists:
      case Selection.Term.Identifier(name) =>
        chain.exists: link => link.id == name || link.moniker.lay(false)(_ == name)

      // Kaleidoscope glob semantics (`?`, `[a-z]`, `[!a-z]` now work): `*` matches within one
      // `/`-joined link, so spanning a suite path takes `**`, e.g. `jacinta/**` or
      // `**/parseJson`. (Formerly a private translation in which `*` crossed `/`.)
      case Selection.Term.Glob(pattern) =>
        val glob = kaleidoscope.Glob.parse(pattern)

        names.exists(glob.matches(_))
        || glob.matches(path)
        || glob.matches(monikerPath)

  // `strict`: whether a constraint on an axis absent from the coordinates fails (for an
  // exclusion) rather than passes (for an admission).
  private def admitted(coordinates: List[(Axis.Spec, Value)], strict: Boolean): Boolean =
    constraints.all: constraint =>
      coordinates.seek(_(0).label == constraint.axis).lay(!strict): pair =>
        val value = pair(1)

        constraint match
          case Selection.Constraint.Membership(_, values) =>
            values.has(value.text)

          case Selection.Constraint.Least(_, limit, inclusive) =>
            value.numeric.lay(false): numeric =>
              if inclusive then numeric >= limit else numeric > limit

          case Selection.Constraint.Most(_, limit, inclusive) =>
            value.numeric.lay(false): numeric =>
              if inclusive then numeric <= limit else numeric < limit

          case Selection.Constraint.Interval(_, least, most) =>
            value.numeric.lay(false) { numeric => numeric >= least && numeric <= most }

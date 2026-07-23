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
import distillate.*
import gossamer.*
import rudiments.*
import vacuous.*

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

  val all: Selection = Selection(Nil, Nil, Nil, false)

  private def hex(text: Text): Boolean =
    text.length == 6 && text.s.forall: char =>
      char.isDigit || (char >= 'a' && char <= 'f')

  private def identifier(text: Text): Boolean =
    text.length > 0 && Character.isJavaIdentifierStart(text.s.charAt(0)) &&
      text.s.forall(Character.isJavaIdentifierPart(_))

  private def number(text: Text): Optional[Double] =
    if text.s.matches("-?[0-9]+(\\.[0-9]+)?") then text.s.toDouble else Unset

  // Parses command-line selection terms. Identity terms (hashes, monikers, name globs) are
  // unioned; `kind:` terms and axis constraints (`parser=jacinta`, `N<32`, `N=4..64`)
  // intersect with that union. Unrecognized terms are treated as name globs.
  def parse(arguments: List[Text]): Selection =
    arguments.foldLeft(all): (selection, argument) =>
      if argument == t"--list" then selection.copy(listOnly = true)
      else if argument.starts(t"kind:") then
        val kinds = argument.skip(5) match
          case t"test"    => List(Entry.Kind.Check)
          case t"bench"   => List(Entry.Kind.Bench)
          case t"stress"  => List(Entry.Kind.Stress)
          case t"profile" => List(Entry.Kind.Profile)
          case _          => Nil

        selection.copy(kinds = selection.kinds ::: kinds)
      else constraint(argument).lay(selection.copy(terms = selection.terms :+ term(argument))):
        constraint => selection.copy(constraints = selection.constraints :+ constraint)

  private def term(argument: Text): Term =
    if identifier(argument) || hex(argument) then Term.Identifier(argument)
    else Term.Glob(argument)

  private def constraint(argument: Text): Optional[Constraint] =
    def split(operator: Text): Optional[(Text, Text)] =
      val index = argument.s.indexOf(operator.s)
      if index <= 0 then Unset else (argument.keep(index), argument.skip(index + operator.length))

    def bound(operator: Text)(make: (Text, Double) => Constraint): Optional[Constraint] =
      split(operator).let: (axis, value) =>
        number(value).let(make(axis, _))

    bound(t"<=")(Constraint.Most(_, _, true))
    . or(bound(t">=")(Constraint.Least(_, _, true)))
    . or(bound(t"<")(Constraint.Most(_, _, false)))
    . or(bound(t">")(Constraint.Least(_, _, false)))
    . or:
        split(t"=").let: (axis, value) =>
          if value.contains(t"..") then
            val index = value.s.indexOf("..")
            val least = number(value.keep(index))
            val most = number(value.skip(index + 2))

            least.let { least => most.let(Constraint.Interval(axis, least, _)) }
          else Constraint.Membership(axis, value.cut(t",").to(Set))

  private[probably] def globRegex(pattern: Text): Text =
    pattern.cut(t"*").map { part => java.util.regex.Pattern.quote(part.s).nn.tt }.join(t".*")

// A subset of a suite's tests, parsed from command-line terms: which tests run (and, for
// axial tests and benchmarks, which of their cells), or — with `--list` — which are only
// enumerated. An empty selection admits everything.
case class Selection
  ( terms:       List[Selection.Term],
    kinds:       List[Entry.Kind],
    constraints: List[Selection.Constraint],
    listOnly:    Boolean ):

  def trivial: Boolean = terms.isEmpty && kinds.isEmpty && constraints.isEmpty

  def admits(id: TestId, kind: Entry.Kind, coordinates: List[(Axis.Spec, Value)]): Boolean =
    admitted(kind) && admitted(id) && admitted(coordinates)

  private def admitted(kind: Entry.Kind): Boolean = kinds.isEmpty || kinds.contains(kind)

  private def ancestry(id: TestId): List[TestId] =
    id :: id.suite.let { suite => ancestry(suite.id) }.or(Nil)

  private def admitted(id: TestId): Boolean = terms.isEmpty || locally:
    val chain = ancestry(id)
    val names = chain.reverse.map(_.name.text)
    val path = names.join(t"/")

    // A path of monikers where declared, falling back to names, so that mixed selections
    // like `jacinta/parseJson` work even when only some links are named.
    val monikerPath = chain.reverse.map { link => link.moniker.or(link.name.text) }.join(t"/")

    terms.exists:
      case Selection.Term.Identifier(name) =>
        chain.exists: link =>
          link.id == name || link.moniker.lay(false)(_ == name)

      case Selection.Term.Glob(pattern) =>
        val regex = Selection.globRegex(pattern)

        names.exists(_.s.matches(regex.s))
        || path.s.matches(regex.s)
        || monikerPath.s.matches(regex.s)

  private def admitted(coordinates: List[(Axis.Spec, Value)]): Boolean =
    constraints.all: constraint =>
      coordinates.find(_(0).label == constraint.axis).fold(true): pair =>
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

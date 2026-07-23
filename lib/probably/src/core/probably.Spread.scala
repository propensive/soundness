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

import scala.deriving.*

import chiaroscuro.*
import distillate.*
import vacuous.*

// A test spread over the domain of one axis: its body runs once per axis value, and each
// verdict is recorded at that value’s coordinate of a single named test. A partial body
// (`{ case … }`) leaves gaps: values it does not define are skipped. Assertion predicates
// may consider just the result, or the axis value and the result together.
//
// The body is stored as a function OF the harness which yields the partial function, not
// as a partial function over harness-contextual results: reading a root-captured value out
// of a partial function’s range charges an unusable reach capability, whereas this field
// reads as an ordinary path capability, exactly like `Test.action`.
case class Spread[value, result]
  ( id: TestId, axis: Axis[value], action: Harness => (value ~> result) )

object Spread:
  extension [value, result](spread: Spread[value, result]^)
    def assert[report]()
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.axis, spread.action, { (_, _) => true }, false)

    def assert[report](predicate: result => Boolean)
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.axis, spread.action, { (_, result) => predicate(result) }, false)

    def assert[report](predicate: (value, result) => Boolean)
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.axis, spread.action, predicate, false)

    def aspire[report](predicate: (value, result) => Boolean)
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.axis, spread.action, predicate, true)

  private def run[value, result, report]
    ( id:           TestId,
      axis:         Axis[value],
      action:       (Harness => (value ~> result))^,
      predicate:    (value, result) => Boolean,
      aspirational: Boolean )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Verdict],
            details:   Inclusion[report, Verdict.Detail] )
  :   Unit =

    // Definedness may not depend on the harness, so gaps are probed with a fresh one; the
    // real harness reaches each cell’s body through `Runner.run`, exactly as for `Test`.
    val probe: value ~> result = action(Harness())
    val values = axis.values
    var index = 0

    while index < values.length do
      val value = values(index)

      if probe.isDefinedAt(value) then
        val coordinates = List(axis.coordinate(value))
        val test = Test[result](id, action(_)(value))

        probably.internal.assertion[result, result, report, Unit]
          ( runner,
            test,
            predicate(value, _),
            { _ => () },
            Unset,
            inclusion,
            details,
            Decomposable.any[result],
            aspirational,
            coordinates,
            true )

      index += 1

// A test spread over the domains of two axes: one cell per combination the body defines,
// rendered as a grid with gaps at undefined combinations.
case class Spread2[left, right, result]
  ( id:     TestId,
    first:  Axis[left],
    second: Axis[right],
    action: Harness => (((left, right)) ~> result) )

object Spread2:
  extension [left, right, result](spread: Spread2[left, right, result]^)
    def assert[report]()
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.first, spread.second, spread.action, { (_, _, _) => true }, false)

    def assert[report](predicate: result => Boolean)
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run
        ( spread.id,
          spread.first,
          spread.second,
          spread.action,
          { (_, _, result) => predicate(result) },
          false )

    def assert[report](predicate: (left, right, result) => Boolean)
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.first, spread.second, spread.action, predicate, false)

    def aspire[report](predicate: (left, right, result) => Boolean)
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Verdict],
              details:   Inclusion[report, Verdict.Detail] )
    :   Unit =

      run(spread.id, spread.first, spread.second, spread.action, predicate, true)

  private def run[left, right, result, report]
    ( id:           TestId,
      first:        Axis[left],
      second:       Axis[right],
      action:       (Harness => (((left, right)) ~> result))^,
      predicate:    (left, right, result) => Boolean,
      aspirational: Boolean )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Verdict],
            details:   Inclusion[report, Verdict.Detail] )
  :   Unit =

    val probe: ((left, right)) ~> result = action(Harness())
    val lefts = first.values
    val rights = second.values
    var leftIndex = 0

    while leftIndex < lefts.length do
      val left = lefts(leftIndex)
      var rightIndex = 0

      while rightIndex < rights.length do
        val right = rights(rightIndex)

        if probe.isDefinedAt((left, right)) then
          val coordinates = List(first.coordinate(left), second.coordinate(right))
          val test = Test[result](id, action(_)((left, right)))

          probably.internal.assertion[result, result, report, Unit]
            ( runner,
              test,
              predicate(left, right, _),
              { _ => () },
              Unset,
              inclusion,
              details,
              Decomposable.any[result],
              aspirational,
              coordinates,
              true )

        rightIndex += 1

      leftIndex += 1

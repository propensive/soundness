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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package aviation

import prepositional.*
import symbolism.*
import vacuous.*

object Period:
  // Split a period into consecutive `length`-long segments. The step is whatever the point can be
  // advanced by — a `Duration` or `Timespan` for an `Instant over X`, a `Timespan` for a
  // `Timestamp`/`Moment` (irregular spans pull in their `Calendar`/`Disambiguation`). When `length`
  // doesn't divide the period exactly, `partial` decides whether the short final segment is kept.
  extension [point](period: Period[point])
    def segments[step](length: step, partial: Boolean = true)
      ( using addable: point is Addable by step to point, order: Ordering[point] )
    :   List[Period[point]] =

      @scala.annotation.tailrec
      def recur(current: point, acc: List[Period[point]]): List[Period[point]] =
        if !order.gt(period.finish, current) then acc.reverse else
          val next = current + length

          if !order.gt(next, current) || order.gt(next, period.finish)
          then (if partial then Period(current, period.finish) :: acc else acc).reverse
          else recur(next, Period(current, next) :: acc)

      recur(period.start, Nil)

// A range between two points of the same type — an `Instant over X`, a `Timestamp`, or a `Moment`.
// `duration` is the points' difference, whose type follows the point: a `Duration` for an
// `Instant over X` (in that timeline's seconds), a `Timespan` for a `Timestamp`/`Moment`. (Ordering
// uses the non-inline `Ordering`, since the point is abstract here.)
case class Period[point](start: point, finish: point):
  def duration(using subtractable: point is Subtractable by point): subtractable.Result =
    subtractable.subtract(finish, start)

  def intersect(period: Period[point])(using order: Ordering[point]): Optional[Period[point]] =
    val start2 = order.max(period.start, start)
    val finish2 = order.min(period.finish, finish)
    if order.gteq(start2, finish2) then Unset else Period(start2, finish2)

  def union(period: Period[point])(using order: Ordering[point]): Set[Period[point]] =
    if intersect(period).absent then Set(this, period)
    else Set(Period(order.min(start, period.start), order.max(finish, period.finish)))

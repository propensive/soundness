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
package aviation

import rudiments.*
import denominative.*
import symbolism.*
import vacuous.*
import rudiments.sortingAlgorithms.timsort

// An iCalendar recurrence set: the union of one or more recurrences' occurrence streams (`include`,
// e.g. each `rrule.occurrences`) plus explicit extra dates (`rdates`, RFC 5545 `RDATE`), minus
// excluded dates (`exdates`, `EXDATE`). It is itself `Recurrent` — the streams are merged into one
// ascending, de-duplicated stream with the exclusions removed — so it composes uniformly with any
// other `Recurrent`.
object RecurrenceSet:
  given recurrent: [point] => point is Comparable
  =>  ( RecurrenceSet[point] is Recurrent { type Topic = point } ) =

    set =>
      val excluded: Set[point] = set.exdates.to[Set]
      val streams: List[Chain[point]] = set.include + List(set.rdates.sort.to[Chain])
      dedup(streams.occupied.lay(Chain.empty[point])(_.reduce(merge))).filter(!excluded.has(_))

  // Lazily merge two ascending streams into one ascending stream (emit the lesser head first).
  // The `#::` matches prove non-emptiness structurally and force nothing beyond the two heads;
  // the n-way merge is a `reduce` of this over the (proven non-empty) list of streams.
  private def merge[point](left: Chain[point], right: Chain[point])(using order: point is Comparable)
  :   Chain[point] =

    left match
      case leftHead #:: leftTail =>
        right match
          case rightHead #:: rightTail =>
            if order.atMost(leftHead, rightHead) then leftHead #:: merge(leftTail, right)
            else rightHead #:: merge(left, rightTail)

          case _ =>
            left

      case _ =>
        right

  // Drop duplicates from an ascending stream (equal values are adjacent).
  private def dedup[point](stream: Chain[point])(using order: point is Comparable): Chain[point] =
    stream match
      case first #:: rest => first #:: dedup(rest.skip(order.same(_, first)))
      case _              => Chain.empty

case class RecurrenceSet[point]
  ( include: List[Chain[point]] = Nil,
    rdates:  List[point]           = Nil,
    exdates: List[point]           = Nil )

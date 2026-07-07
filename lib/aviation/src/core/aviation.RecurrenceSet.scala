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
┃    Soundness, version 0.63.0.                                                                    ┃
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

// An iCalendar recurrence set: the union of one or more recurrences' occurrence streams (`include`,
// e.g. each `rrule.occurrences`) plus explicit extra dates (`rdates`, RFC 5545 `RDATE`), minus
// excluded dates (`exdates`, `EXDATE`). It is itself `Recurrent` — the streams are merged into one
// ascending, de-duplicated stream with the exclusions removed — so it composes uniformly with any
// other `Recurrent`.
object RecurrenceSet:
  given recurrent: [point] => Ordering[point]
  =>  ( RecurrenceSet[point] is Recurrent { type Topic = point } ) =

    set =>
      val excluded = set.exdates.to(Set)
      val streams = set.include :+ LazyList.from(set.rdates.sorted)
      dedup(merge(streams)).filterNot(excluded.contains)

  // Lazily merge ascending streams into one ascending stream (repeatedly emit the least head).
  private def merge[point](streams: List[LazyList[point]])(using order: Ordering[point])
  :   LazyList[point] =

    streams.filter(_.nonEmpty) match
      case Nil => LazyList.empty

      case nonEmpty =>
        val index = nonEmpty.indices.minBy(nonEmpty(_).head)
        val chosen = nonEmpty(index)

        chosen.head #:: merge(nonEmpty.updated(index, chosen.tail))

  // Drop duplicates from an ascending stream (equal values are adjacent).
  private def dedup[point](stream: LazyList[point])(using order: Ordering[point]): LazyList[point] =
    stream match
      case head #:: tail => head #:: dedup(tail.dropWhile(order.equiv(_, head)))
      case _             => LazyList.empty

case class RecurrenceSet[point]
  ( include: List[LazyList[point]] = Nil,
    rdates:  List[point]           = Nil,
    exdates: List[point]           = Nil )

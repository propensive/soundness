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
package rudiments

import denominative.*
import prepositional.*

// Disjoint-group and sliding-window iteration (issue #1666, category 7): the read side of
// mismatched-stride codecs. Each combinator walks whole groups of a static arity, passing
// the *values* — a codec's transformation needs the elements, not their positions — and
// returns the branded remainder (the trailing partial group, possibly empty), which feeds
// `iterate`/`at` for the tail-handling the codec defines. The write side of these loops is
// sequential by nature, which `Scribe.append` already expresses, so no write-side grouping
// is needed.
extension [collection](value: collection)
  // Whole disjoint pairs, in order; the branded remainder holds the 0 or 1 trailing element.
  inline def pairs
    ( using countable: collection is Countable, indexable: (collection is Indexable by Ordinal) )
    ( inline lambda: (indexable.Result, indexable.Result) => Unit )
  :   Interval in value.type =

    val size = countable.size(value)
    val full = size - size%2
    var index = 0

    while index < full do
      lambda
        ( indexable.access(value, Ordinal.zerary(index)),
          indexable.access(value, Ordinal.zerary(index + 1)) )

      index += 2

    Interval.zerary(full, size).asInstanceOf[Interval in value.type]

  // Whole disjoint triples; the branded remainder holds the 0-2 trailing elements.
  inline def triples
    ( using countable: collection is Countable, indexable: (collection is Indexable by Ordinal) )
    ( inline lambda: (indexable.Result, indexable.Result, indexable.Result) => Unit )
  :   Interval in value.type =

    val size = countable.size(value)
    val full = size - size%3
    var index = 0

    while index < full do
      lambda
        ( indexable.access(value, Ordinal.zerary(index)),
          indexable.access(value, Ordinal.zerary(index + 1)),
          indexable.access(value, Ordinal.zerary(index + 2)) )

      index += 3

    Interval.zerary(full, size).asInstanceOf[Interval in value.type]

  // Whole disjoint quadruples; the branded remainder holds the 0-3 trailing elements.
  inline def quads
    ( using countable: collection is Countable, indexable: (collection is Indexable by Ordinal) )
    ( inline lambda:
        (indexable.Result, indexable.Result, indexable.Result, indexable.Result) => Unit )
  :   Interval in value.type =

    val size = countable.size(value)
    val full = size - size%4
    var index = 0

    while index < full do
      lambda
        ( indexable.access(value, Ordinal.zerary(index)),
          indexable.access(value, Ordinal.zerary(index + 1)),
          indexable.access(value, Ordinal.zerary(index + 2)),
          indexable.access(value, Ordinal.zerary(index + 3)) )

      index += 4

    Interval.zerary(full, size).asInstanceOf[Interval in value.type]

  // Every adjacent (overlapping) pair, in order: the boundary-window shape. A collection of
  // fewer than two elements yields nothing.
  inline def adjacent
    ( using countable: collection is Countable, indexable: (collection is Indexable by Ordinal) )
    ( inline lambda: (indexable.Result, indexable.Result) => Unit )
  :   Unit =

    val size = countable.size(value)
    var index = 1

    while index < size do
      lambda
        ( indexable.access(value, Ordinal.zerary(index - 1)),
          indexable.access(value, Ordinal.zerary(index)) )

      index += 1

  // Whole disjoint groups of five; the branded remainder holds the 0-4 trailing elements.
  inline def quints
    ( using countable: collection is Countable, indexable: (collection is Indexable by Ordinal) )
    ( inline lambda:
        ( indexable.Result, indexable.Result, indexable.Result, indexable.Result,
          indexable.Result ) => Unit )
  :   Interval in value.type =

    val size = countable.size(value)
    val full = size - size%5
    var index = 0

    while index < full do
      lambda
        ( indexable.access(value, Ordinal.zerary(index)),
          indexable.access(value, Ordinal.zerary(index + 1)),
          indexable.access(value, Ordinal.zerary(index + 2)),
          indexable.access(value, Ordinal.zerary(index + 3)),
          indexable.access(value, Ordinal.zerary(index + 4)) )

      index += 5

    Interval.zerary(full, size).asInstanceOf[Interval in value.type]

  // Whole disjoint octuples; the branded remainder holds the 0-7 trailing elements.
  inline def octuples
    ( using countable: collection is Countable, indexable: (collection is Indexable by Ordinal) )
    ( inline lambda:
        ( indexable.Result, indexable.Result, indexable.Result, indexable.Result,
          indexable.Result, indexable.Result, indexable.Result, indexable.Result ) => Unit )
  :   Interval in value.type =

    val size = countable.size(value)
    val full = size - size%8
    var index = 0

    while index < full do
      lambda
        ( indexable.access(value, Ordinal.zerary(index)),
          indexable.access(value, Ordinal.zerary(index + 1)),
          indexable.access(value, Ordinal.zerary(index + 2)),
          indexable.access(value, Ordinal.zerary(index + 3)),
          indexable.access(value, Ordinal.zerary(index + 4)),
          indexable.access(value, Ordinal.zerary(index + 5)),
          indexable.access(value, Ordinal.zerary(index + 6)),
          indexable.access(value, Ordinal.zerary(index + 7)) )

      index += 8

    Interval.zerary(full, size).asInstanceOf[Interval in value.type]

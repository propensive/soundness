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
import vacuous.*

// The 2-D geometry helper for confined indexing (issue #1666): a `Lattice` brands nothing
// itself, but derives *linear* branded intervals and ordinals for its underlying collection
// from two-dimensional coordinates, so all access flows through the existing confined
// machinery — and since `Scribe` is `Countable`, the same lattice addresses a write handle.
//
// The dangerous arithmetic it replaces is `y*stride + x` with stride ≠ width: pixel planes,
// interlaced rows, subsampled chroma. The lender clamps the height so that every row it can
// mint lies wholly within the storage, making `row` and `point` total by construction; a
// partial trailing row is simply outside the lattice.
final class Lattice[brand] @scala.annotation.publicInBinary private[rudiments]
  ( val width: Int, val height: Int, stride: Int, offset: Int ):

  // Row `y` as a branded linear interval, or `Unset` outside the lattice.
  inline def row(y: Int): Optional[Interval in brand] =
    if y < 0 || y >= height then Unset else
      val start = offset + y*stride
      Interval.zerary(start, start + width).asInstanceOf[Interval in brand]

  // The linear position of `(x, y)`, or `Unset` outside the lattice.
  inline def point(x: Int, y: Int): Optional[Ordinal in brand] =
    if x < 0 || x >= width || y < 0 || y >= height then Unset
    else Ordinal.zerary(offset + y*stride + x).asInstanceOf[Ordinal in brand]

  // Every row in order, as its index and branded interval.
  inline def rows(inline lambda: (Int, Interval in brand) => Unit): Unit =
    var y = 0

    while y < height do
      val start = offset + y*stride
      lambda(y, Interval.zerary(start, start + width).asInstanceOf[Interval in brand])
      y += 1

extension [collection: Countable](value: collection)
  // Lend a lattice of `width`-element rows spaced `stride` apart, starting at `offset`: the
  // height is the number of whole rows that fit, so everything the lattice mints is in
  // range. `stride` is clamped to at least `width` (overlapping rows would alias), and a
  // degenerate geometry yields a zero-height lattice rather than anything partial.
  inline def lattice[result](width: Int, stride: Int, offset: Int)
    ( inline lambda: Lattice[value.type] => result )
  :   result =

    val size = summon[collection is Countable].size(value)
    val width2 = width.max(0)
    val stride2 = stride.max(width2).max(1)
    val offset2 = offset.max(0)

    val height =
      if width2 == 0 || offset2 + width2 > size then 0
      else (size - offset2 - width2)/stride2 + 1

    lambda(new Lattice[value.type](width2, height, stride2, offset2))

  // A lattice over the whole extent from the start.
  inline def lattice[result](width: Int)(inline lambda: Lattice[value.type] => result): result =
    lattice(width, width, 0)(lambda)

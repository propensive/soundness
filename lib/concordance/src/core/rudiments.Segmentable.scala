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

import scala.collection.immutable.IndexedSeq

import anticipation.*
import denominative.*
import prepositional.*

object Segmentable:
  given indexedSeq: [element]
  =>  ( IndexedSeq[element] is Segmentable { type Segment = IndexedSeq[element] } ) =
    (sequence, interval) => sequence.slice(interval.start.n0, interval.limit.n0)

  // Copied with the element's `ClassTag` rather than through `IArray.slice`, which
  // materializes one from the array's runtime class on every call — a `ClassValue` lookup
  // that dominated a binary parser's profile. The bounds clamp as `slice`'s do.
  given iarray: [element: scala.reflect.ClassTag]
  =>  ( (Array[element]^{}) is Segmentable { type Segment = Array[element]^{} } ) =
    (iarray: Array[element]^{}, interval: Interval) =>
      val length = iarray.readable.length
      val start = interval.start.n0.max(0).min(length)
      val end = interval.limit.n0.max(start).min(length)
      val copy = Array.allocate[element](end - start)
      System.arraycopy(Array.unsafeJvm(iarray), start, copy.raw, 0, end - start)
      Array.freeze(copy)

  // Opaque `Sequence` is no longer an `IndexedSeq` subtype, so it needs its own instance.
  // `Self` is subtype-parametric (branded receivers match) but `Segment` is the PLAIN type:
  // a segment of a non-empty value may be empty, so the proof must not survive — the same
  // soundness rule as `Truncable`'s `Result`.
  given sequence: [element, sequence <: Sequence[element]]
  =>  ( sequence is Segmentable { type Segment = Sequence[element] } ) =
    (sequence, interval) => Sequence.slice(sequence, interval.start.n0, interval.limit.n0)

  given list: [element, list <: List[element]]
  =>  ( list is Segmentable { type Segment = List[element] } ) =
    (list, interval) => List.slice(list, interval.start.n0, interval.limit.n0)

  given text: (Text is Segmentable { type Segment = Text }) = (text, interval) =>
    val min = interval.start.n0.max(0)
    val max = interval.limit.n0.min(text.s.length)
    text.s.substring(min, max).nn.tt

trait Segmentable extends Typeclass.Pure:
  // The output shape: `Self` for the shape-preserving textual instances (fixed once in
  // `Textual`), the plain unbranded type for collections.
  type Segment

  def segment(entity: Self, interval: Interval): Segment

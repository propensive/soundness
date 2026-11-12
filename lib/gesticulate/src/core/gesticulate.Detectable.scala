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
package gesticulate

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
import zephyrine.*

// The ability to detect a media type from content: `("image/gif" is Detectable under Likely over
// Probe to (Raster in Gif))` says that a stream can be recognised as `image/gif` from a forward
// read, that a match is `Likely`, and that such content is read as a `Raster in Gif`.
//
// `Transport` is the access the detector needs: a `Probe`, the forward-only view of a stream, or
// an `Expanse`, for a format which is identified by something a forward read cannot reach — a
// ZIP file by the index at its end. An instance over a `Probe` serves an `Expanse` too.
//
// `Limit` is how much a match establishes: see `Plausible`, `Likely` and `Certain`.
//
// Each instance is a contextual value in the `detectables` family, imported by name, and the
// pattern `Media["image/gif"](gif)` compiles only where one is in scope.
object Detectable:
  final class Sequential[media <: Label, limit <: Plausible, result]
    ( lambda: (probe: Probe^) -> Boolean )
  extends Detectable:
    type Self = media
    type Limit = limit
    type Transport = Probe
    type Result = result

    def detect(probe: Probe^): Boolean = lambda(probe)

  final class Positional[media <: Label, limit <: Plausible, result]
    ( lambda: (expanse: Expanse^) -> Boolean )
  extends Detectable:
    type Self = media
    type Limit = limit
    type Transport = Expanse
    type Result = result

    def detect(expanse: Expanse^): Boolean = lambda(expanse)

  // A detector which reads forwards from the start of the content.
  def apply[media <: Label, limit <: Plausible, result](lambda: (probe: Probe^) -> Boolean)
  :   Sequential[media, limit, result] =

    Sequential[media, limit, result](lambda)

  // A detector which needs random access to the content.
  def positional[media <: Label, limit <: Plausible, result]
    ( lambda: (expanse: Expanse^) -> Boolean )
  :   Positional[media, limit, result] =

    Positional[media, limit, result](lambda)

  // A detector for a format which reserves `bytes` at its start.
  def signature[media <: Label, limit <: Plausible, result](bytes: Data)
  :   Sequential[media, limit, result] =

    signatureAt[media, limit, result](0, bytes)

  // The same, with the bytes written out: `Detectable.signature(0x1f, 0x8b)`.
  def signature[media <: Label, limit <: Plausible, result](bytes: Int*)
  :   Sequential[media, limit, result] =

    signatureAt[media, limit, result](0, bytes.map(_.toByte).to[Array])

  // A detector for a format which reserves the bytes written out at `offset`:
  // `Detectable.signatureAt(257)(0x75, 0x73, 0x74, 0x61, 0x72)`.
  def signatureAt[media <: Label, limit <: Plausible, result](offset: Int)(bytes: Int*)
  :   Sequential[media, limit, result] =

    signatureAt[media, limit, result](offset, bytes.map(_.toByte).to[Array])

  // A detector for a format which reserves `bytes` at `offset`.
  def signatureAt[media <: Label, limit <: Plausible, result](offset: Int, bytes: Data)
  :   Sequential[media, limit, result] =

    Sequential[media, limit, result]: probe => probe.skip(offset) == offset && probe.expect(bytes)

  // Applies a detector to `cursor`, which is left where it was found.
  private[gesticulate] def sequential(detectable: Detectable, cursor: Cursor[Data, {}]^): Boolean =
    detectable match
      case detectable: Sequential[?, ?, ?] => Probe.attempt(cursor)(detectable.detect(_))
      case detectable: Positional[?, ?, ?] => false

  // Applies a detector to `expanse`: a sequential one reads it forwards from its start, a block
  // at a time, and only as far as it asks.
  private[gesticulate] def positional(detectable: Detectable, expanse: Expanse^): Boolean =
    detectable match
      case detectable: Positional[?, ?, ?] =>
        detectable.detect(expanse)

      case detectable: Sequential[?, ?, ?] =>
        val size = expanse.size

        def blocks(offset: Long): Chain[Data] =
          if offset >= size then Chain() else
            val block = expanse.read(offset, (size - offset).min(4096L).toInt)
            if block.length == 0 then Chain() else block #:: blocks(offset + block.length)

        Probe.attempt(Cursor[Data](Chain.defer(blocks(0L))))(detectable.detect(_))

  // The contextual detector for `label` which establishes the most, among those which read
  // forwards and establish at least `floor`, applied to `cursor`.
  transparent inline def cursor[label <: Label, floor <: Plausible]
    ( cursor: Cursor[Data, {}]^ )
  :   Option[Any] =

    gesticulate.internal.check[label]

    scala.compiletime.summonFrom:
      case detectable: (`label` is Detectable under (? <: (floor & Certain)) over Probe) =>
        detection[label, detectable.Limit, detectable.Result](sequential(detectable, cursor))

      case detectable: (`label` is Detectable under (? <: (floor & Likely)) over Probe) =>
        detection[label, detectable.Limit, detectable.Result](sequential(detectable, cursor))

      case detectable: (`label` is Detectable under (? <: floor) over Probe) =>
        detection[label, detectable.Limit, detectable.Result](sequential(detectable, cursor))

      case _ =>
        gesticulate.internal.undetectable[label, floor, Probe]

  // The same for content with random access, which every detector can read; between two
  // detectors which establish as much, the one which uses the random access is preferred.
  transparent inline def expanse[label <: Label, floor <: Plausible](expanse: Expanse^)
  :   Option[Any] =

    gesticulate.internal.check[label]

    scala.compiletime.summonFrom:
      case detectable: (`label` is Detectable under (? <: (floor & Certain)) over Expanse) =>
        detection[label, detectable.Limit, detectable.Result](positional(detectable, expanse))

      case detectable: (`label` is Detectable under (? <: (floor & Certain)) over Probe) =>
        detection[label, detectable.Limit, detectable.Result](positional(detectable, expanse))

      case detectable: (`label` is Detectable under (? <: (floor & Likely)) over Expanse) =>
        detection[label, detectable.Limit, detectable.Result](positional(detectable, expanse))

      case detectable: (`label` is Detectable under (? <: (floor & Likely)) over Probe) =>
        detection[label, detectable.Limit, detectable.Result](positional(detectable, expanse))

      case detectable: (`label` is Detectable under (? <: floor) over Expanse) =>
        detection[label, detectable.Limit, detectable.Result](positional(detectable, expanse))

      case detectable: (`label` is Detectable under (? <: floor) over Probe) =>
        detection[label, detectable.Limit, detectable.Result](positional(detectable, expanse))

      case _ =>
        gesticulate.internal.undetectable[label, floor, Expanse]

  inline def detection[label <: Label, limit <: Plausible, result](matched: Boolean)
  :   Option[Detection of label under limit to result] =

    if matched
    then Some(Detection.Bound[label, limit, result](scala.compiletime.constValue[label].tt))
    else None

  // Data in memory, as content with random access. Public, because the inlined `unapply` calls
  // it wherever it is expanded, and an accessor synthesized for a private method cannot take
  // frozen `Data`.
  def memory(data: Data): Expanse = new Memory(data)

  private final class Memory(data: Data) extends Expanse:
    def size: Long = data.length.toLong

    def read(offset: Long, length: Int): Data =
      val start = offset.max(0L).min(size).toInt
      val end = (offset + length).max(start.toLong).min(size).toInt
      data.segment(start.z till end.z)

// Sealed, so that a detector is one of the two shapes above and never reaches the content except
// through the `Probe` or the `Expanse` it is handed.
sealed trait Detectable extends Typeclass.Pure, Resultant, Limited, Transportive:
  type Self <: Label
  type Limit <: Plausible
  type Transport <: Probe | Expanse

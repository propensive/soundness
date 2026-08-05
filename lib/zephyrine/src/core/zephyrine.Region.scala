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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package zephyrine

import scala.caps

import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*

// A `Region` is a bounds-safe fragment of a medium's backing storage (issue #1666): an opaque
// reference to the storage that grants no direct access — no `length`, no raw indexed read.
// The valid extent travels separately as an `Interval` branded to the region's identity
// (`Interval in region.type`), and the only producers of branded `Ordinal`s are the trusted
// combinators below, so `apply` is total: no bounds check, no `Optional`. A region plus its
// extent is one reference and one `Long` — allocation-free, replacing the unsafe
// `(storage, offset, length)` triples previously threaded through streams and ducts.
//
// `Slate` is the writable counterpart, for duct output storage: `update` in place of `apply`,
// with the same branding discipline.
//
// Both erase to an untyped `AnyRef` carrier; since `Addressable` instances are unique per
// medium, the casts back to `addressable.Storage` are sound — the same argument as
// `Stream.storage0`. Validity is temporal (a region is valid until the lender's next refill or
// reuse), which branding alone cannot express; that remains the single-owner discipline, and
// the lender shape below is the hook by which separation checking will later enforce it.
//
// Within the trusted kernel, a brand must be widened away (`val interval: Interval = range`)
// before the opaque type's own inline ops apply: the inliner's opaque transparency does not
// see through the refinement `Interval { type Form = region.type }`.

opaque type Region[medium] = AnyRef

object Region:
  // The trusted boundary constructor: binds the region to a stable name and lends the caller
  // its branded extent, clamped to the storage's true extent so the totality invariant is
  // established by construction. The brand is sound because storage length is fixed at
  // allocation and `region` cannot be rebound.
  inline def over[medium, result](using addressable: medium is Addressable)
    ( storage: addressable.Storage^{caps.any.rd}, offset: Int, limit: Int )
    ( inline lambda: (region: Region[medium]) => (Interval in region.type) => result )
  :   result =

    val size = addressable.storageSize(storage)
    val start = offset.max(0).min(size)
    val end = limit.max(start).min(size)
    val region: Region[medium] = storage.asInstanceOf[AnyRef]
    lambda(region)(Interval.zerary(start, end).asInstanceOf[Interval in region.type])

  extension [medium](region: Region[medium])(using addressable: medium is Addressable)
    // Total: only branded ordinals are accepted, and only the trusted combinators produce them.
    inline def apply(index: Ordinal in region.type): addressable.Operand =
      val ordinal: Ordinal = index
      addressable.storageAddress(region.asInstanceOf[addressable.Storage], ordinal.n0)

    inline def visit(range: Interval in region.type)
      ( inline lambda: (Ordinal in region.type) => Unit )
    :   Unit =

      val interval: Interval = range
      var index: Int = interval.start.n0
      val end: Int = interval.limit.n0

      while index < end do
        lambda(region.ordinal(index))
        index += 1

    // The derived-index answer for unrolled hot loops (`stepUtf8`'s `bytes(src + 7)`): the
    // `+1`..`+7` arithmetic lives here, proven once, and user code only ever sees branded
    // ordinals. `whole` receives eight consecutive in-range ordinals; `rest` mops up the tail.
    inline def visit8(range: Interval in region.type)
      ( inline whole: ( Ordinal in region.type, Ordinal in region.type,
                        Ordinal in region.type, Ordinal in region.type,
                        Ordinal in region.type, Ordinal in region.type,
                        Ordinal in region.type, Ordinal in region.type ) => Unit )
      ( inline rest: (Ordinal in region.type) => Unit )
    :   Unit =

      val interval: Interval = range
      var index: Int = interval.start.n0
      val end: Int = interval.limit.n0

      while index + 8 <= end do
        whole
         ( region.ordinal(index), region.ordinal(index + 1), region.ordinal(index + 2),
           region.ordinal(index + 3), region.ordinal(index + 4), region.ordinal(index + 5),
           region.ordinal(index + 6), region.ordinal(index + 7) )

        index += 8

      while index < end do
        rest(region.ordinal(index))
        index += 1

    // Bulk operations, for the `arraycopy`/`materialize`-shaped consumers: each wraps one
    // `Addressable` primitive with the extent's bounds, so no caller ever re-derives offsets.
    inline def materialize(range: Interval in region.type): medium =
      val interval: Interval = range
      addressable.materialize
       (region.asInstanceOf[addressable.Storage], interval.start.n0, interval.size)

    inline def cloneTo(range: Interval in region.type)(target: addressable.Target): Unit =
      val interval: Interval = range
      addressable.cloneStorage
       (region.asInstanceOf[addressable.Storage], interval.start.n0, interval.size)(target)

    // Copies as much of `range` as fits in `space` — the minimum of the two sizes, returned —
    // so the operation is total by construction.
    inline def blit(range: Interval in region.type)(slate: Slate[medium])
      ( space: Interval in slate.type )
    :   Int =

      val source: Interval = range
      val dest: Interval = space
      val count = source.size.min(dest.size)

      addressable.transfer
       ( region.asInstanceOf[addressable.Storage], source.start.n0,
         slate.asInstanceOf[addressable.Storage], dest.start.n0, count )

      count

    // The escape hatch for kernels whose index arithmetic outruns the combinators (compressor
    // back-references and the like): the raw storage, behind `Unsafe`, with the caller taking
    // back responsibility for bounds. Unannotated, like `Stream.storage`: the cast launders
    // the capture, and the `Unsafe` gate marks the responsibility boundary.
    inline def raw(using Unsafe): addressable.Storage =
      region.asInstanceOf[addressable.Storage]

    // The only branded-ordinal producer, for the trusted combinators above.
    private inline def ordinal(inline n: Int): Ordinal in region.type =
      Ordinal.zerary(n).asInstanceOf[Ordinal in region.type]

// Brand-preserving narrowing: a sub-interval of a valid extent is itself valid, so clamping
// preserves the brand. `capped` keeps at most the first `count` indexes of the extent.
extension [form](range: Interval in form)
  inline def capped(count: Int): Interval in form =
    val interval: Interval = range
    Interval.sized(interval.start.n0, interval.size.min(count.max(0))).asInstanceOf[Interval in form]

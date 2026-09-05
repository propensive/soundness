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

import scala.reflect.ClassTag

import denominative.*
import prepositional.*

// The write-side counterpart of confined reading (issue #1666), as a builder-lender: an
// opaque handle over a freshly-allocated array that grants writes (and read-back) only
// through `Ordinal`s branded to the handle's identity, so no index it accepts can be out of
// range. `Array.build` allocates, lends the scribe with its branded extent, and freezes the
// result — the only writer is statically retired when the lender returns, so the freeze is
// sound by construction, exactly as `Array.freeze`'s `consume` form.
//
// A `Scribe` is `Countable`, so the whole confined-scan family (`iterate`, `spot`, `lead`,
// `pare`, `retrace`) applies to it directly: `scribe.iterate { i => scribe(i) = ... }`.
//
// The accessors live in the companion (found through implicit scope) rather than at the top
// level, so same-named extensions imported by wildcard cannot shadow them.
// The carrier holds the buffer untyped, as `Region`/`Slate` (an opaque alias of `scala.Array`
// directly would be mutable-classified by the mutalias patch, annotating every in-module
// signature with `^{any.rd}` and breaking capture-free summons across the boundary), plus the
// append cursor. The casts are sound: a scribe is minted only over the array `Array.scribe`
// just allocated.
opaque type Scribe[element] = Scribe.Core

object Scribe:
  // Public (with a package-private constructor routed through `apply`) because the inline
  // lender and accessors expand in caller packages, which must be able to reference the
  // carrier and its members. Minting a scribe grants only bounds-confined access to the
  // array, so the public mint is not a safety hole; the freeze soundness argument belongs
  // to `Array.scribe`, which confines the scribe it mints to one lambda.
  final class Core private[rudiments] (buffer0: AnyRef, val growable: Boolean):
    // Untracked: the cursor is reached only through the scribe, which the lender confines
    // to one lambda; `Stateful` would force capability typing onto a transient builder.
    @scala.caps.unsafe.untrackedCaptures
    var mark: Int = 0

    // Reassignable, which is the whole of what growth needed: a branded `Ordinal` stays valid
    // when the array only ever grows, so the brand is a lower bound on validity rather than an
    // exact claim, and nothing about the confinement argument changes. A scribe whose size was
    // given is NOT growable — `Array.scribe` promises an array of exactly that size, and its
    // callers rely on it — so `append` and `place` still clamp there, and only the unsized
    // lender extends.
    @scala.caps.unsafe.untrackedCaptures
    var buffer: AnyRef = buffer0
  // Written out (not a SAM lambda): the lambda form infers a capture-annotated `Self`
  // (`Scribe[element]^{any.rd}`), which then fails to match capture-free summons.
  given countable: [element] => Scribe[element] is Countable:
    def size(self: Scribe[element]): Int =
      self.asInstanceOf[Core].buffer.asInstanceOf[scala.Array[element]].length

  def apply[element](buffer: scala.Array[element]^): Scribe[element] =
    new Core(buffer.asInstanceOf[AnyRef], false)

  private[rudiments] def growing[element](buffer: scala.Array[element]): Scribe[element] =
    new Core(buffer.asInstanceOf[AnyRef], true)

  // Doubling, so appending n elements costs O(n) rather than O(n²). Only reached from a
  // growable scribe.
  private[rudiments] def reserve[element: ClassTag](core: Core, extra: Int): Unit =
    val target = core.buffer.asInstanceOf[scala.Array[element]^]
    val required = core.mark + extra

    if required > target.length then
      var capacity = (target.length*2).max(16)
      while capacity < required do capacity *= 2
      val bigger: scala.Array[element]^ = new scala.Array[element](capacity)
      System.arraycopy(target, 0, bigger, 0, core.mark)
      core.buffer = bigger.asInstanceOf[AnyRef]

  private[rudiments] inline def over[element, result](buffer: scala.Array[element]^)
    ( inline lambda: (scribe: Scribe[element]) => (Interval in scribe.type) => result )
  :   result =

    val scribe = Scribe[element](buffer)
    lambda(scribe)(Interval.initial(buffer.length).asInstanceOf[Interval in scribe.type])

  extension [element](scribe: Scribe[element])
    inline def update(index: Ordinal in scribe.type, value: element): Unit =
      val ordinal: Ordinal = index
      // The cast re-asserts the exclusivity the carrier forgot: the array is reached only
      // through this scribe, which the lender confines to one lambda.
      scribe.asInstanceOf[Core].buffer.asInstanceOf[scala.Array[element]^](ordinal.n0) = value

    inline def apply(index: Ordinal in scribe.type): element =
      val ordinal: Ordinal = index
      scribe.asInstanceOf[Core].buffer.asInstanceOf[scala.Array[element]](ordinal.n0)

    // Sequential writes for data-dependent positions: each `append` writes at the scribe's own
    // cursor and advances it, clamping silently at the end of the buffer, so no position a
    // caller can reach is out of range. The dominant safe form for compaction and filtering
    // loops, whose write index advances irregularly while the read index scans.
    inline def append(value: element)(using ClassTag[element]): Unit =
      val core = scribe.asInstanceOf[Core]
      if core.growable then Scribe.reserve[element](core, 1)
      val target = core.buffer.asInstanceOf[scala.Array[element]^]

      if core.mark < target.length then
        target(core.mark) = value
        core.mark += 1

    // Bulk append at the cursor. `place` cannot serve here: it takes an `Ordinal in
    // scribe.type`, and an unsized scribe hands out no branded extent to draw one from. This is
    // the operation `ByteArrayOutputStream.write(bytes)` was reached for, and the reason the
    // class was used at all — one `arraycopy` per chunk rather than a byte at a time.
    @targetName("appendArray")
    inline def append(source: Array[element]^{})(using ClassTag[element]): Unit =
      val core = scribe.asInstanceOf[Core]
      val count = source.readable.length
      if core.growable then Scribe.reserve[element](core, count)
      val target = core.buffer.asInstanceOf[scala.Array[element]^]
      val room = count.min(target.length - core.mark)

      if room > 0 then
        System.arraycopy(source.asInstanceOf[scala.Array[element]], 0, target, core.mark, room)
        core.mark += room

    // A window of the source, for framing formats which append a slice of a larger block.
    inline def append(source: Array[element]^{}, from: Int, count: Int)(using ClassTag[element])
    :   Unit =

      val core = scribe.asInstanceOf[Core]
      if core.growable then Scribe.reserve[element](core, count)
      val target = core.buffer.asInstanceOf[scala.Array[element]^]
      val room = count.min(target.length - core.mark)

      if room > 0 then
        System.arraycopy(source.asInstanceOf[scala.Array[element]], from, target, core.mark, room)
        core.mark += room

    // The number of elements appended so far.
    inline def mark: Int = scribe.asInstanceOf[Core].mark

    // Bulk copy of a whole frozen array into the scribe at `at`, clamped to the space that
    // remains: the confined form of `place`. Returns the count copied.
    inline def place(source: Array[element]^{}, at: Ordinal in scribe.type)(using ClassTag[element])
    :   Int =

      val ordinal: Ordinal = at
      val core = scribe.asInstanceOf[Core]

      if core.growable
      then Scribe.reserve[element](core, ordinal.n0 + source.readable.length - core.mark)

      val target: scala.Array[element]^ = core.buffer.asInstanceOf[scala.Array[element]^]
      val count = source.readable.length.min(target.length - ordinal.n0)

      System.arraycopy
        (source.asInstanceOf[scala.Array[element]], 0, target, ordinal.n0, count)

      count

extension (companion: Array.type)
  // Allocate, lend, freeze — with no size given, so the scribe grows as it is written and the
  // result is exactly what was written, not a capacity. This is what
  // `java.io.ByteArrayOutputStream` was used for across a dozen libraries: never as a stream,
  // always as a buffer whose total is not known in advance. Unlike that class, nothing here is
  // synchronized (the buffer is owned by the block that builds it), nothing declares an
  // `IOException` it cannot throw, and the terminal freeze does not copy when the buffer was
  // filled exactly, where `toByteArray` copied on every call.
  //
  // The lambda receives no branded extent, because there is no fixed extent to brand: an
  // unsized scribe is written through `append` and `place`, whose positions are its own.
  // A plain `def`, not `inline`: inlining it reduces `Scribe[element]` to its representation in
  // the expansion, so a caller's lambda is asked for `Scribe.Core` and does not conform. There
  // is no dependent branded type here to justify inlining, as there is for `scribe`.
  def collect[element: ClassTag](hint: Int = 16)(lambda: Scribe[element] => Unit)
  :   Array[element]^{} =

    val scribe = Scribe.growing[element](new scala.Array[element](hint.max(16)))
    lambda(scribe)

    val core = scribe.asInstanceOf[Scribe.Core]
    val written = core.mark
    val target = core.buffer.asInstanceOf[scala.Array[element]]

    // No copy when the buffer was filled exactly.
    if written == target.length then target.asInstanceOf[Array[element]^{}]
    else
      val exact: scala.Array[element]^ = new scala.Array[element](written)
      System.arraycopy(target, 0, exact, 0, written)

      exact.asInstanceOf[Array[element]^{}]

  // Allocate, lend, freeze: `lambda` receives the scribe and its branded extent, and the
  // frozen result is returned once the only writer has been retired.
  inline def scribe[element: ClassTag](size: Int)
    ( inline lambda: (scribe: Scribe[element]) => (Interval in scribe.type) => Unit )
  :   Array[element]^{} =

    val buffer: scala.Array[element]^ = new scala.Array[element](size.max(0))
    Scribe.over[element, Unit](buffer)(lambda)
    buffer.asInstanceOf[Array[element]^{}]
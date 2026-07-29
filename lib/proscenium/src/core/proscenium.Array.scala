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
package proscenium

import scala.caps
import scala.reflect.ClassTag

// An `Array` is a fixed-size mutable array whose access rights are tracked by separation
// checking: any reference can read, but only an exclusive (`^`) reference can write, aliased
// writers are rejected, and `freeze` consumes the array to yield the frozen form,
// `Array[element]^{}`, without copying -- sound because `consume` statically retires every
// writer. The frozen form is not a parallel immutable type: an array no capture set can
// ever write through *is* immutable, so `^{}` is the whole story. An opaque alias rather
// than a wrapper class: the mutalias compiler patch classifies the alias as mutable
// wherever it appears, so hiding `scala.Array` costs nothing at runtime.
//
// The total, `Optional`-returning read (`at`) lives in `vacuous`, which sits above this
// module; it is built from `length` and the bounds-partial `readUnchecked` below.
object Array:
  def apply[element: ClassTag](size: Int): Array[element]^ = new scala.Array[element](size)

  // The frozen literal: builds a fresh array no writer can ever alias, so the purity
  // launder is discharged by construction -- the same argument as `freeze`. Not an `apply`
  // overload: `apply(size)` would ambiguate single-`Int` literals.
  def of[element: ClassTag](elements: element*): Array[element]^{} =
    caps.unsafe.unsafeAssumePure(elements.toArray)

  // A bare JVM array, allocated through the companion so that interior scratch (the
  // fields of `caps.Mutable` state machines, and other code below the reach of `Array`'s
  // exclusivity discipline) shares this vocabulary without spelling `new`. Named
  // `scratch`, not `raw`, because `raw` is the exclusive-reference interop view below.
  inline def scratch[element: ClassTag](size: Int): scala.Array[element]^ =
    new scala.Array[element](size)

  // Freezing consumes the array: `consume` statically retires every writer, so the
  // surviving reference can drop its write capability without copying -- the launder only
  // forgets a capability that is no longer exercisable. This is the only sound producer of
  // `Array[element]^{}` from a mutable array.
  def freeze[element](consume array: Array[element]^): Array[element]^{} =
    caps.unsafe.unsafeAssumePure(array)

  // The trusted boundary cast, successor to `IArray.unsafeFromArray`: asserts that the
  // caller retains no writer to `array`. For interop edges that cannot thread `consume`.
  // Routed through the (immutable, so capture-free) stdlib `IArray` because a read-only
  // reference cannot feed `unsafeAssumePure` directly: inference demands an exclusive set.
  def unsafeFrozen[element](array: scala.Array[element]): Array[element]^{} =
    frozen(array.asInstanceOf[scala.IArray[element]])

  // Rehomes a stdlib `IArray` -- immutable by construction -- as a frozen `Array`: the
  // return path of the compat shims, which delegate to the stdlib's `IArray` operations.
  // Not `inline`: inline expansion of the cast inside capturing lambdas crashes the
  // capture checker's boxer (boxDeeply assertion).
  def frozen[element](iarray: scala.IArray[element]): Array[element]^{} =
    caps.unsafe.unsafeAssumePure(iarray.asInstanceOf[scala.Array[element]])

  // Pattern support for any readable reference: `case Array(a, b)` reads but never writes,
  // so frozen, shared and exclusive receivers all subsume into `^{caps.any.rd}`.
  def unapplySeq[element](array: Array[element]^{caps.any.rd}): Option[Seq[element]] =
    Some(array.asInstanceOf[scala.IArray[element]].toSeq)

  // Linear growth for accumulating builders: consumes the old buffer, so the idiom is
  // recursion threading the buffer through `consume` parameters -- a `var` cannot hold an
  // exclusive buffer.
  def grow[element: ClassTag](consume buffer: Array[element]^, size: Int): Array[element]^ =
    val count = buffer.length.min(size)
    val bigger: Array[element]^ = new scala.Array[element](size)
    java.lang.System.arraycopy(buffer, 0, bigger, 0, count)
    bigger

  extension [element, C^](buffer: Array[element]^{C})
    def length: Int = buffer.length

    // The bounds-partial shared read: exists for layering (`vacuous`'s total `at` wraps
    // it in a bounds check), not for use at call sites, which should prefer `at` or an
    // exclusive reference's `apply`. Reading through a shared reference is sound because
    // separation excludes live writers wherever readers alias.
    def readUnchecked(index: Int): element = buffer(index)

  extension [element](buffer: Array[element]^)
    // An exclusive reference has sole ownership, so it may also read without the
    // `Optional` guard: nobody else can have resized or replaced the content.
    def apply(index: Int): element = buffer(index)

    def update(index: Int, value: element): Unit = buffer(index) = value

    def fill(value: element): Unit =
      var index = 0

      while index < buffer.length do
        buffer(index) = value
        index += 1

    // One `copyFrom` for every readable source: frozen (`^{}`) and shared references both
    // subsume into `^{caps.any.rd}`, and the `IArray` branch of the union keeps existing
    // call sites compiling until `IArray` itself is retired -- an overload is impossible
    // because the two source types share an erasure.
    def copyFrom
      ( source: IArray[element] | Array[element]^{caps.any.rd},
        sourceStart: Int,
        targetStart: Int,
        count: Int )
    :   Unit =
      java.lang.System.arraycopy
        (source.asInstanceOf[scala.Array[element]], sourceStart, buffer, targetStart, count)

    // The underlying JVM array, exclusively: the escape for interop (`random.nextBytes`,
    // `stream.read`, `System.arraycopy` from external sources). The result aliases the
    // array, so it shares the array's exclusivity rather than escaping it.
    def raw: scala.Array[element]^ = buffer

  extension [element](array: Array[element]^{caps.any.rd})
    // The read-only stdlib view: any reference may read (separation excludes live
    // writers), so the compat shims delegate through the stdlib's immutable `IArray`
    // surface, which exposes no write operations.
    inline def readable: scala.IArray[element] = array.asInstanceOf[scala.IArray[element]]

opaque type Array[element] = scala.Array[element]

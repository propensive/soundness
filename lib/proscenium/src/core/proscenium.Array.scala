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

import scala.annotation.targetName
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
// The total, `Optional`-returning read (`at`) lives in `concordance`, which sits above this
// module; it is built from `length` and the bounds-partial `readUnchecked` below.
object Array:
  // Named `allocate`, not `apply`: the companion's `apply` is the frozen element-wise
  // literal, and a single-`Int` application must mean the one-element array, never a size.
  inline def allocate[element: ClassTag](size: Int): Array[element]^ = new scala.Array[element](size)

  // The frozen literal: builds a fresh array no writer can ever alias, so the purity
  // launder is discharged by construction -- the same argument as `freeze`. It can be
  // `apply` because allocation is `allocate`: a single-`Int` application unambiguously
  // means the one-element array.
  def apply[element: ClassTag](elements: element*): Array[element]^{} =
    // [construction-fresh] fresh array literal no writer can alias
    caps.unsafe.unsafeAssumePure(elements.toArray)

  // Frozen constructors: each fills a fresh exclusive array and freezes it, so the frozen
  // result is produced by `freeze`'s checked transfer, not by a launder. The loops are the
  // combinator internals the loop standard reserves `while` for.
  def empty[element: ClassTag]: Array[element]^{} = freeze(allocate[element](0))

  def from[element: ClassTag](elements: IterableOnce[element]^): Array[element]^{} =
    // [construction-fresh] the iterator's fresh array is aliased by no writer
    unsafeFrozen(elements.iterator.toArray)

  // A literal length is a proof of length: these overloads brand the result `Sized[n]`.
  // Overloading resolution prefers them for any *stable* argument (the singleton-bounded
  // parameter is more specific than `Int`), which includes a `val`, so each is `transparent
  // inline` and brands only when the length is a compile-time constant; a `val` length yields
  // the plain array, as the `Int` overload does for an expression. The `DummyImplicit` only
  // keeps their erased signatures distinct from the unproven forms.
  transparent inline def fill[element: ClassTag, length <: Int & Singleton](count: length)
    ( element: => element )
    ( using DummyImplicit )
  :   Array[element]^{} =

    inline scala.compiletime.constValueOpt[length] match
      case Some(_) =>
        fill[element](count: Int)(element).asInstanceOf[Array[element]^{} & Sized[length]]
      case None    => fill[element](count: Int)(element)

  transparent inline def tabulate[element: ClassTag, length <: Int & Singleton](count: length)
    ( lambda: Int => element )
    ( using DummyImplicit )
  :   Array[element]^{} =

    inline scala.compiletime.constValueOpt[length] match
      case Some(_) =>
        tabulate[element](count: Int)(lambda).asInstanceOf[Array[element]^{} & Sized[length]]
      case None    => tabulate[element](count: Int)(lambda)

  // Concatenation; with two proven lengths the result's length is their sum, decided at the
  // type level when both are literals and carried symbolically otherwise.
  def concat[element: ClassTag]
    ( left: Array[element]^{caps.any.rd}, right: Array[element]^{caps.any.rd} )
  :   Array[element]^{} =

    val array: Array[element]^ = allocate[element](left.length + right.length)
    array.place(left)
    array.place(right, left.length)
    freeze(array)

  // The three-operand form, for splicing: `Array.concat(xs.before(i), replacement,
  // xs.after(i))` rebuilds a frozen array with a segment replaced, in one pass. A fixed arity
  // rather than a repeated parameter: a frozen array as the element of a varargs `Seq` is
  // re-freshened to `^{any.rd}` by the alias-captures rule, and the segments' element types
  // then fail to unify under the invariant `Array` -- so until that is lifted, a splice is
  // exactly three parts, which is what every splice of a fixed layout needs.
  def concat[element: ClassTag]
    ( left:   Array[element]^{caps.any.rd},
      middle: Array[element]^{caps.any.rd},
      right:  Array[element]^{caps.any.rd} )
  :   Array[element]^{} =

    val array: Array[element]^ = allocate[element](left.length + middle.length + right.length)
    array.place(left)
    array.place(middle, left.length)
    array.place(right, left.length + middle.length)
    freeze(array)

  def concat[element: ClassTag, left <: Int, right <: Int]
    ( leftArray:  Array[element]^{caps.any.rd} & Sized[left],
      rightArray: Array[element]^{caps.any.rd} & Sized[right] )
    ( using DummyImplicit )
  :   Array[element]^{} & Sized[scala.compiletime.ops.int.+[left, right]] =

    concat[element](leftArray, rightArray)
    . asInstanceOf[Array[element]^{} & Sized[scala.compiletime.ops.int.+[left, right]]]

  def fill[element: ClassTag](count: Int)(element: => element): Array[element]^{} =
    val array: Array[element]^ = allocate[element](count)
    var index = 0

    while index < count do
      array(index) = element
      index += 1

    freeze(array)

  def tabulate[element: ClassTag](count: Int)(lambda: Int => element): Array[element]^{} =
    val array: Array[element]^ = allocate[element](count)
    var index = 0

    while index < count do
      array(index) = lambda(index)
      index += 1

    freeze(array)

  def range(start: Int, end: Int): Array[Int]^{} =
    val count = (end - start).max(0)
    val array: Array[Int]^ = allocate[Int](count)
    var index = 0

    while index < count do
      array(index) = start + index
      index += 1

    freeze(array)

  // Element-wise equality of two readable arrays: the opaque alias cannot override `==`,
  // which stays the JVM's reference comparison, so content equality is a named operation.
  def same[element](left: Array[element]^{caps.any.rd}, right: Array[element]^{caps.any.rd})
  :   Boolean =
    left.length == right.length && {
      var index = 0
      var equal = true

      while equal && index < left.length do
        equal = left(index) == right(index)
        index += 1

      equal
    }

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
    // [transfer] freeze: consume retires writers, drops write capability
    caps.unsafe.unsafeAssumePure(array)

  // The native counterpart of `Expr.ofList` for a frozen array (see `List.quoted`): a list of
  // lifted elements becomes one lifted frozen array. The element's `ClassTag` is resolved at
  // the expansion site, where the element type is concrete.
  def quoted[element: scala.quoted.Type](elements: List[scala.quoted.Expr[element]])
    ( using quotes: scala.quoted.Quotes )
  :   scala.quoted.Expr[Array[element]^{}] =

    quotedElements(elements.stdlib)

  // The same, from a frozen array of lifted elements, which is what a macro walking an array
  // of children naturally holds.
  def quoted[element: scala.quoted.Type](elements: Array[scala.quoted.Expr[element]]^{})
    ( using quotes: scala.quoted.Quotes )
  :   scala.quoted.Expr[Array[element]^{}] =

    quotedElements(elements.stdlib)

  private def quotedElements[element: scala.quoted.Type]
    ( elements: scala.collection.immutable.Seq[scala.quoted.Expr[element]] )
    ( using quotes: scala.quoted.Quotes )
  :   scala.quoted.Expr[Array[element]^{}] =

    scala.quoted.Expr.summon[ClassTag[element]] match
      case Some(classTag) =>
        // Cast-erased: a frozen array's `^{}` is re-freshened to `any.rd` inside a quote, so
        // the lifted tree's type is restated here, once, for every macro that lifts an array.
        '{Array.from[element](${scala.quoted.Expr.ofList(elements)})(using $classTag)}
        . asInstanceOf[scala.quoted.Expr[Array[element]^{}]]

      case None =>
        quotes.reflect.report.errorAndAbort
          ("no ClassTag is available for the elements of the array")

  // The inverse of `freeze`: an exclusive copy of any readable array, which the caller may
  // write through -- `copy(i) = value` -- and then `freeze` again. This is the shape for an
  // edit of several slots; a one-slot edit is `define`, and a splice is `concat` of segments.
  // A copy, never the array itself: the source may be frozen or shared, so other readers
  // must keep seeing it unchanged, and nothing is laundered.
  def thaw[element: ClassTag](array: Array[element]^{caps.any.rd}): Array[element]^ =
    val copy: Array[element]^ = allocate[element](array.length)
    copy.place(array)
    copy

  // The trusted boundary cast: asserts that the caller retains no writer to `array`. For
  // interop edges that cannot thread `consume`. A read-only reference cannot feed
  // `unsafeAssumePure` directly (inference demands an exclusive set), so the cast first
  // reasserts the exclusive form the launder expects.
  def unsafeFrozen[element](array: scala.Array[element]): Array[element]^{} =
    // [java-boundary] trusted boundary cast: the caller retains no writer
    caps.unsafe.unsafeAssumePure(array.asInstanceOf[scala.Array[element]^])

  // Pattern support for any readable reference: `case Array(a, b)` reads but never writes,
  // so frozen, shared and exclusive receivers all subsume into `^{caps.any.rd}`.
  def unapplySeq[element](array: Array[element]^{caps.any.rd}): Option[Seq[element]] =
    Some(array.stdlib)

  // The JVM array behind a readable reference, for Java APIs that read but never write their
  // array argument (`OutputStream.write`, `MessageDigest.update`, `SecretKeySpec`, and the
  // rest of the JDK's byte-array surface). Nothing is asserted about *this* side -- a readable
  // reference has no writer to lose -- so the whole assertion is about the callee, which a
  // Java signature cannot express. Prefer it to an ambient `Unsafe`: the claim is then one
  // greppable call, checkable against one Javadoc, rather than a capability that silently
  // licenses every unsafe operation in its scope.
  def unsafeJvm[element](array: Array[element]^{caps.any.rd}): scala.Array[element] =
    array.asInstanceOf[scala.Array[element]]

  // Linear growth for accumulating builders: consumes the old buffer, so the idiom is
  // recursion threading the buffer through `consume` parameters -- a `var` cannot hold an
  // exclusive buffer.
  def grow[element: ClassTag](consume buffer: Array[element]^, size: Int): Array[element]^ =
    val count = buffer.length.min(size)
    val bigger: Array[element]^ = new scala.Array[element](size)
    java.lang.System.arraycopy(buffer, 0, bigger, 0, count)
    bigger

  extension [element, C^](buffer: Array[element]^{C})
    inline def length: Int = buffer.length

    // The bounds-partial shared read. `concordance`'s total `at` wraps it in a bounds check,
    // and that or a confined ordinal is what a call site should normally use; a bare call is
    // the loop standard's data-derived shape, which carries a comment naming its proof.
    // Reading through a shared reference is sound because separation excludes live writers
    // wherever readers alias.
    inline def readUnchecked(index: Int): element = buffer(index)

  // An exclusive reference has sole ownership, so it may also read without the `Optional`
  // guard: nobody else can have resized or replaced the content. Inline, so that a call with a
  // concrete element type compiles to the JVM's own array access rather than the boxing
  // `ScalaRunTime` path; with an inline receiver, because an inline method with an ordinary
  // receiver binds it to a proxy whose capture set is read-only, which cannot be written through.
  // An inline receiver is evaluated where it is used, after the index and value arguments.
  extension [element](inline buffer: Array[element]^)
    inline def apply(index: Int): element = buffer(index)
    inline def update(index: Int, value: element): Unit = buffer(index) = value

  extension [element](buffer: Array[element]^)
    def fill(value: element): Unit =
      var index = 0

      while index < buffer.length do
        buffer(index) = value
        index += 1

    // Copying a source INTO this array, which is what `place` means throughout the collection
    // (`rudiments.place`, `concordance.Scribe.place`); `snapshot` is its counterpart, copying
    // out into a fresh array. One overload set for every readable source: frozen (`^{}`) and
    // shared references both subsume into `^{caps.any.rd}`.
    //
    // Indexed by `Int` rather than by `Ordinal`, unlike most of the collection: `denominative`
    // sits above `proscenium`, so an ordinal cannot be named here. `Array`'s own `apply`,
    // `update` and `readUnchecked` are `Int`-indexed for the same reason, and the ordinal-taking
    // façade lives in `rudiments`.
    def place(source: Array[element]^{caps.any.rd}): Unit =
      place(source, 0, 0, source.length)

    def place(source: Array[element]^{caps.any.rd}, at: Int): Unit =
      place(source, 0, at, source.length)

    def place
      ( source: Array[element]^{caps.any.rd},
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
    // The greppable stdlib bridge, as `List.stdlib`: a zero-copy, read-only `ArraySeq` view
    // for the stdlib and Java APIs that want a `Seq`. Not for element access: its generic
    // `apply` boxes, and it is a partial read the collection vocabulary does not offer.
    inline def stdlib: scala.collection.immutable.ArraySeq[element] =
      scala.collection.immutable.ArraySeq
      . unsafeWrapArray(array.asInstanceOf[scala.Array[element]])

    // The one constructive frozen-array operation the generic collections API cannot serve:
    // murmuration's `map` requires a `Mappable` instance, and the frozen array has none (its
    // element type carries a `ClassTag` obligation no typeclass slot can express). NOT
    // `inline`: expansion re-typechecks the body at the call site, and in this file — where
    // the opaque alias is transparent — the receiver proxy dealiases to `scala.Array`, whose
    // exclusive `{}` cannot subsume the read-only receiver. The explicit `element2` parameter
    // is what stops a nested frozen array's fresh `any.rd` being re-minted at the application
    // site.
    @targetName("frozenMap")
    def map[element2: scala.reflect.ClassTag](lambda: element => element2)
    :   Array[element2]^{} =
      val result: Array[element2]^ = Array.allocate[element2](array.length)
      var index = 0

      while index < array.length do
        result(index) = lambda(array(index))
        index += 1

      Array.freeze(result)

  given arrayIsSpreadable: [element] => (Spreadable[Array[element]^{}] { type Out = scala.Array[element]^{} }) =
    new Spreadable[Array[element]^{}]:
      type Out = scala.Array[element]^{}
      def spread(value: Array[element]^{}): scala.Array[element]^{} = value

opaque type Array[element] = scala.Array[element]

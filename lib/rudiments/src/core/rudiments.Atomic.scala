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

import java.util.concurrent.atomic as juca

import denominative.*
import vacuous.*

// The atomic cells: opaque wrappers over `java.util.concurrent.atomic`, presenting reads that do
// not need `.nn` and names that say what an operation is for rather than which JDK method
// performs it. Every operation is `inline` and every type erases to the Java class it wraps, so
// the bytecode is what the raw Java call would have produced.
//
// Nested inside two objects, never declared at package level: a top-level opaque type is lifted
// into a synthesized `$package` wrapper, which the compiler warns about and whose abstraction
// leaks (see `ultimatum.internal`). Nesting under `Atomic` is also what frees the names —
// `Counter`, `Tally`, `Cell` and `Flag` are each taken at the top level of the `soundness`
// namespace — while `Atomic.Count` and `Atomic.Flag` read as the English they are.
//
// The extensions sit beside the opaque types rather than inside each companion, following
// `hypotenuse.internal`: an opaque type's prefix is an anchor of its implicit scope, so
// `object Atomic` is searched for a member of an `Atomic.Count` with no import. Unlike
// hypotenuse, no `@targetName` is needed: the five types erase to five *different* Java
// classes, where `U32` and `S32` both erase to `Int`.
//
// `Atomic[value]` is a match type over the five concrete types, not a representation: the
// distinction is what makes it work. A match type AS the representation cannot reduce for an
// abstract type parameter, and `contingency.Accrual.AccrueTactic` holds a cell over an abstract
// `accrual` — so the operations would have nothing to dispatch on. Layered *over* concrete types
// it is pure spelling: `Atomic[Int]` reduces to `Atomic.Count` and picks up `Count`'s own
// extensions, while an abstract context writes `Atomic.Cell[accrual]` and loses nothing.
//
// Its value is that it makes the unboxed spelling the obvious one. `Atomic.Cell[Int]` is legal
// and boxes every write; `Atomic[Int]` is what a reader reaches for, and is an `AtomicInteger`.
//
// `Byte`, `Short`, `Char`, `Float` and `Double` fall to `Cell` and therefore box: the JDK has no
// unboxed atomic for them. `Double` could be packed into an `AtomicLong` through
// `doubleToRawLongBits` if a call site ever needs it; none does.
object Atomic:
  opaque type Count        = juca.AtomicInteger
  opaque type Tally        = juca.AtomicLong
  opaque type Flag         = juca.AtomicBoolean
  opaque type Cell[value]  = juca.AtomicReference[value]
  opaque type Cells[value] = juca.AtomicReferenceArray[Optional[value]]

  // Mirrors the match type's arms, so `Atomic(0)` is an `Atomic[Int]` and needs no type
  // ascription. Note that the ARGUMENT drives the choice, not an explicit type argument:
  // `Atomic[Int](0)` applies the generic arm and yields `Cell[Int]`, which is boxed and does
  // not conform to `Atomic[Int]`. It is a type error rather than a silent boxing, but write
  // `Atomic(0)`.
  def apply(initial: Int): Count = juca.AtomicInteger(initial)
  def apply(initial: Long): Tally = juca.AtomicLong(initial)
  def apply(initial: Boolean): Flag = juca.AtomicBoolean(initial)
  def apply[value](initial: value): Cell[value] = juca.AtomicReference(initial)

  object Count:
    def apply(initial: Int = 0): Count = juca.AtomicInteger(initial)

    // The transition operations live in each companion rather than beside the opaque types,
    // and that placement is load-bearing rather than cosmetic. A macro expanded from a DIRECT
    // member of the object which declares the opaque types makes every incremental rebuild
    // that touches `atomicMacros` fail with "Cyclic reference involving val <import>" — and
    // stay failing until a clean. Expanded one level in, from the companion, it does not.
    // `hypotenuse.internal` is arranged the same way, with its macro calls inside `object
    // U32` rather than beside `opaque type U32`, and does not have the defect.
    //
    // The companion is in the opaque type's implicit scope, so the extensions resolve exactly
    // as they would have.
    extension (count: Count)
      // The value this call displaced, and the value it installed. The transition is written
      // as a lambda literal whose shape is recognised at compiletime: `_ + 1` becomes
      // `getAndIncrement`/`incrementAndGet`, `_ + n` becomes `getAndAdd`/`addAndGet`, a
      // constant becomes `getAndSet`, and anything else becomes a compare-and-set retry loop
      // with the transition beta-reduced into it. Nothing is allocated either way.
      inline def ere(inline transition: Int => Int): Int =
        ${atomicMacros.count('count, 'transition, true)}

      inline def since(inline transition: Int => Int): Int =
        ${atomicMacros.count('count, 'transition, false)}

      // Setting is a transition with nothing to read, so it needs no lambda: `count.ere(5)`
      // rather than `count.ere(_ => 5)`. There is deliberately no `since` counterpart — it
      // would return its own argument. Setting without wanting the old value is `count() = 5`.
      inline def ere(inline supplied: Int): Int = count.getAndSet(supplied)

  object Tally:
    def apply(initial: Long = 0L): Tally = juca.AtomicLong(initial)

    // In the companion for the reason given on `Count`'s: a macro expanded from a direct member
    // of the object declaring the opaque types poisons incremental rebuilds.
    extension (tally: Tally)
      inline def ere(inline transition: Long => Long): Long =
        ${atomicMacros.tally('tally, 'transition, true)}

      inline def since(inline transition: Long => Long): Long =
        ${atomicMacros.tally('tally, 'transition, false)}

      inline def ere(inline supplied: Long): Long = tally.getAndSet(supplied)

  object Flag:
    def apply(initial: Boolean = false): Flag = juca.AtomicBoolean(initial)

    // `flag.ere(_ => true)` returning `false` is "I won the race to close", which is what every
    // one-shot flag in the collection is really asking; no `raise`/`lower` pair is needed on
    // top of it.
    extension (flag: Flag)
      inline def ere(inline transition: Boolean => Boolean): Boolean =
        ${atomicMacros.flag('flag, 'transition, true)}

      inline def since(inline transition: Boolean => Boolean): Boolean =
        ${atomicMacros.flag('flag, 'transition, false)}

      // `flag.ere(true)` returning `false` is the one-shot idiom at its shortest: "I am the
      // call that closed it".
      inline def ere(inline supplied: Boolean): Boolean = flag.getAndSet(supplied)

  object Cell:
    def apply[value](initial: value): Cell[value] = juca.AtomicReference(initial)

    // An initially-absent cell. `Cell(Unset)` would infer `Cell[Unset]`, and spelling
    // `Cell[Optional[Monitor]](Unset)` at each site reads worse than naming the state.
    def vacant[value]: Cell[Optional[value]] = juca.AtomicReference(Unset)

    extension [value](cell: Cell[value])
      inline def ere(inline transition: value => value): value =
        ${atomicMacros.cell('cell, 'transition, true)}

      inline def since(inline transition: value => value): value =
        ${atomicMacros.cell('cell, 'transition, false)}

      // The value overload, as on the other cells — with one measured limitation. Where `value`
      // is ITSELF a function type, this overload is unreachable: `ere` commits to the
      // transition overload and reports `Required: (Int => Int) => Int => Int`, for a lambda
      // and for a named function value alike. That is a loud failure rather than a quiet
      // mis-selection, and the spelling to use is `cell() = supplied`.
      //
      // `Cell[Any]` is the one quiet case: a lambda literal satisfies both overloads and is
      // read as a transition — the likelier intent, but chosen without a diagnostic, so store
      // a lambda into such a cell with `cell() = lambda`.
      inline def ere(inline supplied: value): value =
        cell.getAndSet(supplied).asInstanceOf[value]

      // The escape hatch, for a transition whose shape cannot be inspected because it is a
      // function VALUE rather than a literal — `contingency.Accrual` transitions with
      // `combine`, a constructor parameter. `inline`, so the lambda still beta-reduces and
      // nothing is allocated; what is lost is the shape check, and the obligation that check
      // would have discharged is stated here instead: `transition` MAY BE RE-RUN under
      // contention, so it must be pure. `revise` yields the value it installed, as `since`
      // does; no call site in the collection needs the displaced value from a transition whose
      // shape cannot be read.
      inline def revise(inline transition: value => value): value =
        val box: juca.AtomicReference[value] = cell
        var current: value = box.get().asInstanceOf[value]
        var settled: Boolean = false

        while !settled do
          val next: value = transition(current)

          if next.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then settled = true
          else if box.compareAndSet(current, next) then
            current = next
            settled = true
          else current = box.get().asInstanceOf[value]

        current

  object Cells:
    def apply[value](size: Int): Cells[value] = juca.AtomicReferenceArray(size)

    // In this companion rather than `Countable`'s: denominative sits below rudiments, so it
    // cannot see this type. Companion-to-companion placement resolves identically.
    given countable: [value] => Cells[value] is Countable = _.length()

  // A volatile read, written `cell()`. `asInstanceOf`, not `.nn`: `.nn` emits a null test and
  // a call to `Scala3RunTime.nnFail`, and — decisively — it THROWS on a cell holding `Unset`,
  // because `Unset` is `null` at runtime. `Cell[Optional[x]]().nn` is a latent NPE; this is
  // not. For an abstract `value` the cast erases to nothing at all.
  extension [value](cell: Cell[value])
    inline def apply(): value = cell.get().asInstanceOf[value]

    // A volatile store, written `cell() = value`.
    inline def update(value: value): Unit = cell.set(value)

    // An ordered (release) store: cheaper than `update`, and ordered before every subsequent
    // store, which is what makes a single-producer ring's slot write safe to be seen only
    // through the index published after it. Not a substitute for `update` where the store
    // itself is the publication point. zephyrine's own word for this is "publish".
    inline def publish(value: value): Unit = cell.lazySet(value)

    inline def swap(value: value): value = cell.getAndSet(value).asInstanceOf[value]

    inline def replace(expected: value, updated: value): Boolean =
      cell.compareAndSet(expected, updated)

  extension (count: Count)
    inline def apply(): Int = count.get()
    inline def update(value: Int): Unit = count.set(value)
    inline def publish(value: Int): Unit = count.lazySet(value)
    inline def swap(value: Int): Int = count.getAndSet(value)

    inline def replace(expected: Int, updated: Int): Boolean =
      count.compareAndSet(expected, updated)


  extension (tally: Tally)
    inline def apply(): Long = tally.get()
    inline def update(value: Long): Unit = tally.set(value)
    inline def publish(value: Long): Unit = tally.lazySet(value)
    inline def swap(value: Long): Long = tally.getAndSet(value)

    inline def replace(expected: Long, updated: Long): Boolean =
      tally.compareAndSet(expected, updated)

  extension (flag: Flag)
    inline def apply(): Boolean = flag.get()
    inline def update(state: Boolean): Unit = flag.set(state)
    inline def publish(state: Boolean): Unit = flag.lazySet(state)
    inline def swap(state: Boolean): Boolean = flag.getAndSet(state)

    inline def replace(expected: Boolean, updated: Boolean): Boolean =
      flag.compareAndSet(expected, updated)

  // `Optional`, not `value`: a freshly-allocated array is all-null, so a slot read is
  // genuinely partial. That is the honest difference from `Cell`, which is constructed with an
  // initial value and whose reads are therefore total.
  //
  // Indexed by `Ordinal`, not `Int`: a bare `Int` index is precisely the unchecked scalar
  // `denominative` exists to abolish, and it costs nothing — `Ordinal` is an opaque `Int` and
  // `n0` is the identity, so a masked ring index compiles to the `iand` alone. The ordinal is
  // unbranded deliberately: a masked index is in range by construction of the mask, so there
  // is nothing for branding to prove and its range check would not be free.
  extension [value](cells: Cells[value])
    inline def apply(ordinal: Ordinal): Optional[value] =
      cells.get(ordinal.n0).asInstanceOf[Optional[value]]

    inline def update(ordinal: Ordinal, value: Optional[value]): Unit =
      cells.set(ordinal.n0, value)

    inline def publish(ordinal: Ordinal, value: Optional[value]): Unit =
      cells.lazySet(ordinal.n0, value)

    inline def swap(ordinal: Ordinal, value: Optional[value]): Optional[value] =
      cells.getAndSet(ordinal.n0, value).asInstanceOf[Optional[value]]

    inline def replace(ordinal: Ordinal, expected: Optional[value], updated: Optional[value])
    :   Boolean =

      cells.compareAndSet(ordinal.n0, expected, updated)

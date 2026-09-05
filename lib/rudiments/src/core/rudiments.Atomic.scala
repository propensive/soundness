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
// Each is named for what it holds — `Atomic.Int`, `Atomic.Bool`, `Atomic.Ref` — rather than for
// a role. An earlier arrangement called them `Count`, `Tally` and `Flag`, which read well but
// asserted a distinction that does not exist: nothing in `Count` says thirty-two bits and
// nothing in `Tally` says sixty-four, so the one thing a reader needs from the name was the one
// thing it withheld.
//
// `Int` and `Long` shadow the primitives within this object, which is why it says `scala.Int`
// wherever the primitive is meant. The shadowing does not reach a normal user, who writes
// `Atomic.Int`; it would reach anyone writing `import Atomic.*`, so do not.
//
// Nested inside an object, never declared at package level: a top-level opaque type is lifted
// into a synthesized `$package` wrapper, which the compiler warns about and whose abstraction
// leaks (see `ultimatum.internal`).
//
// Not wrapped, and deliberately: `LongAdder` and the accumulators, whose `sum` is not
// linearizable and which therefore cannot answer `ere`/`since`; `AtomicMarkableReference` and
// `AtomicStampedReference`, which carry a second field and so do not fit this shape; and the
// three `*FieldUpdater`s, which are a reflective pre-`VarHandle` workaround with no opaque-type
// analogue. None is used anywhere in the collection.
object Atomic:
  opaque type Int          = juca.AtomicInteger
  opaque type Long         = juca.AtomicLong
  opaque type Bool         = juca.AtomicBoolean
  opaque type Ref[value]   = juca.AtomicReference[value]
  opaque type Ints         = juca.AtomicIntegerArray
  opaque type Longs        = juca.AtomicLongArray
  opaque type Refs[value]  = juca.AtomicReferenceArray[Optional[value]]

  // Mirrors the match type's arms, so `Atomic(0)` is an `Atomic[Int]` and needs no type
  // ascription. Note that the ARGUMENT drives the choice, not an explicit type argument:
  // `Atomic[Int](0)` applies the generic arm and yields `Ref[Int]`, which is boxed and does not
  // conform to `Atomic[Int]`. It is a type error rather than a silent boxing, but write
  // `Atomic(0)`.
  def apply(initial: scala.Int): Int = juca.AtomicInteger(initial)
  def apply(initial: scala.Long): Long = juca.AtomicLong(initial)
  def apply(initial: Boolean): Bool = juca.AtomicBoolean(initial)
  def apply[value](initial: value): Ref[value] = juca.AtomicReference(initial)

  object Int:
    def apply(initial: scala.Int = 0): Int = juca.AtomicInteger(initial)

    // Every extension lives in a companion rather than beside the opaque types, and for the
    // transition operations that placement is load-bearing rather than cosmetic. A macro
    // expanded from a DIRECT member of the object which declares the opaque types makes every
    // incremental rebuild that touches `atomicMacros` fail with "Cyclic reference involving val
    // <import>" — and stay failing until a clean. Expanded one level in, from the companion, it
    // does not. `hypotenuse.internal` is arranged the same way, with its macro calls inside
    // `object U32` rather than beside `opaque type U32`, and does not have the defect. The rest
    // follow for uniformity; a companion is in its type's implicit scope either way, so
    // resolution is unaffected.
    extension (int: Int)
      inline def apply(): scala.Int = int.get()
      inline def update(value: scala.Int): Unit = int.set(value)

      // An ordered (release) store: cheaper than `update`, and ordered before every subsequent
      // store, which is what makes a single-producer ring's slot write safe to be seen only
      // through the index published after it. Not a substitute for `update` where the store
      // itself is the publication point. zephyrine's own word for this is "publish".
      inline def publish(value: scala.Int): Unit = int.lazySet(value)

      inline def swap(value: scala.Int): scala.Int = int.getAndSet(value)

      inline def replace(expected: scala.Int, updated: scala.Int): Boolean =
        int.compareAndSet(expected, updated)

      // The value this call displaced, and the value it installed. The transition is written as
      // a lambda literal whose shape is recognised at compiletime: `_ + 1` becomes
      // `getAndIncrement`/`incrementAndGet`, `_ + n` becomes `getAndAdd`/`addAndGet`, a
      // constant becomes `getAndSet`, and anything else becomes a compare-and-set retry loop
      // with the transition beta-reduced into it. Nothing is allocated either way.
      inline def ere(inline transition: scala.Int => scala.Int): scala.Int =
        ${atomicMacros.int('int, 'transition, true)}

      inline def since(inline transition: scala.Int => scala.Int): scala.Int =
        ${atomicMacros.int('int, 'transition, false)}

      // Setting is a transition with nothing to read, so it needs no lambda: `int.ere(5)` rather
      // than `int.ere(_ => 5)`. There is deliberately no `since` counterpart — it would return
      // its own argument. Setting without wanting the old value is `int() = 5`.
      inline def ere(inline supplied: scala.Int): scala.Int = int.getAndSet(supplied)

  object Long:
    def apply(initial: scala.Long = 0L): Long = juca.AtomicLong(initial)

    extension (long: Long)
      inline def apply(): scala.Long = long.get()
      inline def update(value: scala.Long): Unit = long.set(value)
      inline def publish(value: scala.Long): Unit = long.lazySet(value)
      inline def swap(value: scala.Long): scala.Long = long.getAndSet(value)

      inline def replace(expected: scala.Long, updated: scala.Long): Boolean =
        long.compareAndSet(expected, updated)

      inline def ere(inline transition: scala.Long => scala.Long): scala.Long =
        ${atomicMacros.long('long, 'transition, true)}

      inline def since(inline transition: scala.Long => scala.Long): scala.Long =
        ${atomicMacros.long('long, 'transition, false)}

      inline def ere(inline supplied: scala.Long): scala.Long = long.getAndSet(supplied)

  object Bool:
    def apply(initial: Boolean = false): Bool = juca.AtomicBoolean(initial)

    extension (bool: Bool)
      inline def apply(): Boolean = bool.get()
      inline def update(state: Boolean): Unit = bool.set(state)
      inline def publish(state: Boolean): Unit = bool.lazySet(state)
      inline def swap(state: Boolean): Boolean = bool.getAndSet(state)

      inline def replace(expected: Boolean, updated: Boolean): Boolean =
        bool.compareAndSet(expected, updated)

      inline def ere(inline transition: Boolean => Boolean): Boolean =
        ${atomicMacros.bool('bool, 'transition, true)}

      inline def since(inline transition: Boolean => Boolean): Boolean =
        ${atomicMacros.bool('bool, 'transition, false)}

      // `bool.ere(true)` returning `false` is the one-shot idiom at its shortest: "I am the call
      // that closed it".
      inline def ere(inline supplied: Boolean): Boolean = bool.getAndSet(supplied)

  object Ref:
    def apply[value](initial: value): Ref[value] = juca.AtomicReference(initial)

    // An initially-absent cell. `Ref(Unset)` would infer `Ref[Unset]`, and spelling
    // `Ref[Optional[Monitor]](Unset)` at each site reads worse than naming the state.
    def vacant[value]: Ref[Optional[value]] = juca.AtomicReference(Unset)

    // A volatile read, written `ref()`. `asInstanceOf`, not `.nn`: `.nn` emits a null test and a
    // call to `Scala3RunTime.nnFail`, and — decisively — it THROWS on a cell holding `Unset`,
    // because `Unset` is `null` at runtime. `Ref[Optional[x]]().nn` is a latent NPE; this is
    // not. For an abstract `value` the cast erases to nothing at all.
    extension [value](ref: Ref[value])
      inline def apply(): value = ref.get().asInstanceOf[value]
      inline def update(value: value): Unit = ref.set(value)
      inline def publish(value: value): Unit = ref.lazySet(value)
      inline def swap(value: value): value = ref.getAndSet(value).asInstanceOf[value]

      inline def replace(expected: value, updated: value): Boolean =
        ref.compareAndSet(expected, updated)

      inline def ere(inline transition: value => value): value =
        ${atomicMacros.ref('ref, 'transition, true)}

      inline def since(inline transition: value => value): value =
        ${atomicMacros.ref('ref, 'transition, false)}

      // The value overload, as on the other cells — with one measured limitation. Where `value`
      // is ITSELF a function type, this overload is unreachable: `ere` commits to the transition
      // overload and reports `Required: (Int => Int) => Int => Int`, for a lambda and for a named
      // function value alike. That is a loud failure rather than a quiet mis-selection, and the
      // spelling to use is `ref() = supplied`.
      //
      // `Ref[Any]` is the one quiet case: a lambda literal satisfies both overloads and is read
      // as a transition — the likelier intent, but chosen without a diagnostic, so store a
      // lambda into such a cell with `ref() = lambda`.
      inline def ere(inline supplied: value): value =
        ref.getAndSet(supplied).asInstanceOf[value]

      // The escape hatch, for a transition whose shape cannot be inspected because it is a
      // function VALUE rather than a literal — `contingency.Accrual` transitions with `combine`,
      // a constructor parameter. `inline`, so the lambda still beta-reduces and nothing is
      // allocated; what is lost is the shape check, and the obligation that check would have
      // discharged is stated here instead: `transition` MAY BE RE-RUN under contention, so it
      // must be pure. `revise` yields the value it installed, as `since` does; no call site in
      // the collection needs the displaced value from a transition whose shape cannot be read.
      inline def revise(inline transition: value => value): value =
        val box: juca.AtomicReference[value] = ref
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

  // The arrays are indexed by `Ordinal`, not `Int`: a bare `Int` index is precisely the
  // unchecked scalar `denominative` exists to abolish, and it costs nothing — `Ordinal` is an
  // opaque `Int` and `n0` is the identity, so a masked ring index compiles to the `iand` alone.
  // The ordinal is unbranded deliberately: a masked index is in range by construction of the
  // mask, so there is nothing for branding to prove and its range check would not be free.
  //
  // No `ere`/`since` on any of the three. A per-slot transition would need the ordinal threaded
  // through the macro, and no call site in the collection does compare-and-set on a slot —
  // zephyrine's rings publish and read, and nothing else uses an atomic array at all.
  object Ints:
    def apply(size: scala.Int): Ints = juca.AtomicIntegerArray(size)

    // In this companion rather than `Countable`'s: denominative sits below rudiments, so it
    // cannot see this type. Companion-to-companion placement resolves identically.
    given countable: Ints is Countable = _.length()

    // A slot reads as `scala.Int`, not `Optional[scala.Int]`: a freshly-allocated primitive
    // array is all zeros, and zero is a value rather than an absence. `Refs` differs precisely
    // because its fresh slots are null, which is an absence.
    extension (ints: Ints)
      inline def apply(ordinal: Ordinal): scala.Int = ints.get(ordinal.n0)
      inline def update(ordinal: Ordinal, value: scala.Int): Unit = ints.set(ordinal.n0, value)

      inline def publish(ordinal: Ordinal, value: scala.Int): Unit =
        ints.lazySet(ordinal.n0, value)

      inline def swap(ordinal: Ordinal, value: scala.Int): scala.Int =
        ints.getAndSet(ordinal.n0, value)

      inline def replace(ordinal: Ordinal, expected: scala.Int, updated: scala.Int): Boolean =
        ints.compareAndSet(ordinal.n0, expected, updated)

  object Longs:
    def apply(size: scala.Int): Longs = juca.AtomicLongArray(size)

    given countable: Longs is Countable = _.length()

    extension (longs: Longs)
      inline def apply(ordinal: Ordinal): scala.Long = longs.get(ordinal.n0)
      inline def update(ordinal: Ordinal, value: scala.Long): Unit = longs.set(ordinal.n0, value)

      inline def publish(ordinal: Ordinal, value: scala.Long): Unit =
        longs.lazySet(ordinal.n0, value)

      inline def swap(ordinal: Ordinal, value: scala.Long): scala.Long =
        longs.getAndSet(ordinal.n0, value)

      inline def replace(ordinal: Ordinal, expected: scala.Long, updated: scala.Long): Boolean =
        longs.compareAndSet(ordinal.n0, expected, updated)

  object Refs:
    def apply[value](size: scala.Int): Refs[value] = juca.AtomicReferenceArray(size)

    given countable: [value] => Refs[value] is Countable = _.length()

    // `Optional`, not `value`: a freshly-allocated reference array is all-null, so a slot read
    // is genuinely partial. That is the honest difference from `Ref`, which is constructed with
    // an initial value and whose reads are therefore total, and from `Ints`, whose fresh slots
    // hold a real zero.
    extension [value](refs: Refs[value])
      inline def apply(ordinal: Ordinal): Optional[value] =
        refs.get(ordinal.n0).asInstanceOf[Optional[value]]

      inline def update(ordinal: Ordinal, value: Optional[value]): Unit =
        refs.set(ordinal.n0, value)

      inline def publish(ordinal: Ordinal, value: Optional[value]): Unit =
        refs.lazySet(ordinal.n0, value)

      inline def swap(ordinal: Ordinal, value: Optional[value]): Optional[value] =
        refs.getAndSet(ordinal.n0, value).asInstanceOf[Optional[value]]

      inline def replace(ordinal: Ordinal, expected: Optional[value], updated: Optional[value])
      :   Boolean =

        refs.compareAndSet(ordinal.n0, expected, updated)

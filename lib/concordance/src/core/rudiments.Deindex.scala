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

import scala.caps
import scala.compiletime.*

import denominative.*
import prepositional.*
import vacuous.*

// Deindexing (issue #1666): `apply` and `at` share one compile-time dispatch — a confined
// index (`Operand in value.type`) reaches `access` directly and returns a bare `Result`;
// any other index is bounds-checked and returns `Optional`. `apply` is the destination of
// the `at` migration; `at` remains as the bridge. In its own file so the extension group
// sits at package scope rather than shadowing same-file resolution (the `Tagged` unwrapping
// `apply` in particular).
// The typeclass arrives as an explicit `using` clause rather than a context bound: a context
// bound's evidence is resolved *after* the compiler commits to this extension as the `apply`
// candidate, so a failed summon becomes an error at the call site — blocking the `Dynamic`
// fallback (`applyDynamicNamed` factories such as honeycomb tags and `Http.Response`) and
// shadowing every domain-specific `apply` extension. With the explicit clause, a receiver with
// no `Applicable` instance discards this candidate silently and resolution proceeds as if it
// did not exist.
// The interval form of `unsafeAttested`, for brand-taking bulk operations (`iterate(range)`): same
// discipline, same soundness boundary. The receiver is the INTERVAL —
// `interval.unsafeAttested(xs)` reads as "this interval, attested for xs" — because a same-name
// overload on the collection receiver merges into the `Applicable` group's overload set, where
// the path-dependent `Operand` cannot be excluded before evidence resolution and the call
// reports ambiguity.
extension (interval: Interval)
  inline def unsafeAttested[within](within: within)(using erased unsafe: Unsafe): Interval in within.type =
    interval.asInstanceOf[Interval in within.type]

extension [self](value: self)(using applicable: Applicable { type Self = self })
  inline def defines(index: applicable.Operand): Boolean = applicable.contains(value, index)

  // Checks that `index` is defined for *this* value and, if so, returns it *confined* to it
  // (`Operand in value.type`), which `at` recognizes statically: the subsequent access returns a
  // bare `Result` with no second bounds check — `map.confine(key).let(map.at(_))`,
  // or `map.pick(key)(map.at(_))` when the lambda is the whole use. It generalizes
  // denominative's `within` (the `Ordinal` producer) to any index or key type. Sound for
  // immutable receivers on stable paths, like `within` and `at`'s confined branch. Not `inline`:
  // see the note on `prim`/`sec`/`ter` below (same capture-checking issue).
  def confine(index: applicable.Operand): Optional[applicable.Operand in value.type] =
    if applicable.contains(value, index) then index.asInstanceOf[applicable.Operand in value.type]
    else Unset

  // The fused form of `confine` and `let`: the checked index reaches the lambda already confined,
  // so the body's reads are total, and no `Optional[Operand]` is built in between. The safe twin
  // of the block-scoped `unsafeAttested` below —
  //     array.pick(ordinal): ordinal => array(ordinal)   // Optional[element], total inside
  // When the body only reads the element, `at` already says it: `array.at(ordinal)`.
  inline def pick[result](index: applicable.Operand)
    ( inline lambda: (applicable.Operand in value.type) => result )
  :   Optional[result] =

    if applicable.contains(value, index)
    then lambda(index.asInstanceOf[applicable.Operand in value.type])
    else Unset

  // As `confine`, minus the check: mints `Operand in value.type` on the caller's word, gated by
  // `Unsafe`, for the two places a proof cannot follow the program — the quote boundary and
  // index arithmetic. Every call site carries a comment naming the construction that proves the
  // bound: the comment is the proof, as `Retained`'s construction-site mint is in dissonance.
  // The subsequent access (`value(index)`) is the EXISTING confined read: total, bare, no
  // second check. Same soundness boundary as `confine`: immutable receivers on stable paths.
  def unsafeAttested(index: applicable.Operand)(using erased unsafe: Unsafe)
  :   applicable.Operand in value.type =

    index.asInstanceOf[applicable.Operand in value.type]

  // The block-scoped form: the attestation's extent is the lambda —
  //     array.unsafeAttested(ordinal): ordinal => array(ordinal)   // total inside
  inline def unsafeAttested[result](index: applicable.Operand)
    (inline lambda: (applicable.Operand in value.type) => result)(using erased unsafe: Unsafe)
  :   result =

    lambda(index.asInstanceOf[applicable.Operand in value.type])


  // A single `at` that dispatches at compile time on the index type: an index statically known to
  // be confined to *this* `value` (an `Operand in value.type`, hence in range) returns a bare
  // `Result`; any other index is bounds-checked and returns `Optional`. The declared return type is
  // `Optional`, so non-reducing (e.g. generic) call sites are safe; a confined index narrows to a
  // bare `Result`.
  // The universal deindexing `apply` (issue #1666), toward which `at` call sites are
  // migrating: identical compile-time dispatch — a confined index returns a bare `Result`
  // with no bounds check; any other index is checked and returns `Optional`.
  // The index is coerced with a cast, not by applying `sub`: every `<:<` is `refl` at runtime,
  // whose `apply` erases to `(Object)Object` and so boxes and unboxes an `Ordinal` on every
  // read. The cast is a no-op at the same erasure, and `sub` still carries the proof.
  transparent inline def apply[index](ordinal: index)(using sub: index <:< applicable.Operand)
  :   Optional[applicable.Result] =

    summonFrom:
      case _: (`index` <:< (applicable.Operand in value.type)) =>
        applicable.access(value, ordinal.asInstanceOf[applicable.Operand])

      case _ =>
        val key: applicable.Operand = ordinal.asInstanceOf[applicable.Operand]

        optimizable[applicable.Result]: default =>
          if applicable.contains(value, key) then applicable.access(value, key) else default

  transparent inline def at[index](ordinal: index)(using sub: index <:< applicable.Operand)
  :   Optional[applicable.Result] =

    summonFrom:
      case _: (`index` <:< (applicable.Operand in value.type)) =>
        applicable.access(value, ordinal.asInstanceOf[applicable.Operand])

      case _ =>
        val key: applicable.Operand = ordinal.asInstanceOf[applicable.Operand]

        optimizable[applicable.Result]: default =>
          if applicable.contains(value, key) then applicable.access(value, key) else default

// The `Sized` producer, the counterpart of `occupied`: one length check, recorded in the type.
// A plain method, not `inline`: an inline body returning the `Optional` union of a frozen
// array re-freshens its capture set at each call site (the `Optional[Data]` trap).
extension [self](value: self)(using countable: Countable { type Self = self })
  def sized[length <: Int](using length: ValueOf[length]): Optional[self & Sized[length]] =
    if countable.size(value) == length.value
    then value.asInstanceOf[self & Sized[length]]
    else Unset

// A literal ordinal on a `Sized` array is confined at compile time: `sizedMacros.read` checks
// `i < n` as it expands, so the read is total and an out-of-range literal does not compile;
// a variable ordinal takes the `at` route and yields `Optional`.
extension [element, length <: Int](array: Array[element]^{caps.any.rd} & Sized[length])
  transparent inline def apply(inline ordinal: Ordinal): Any =
    ${sizedMacros.read[element, length]('array, 'ordinal)}

extension [key, value](map: scala.collection.Map[key, value])
  inline def defines(key: key): Boolean = map.contains(key)
  inline def bijection: Bijection[key, value] = Bijection(map.to(scala.collection.immutable.Map))

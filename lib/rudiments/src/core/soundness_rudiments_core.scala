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
package soundness

export
  rudiments
  // `Scribe`/`scribe` and `Surveyor`/`survey` are deliberately absent: their lenders and
  // combinators are dependent-typed inline extensions, which synthesized export forwarders break
  // (the same policy as zephyrine's `Region`/`Slate`). Consumers import them from `rudiments`
  // directly.
  . { !!, &, all, also, and, annex, Atomic, b, bi, Bijection, bijection, Bytes, bytes, collate,
      DecimalConverter, Defaulting, Defaulting2, Digit, Dynamical, Dynamically, dynamically, each,
      establish, Exit,
      Termination, fixpoint, fuse, gib,
      give, indexBy, intercalate, javaInputStream, kib,
      longestTrain,
      Loop, loop, mean, mib, Mutex, next, ordinal, pipe, place, plus,
      prior, probe, product, Fixpoint, reflectClass, repeat, runs, runsBy, segment, Segmentable,
      before, upto, from, after, snip, tail, Appendable, Prependable, `:+`, `+:`,
      indexed, sort, order, maximize, minimize, sift, snapshot, state, std, sumBy, tap, that, tib, to, total, tri,
      triple, tuple, twin,
      typed, typeName, unit, unsafeMatchable, unwind, upsert,
      variance, waive, weave, when, yet, upon, context,
      mean2, unique, seek, reap, where, Scoped, unused,
      Populated, head, last, lead, reduce, populatedEquality, sized, sizedEquality,
      sizedEqualityReversed }

// The two interval `each` overloads are re-declared here, beside the exported collection `each`,
// rather than exported from denominative: two toplevel definitions of one name in `package
// soundness` do not overload — one is silently dropped — so a name must be owned by a single
// compilation unit. Both overloads must also share one scope, or the unbranded alternative wins
// for a branded receiver and silently strips the brand.
extension (interval: denominative.Interval)
  inline def each(inline lambda: denominative.Ordinal => Unit): Unit =
    denominative.each(interval)(lambda)

extension [form](range: prepositional.`in`[denominative.Interval, form])
  inline def each(inline lambda: prepositional.`in`[denominative.Ordinal, form] => Unit): Unit =
    denominative.each(range)(lambda)

// The `Deindex` extension group (`apply`, `at`, `defines`, `confine`, `pick`, `prim`, `sec`,
// `ter`) is re-declared here rather than exported: its typeclass evidence is a dependent
// leading `using` clause (required so a failed summon discards the candidate instead of
// erroring — see `rudiments.Deindex`), and synthesized export forwarders cannot carry that
// shape. Each method delegates to the `rudiments` original inline, so `summonFrom` dispatch
// and confined-index narrowing behave identically.
extension (interval: denominative.Interval)
  inline def unsafeAttested[within](within: within)(using erased unsafe: vacuous.Unsafe)
  :   prepositional.`in`[denominative.Interval, within.type] =

    interval.asInstanceOf[prepositional.`in`[denominative.Interval, within.type]]

extension [self](value: self)(using applicable: denominative.Applicable { type Self = self })
  // `defines` and `confine` duplicate the (one-line) `rudiments.Deindex` bodies rather than
  // delegate: inline-to-inline delegation leaves the evidence proxy's `Self` unreduced at
  // expansion sites.
  def defines(index: applicable.Operand): Boolean = applicable.contains(value, index)

  def confine(index: applicable.Operand)
  :   vacuous.Optional[prepositional.`in`[applicable.Operand, value.type]] =

    if applicable.contains(value, index)
    then index.asInstanceOf[prepositional.`in`[applicable.Operand, value.type]]
    else vacuous.Unset

  // Duplicated for the same reason as `confine`.
  inline def pick[result](index: applicable.Operand)
    ( inline lambda: prepositional.`in`[applicable.Operand, value.type] => result )
  :   vacuous.Optional[result] =

    if applicable.contains(value, index)
    then lambda(index.asInstanceOf[prepositional.`in`[applicable.Operand, value.type]])
    else vacuous.Unset

  // Re-declared like the rest of the group; see `rudiments.unsafeAttested` for the discipline.
  def unsafeAttested(index: applicable.Operand)(using erased unsafe: vacuous.Unsafe)
  :   prepositional.`in`[applicable.Operand, value.type] =

    index.asInstanceOf[prepositional.`in`[applicable.Operand, value.type]]

  inline def unsafeAttested[result](index: applicable.Operand)
    ( inline lambda: prepositional.`in`[applicable.Operand, value.type] => result )
    ( using erased unsafe: vacuous.Unsafe )
  :   result =

    lambda(index.asInstanceOf[prepositional.`in`[applicable.Operand, value.type]])


  // The index parameter is typed directly as `applicable.Operand` (not a bounded type
  // parameter): a call with any other index type makes this candidate inapplicable by
  // argument typing, so resolution falls through to more specific `apply` extensions (e.g.
  // compat's frozen-array `apply(Int)`). Declared `inline` so a confined argument's precise
  // type (`Operand in value.type`) survives substitution and the delegate still narrows.
  // Same `[index]`-and-evidence shape as the `rudiments` originals (a parameter typed
  // `applicable.Operand` with singleton-type dispatch in the body does not survive TASTY:
  // consumers compiling against the pickled extension cannot construct the candidate). The
  // delegation is inline, so the precise index type flows through and the originals'
  // confined/checked dispatch behaves identically here.
  transparent inline def apply[index](ordinal: index)(using sub: index <:< applicable.Operand)
  :   vacuous.Optional[applicable.Result] =

    rudiments.apply(value)(ordinal)

  transparent inline def at[index](ordinal: index)(using sub: index <:< applicable.Operand)
  :   vacuous.Optional[applicable.Result] =

    rudiments.at(value)(ordinal)

// The `Sized` read, re-declared like the group above: `inline` so the literal ordinal reaches
// the macro behind `rudiments.apply` as a literal tree, which is what it checks.
extension [element, length <: Int]
  ( array: proscenium.Array[element]^{scala.caps.any.rd} & proscenium.Sized[length] )

  transparent inline def apply(inline ordinal: denominative.Ordinal): Any =
    rudiments.apply(array)(ordinal)

// Re-declared for the same reason as the `Deindex` group above: the typeclass evidence is a
// dependent leading `using` clause, which synthesized export forwarders cannot carry.
extension [self](value: self)(using definable: denominative.Definable { type Self = self })
  def define(index: definable.Operand, result: definable.Result): self =
    definable.define(value, index, result)

extension [self](value: self)(using omissible: denominative.Omissible { type Self = self })
  def omit(index: omissible.Operand): self = omissible.omit(value, index)

// `keep` and `skip` are re-declared rather than exported (unlike their siblings above, in the
// export clause): `Chain`'s lazy forms live in its companion — implicit scope — and a call on a
// `Chain` receiver only reaches them when the lexical candidate's failed `Segmentable` summon
// DISCARDS the candidate, which the originals' trailing `using` clause does but a synthesized
// export forwarder does not. Each delegates to the `rudiments` original.
extension [value](value: value)
  ( using segmentable: value is rudiments.Segmentable,
          countable:   value is denominative.Countable )

  def keep(count: Int, bidi: anticipation.Bidi = anticipation.Bidi.Ltr): segmentable.Segment =
    rudiments.keep(value)(count, bidi)

  def skip(count: Int, bidi: anticipation.Bidi = anticipation.Bidi.Ltr): segmentable.Segment =
    rudiments.skip(value)(count, bidi)

extension [value](value: value)
  ( using traversable: value is murmuration.Traversable,
          segmentable: value is rudiments.Segmentable,
          countable:   value is denominative.Countable )

  def keep(predicate: traversable.Operand => Boolean): segmentable.Segment =
    rudiments.keep(value)(predicate)

  def keep(predicate: traversable.Operand => Boolean, bidi: anticipation.Bidi)
  :   segmentable.Segment =

    rudiments.keep(value)(predicate, bidi)

  def skip(predicate: traversable.Operand => Boolean): segmentable.Segment =
    rudiments.skip(value)(predicate)

  def skip(predicate: traversable.Operand => Boolean, bidi: anticipation.Bidi)
  :   segmentable.Segment =

    rudiments.skip(value)(predicate, bidi)



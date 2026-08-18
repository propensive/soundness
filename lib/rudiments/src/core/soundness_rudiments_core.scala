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
  // `Scribe`/`scribe` and `Surveyor`/`survey` are deliberately absent: their lenders and combinators are
  // dependent-typed inline extensions, which synthesized export forwarders break (the same
  // policy as zephyrine's `Region`/`Slate`). Consumers import them from `rudiments` directly.
  . { !!, &, all, also, and, annex, b, bi, Bijection, bijection, Bytes, bytes, collate, Counter,
      DecimalConverter, Defaulting, Defaulting2, Digit, each, establish, Exit, fixpoint,
      fuse, gib,
      give, immutable, indexBy, intercalate, javaInputStream, kib,
      longestTrain,
      Loop, loop, matchable, mean, mib, mutable, Mutex, next, ordinal, pipe, place, plus,
      prior, probe, product, Fixpoint, reflectClass, repeat, runs, runsBy, segment, Segmentable,
      before, upto, from, after, keep, skip, snip, Appendable, Prependable, `:+`, `+:`,
      indexed, least, most, sift, snapshot, state, std, sumBy, tap, that, tib, to, total, tri, triple, tuple, twin,
      typed, typeName, unit, unwind, upsert, variance, waive, weave, when, yet, upon, context,
      mean2, unique, seek, where,
      Populated, head, last, reduce, populatedEquality }

// The `Deindex` extension group (`apply`, `at`, `defines`, `confine`, `prim`, `sec`, `ter`) is
// re-declared here rather than exported: its typeclass evidence is a dependent leading `using`
// clause (required so a failed summon discards the candidate instead of erroring — see
// `rudiments.Deindex`), and synthesized export forwarders cannot carry that shape. Each method
// delegates to the `rudiments` original inline, so `summonFrom` dispatch and confined-index
// narrowing behave identically.
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

// Re-declared for the same reason as the `Deindex` group above: the typeclass evidence is a
// dependent leading `using` clause, which synthesized export forwarders cannot carry.
extension [self](value: self)(using definable: denominative.Definable { type Self = self })
  def define(index: definable.Operand, result: definable.Result): self =
    definable.define(value, index, result)

extension [element](sequence: proscenium.List[element])
  // Mirrors the ungated `List` special case in `rudiments.Deindex` (same non-`inline`
  // rationale); duplicated because `rudiments.prim` resolves to the `Applicable`-bound
  // overload when referenced qualified.
  def prim: vacuous.Optional[element] =
    if sequence.stdlib.isEmpty then vacuous.Unset else sequence.stdlib.head

extension [self](inline value: self)
  (using applicable: denominative.Applicable { type Self = self; type Operand = denominative.Ordinal })

  inline def prim: vacuous.Optional[applicable.Result] = rudiments.prim(value)
  inline def sec: vacuous.Optional[applicable.Result] = rudiments.sec(value)
  inline def ter: vacuous.Optional[applicable.Result] = rudiments.ter(value)

// `zip` is deliberately NOT re-exported: zeppelin's contextual archive accessor owns the bare
// name `zip` in this package, and a generic-receiver extension overload commits without falling
// through when its givens fail. The extension remains available via `import rudiments.*`; the
// collection aliases will host `zip` in their companions (implicit scope) once opaque.

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
extension [self](value: self)(using applicable: Applicable { type Self = self })
  inline def defines(index: applicable.Operand): Boolean = applicable.contains(value, index)

  // Checks that `index` is defined for *this* value and, if so, returns it *confined* to it
  // (`Operand in value.type`), which `at` recognizes statically: the subsequent access returns a
  // bare `Result` with no second bounds check — `map.confine(key).let(map.at(_))`. It generalizes
  // denominative's `within` (the `Ordinal` producer) to any index or key type. Sound for
  // immutable receivers on stable paths, like `within` and `at`'s confined branch. Not `inline`:
  // see the note on `prim`/`sec`/`ter` below (same capture-checking issue).
  def confine(index: applicable.Operand): Optional[applicable.Operand in value.type] =
    if applicable.contains(value, index) then index.asInstanceOf[applicable.Operand in value.type]
    else Unset

  // A single `at` that dispatches at compile time on the index type: an index statically known to
  // be confined to *this* `value` (an `Operand in value.type`, hence in range) returns a bare
  // `Result`; any other index is bounds-checked and returns `Optional`. The declared return type is
  // `Optional`, so non-reducing (e.g. generic) call sites are safe; a confined index narrows to a
  // bare `Result`.
  // The universal deindexing `apply` (issue #1666), toward which `at` call sites are
  // migrating: identical compile-time dispatch — a confined index returns a bare `Result`
  // with no bounds check; any other index is checked and returns `Optional`.
  transparent inline def apply[index](ordinal: index)(using sub: index <:< applicable.Operand)
  :   Optional[applicable.Result] =

    summonFrom:
      case _: (`index` <:< (applicable.Operand in value.type)) =>
        applicable.access(value, sub(ordinal))

      case _ =>
        val key: applicable.Operand = sub(ordinal)

        optimizable[applicable.Result]: default =>
          if applicable.contains(value, key) then applicable.access(value, key) else default

  transparent inline def at[index](ordinal: index)(using sub: index <:< applicable.Operand)
  :   Optional[applicable.Result] =

    summonFrom:
      case _: (`index` <:< (applicable.Operand in value.type)) =>
        applicable.access(value, sub(ordinal))

      case _ =>
        val key: applicable.Operand = sub(ordinal)

        optimizable[applicable.Result]: default =>
          if applicable.contains(value, key) then applicable.access(value, key) else default

extension [key, value](map: scala.collection.Map[key, value])
  inline def defines(key: key): Boolean = map.contains(key)
  inline def bijection: Bijection[key, value] = Bijection(map.to(scala.collection.immutable.Map))

extension [self](inline value: self)
  (using applicable: Applicable { type Self = self; type Operand = Ordinal })
  inline def prim: Optional[applicable.Result] = value.at(Prim)
  inline def sec: Optional[applicable.Result] = value.at(Sec)
  inline def ter: Optional[applicable.Result] = value.at(Ter)

extension [element](sequence: List[element])

  // Deliberately NOT `inline`: an `inline def` returning the union `Optional[element]` re-infers the
  // expanded body's type at each call site, where capture checking stamps a fresh `^` capture
  // variable on the union — which is spurious (and an error) when `element` is a pure type such as
  // `Text`. A plain method keeps the declared `Optional[element]` result and stays capture-clean.
  // Only `prim` gets this ungated `List` special case (O(1)); `sec`/`ter` go through the
  // `LinearAccessComplexity`-gated `Applicable` route like every other positional access on `List`.
  def prim: Optional[element] = if sequence.stdlib.isEmpty then Unset else sequence.stdlib.head

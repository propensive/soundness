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
package gastronomy

import scala.{caps, util}

import java.lang as jl

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

object Digestible extends Derivable[Digestible]:
  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Digestible =
    (digestion, value) => fields(value):
      [field] => field => contextual.digest(digestion, field)

  inline def disjunction[derivation: SumReflection]: derivation is Digestible =
    (digestion, value) =>
      variant(value): [variant <: derivation] => variant =>
        int.digest(digestion, index)
        contextual.digest(digestion, variant)


  // The collection instances retain their by-name element digesters, which share each
  // instance's given-resolution lifetime. As in `Inspectable`, the by-name is bound as a
  // *pure thunk* before the SAM body — the narrowest form of the codec-thunk seal (see
  // rep/DECISIONS.md): a by-name parameter is unnameable in a capture set, but the thunk
  // yields a compiler-verified-pure instance (`Digestible` is a `Typeclass.Pure`), so only
  // the thunk is asserted and the SAM instance itself is checked pure. The thunk keeps
  // resolution lazy, which recursive derivations depend on.
  given optional: [value] => (digestible: => value is Digestible)
  =>  util.NotGiven[Unset.type <:< value]
  =>  Optional[value] is Digestible =

    val dig: () -> (value is Digestible) = caps.unsafe.unsafeAssumePure(() => digestible)
    (acc, value) => value.let(dig().digest(acc, _))


  given list: [list <: List, value] => (digestible: => value is Digestible)
  =>  list[value] is Digestible =

    val dig: () -> (value is Digestible) = caps.unsafe.unsafeAssumePure(() => digestible)
    (digestion, list) => list.each(dig().digest(digestion, _))


  given set: [set <: Set, value] => (digestible: => value is Digestible)
  =>  set[value] is Digestible =

    val dig: () -> (value is Digestible) = caps.unsafe.unsafeAssumePure(() => digestible)
    (digestion, set) => set.stdlib.each(dig().digest(digestion, _))


  given series: [series <: Series, value] => (digestible: => value is Digestible)
  =>  series[value] is Digestible =

    val dig: () -> (value is Digestible) = caps.unsafe.unsafeAssumePure(() => digestible)
    (digestion, series) => series.stdlib.each(dig().digest(digestion, _))


  given iarray: [value] => (digestible: => value is Digestible) => (Array[value]^{}) is Digestible =
    val dig: () -> (value is Digestible) = caps.unsafe.unsafeAssumePure(() => digestible)
    (digestion, iarray) => iarray.each(dig().digest(digestion, _))


  given map: [key, value] => (keyDigestible: => key is Digestible)
  =>  ( valueDigestible: => value is Digestible )
  =>  Map[key, value] is Digestible =

    val digKey: () -> (key is Digestible) = caps.unsafe.unsafeAssumePure(() => keyDigestible)

    val digValue: () -> (value is Digestible) =
      caps.unsafe.unsafeAssumePure(() => valueDigestible)

    (digestion, map) =>
      map.stdlib.each: (key, value) =>
        digKey().digest(digestion, key)
        digValue().digest(digestion, value)


  given stream: [value] => (digestible: => value is Digestible) => Progression[value] is Digestible =
    val dig: () -> (value is Digestible) = caps.unsafe.unsafeAssumePure(() => digestible)
    (digestion, iterable) => iterable.each(dig().digest(digestion, _))

  given int: Int is Digestible = (digestion, value) =>
    digestion.append((24 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given long: Long is Digestible = (digestion, value) =>
    digestion.append((56 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given double: Double is Digestible = long.contramap(jl.Double.doubleToRawLongBits(_))
  given float: Float is Digestible = int.contramap(jl.Float.floatToRawIntBits(_))

  given boolean: Boolean is Digestible =
    (digestion, boolean) => digestion.append(Array.of(if boolean then 1.toByte else 0.toByte))

  given byte: Byte is Digestible = (digestion, byte) => digestion.append(Array.of(byte))

  given short: Short is Digestible =
    (digestion, short) => digestion.append(Array.of((short >> 8).toByte, short.toByte))

  given char: Char is Digestible =
    (digestion, char) => digestion.append(Array.of((char >> 8).toByte, char.toByte))

  given text: [text <: Text] => text is Digestible =
    (digestion, text) => digestion.append(text.in[Data](using charEncoders.utf8Encoder))

  given bytes: Data is Digestible = _.append(_)
  given digest: Digest is Digestible = (digestion, digest) => digestion.append(digest.data)

  given encodable: [value: Encodable in Data] => value is Digestible =
    bytes.contramap(value.encode)

trait Digestible extends Typeclass.Pure:
  digestible: Digestible =>

    // Exclusivity lives at the method level (`Digestion^`), not the instance: a
    // `Digestible` is a pure value which may be handed any exclusive accumulator.
    def digest(digestion: Digestion^, value: Self): Unit

  // Deviates from the impure-lambda combinator convention: a capturing `contramap` result
  // makes every SAM conversion to `Digestible` capture-tracked, which rejects the (sealed,
  // derivation-facing) collection givens above whose closures retain their by-name element
  // digesters. Digest transforms are pure in practice, so the pure form is kept.
  def contramap[self2](lambda: self2 -> Self): self2 is Digestible = new Digestible:
    type Self = self2

    def digest(digestion: Digestion^, value: Self): Unit =
      digestible.digest(digestion, lambda(value))

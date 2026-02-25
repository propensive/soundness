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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.lang as jl

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*
import wisteria.*

object Digestible extends Derivable[Digestible]:
  inline def join[derivation <: Product: ProductReflection]: derivation is Digestible =
    (digestion, value) => fields(value):
      [field] => field => context.digest(digestion, field)

  inline def split[derivation: SumReflection]: derivation is Digestible =
    (digestion, value) =>
      variant(value): [variant <: derivation] => variant =>
        int.digest(digestion, index)
        context.digest(digestion, variant)


  given optional: [value] => (digestible: => value is Digestible)
  =>  util.NotGiven[Unset.type <:< value]
  =>  Optional[value] is Digestible =

    (acc, value) => value.let(digestible.digest(acc, _))


  given list: [list <: List, value] => (digestible: => value is Digestible)
  =>  list[value] is Digestible =

    (digestion, list) => list.each(digestible.digest(digestion, _))


  given set: [set <: Set, value] => (digestible: => value is Digestible)
  =>  set[value] is Digestible =

    (digestion, set) => set.each(digestible.digest(digestion, _))


  given trie: [trie <: Trie, value] => (digestible: => value is Digestible)
  =>  trie[value] is Digestible =

    (digestion, trie) => trie.each(digestible.digest(digestion, _))


  given iarray: [value] => (digestible: => value is Digestible) => IArray[value] is Digestible =
    (digestion, iarray) => iarray.each(digestible.digest(digestion, _))


  given map: [key, value] => (keyDigestible: => key is Digestible)
  =>  ( valueDigestible: => value is Digestible )
  =>  Map[key, value] is Digestible =

    (digestion, map) =>
      map.each: (key, value) =>
        keyDigestible.digest(digestion, key)
        valueDigestible.digest(digestion, value)


  given stream: [value] => (digestible: => value is Digestible) => Stream[value] is Digestible =
    (digestion, iterable) => iterable.each(digestible.digest(digestion, _))

  given int: Int is Digestible = (digestion, value) =>
    digestion.append((24 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given long: Long is Digestible = (digestion, value) =>
    digestion.append((52 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given double: Double is Digestible = long.contramap(jl.Double.doubleToRawLongBits(_))
  given float: Float is Digestible = int.contramap(jl.Float.floatToRawIntBits(_))

  given boolean: Boolean is Digestible =
    (digestion, boolean) => digestion.append(IArray(if boolean then 1.toByte else 0.toByte))

  given byte: Byte is Digestible = (digestion, byte) => digestion.append(IArray(byte))

  given short: Short is Digestible =
    (digestion, short) => digestion.append(IArray((short >> 8).toByte, short.toByte))

  given char: Char is Digestible =
    (digestion, char) => digestion.append(IArray((char >> 8).toByte, char.toByte))

  given text: [text <: Text] => text is Digestible =
    (digestion, text) => digestion.append(text.data(using charEncoders.utf8))

  given bytes: Data is Digestible = _.append(_)
  given digest: Digest is Digestible = (digestion, digest) => digestion.append(digest.data)

  given encodable: [value: Encodable in Data] => value is Digestible =
    bytes.contramap(value.encode)

trait Digestible extends Typeclass:
  digestible =>

    def digest(digestion: Digestion, value: Self): Unit

  def contramap[self2](lambda: self2 => Self): self2 is Digestible = new Digestible:
    type Self = self2

    def digest(digestion: Digestion, value: Self): Unit =
      digestible.digest(digestion, lambda(value))

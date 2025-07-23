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
┃    Soundness, version 0.40.0.                                                                    ┃
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

import scala.collection as sc
import scala.compiletime.*, ops.int.*

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
      variant(value):
        [variant <: derivation] => variant =>
          int.digest(digestion, index)
          context.digest(digestion, variant)

  given optional: [digestible: Digestible] => util.NotGiven[Unset.type <:< digestible]
        =>  Optional[digestible] is Digestible =
    (acc, value) => value.let(digestible.digest(acc, _))

  given list: [list <: List, value: Digestible] => list[value] is Digestible =
    (digestion, list) => list.each(value.digest(digestion, _))

  given set: [set <: Set, value: Digestible] => set[value] is Digestible =
    (digestion, set) => set.each(value.digest(digestion, _))

  given trie: [trie <: Trie, value: Digestible] => trie[value] is Digestible =
    (digestion, trie) => trie.each(value.digest(digestion, _))

  given iarray: [value: Digestible] => IArray[value] is Digestible =
    (digestion, iarray) => iarray.each(value.digest(digestion, _))

  given map: [digestible: Digestible, digestible2: Digestible]
        =>  Map[digestible, digestible2] is Digestible =
    (digestion, map) => map.each: (key, value) =>
      digestible.digest(digestion, key)
      digestible2.digest(digestion, value)

  given stream: [value: Digestible] => Stream[value] is Digestible =
    (digestion, iterable) => iterable.each(value.digest(digestion, _))

  given int: Int is Digestible = (digestion, value) =>
    digestion.append((24 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given long: Long is Digestible = (digestion, value) =>
    digestion.append((52 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given double: Double is Digestible =
    (digestion, value) => long.digest(digestion, jl.Double.doubleToRawLongBits(value))

  given float: Float is Digestible =
    (digestion, value) => int.digest(digestion, jl.Float.floatToRawIntBits(value))

  given boolean: Boolean is Digestible =
    (digestion, boolean) => digestion.append(IArray(if boolean then 1.toByte else 0.toByte))

  given byte: Byte is Digestible = (digestion, byte) => digestion.append(IArray(byte))

  given short: Short is Digestible =
    (digestion, short) => digestion.append(IArray((short >> 8).toByte, short.toByte))

  given char: Char is Digestible =
    (digestion, char) => digestion.append(IArray((char >> 8).toByte, char.toByte))

  given text: [text <: Text] => text is Digestible =
    (digestion, text) => digestion.append(text.bytes(using charEncoders.utf8))

  given bytes: Bytes is Digestible = _.append(_)
  given digest: Digest is Digestible = (digestion, digest) => digestion.append(digest.bytes)

  given encodable: [value: Encodable in Bytes] => value is Digestible =
    bytes.contramap(value.encode)

trait Digestible extends Typeclass:
  digestible =>

    def digest(digestion: Digestion, value: Self): Unit

  def contramap[self2](lambda: self2 => Self): self2 is Digestible = new Digestible:
    type Self = self2

    def digest(digestion: Digestion, value: Self): Unit =
      digestible.digest(digestion, lambda(value))

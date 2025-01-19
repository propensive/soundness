/*
    Gastronomy, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gastronomy

import java.lang as jl

import scala.collection as sc
import scala.compiletime.*, ops.int.*

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

object Digestible extends Derivable[Digestible]:
  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Digestible =
    (digestion, value) => fields(value):
      [FieldType] => field => context.digest(digestion, field)

  inline def split[DerivationType: SumReflection]: DerivationType is Digestible =
    (digestion, value) =>
      variant(value):
        [VariantType <: DerivationType] => variant =>
          int.digest(digestion, index)
          context.digest(digestion, variant)

  given optional: [ValueType: Digestible] => util.NotGiven[Unset.type <:< ValueType]
      => Optional[ValueType] is Digestible =
    (acc, value) => value.let(ValueType.digest(acc, _))

  given iterable: [IterableType <: Iterable, ValueType: Digestible]
      => IterableType[ValueType] is Digestible =
    (digestion, iterable) => iterable.each(ValueType.digest(digestion, _))

  given map: [KeyType: Digestible, ValueType: Digestible]
      => Map[KeyType, ValueType] is Digestible =
    (digestion, map) => map.each: (key, value) =>
      KeyType.digest(digestion, key)
      ValueType.digest(digestion, value)

  given lazyList: [ValueType: Digestible] => LazyList[ValueType] is Digestible =
    (digestion, iterable) => iterable.each(ValueType.digest(digestion, _))

  given int: Int is Digestible = (digestion, value) =>
    digestion.append((24 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given long: Long is Digestible = (digestion, value) =>
    digestion.append((52 to 0 by -8).map(value >> _).map(_.toByte).toArray.immutable(using Unsafe))

  given Double is Digestible =
    (digestion, value) => long.digest(digestion, jl.Double.doubleToRawLongBits(value))

  given Float is Digestible =
    (digestion, value) => int.digest(digestion, jl.Float.floatToRawIntBits(value))

  given Boolean is Digestible =
    (digestion, boolean) => digestion.append(IArray(if boolean then 1.toByte else 0.toByte))

  given Byte is Digestible = (digestion, byte) => digestion.append(IArray(byte))

  given Short is Digestible =
    (digestion, short) => digestion.append(IArray((short >> 8).toByte, short.toByte))

  given Char is Digestible =
    (digestion, char) => digestion.append(IArray((char >> 8).toByte, char.toByte))

  given Text is Digestible =
    (digestion, text) => digestion.append(text.bytes(using charEncoders.utf8))

  given bytes: Bytes is Digestible = _.append(_)
  given Digest is Digestible = (digestion, digest) => digestion.append(digest.bytes)

  given [ValueType: Encodable in Bytes] => ValueType is Digestible =
    bytes.contramap(ValueType.encode)

trait Digestible:
  digestible =>

  type Self
  def digest(digestion: Digestion, value: Self): Unit

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Digestible = new Digestible:
    type Self = SelfType2

    def digest(digestion: Digestion, value: Self): Unit =
      digestible.digest(digestion, lambda(value))

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
package stratiform

import adversaria.*
import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import vacuous.*
import wisteria.*

import Tels.Polarity

// Constructors for fused `Encodable & Schematic` / `Decodable & Schematic`
// instances, built from a value's separate `Encodable`/`Decodable` and `Schematic`
// instances. Deliberately *methods*, not givens: a fused given is a subtype of
// both its codec typeclass and `Schematic`, so two such givens would be ambiguous
// for any bare `Schematic` summon. Call them where a fused instance is wanted
// (e.g. `given … = telSchematics.decodable[T]`). Mirrors jacinta's `jsonSchematics`.
object telSchematics:
  def encodable[value](using encoder: value is Encodable in Tel)
                      (using schema0: value is Schematic over Tels.Type)
  :     value is Encodable & Schematic in Tel over Tels.Type =
    new Encodable with Schematic:
      type Self = value
      type Form = Tel
      type Transport = Tels.Type
      def encoded(value: value): Tel = encoder.encoded(value)
      def schema(): Tels.Type = schema0.schema()

  def decodable[value](using decoder: value is Decodable in Tel)
                      (using schema0: value is Schematic over Tels.Type)
  :     value is Decodable & Schematic in Tel over Tels.Type =
    new Decodable with Schematic:
      type Self = value
      type Form = Tel
      type Transport = Tels.Type
      def decoded(tel: Tel): value = decoder.decoded(tel)
      def schema(): Tels.Type = schema0.schema()

// TEL refinement of the shared `anticipation.Schematic`: it adds a field-level
// `polarity` so the product derivation can mark `Optional` fields as `Loose` (TEL
// records optionality on the field, not on the schema node). The schema
// representation (`Transport`) is a `Tels.Type`. `verify` / `tels` still require
// only the shared `Schematic over Tels.Type`, which these givens satisfy.
trait TelSchematic extends Schematic:
  def polarity: Tels.Polarity = Tels.Polarity.Tight

// Schema derivation for TEL: scalars map to `Tels.Scalar`, products to a
// `Tels.Struct` of `Field`s, collections to a `Struct` with a repeatable `item`
// field, and `Map` to repeatable `entries` of `key`/`value`. Mixed into `object
// Tels` so the givens sit in the `Transport` companion's implicit scope.
trait Tels2:
  given text: Text is TelSchematic over Tels.Type = () => Tels.Scalar(IArray.empty)
  given string: String is TelSchematic over Tels.Type = () => Tels.Scalar(IArray.empty)
  given int: Int is TelSchematic over Tels.Type = () => Tels.Scalar(IArray.empty)
  given long: Long is TelSchematic over Tels.Type = () => Tels.Scalar(IArray.empty)
  given double: Double is TelSchematic over Tels.Type = () => Tels.Scalar(IArray.empty)
  given boolean: Boolean is TelSchematic over Tels.Type = () => Tels.Scalar(IArray.empty)

  given optional: [value: Schematic over Tels.Type]
  =>  Optional[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Optional[value]
      type Transport = Tels.Type
      def schema(): Tels.Type = value.schema()
      override def polarity: Tels.Polarity = Polarity.Loose

  given list: [value: Schematic over Tels.Type] => List[value] is TelSchematic over Tels.Type =
    () => Tels2.itemType(value.schema())

  given set: [value: Schematic over Tels.Type] => Set[value] is TelSchematic over Tels.Type =
    () => Tels2.itemType(value.schema())

  given series: [value: Schematic over Tels.Type] => Series[value] is TelSchematic over Tels.Type =
    () => Tels2.itemType(value.schema())

  given map: [key: Schematic over Tels.Type, value: Schematic over Tels.Type]
  =>  Map[key, value] is TelSchematic over Tels.Type =
    () =>
      val entry =
        Tels.Struct
          ( IArray
              ( Tels.Field(Polarity.Tight, Polarity.Implicit, t"key", key.schema(), Unset),
                Tels.Field(Polarity.Tight, Polarity.Implicit, t"value", value.schema(), Unset) ),
            IArray.empty )

      Tels.Struct
        ( IArray(Tels.Field(Polarity.Implicit, Polarity.Loose, t"entries", entry, Unset)),
          IArray.empty )

  inline given schematic: [value: Reflection] => value is TelSchematic over Tels.Type =
    TelsDerivation.derived

  // Assembles a self-contained `Tels` document whose root struct is the schema of
  // `value`.
  def tels[value: Schematic over Tels.Type](name: Text): Tels = value.schema().absolve match
    case struct: Tels.Struct =>
      Tels(name, struct, IArray.empty, Unset, IArray.empty, IArray.empty, IArray.empty)

object Tels2:
  // A collection encodes as a wrapping compound with repeatable `item` children,
  // so its schema is a `Struct` with a single repeatable `item` field.
  private[stratiform] def itemType(element: Tels.Type): Tels.Type =
    Tels.Struct
      ( IArray(Tels.Field(Polarity.Implicit, Polarity.Loose, t"item", element, Unset)),
        IArray.empty )

object TelsDerivation extends Derivable[TelSchematic over Tels.Type]:
  inline def conjunction[derivation <: Product: ProductReflection]
  :   derivation is TelSchematic over Tels.Type =

    () =>
      val renames: Map[Text, Text] = relabelling[derivation, Tel]

      val members =
        contexts:
          [field] => schematic =>
            val keyword: Text = renames.getOrElse(label, Tel.camelToKebab(label.s))
            Tels.Field(schematic.polarity, Polarity.Implicit, keyword, schematic.schema(), Unset)

        . to(List)

      Tels.Struct(IArray.from(members), IArray.empty)

  // TEL sums (selects) are not yet derived structurally; a permissive empty
  // struct lets a containing type still derive a schema.
  inline def disjunction[derivation: SumReflection]: derivation is TelSchematic over Tels.Type =
    () => Tels.Struct(IArray.empty, IArray.empty)

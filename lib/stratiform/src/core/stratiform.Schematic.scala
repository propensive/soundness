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
import gossamer.*
import prepositional.*
import vacuous.*
import wisteria.*

import Tels.Polarity

// Derives a TEL schema (`Tels.Type`) from a Scala type, mirroring jacinta's
// `Schematic`/`JsonSchema` derivation. A scalar maps to `Tels.Scalar`, a product
// to a `Tels.Struct` of `Field`s (one per case-class field, keyed by the
// camel→kebab field name and respecting `@name[Tel]` renames), a collection to a
// `Struct` carrying a single repeatable `item` field — matching the `item`-keyword
// collection encoding — and `Optional` loosens the enclosing field's `required`
// polarity. A full schema document is assembled by `Schematic.tels`.
object Schematic:
  given text: Text is Schematic = () => Tels.Scalar(IArray.empty)
  given string: String is Schematic = () => Tels.Scalar(IArray.empty)
  given int: Int is Schematic = () => Tels.Scalar(IArray.empty)
  given long: Long is Schematic = () => Tels.Scalar(IArray.empty)
  given double: Double is Schematic = () => Tels.Scalar(IArray.empty)
  given boolean: Boolean is Schematic = () => Tels.Scalar(IArray.empty)

  given optional: [value: Schematic] => Optional[value] is Schematic = new Schematic:
    type Self = Optional[value]
    def schema(): Tels.Type = value.schema()
    override def required: Polarity = Polarity.Loose

  given list: [value: Schematic] => List[value] is Schematic =
    () => collectionType(value.schema())

  given set: [value: Schematic] => Set[value] is Schematic =
    () => collectionType(value.schema())

  given series: [value: Schematic] => Series[value] is Schematic =
    () => collectionType(value.schema())

  // A `Map` is a series of repeatable `entries`, each a struct of a `key` and a
  // `value` field — matching the `entries`/`key`/`value` encoding.
  given map: [key: Schematic, value: Schematic] => Map[key, value] is Schematic =
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

  // A collection encodes as a wrapping compound whose repeatable `item` children
  // are the elements, so its schema is a `Struct` with a single repeatable `item`
  // field of the element type.
  private def collectionType(element: Tels.Type): Tels.Type =
    Tels.Struct
      ( IArray(Tels.Field(Polarity.Implicit, Polarity.Loose, t"item", element, Unset)),
        IArray.empty )

  inline given schematic: [value: Reflection] => value is Schematic = TelsDerivation.derived

  // Assembles a full, self-contained `Tels` document whose root struct is the
  // schema of `value`.
  def tels[value: Schematic](name: Text): Tels = value.schema().absolve match
    case struct: Tels.Struct =>
      Tels(name, struct, IArray.empty, Unset, IArray.empty, IArray.empty, IArray.empty)

object TelsDerivation extends Derivable[Schematic]:
  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Schematic =
    () =>
      val renames: Map[Text, Text] = relabelling[derivation, Tel]

      val members =
        contexts:
          [field] => schematic =>
            val keyword: Text = renames.getOrElse(label, Tel.camelToKebab(label.s))
            Tels.Field(schematic.required, Polarity.Implicit, keyword, schematic.schema(), Unset)

        . to(List)

      Tels.Struct(IArray.from(members), IArray.empty)

  // TEL sums (selects) are not yet derived structurally; a permissive empty
  // struct is produced so a containing type can still derive a schema.
  inline def disjunction[derivation: SumReflection]: derivation is Schematic =
    () => Tels.Struct(IArray.empty, IArray.empty)

trait Schematic extends Typeclass:
  def schema(): Tels.Type

  // The field-level `required` polarity contributed by this type when it appears
  // as a case-class field; `Optional` loosens it.
  def required: Tels.Polarity = Tels.Polarity.Tight

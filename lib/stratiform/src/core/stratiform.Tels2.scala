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

import scala.compiletime.*

import adversaria.*
import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import vacuous.*
import wisteria.*

import Tels.Polarity

// Constructors for fused `Encodable & Schematic` / `Decodable & Schematic`
// instances. The schema is taken from the codec itself (`Tel.Encodable` /
// `Tel.Decodable` *carry* a `Shape`), never resolved independently, so the fused
// instance is coherent by construction — a gated or `… in Text`-branch codec
// always pairs with the schema for exactly what it reads/writes. The carried
// `Shape` is reified into a `Tels.Type` here. Deliberately *methods*, not givens
// (a fused given is `<: Schematic`, so two would be ambiguous for a bare
// `Schematic` summon). Mirrors jacinta's `jsonSchematics`.
object telSchematics:
  def encodable[value](using encoder: value is Tel.Encodable)
  :   value is Encodable & Schematic in Tel over Tels.Type =

    new Encodable with Schematic:
      type Self = value
      type Form = Tel
      type Transport = Tels.Type
      def encoded(value: value): Tel = encoder.encoded(value)
      def schema(): Tels.Type = Tels2.reify(encoder.shape())

  def decodable[value](using decoder: value is Tel.Decodable)
  :   value is Decodable & Schematic in Tel over Tels.Type =

    new Decodable with Schematic:
      type Self = value
      type Form = Tel
      type Transport = Tels.Type
      def decoded(tel: Tel): value = decoder.decoded(tel)
      def schema(): Tels.Type = Tels2.reify(decoder.shape())

// TEL refinement of the shared `anticipation.Schematic`: it adds field-level
// `polarity` (so the product derivation can mark `Optional` fields `Loose`) and
// `repeatable` (so `List`/`Set` fields become repeatable fields — TEL records both
// optionality and repeatability on the field, not on the schema node). The schema
// representation (`Transport`) is a `Tels.Type`. `tels` still requires only the
// shared `Schematic over Tels.Type`, which these givens satisfy.
trait TelSchematic extends Schematic:
  def polarity: Tels.Polarity = Tels.Polarity.Tight
  def repeatable: Tels.Polarity = Tels.Polarity.Implicit

  // Named `select` definitions a sum type contributes to the schema's namespace
  // (a select is referenced by name, never inlined). Empty for scalars and
  // products; a sum returns its own `SelectDefinition` here while `schema()`
  // returns a `Reference` to it.
  def selectDefinitions: List[Tels.SelectDefinition] = Nil

object Tels2:
  // Reifies a codec's format-neutral `Shape` (carried by `Tel.Encodable` /
  // `Tel.Decodable`) into a concrete `Tels.Type`. TEL records optionality and
  // repeatability on the *field*, so an `Opt` field becomes a `Loose` member and an
  // `Arr` (list/set) field a `Loose`/repeatable member whose type is the element's
  // schema (collections are repeated fields, per `#1291`, not wrapper structs).
  // Field keywords are camel→kebab-cased to match the encoding. A sum carries a
  // permissive `Any` shape; precise schemas come from the standalone `Schematic`.
  private[stratiform] def reify(shape: Shape): Tels.Type = shape match
    case Shape.Str | Shape.Whole | Shape.Real | Shape.Bool | Shape.Empty =>
      Tels.Scalar(IArray.empty)

    case Shape.Any        => Tels.Struct(IArray.empty, IArray.empty)
    case Shape.OneOf(_)   => Tels.Struct(IArray.empty, IArray.empty)
    case Shape.Opt(inner) => reify(inner)
    case Shape.Arr(items) => reify(items)

    case Shape.Dict(key, value) =>
      val entry =
        Tels.Struct
          ( IArray
              ( Tels.Field(Polarity.Tight, Polarity.Implicit, t"key", reify(key), Unset),
                Tels.Field(Polarity.Tight, Polarity.Implicit, t"value", reify(value), Unset) ),
            IArray.empty )

      Tels.Struct
        ( IArray(Tels.Field(Polarity.Implicit, Polarity.Loose, t"entries", entry, Unset)),
          IArray.empty )

    case Shape.Obj(fields, required) =>
      val members = fields.map: (label, fieldShape) =>
        val repeatable = fieldShape match
          case Shape.Arr(_) => Polarity.Loose
          case _            => Polarity.Implicit

        val polarity = fieldShape match
          case Shape.Arr(_) | Shape.Opt(_) => Polarity.Loose

          case _ =>
            if required.contains(label) then Polarity.Tight else Polarity.Loose

        Tels.Field
          ( polarity, repeatable, Tel.camelToKebab(label.s), reify(fieldShape), Unset )

      Tels.Struct(IArray.from(members), IArray.empty)

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

  // A `List`/`Set` field is a repeatable field whose type is the element's schema
  // (TEL repeats the field rather than wrapping it), so the schema node is the
  // element schema and the field is marked `Loose` (0+) and repeatable.
  given list: [value: Schematic over Tels.Type] => List[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = List[value]
      type Transport = Tels.Type
      def schema(): Tels.Type = value.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose

  given set: [value: Schematic over Tels.Type] => Set[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Set[value]
      type Transport = Tels.Type
      def schema(): Tels.Type = value.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose

  given series: [value: Schematic over Tels.Type] => Series[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Series[value]
      type Transport = Tels.Type
      def schema(): Tels.Type = value.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose

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
  // `value`, registering any `select` definitions the type contributes. A top-level
  // sum's schema is a `Reference`, so its document root is a struct with a single
  // select member referencing the registered `SelectDefinition`.
  def tels[value](name: Text)(using schematic: value is TelSchematic over Tels.Type): Tels =
    val selects: IArray[Tels.SelectDefinition] = IArray.from(schematic.selectDefinitions)

    schematic.schema().absolve match
      case struct: Tels.Struct =>
        Tels(name, struct, IArray.empty, Unset, IArray.empty, IArray.empty, selects)

      case Tels.Reference(reference) =>
        val member = Tels.SelectRef(Polarity.Implicit, Polarity.Implicit, reference)
        val root   = Tels.Struct(IArray(member), IArray.empty)
        Tels(name, root, IArray.empty, Unset, IArray.empty, IArray.empty, selects)

object TelsDerivation extends Derivable[TelSchematic over Tels.Type]:
  inline def conjunction[derivation <: Product: ProductReflection]
  :   derivation is TelSchematic over Tels.Type =

    () =>
      val renames: Map[Text, Text] = relabelling[derivation, Tel]

      val members =
        contexts:
          [field] => schematic =>
            val keyword: Text = renames.getOrElse(label, Tel.camelToKebab(label.s))
            Tels.Field(schematic.polarity, schematic.repeatable, keyword, schematic.schema(), Unset)

        . to(List)

      Tels.Struct(IArray.from(members), IArray.empty)

  // The variants of a sum, as schema `Variant`s: each variant's keyword is its
  // kebab-cased name (the discriminator the codec writes) and its type is the
  // variant product's own derived struct. A value-less fold over the mirror's
  // element types, so the whole select is available without an instance.
  private transparent inline def variants[variants <: Tuple, labels <: Tuple]
  :   List[Tels.Variant] =

    inline erasedValue[variants] match
      case _: (variant *: moreVariants) =>
        inline erasedValue[labels] match
          case _: (label *: moreLabels) =>
            val keyword: Text = Tel.camelToKebab(constValue[label & String])
            val variantType: Tels.Type = infer[variant is TelSchematic over Tels.Type].schema()
            Tels.Variant(keyword, variantType) :: variants[moreVariants, moreLabels]

      case _ =>
        Nil

  // The schematic for a sum: `schema()` indirects to the named select; the select
  // itself is surfaced through `selectDefinitions` for registration. A plain `def`
  // (not the inline body) so the anonymous class is compiled once, not per call site.
  private def selectSchematic(select: Tels.SelectDefinition): TelSchematic over Tels.Type =
    new TelSchematic:
      type Transport = Tels.Type
      def schema(): Tels.Type = Tels.Reference(select.name)
      override def selectDefinitions: List[Tels.SelectDefinition] = List(select)

  // A sum derives to a named `SelectDefinition` (its variants) registered in the
  // namespace, with `schema()` returning a `Reference` to it — the indirected form
  // `Tel.Type.assign` and BinTEL resolve. Mirrors the codec's discriminator.
  inline def disjunction[derivation](using reflection: SumReflection[derivation])
  :   derivation is TelSchematic over Tels.Type =

    val name: Text = constValue[reflection.MirroredLabel].tt

    val selectVariants: List[Tels.Variant] =
      variants[reflection.MirroredElemTypes, reflection.MirroredElemLabels]

    val select = Tels.SelectDefinition(name, IArray.from(selectVariants), IArray.empty)
    selectSchematic(select).asInstanceOf[derivation is TelSchematic over Tels.Type]

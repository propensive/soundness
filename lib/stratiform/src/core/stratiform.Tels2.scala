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
package stratiform
import rudiments.unsafeMutable

import adversaria.*
import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import vacuous.*
import wisteria.*
import rudiments.*

import Tels.Polarity

// Constructors for fused `Encodable & Schematic` / `Decodable & Schematic`
// instances. The schema is taken from the codec itself (`Tel.Encodable` /
// `Tel.Decodable` *carry* a `Morphology`), never resolved independently, so the fused
// instance is coherent by construction — a gated or `… in Text`-branch codec
// always pairs with the schema for exactly what it reads/writes. The carried
// `Morphology` is reified into a `Tels.Type` here. Deliberately *methods*, not givens
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

object TelSchematic:
  case class Definitions
    ( records: List[Tels.RecordDefinition], selects: List[Tels.SelectDefinition] )

  // The `TelSchematic` refinements of a member's schematic, which may be a plain `Schematic`.
  def fieldTypeOf(schematic: Schematic over Tels.Type): Tels.Type = schematic match
    case schematic: TelSchematic => schematic.fieldType
    case other                   => other.schema()

  def definitionsOf(schematic: Schematic over Tels.Type, seen: scala.collection.immutable.Set[Text])
  :   Definitions =

    schematic match
      case schematic: TelSchematic => schematic.definitions(seen)
      case _                       => Definitions(Nil, Nil)

  def layersOf(schematic: Schematic over Tels.Type, seen: scala.collection.immutable.Set[Text])
  :   List[Tels.Layer] =

    schematic match
      case schematic: TelSchematic => schematic.layers(seen)
      case _                       => Nil

  // Layers of the same name, contributed by several fields or nested records, merged into one,
  // in order of first appearance.
  def merge(layers: List[Tels.Layer]): List[Tels.Layer] =
    val buffer = scala.collection.mutable.LinkedHashMap.empty[Text, Tels.Layer]

    layers.each: layer =>
      buffer.get(layer.name) match
        case scala.Some(existing) =>
          val members =
            Array.frozen(existing.overlay.members.readable ++ layer.overlay.members.readable)

          val validators =
            Array.frozen(existing.overlay.validators.readable ++ layer.overlay.validators.readable)

          val records = Array.frozen(existing.records.readable ++ layer.records.readable)
          val scalars = Array.frozen(existing.scalars.readable ++ layer.scalars.readable)
          val selects = Array.frozen(existing.selects.readable ++ layer.selects.readable)
          val overlay = Tels.Struct(members, validators)
          buffer(layer.name) = Tels.Layer(layer.name, overlay, records, scalars, selects)

        case scala.None =>
          buffer(layer.name) = layer

    proscenium.List.from(buffer.values)

  // The first definition of each name.
  def distinct[definition](definitions: List[definition], name: definition => Text)
  :   List[definition] =

    val seen = scala.collection.mutable.HashSet.empty[Text]
    definitions.filter: definition => seen.add(name(definition))

// TEL refinement of the shared `anticipation.Schematic`: it adds field-level
// `polarity` (so the product derivation can mark `Optional` fields `Loose`) and
// `repeatable` (so `List`/`Set` fields become repeatable fields — TEL records both
// optionality and repeatability on the field, not on the schema node). The schema
// representation (`Transport`) is a `Tels.Type`. `tels` still requires only the
// shared `Schematic over Tels.Type`, which these givens satisfy.
trait TelSchematic extends Schematic:
  type Transport = Tels.Type

  // Implicit, as a hand-written `field` is required by default: a derived schema then hashes as
  // the schema its author would write.
  def polarity: Tels.Polarity = Tels.Polarity.Implicit
  def repeatable: Tels.Polarity = Tels.Polarity.Implicit

  // The type a member of this type is declared with: the schema itself, or for a product, a
  // reference to its registered record — TELS names definitions, never nests structs.
  def fieldType: Tels.Type = schema()

  // Named `select` definitions a sum type contributes to the schema's namespace
  // (a select is referenced by name, never inlined). Empty for scalars and
  // products; a sum returns its own `SelectDefinition` here while `schema()`
  // returns a `Reference` to it.
  def selectDefinitions: List[Tels.SelectDefinition] = Nil

  // Every definition this type and the types it references contribute — its own record (for a
  // product) or select (for a sum), and those of its fields or variants — with `seen` cutting
  // off the recursion through a recursive type.
  def definitions(seen: scala.collection.immutable.Set[Text]): TelSchematic.Definitions =
    TelSchematic.Definitions(Nil, selectDefinitions)

  // The layers a product's `@layer`-annotated fields form, as a *nested* record contributes them:
  // each layer refining the record by the fields it groups; `seen` cuts off the recursion through
  // a recursive type, as for `definitions`.
  def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] = Nil

  // The same layers as the *root* document contributes them: its own fields in the overlay.
  def rootLayers: List[Tels.Layer] = layers(scala.collection.immutable.Set())

object Tels2:
  // Reifies a codec's format-neutral `Morphology` (carried by `Tel.Encodable` /
  // `Tel.Decodable`) into a concrete `Tels.Type`. TEL records optionality and
  // repeatability on the *field*, so an `Opt` field becomes a `Loose` member and an
  // `Arr` (list/set) field a `Loose`/repeatable member whose type is the element's
  // schema (collections are repeated fields, per `#1291`, not wrapper structs).
  // Field keywords are camel→kebab-cased to match the encoding. A sum carries a
  // permissive `Any` shape; precise schemas come from the standalone `Schematic`.
  private[stratiform] def reify(shape: Morphology): Tels.Type = shape match
    case Morphology.Str | Morphology.Whole | Morphology.Real | Morphology.Bool | Morphology.Empty =>
      Tels.Scalar(Array.empty)

    case Morphology.Any        => Tels.Struct(Array.empty, Array.empty)
    case Morphology.OneOf(_)   => Tels.Struct(Array.empty, Array.empty)
    case Morphology.Opt(inner) => reify(inner)
    case Morphology.Arr(items) => reify(items)

    case Morphology.Dict(key, value) =>
      val entry =
        Tels.Struct
          ( Array
              ( Tels.Field(Polarity.Tight, Polarity.Implicit, t"key", reify(key), Unset),
                Tels.Field(Polarity.Tight, Polarity.Implicit, t"value", reify(value), Unset) ),
            Array.empty )

      Tels.Struct
        ( Array(Tels.Field(Polarity.Implicit, Polarity.Loose, t"entries", entry, Unset)),
          Array.empty )

    case Morphology.Obj(fields, required) =>
      val members: List[Tels.Member] = fields.map: (label, fieldShape) =>
        val repeatable = fieldShape match
          case Morphology.Arr(_) => Polarity.Loose
          case _                 => Polarity.Implicit

        val polarity = fieldShape match
          case Morphology.Arr(_) | Morphology.Opt(_) => Polarity.Loose

          case _ =>
            if required.has(label) then Polarity.Tight else Polarity.Loose

        Tels.Field
          ( polarity, repeatable, Tel.camelToKebab(label.s), reify(fieldShape), Unset )

      Tels.Struct(members.to[Array], Array.empty)

// Schema derivation for TEL: scalars map to `Tels.Scalar`, products to a
// `Tels.Struct` of `Field`s, collections to a `Struct` with a repeatable `item`
// field, and `Map` to repeatable `entries` of `key`/`value`. Mixed into `object
// Tels` so the givens sit in the `Transport` companion's implicit scope.
trait Tels2:
  // A type declaring a §21.7 encoding via the `Tel.Encoded` marker is a
  // scalar whose derived schema carries that encoding, so the AST and
  // staged BinTEL paths agree on its wire form by construction.
  given encoded: [value, name <: Label]
  =>  (marker: value is Tel.Encoded[name])
  =>  (name0: ValueOf[name])
  =>  value is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty, Text(name0.value))

  given text: Text is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given string: String is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given int: Int is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given long: Long is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given double: Double is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)
  given boolean: Boolean is TelSchematic over Tels.Type = () => Tels.Scalar(Array.empty)

  given optional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( schematic: inner is Schematic over Tels.Type )
  =>  value is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = value
      def schema(): Tels.Type = schematic.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def fieldType: Tels.Type = TelSchematic.fieldTypeOf(schematic)

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        TelSchematic.layersOf(schematic, seen)

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        TelSchematic.definitionsOf(schematic, seen)

  // A `List`/`Set` field is a repeatable field whose type is the element's schema
  // (TEL repeats the field rather than wrapping it), so the schema node is the
  // element schema and the field is marked `Loose` (0+) and repeatable.
  given list: [value: Schematic over Tels.Type as schematic]
  =>  List[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = List[value]
      def schema(): Tels.Type = schematic.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose
      override def fieldType: Tels.Type = TelSchematic.fieldTypeOf(schematic)

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        TelSchematic.layersOf(schematic, seen)

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        TelSchematic.definitionsOf(schematic, seen)

  given set: [value: Schematic over Tels.Type as schematic]
  =>  Set[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Set[value]
      def schema(): Tels.Type = schematic.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose
      override def fieldType: Tels.Type = TelSchematic.fieldTypeOf(schematic)

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        TelSchematic.layersOf(schematic, seen)

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        TelSchematic.definitionsOf(schematic, seen)

  given sequence: [value: Schematic over Tels.Type as schematic]
  =>  Sequence[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Sequence[value]
      def schema(): Tels.Type = schematic.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose
      override def fieldType: Tels.Type = TelSchematic.fieldTypeOf(schematic)

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        TelSchematic.layersOf(schematic, seen)

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        TelSchematic.definitionsOf(schematic, seen)

  // A map's `entries` struct has no record of its own to reference, so it stays inline: a
  // schema document cannot name it, and `Tels.Renderer` reports it as such.
  given map: [key: Schematic over Tels.Type as keys, value: Schematic over Tels.Type as values]
  =>  Map[key, value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Map[key, value]

      def schema(): Tels.Type =
        val keyType = TelSchematic.fieldTypeOf(keys)
        val valueType = TelSchematic.fieldTypeOf(values)

        val entry =
          Tels.Struct
            ( Array
                ( Tels.Field(Polarity.Implicit, Polarity.Implicit, t"key", keyType, Unset),
                  Tels.Field(Polarity.Implicit, Polarity.Implicit, t"value", valueType, Unset) ),
              Array.empty )

        Tels.Struct
          ( Array(Tels.Field(Polarity.Implicit, Polarity.Loose, t"entries", entry, Unset)),
            Array.empty )

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        val left = TelSchematic.definitionsOf(keys, seen)
        val right = TelSchematic.definitionsOf(values, seen)

        TelSchematic.Definitions
          ( left.records.reverse.unwind(right.records),
            left.selects.reverse.unwind(right.selects) )

  inline given schematic: [value: Reflection] => value is TelSchematic over Tels.Type =
    TelsDerivation.derived

  // Assembles a self-contained `Tels` document whose root struct is the schema of
  // `value`, registering any `select` definitions the type contributes. A top-level
  // sum's schema is a `Reference`, so its document root is a struct with a single
  // select member referencing the registered `SelectDefinition`.
  def tels[value](name: Text)(using schematic: value is TelSchematic over Tels.Type): Tels =
    val definitions = schematic.definitions(scala.collection.immutable.Set())
    val records0 = TelSchematic.distinct(definitions.records, _.name)
    val selects = TelSchematic.distinct(definitions.selects, _.name)
    val layers = schematic.rootLayers

    // The root type's own record is registered only when some member references it — a
    // recursive type — since the document struct already declares its members.
    val rootRecord: Optional[Text] = schematic.fieldType match
      case Tels.Reference(reference) => reference
      case _                         => Unset

    def referenced(struct: Tels.Struct): List[Text] =
      proscenium.List.from(struct.members.readable.toList).bind:
        case Tels.Field(_, _, _, Tels.Reference(reference), _, _, _) => List(reference)
        case Tels.Field(_, _, _, nested: Tels.Struct, _, _, _)       => referenced(nested)
        case _                                                        => Nil

    val fromRecords: List[Text] =
      records0.bind: record => referenced(Tels.Struct(record.members, record.validators))

    val fromSelects: List[Text] = selects.bind: select =>
      proscenium.List.from(select.variants.readable.toList).bind: variant =>
        variant.variantType match
          case Tels.Reference(reference) => List(reference)
          case _                         => Nil

    val fromLayers: List[Text] = layers.bind: layer => referenced(layer.overlay)
    val references: List[Text] = fromRecords.reverse.unwind(fromSelects.reverse.unwind(fromLayers))

    val records = records0.filter: record =>
      rootRecord != record.name || references.has(record.name)

    schematic.schema().absolve match
      case struct: Tels.Struct =>
        Tels
          ( name, struct, layers.to[Array], Unset, records.to[Array], Array.empty,
            selects.to[Array] )

      case Tels.Reference(reference) =>
        val member = Tels.SelectRef(Polarity.Implicit, Polarity.Implicit, reference)
        val root   = Tels.Struct(Array(member), Array.empty)
        Tels(name, root, layers.to[Array], Unset, records.to[Array], Array.empty, selects.to[Array])

object TelsDerivation extends Derivable[TelSchematic over Tels.Type]:
  // One derived field: its declaration, the layer it is annotated with, and the definitions and
  // layers its own type contributes.
  private case class Member(field: Tels.Field, layer: Optional[Text], schematic: TelSchematic):

    // Plain members rather than `Optional`'s extensions, which applied inside a traversal's
    // lambda trip the compiler's `wildApprox` assertion (scala/scala3#24824).
    def unlayered: Boolean = layer match
      case Unset => true
      case _     => false

    def layerName: Text = layer match
      case name: Text => name
      case _          => t""

  // The members a layer groups, by the layer's name.
  private case class Group(name: Text, members: List[Member])

  private def structOf(members: List[Member]): Tels.Struct =
    val fields: List[Tels.Member] = members.map: member => member.field
    Tels.Struct(fields.to[Array], Array.empty)

  // A group's layer as a nested record contributes it: refining the record by the grouped fields.
  private def recordLayer(record: Text, group: Group): Tels.Layer =
    val definition = Tels.RecordDefinition(record, structOf(group.members).members, Array.empty)
    val definitions: Array[Tels.RecordDefinition]^{} = Array(definition)
    val overlay = Tels.Struct(Array.empty, Array.empty)
    Tels.Layer(group.name, overlay, definitions, Array.empty, Array.empty)

  // A group's layer as the root document contributes it: the grouped fields in the overlay.
  private def overlayLayer(group: Group): Tels.Layer =
    Tels.Layer(group.name, structOf(group.members), Array.empty, Array.empty, Array.empty)

  // The schematic of a product, whose `schema()` is the struct of its unlayered fields, whose
  // `fieldType` is a reference to its record, and whose `@layer`-annotated fields form layers.
  // A plain `def` (not the inline body) so the anonymous class is compiled once.
  private def productSchematic(name: Text, members0: () -> List[Member])
  :   TelSchematic over Tels.Type =

    new TelSchematic:
      // Deferred until first use: a recursive type's fields summon its own schematic, which
      // must not build its fields to be constructed.
      private lazy val members: List[Member] = members0()
      private lazy val base: List[Member] = members.filter(_.unlayered)
      private lazy val layered: List[Member] = members.filter(!_.unlayered)

      def schema(): Tels.Type = structOf(base)
      override def fieldType: Tels.Type = Tels.Reference(name)

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        if seen.contains(name) then TelSchematic.Definitions(Nil, Nil)
        else
          val record = Tels.RecordDefinition(name, structOf(base).members, Array.empty)
          val nested = members.map(_.schematic.definitions(seen + name))

          TelSchematic.Definitions
            ( record :: nested.bind(_.records),
              nested.bind(_.selects) )

      // Grouped by layer name in order of first appearance.
      private def grouped: List[Group] =
        val named: List[Text] = layered.map(_.layerName)
        val names: List[Text] = TelSchematic.distinct[Text](named, identity)
        names.map: layerName => Group(layerName, layered.filter(_.layerName == layerName))

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        if seen.contains(name) then Nil
        else
          val own: List[Tels.Layer] = grouped.map: group => recordLayer(name, group)
          val nested = members.bind(_.schematic.layers(seen + name))
          TelSchematic.merge(own.reverse.unwind(nested))

      override def rootLayers: List[Tels.Layer] =
        val own: List[Tels.Layer] = grouped.map: group => overlayLayer(group)
        val nested = members.bind(_.schematic.layers(scala.collection.immutable.Set(name)))
        TelSchematic.merge(own.reverse.unwind(nested))

  // The layer a field's `@layer` annotation names, if it carries one. A plain method, so the
  // lookups happen outside the polymorphic lambda `contexts` types the fields under.
  private def layerOf(grouping: Map[Text, Set[layer]], label: Text): Optional[Text] =
    grouping(label).let(_.occupied.let(_.head.name))

  // One field's `Member`, built outside the polymorphic lambda `contexts` types the fields
  // under, where the closures it holds would be typed against live type variables.
  private def member
    ( schematic: TelSchematic, keyword: Text, layerName: Optional[Text] )
  :   Member =

    val field =
      Tels.Field(schematic.polarity, schematic.repeatable, keyword, schematic.fieldType, Unset)

    Member(field, layerName, schematic)

  inline def conjunction[derivation <: Product: ProductReflection]
  :   derivation is TelSchematic over Tels.Type =

    val name: Text = wisteria.internal.sumName[derivation]
    val renames: Map[Text, Text] = relabelling[derivation, Tel]
    val grouping: Map[Text, Set[layer]] = fieldAnnotations[derivation, layer]

    def members: List[Member] =
      val array =
        contexts[derivation]():
          [field] => schematic =>
            val keyword: Text = renames(label).or(Tel.camelToKebab(label.s))
            TelsDerivation.member(schematic, keyword, TelsDerivation.layerOf(grouping, label))

      proscenium.List.from(array.readable.toList)

    productSchematic(name, () => members).asInstanceOf[derivation is TelSchematic over Tels.Type]

  // The schematic for a sum: `schema()` indirects to the named select; the select
  // itself is surfaced through `selectDefinitions` for registration, and the
  // definitions of its variants' types through `definitions`. A plain `def`
  // (not the inline body) so the anonymous class is compiled once, not per call site.
  private def selectSchematic(select: Tels.SelectDefinition, nested: List[TelSchematic])
  :   TelSchematic over Tels.Type =

    new TelSchematic:
      def schema(): Tels.Type = Tels.Reference(select.name)
      override def selectDefinitions: List[Tels.SelectDefinition] = List(select)

      override def definitions(seen: scala.collection.immutable.Set[Text]) =
        if seen.contains(select.name) then TelSchematic.Definitions(Nil, Nil)
        else
          val below = nested.map(_.definitions(seen + select.name))
          TelSchematic.Definitions(below.bind(_.records), select :: below.bind(_.selects))

  // A sum derives to a named `SelectDefinition` (its variants) registered in the
  // namespace, with `schema()` returning a `Reference` to it — the indirected form
  // `Tel.Type.assign` and BinTEL resolve. Mirrors the codec's discriminator. A product
  // variant's type is a reference to its record, registered through `definitions`.
  // One variant and the definitions its type contributes, built outside the polymorphic lambda
  // `choices` types the variants under.
  private def variant(schematic: TelSchematic, keyword: Text): (Tels.Variant, TelSchematic) =
    (Tels.Variant(keyword, schematic.fieldType), schematic)

  inline def disjunction[derivation: SumReflection]
  :   derivation is TelSchematic over Tels.Type =

    val name: Text = wisteria.internal.sumName[derivation]

    val variants =
      val array =
        choices:
          [variant <: derivation] => schematic =>
            TelsDerivation.variant(schematic, Tel.camelToKebab(label.s))

      proscenium.List.from(array.readable.toList)

    val select = Tels.SelectDefinition(name, variants.map(_(0)).to[Array], Array.empty)
    selectSchematic(select, variants.map(_(1)))
    . asInstanceOf[derivation is TelSchematic over Tels.Type]

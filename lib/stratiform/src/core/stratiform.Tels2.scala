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
import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*
import wisteria.*
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

  // The identity of a derived definition — a product's record or a sum's select — as its type: the
  // type's simple name, which the definition takes when no other type in the schema shares it, and
  // the names of the packages, objects and classes enclosing the type, innermost first. Keying
  // on the type rather than the name is what tells `Phrase.Code` from `Element.Code` while still
  // recognising one type reached by several paths.
  case class Identity(name: Text, enclosing: List[Text]):

    // The name qualified by the innermost `depth - 1` enclosing names, each as a TypeName
    // (§20.7): `PhraseCode` for `Phrase.Code` at depth 2.
    def qualified(depth: Int): Text =
      def prefix(enclosing: List[Text], depth: Int): Text = enclosing match
        case segment :: rest if depth > 1 => prefix(rest, depth - 1) + Identity.typeName(segment)
        case _                            => t""

      prefix(enclosing, depth) + name

  object Identity:
    // A segment as part of a TypeName: its ASCII letters and digits, capitalised, so that a package
    // qualifies as `Pkg`. Anything else (an anonymous scope) contributes nothing.
    private def typeName(segment: Text): Text =
      segment.s.filter { char => char < 128 && char.isLetterOrDigit }.capitalize.tt

  // How the definitions of a schema are named. A type whose simple name no other type in the
  // schema shares keeps it, so a schema without clashes is unchanged by naming; the members of a
  // group of distinct types sharing one name are each qualified by their enclosing types, to the
  // shallowest depth that tells them apart, so a name depends only on which types the schema
  // reaches, never on the order it reaches them. Names which coincide even so are `ambiguous`,
  // and make the schema invalid (E210).
  class Naming(names: Map[Identity, Text], val ambiguous: List[Text]):
    def apply(identity: Identity): Text = names(identity).or(identity.name)

  object Naming:
    // Every type by its simple name.
    val simple: Naming = new Naming(Map(), Nil)

    def apply(identities: List[Identity]): Naming =
      val types: List[Identity] = identities.distinct

      // Deepening the qualification until the names differ, or until no name changes any more.
      def resolve(group: List[Identity], depth: Int): List[(Identity, Text)] =
        val names: List[Text] = group.map(_.qualified(depth))
        val deeper: List[Text] = group.map(_.qualified(depth + 1))

        if names.distinct == names || deeper == names then group.zip(names)
        else resolve(group, depth + 1)

      val clashing: List[List[Identity]] = types.group(_.name).values.filter:
        case _ :: _ :: _ => true
        case _           => false

      val renamed: Map[Identity, Text] = clashing.bind(resolve(_, 2)).to[Map]
      val taken: List[Text] = types.map { identity => renamed(identity).or(identity.name) }
      val ambiguous: List[Text] = taken.filter { name => taken.count(_ == name) > 1 }.distinct

      new Naming(renamed, ambiguous)

  // The `TelSchematic` refinements of a member's schematic, which may be a plain `Schematic`.
  def schemaOf(schematic: Schematic over Tels.Type, naming: Naming): Tels.Type = schematic match
    case schematic: TelSchematic => schematic.schema(naming)
    case other                   => other.schema()

  def fieldTypeOf(schematic: Schematic over Tels.Type, naming: Naming): Tels.Type =
    schematic match
      case schematic: TelSchematic => schematic.fieldType(naming)
      case other                   => other.schema()

  def identitiesOf
    ( schematic: Schematic over Tels.Type, seen: scala.collection.immutable.Set[Identity] )
  :   List[Identity] =

    schematic match
      case schematic: TelSchematic => schematic.identities(seen)
      case _                       => Nil

  def definitionsOf
    ( schematic: Schematic over Tels.Type,
      seen:      scala.collection.immutable.Set[Identity],
      naming:    Naming )
  :   Definitions =

    schematic match
      case schematic: TelSchematic => schematic.definitions(seen, naming)
      case _                       => Definitions(Nil, Nil)

  def layersOf
    ( schematic: Schematic over Tels.Type,
      seen:      scala.collection.immutable.Set[Identity],
      naming:    Naming )
  :   List[Tels.Layer] =

    schematic match
      case schematic: TelSchematic => schematic.layers(seen, naming)
      case _                       => Nil

  // Layers of the same name, contributed by several fields or nested records, merged into one,
  // in order of first appearance.
  def merge(layers: List[Tels.Layer]): List[Tels.Layer] =
    val buffer = scala.collection.mutable.LinkedHashMap.empty[Text, Tels.Layer]

    layers.each: layer =>
      buffer.get(layer.name) match
        case scala.Some(existing) =>
          val members =
            existing.overlay.members + layer.overlay.members

          val validators =
            existing.overlay.validators + layer.overlay.validators

          val records = existing.records + layer.records
          val scalars = existing.scalars + layer.scalars
          val selects = combine(existing.selects.stdlib.toList ++ layer.selects.stdlib.toList)
          val overlay = Tels.Struct(members, validators)
          buffer(layer.name) = Tels.Layer(layer.name, overlay, records, scalars, selects)

        case scala.None =>
          buffer(layer.name) = layer

    proscenium.List.from(buffer.values)

  // A layer's `select` definitions with those of the same name combined: a sum reached through
  // several members contributes its layer's excludes once each, since the second `exclude` of a
  // keyword would name a variant the first had already removed.
  private def combine(selects: scala.List[Tels.SelectDefinition])
  :   Array[Tels.SelectDefinition]^{} =

    val names = selects.map(_.name).distinct

    val combined = names.map: name =>
      val same = selects.filter(_.name == name)

      Tels.SelectDefinition
        ( name,
          Array.from(same.flatMap(_.variants.stdlib.toList).distinctBy(_.keyword)),
          Array.from(same.flatMap(_.validators.stdlib.toList).distinct),
          same.map(_.description).find(_.present).optional,
          Array.from(same.flatMap(_.excludes.stdlib.toList).distinct) )

    Array.from(combined)

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

  // The schema with the definitions it references named by `naming`; `schema()` names each type
  // by its simple name, as a schema without clashes does.
  def schema(naming: TelSchematic.Naming): Tels.Type = schema()

  // The type a member of this type is declared with: the schema itself, or for a product, a
  // reference to its registered record — TELS names definitions, never nests structs.
  def fieldType(naming: TelSchematic.Naming): Tels.Type = schema(naming)

  // Named `select` definitions a sum type contributes to the schema's namespace
  // (a select is referenced by name, never inlined). Empty for scalars and
  // products; a sum returns its own `SelectDefinition` here while `schema()`
  // returns a `Reference` to it.
  def selectDefinitions(naming: TelSchematic.Naming): List[Tels.SelectDefinition] = Nil

  // The identities of every definition this type and the types it references contribute, with
  // `seen` cutting off the recursion through a recursive type. `Tels.tels` names them all
  // together, so that two types sharing a simple name are told apart before any is built.
  def identities(seen: scala.collection.immutable.Set[TelSchematic.Identity])
  :   List[TelSchematic.Identity] =

    Nil

  // Every definition this type and the types it references contribute — its own record (for a
  // product) or select (for a sum), and those of its fields or variants — named by `naming`, with
  // `seen` cutting off the recursion through a recursive type.
  def definitions
    ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
  :   TelSchematic.Definitions =

    TelSchematic.Definitions(Nil, selectDefinitions(naming))

  // The layers a product's `@layer`-annotated fields form, as a *nested* record contributes them:
  // each layer refining the record by the fields it groups; `seen` cuts off the recursion through
  // a recursive type, as for `definitions`.
  def layers
    ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
  :   List[Tels.Layer] =

    Nil

  // The same layers as the *root* document contributes them: its own fields in the overlay.
  def rootLayers(naming: TelSchematic.Naming): List[Tels.Layer] =
    layers(scala.collection.immutable.Set(), naming)

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
  =>  ( marker: value is Tel.Encoded[name] )
  =>  ( name0: ValueOf[name] )
  =>  value is TelSchematic over Tels.Type =
    () => Tels.Scalar(Array.empty, name0.value.tt)

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
      override def schema(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.schemaOf(schematic, naming)

      override def fieldType(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.fieldTypeOf(schematic, naming)

      override def identities(seen: scala.collection.immutable.Set[TelSchematic.Identity]) =
        TelSchematic.identitiesOf(schematic, seen)

      override def layers
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   List[Tels.Layer] =

        TelSchematic.layersOf(schematic, seen, naming)

      override def definitions
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   TelSchematic.Definitions =

        TelSchematic.definitionsOf(schematic, seen, naming)

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
      override def schema(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.schemaOf(schematic, naming)

      override def fieldType(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.fieldTypeOf(schematic, naming)

      override def identities(seen: scala.collection.immutable.Set[TelSchematic.Identity]) =
        TelSchematic.identitiesOf(schematic, seen)

      override def layers
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   List[Tels.Layer] =

        TelSchematic.layersOf(schematic, seen, naming)

      override def definitions
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   TelSchematic.Definitions =

        TelSchematic.definitionsOf(schematic, seen, naming)

  given set: [value: Schematic over Tels.Type as schematic]
  =>  Set[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Set[value]
      def schema(): Tels.Type = schematic.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose
      override def schema(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.schemaOf(schematic, naming)

      override def fieldType(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.fieldTypeOf(schematic, naming)

      override def identities(seen: scala.collection.immutable.Set[TelSchematic.Identity]) =
        TelSchematic.identitiesOf(schematic, seen)

      override def layers
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   List[Tels.Layer] =

        TelSchematic.layersOf(schematic, seen, naming)

      override def definitions
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   TelSchematic.Definitions =

        TelSchematic.definitionsOf(schematic, seen, naming)

  given sequence: [value: Schematic over Tels.Type as schematic]
  =>  Sequence[value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Sequence[value]
      def schema(): Tels.Type = schematic.schema()
      override def polarity: Tels.Polarity = Polarity.Loose
      override def repeatable: Tels.Polarity = Polarity.Loose
      override def schema(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.schemaOf(schematic, naming)

      override def fieldType(naming: TelSchematic.Naming): Tels.Type =
        TelSchematic.fieldTypeOf(schematic, naming)

      override def identities(seen: scala.collection.immutable.Set[TelSchematic.Identity]) =
        TelSchematic.identitiesOf(schematic, seen)

      override def layers
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   List[Tels.Layer] =

        TelSchematic.layersOf(schematic, seen, naming)

      override def definitions
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   TelSchematic.Definitions =

        TelSchematic.definitionsOf(schematic, seen, naming)

  // A map's `entries` struct has no record of its own to reference, so it stays inline: a
  // schema document cannot name it, and `Tels.Renderer` reports it as such.
  given map: [key: Schematic over Tels.Type as keys, value: Schematic over Tels.Type as values]
  =>  Map[key, value] is TelSchematic over Tels.Type =
    new TelSchematic:
      type Self = Map[key, value]

      def schema(): Tels.Type = schema(TelSchematic.Naming.simple)

      override def schema(naming: TelSchematic.Naming): Tels.Type =
        val keyType = TelSchematic.fieldTypeOf(keys, naming)
        val valueType = TelSchematic.fieldTypeOf(values, naming)

        val entry =
          Tels.Struct
            ( Array
                ( Tels.Field(Polarity.Implicit, Polarity.Implicit, t"key", keyType, Unset),
                  Tels.Field(Polarity.Implicit, Polarity.Implicit, t"value", valueType, Unset) ),
              Array.empty )

        Tels.Struct
          ( Array(Tels.Field(Polarity.Implicit, Polarity.Loose, t"entries", entry, Unset)),
            Array.empty )

      override def identities(seen: scala.collection.immutable.Set[TelSchematic.Identity]) =
        val left = TelSchematic.identitiesOf(keys, seen)
        left.reverse.unwind(TelSchematic.identitiesOf(values, seen))

      override def definitions
        ( seen: scala.collection.immutable.Set[TelSchematic.Identity], naming: TelSchematic.Naming )
      :   TelSchematic.Definitions =

        val left = TelSchematic.definitionsOf(keys, seen, naming)
        val right = TelSchematic.definitionsOf(values, seen, naming)

        TelSchematic.Definitions
          ( left.records.reverse.unwind(right.records),
            left.selects.reverse.unwind(right.selects) )

  inline given schematic: [value: Reflection] => value is TelSchematic over Tels.Type =
    TelsDerivation.derived

  // Assembles a self-contained `Tels` document whose root struct is the schema of
  // `value`, registering any `select` definitions the type contributes. A top-level
  // sum's schema is a `Reference`, so its document root is a struct with a single
  // select member referencing the registered `SelectDefinition`.
  //
  // Definitions are named by type: a type whose simple name is its own keeps it, and the types
  // of a group sharing one are qualified by their enclosing types (`PhraseCode`, `ElementCode`).
  // Two types the qualification cannot tell apart would be one definition, so the schema is
  // rejected as E210 here, where the types are known, rather than when a value fails to write
  // under it; and the assembled schema is checked as a hand-written one would be.
  def tels[value](name: Text)(using schematic: value is TelSchematic over Tels.Type)
  :   Tels raises Tel.Error =

    val naming = TelSchematic.Naming(schematic.identities(scala.collection.immutable.Set()))
    if !naming.ambiguous.nil then abort(Tel.Error(Tel.Error.Reason.DuplicateDefinition))

    val definitions = schematic.definitions(scala.collection.immutable.Set(), naming)

    // A type reached by several paths contributes its definition once per path.
    val records0 = TelSchematic.distinct(definitions.records, _.name)
    val selects = TelSchematic.distinct(definitions.selects, _.name)
    val layers = schematic.rootLayers(naming)

    // The root type's own record is registered only when some member references it — a
    // recursive type — since the document struct already declares its members.
    val rootRecord: Optional[Text] = schematic.fieldType(naming) match
      case Tels.Reference(reference) => reference
      case _                         => Unset

    def referenced(struct: Tels.Struct): List[Text] =
      struct.members.to[proscenium.List].bind:
        case Tels.Field(_, _, _, Tels.Reference(reference), _, _, _)  => List(reference)
        case Tels.Field(_, _, _, nested: Tels.Struct, _, _, _)        => referenced(nested)
        case _                                                        => Nil

    val fromRecords: List[Text] =
      records0.bind: record => referenced(Tels.Struct(record.members, record.validators))

    val fromSelects: List[Text] = selects.bind: select =>
      select.variants.to[proscenium.List].bind: variant =>
        variant.variantType match
          case Tels.Reference(reference) => List(reference)
          case _                         => Nil

    val fromLayers: List[Text] = layers.bind: layer => referenced(layer.overlay)
    val references: List[Text] = fromRecords.reverse.unwind(fromSelects.reverse.unwind(fromLayers))

    val records = records0.filter: record =>
      rootRecord != record.name || references.has(record.name)

    val schema = schematic.schema(naming).absolve match
      case struct: Tels.Struct =>
        Tels
          ( name, struct, layers.to[Array], Unset, records.to[Array], Array.empty,
            selects.to[Array] )

      case Tels.Reference(reference) =>
        val member = Tels.SelectRef(Polarity.Implicit, Polarity.Implicit, reference)
        val root   = Tels.Struct(Array(member), Array.empty)
        Tels(name, root, layers.to[Array], Unset, records.to[Array], Array.empty, selects.to[Array])

    Tels.Validation.checkBase(schema)
    schema

object TelsDerivation extends Derivable[TelSchematic over Tels.Type]:
  import TelSchematic.{Identity, Naming}

  // One derived field: its keyword, the layer it is annotated with, and the schematic of its type,
  // which gives the field's declaration and the definitions and layers the type contributes.
  private case class Member(keyword: Text, layer: Optional[Text], schematic: TelSchematic):

    // Plain members rather than `Optional`'s extensions, which applied inside a traversal's
    // lambda trip the compiler's `wildApprox` assertion (scala/scala3#24824).
    def unlayered: Boolean = layer match
      case Unset => true
      case _     => false

    def layerName: Text = layer match
      case name: Text => name
      case _          => t""

    def field(naming: Naming): Tels.Field =
      Tels.Field
        ( schematic.polarity, schematic.repeatable, keyword, schematic.fieldType(naming), Unset )

  // The members a layer groups, by the layer's name.
  private case class Group(name: Text, members: List[Member])

  private def structOf(members: List[Member], naming: Naming): Tels.Struct =
    val fields: List[Tels.Member] = members.map: member => member.field(naming)
    Tels.Struct(fields.to[Array], Array.empty)

  // A group's layer as a nested record contributes it: refining the record by the grouped fields.
  private def recordLayer(record: Text, group: Group, naming: Naming): Tels.Layer =
    val members = structOf(group.members, naming).members
    val definition = Tels.RecordDefinition(record, members, Array.empty)
    val definitions: Array[Tels.RecordDefinition]^{} = Array(definition)
    val overlay = Tels.Struct(Array.empty, Array.empty)
    Tels.Layer(group.name, overlay, definitions, Array.empty, Array.empty)

  // A group's layer as the root document contributes it: the grouped fields in the overlay.
  private def overlayLayer(group: Group, naming: Naming): Tels.Layer =
    Tels.Layer(group.name, structOf(group.members, naming), Array.empty, Array.empty, Array.empty)

  // The schematic of a product, whose `schema()` is the struct of its unlayered fields, whose
  // `fieldType` is a reference to its record, and whose `@assign`-annotated fields form layers.
  // A plain `def` (not the inline body) so the anonymous class is compiled once.
  private def productSchematic(identity: Identity, members0: () -> List[Member])
  :   TelSchematic over Tels.Type =

    new TelSchematic:
      // Deferred until first use: a recursive type's fields summon its own schematic, which
      // must not build its fields to be constructed.
      private lazy val members: List[Member] = members0()
      private lazy val base: List[Member] = members.filter(_.unlayered)
      private lazy val layered: List[Member] = members.filter(!_.unlayered)

      def schema(): Tels.Type = schema(Naming.simple)
      override def schema(naming: Naming): Tels.Type = structOf(base, naming)
      override def fieldType(naming: Naming): Tels.Type = Tels.Reference(naming(identity))

      override def identities(seen: scala.collection.immutable.Set[Identity]): List[Identity] =
        if seen.contains(identity) then Nil
        else identity :: members.bind(_.schematic.identities(seen + identity))

      override def definitions(seen: scala.collection.immutable.Set[Identity], naming: Naming)
      :   TelSchematic.Definitions =

        if seen.contains(identity) then TelSchematic.Definitions(Nil, Nil)
        else
          val members1 = structOf(base, naming).members
          val record = Tels.RecordDefinition(naming(identity), members1, Array.empty)
          val nested = members.map(_.schematic.definitions(seen + identity, naming))

          TelSchematic.Definitions
            ( record :: nested.bind(_.records),
              nested.bind(_.selects) )

      // Grouped by layer name in order of first appearance.
      private def grouped: List[Group] =
        val named: List[Text] = layered.map(_.layerName)
        val names: List[Text] = TelSchematic.distinct[Text](named, name => name)
        names.map: layerName => Group(layerName, layered.filter(_.layerName == layerName))

      override def layers(seen: scala.collection.immutable.Set[Identity], naming: Naming)
      :   List[Tels.Layer] =

        if seen.contains(identity) then Nil
        else
          val record = naming(identity)
          val own: List[Tels.Layer] = grouped.map: group => recordLayer(record, group, naming)
          val nested = members.bind(_.schematic.layers(seen + identity, naming))
          TelSchematic.merge(own.reverse.unwind(nested))

      override def rootLayers(naming: Naming): List[Tels.Layer] =
        val own: List[Tels.Layer] = grouped.map: group => overlayLayer(group, naming)
        val seen = scala.collection.immutable.Set(identity)
        val nested = members.bind(_.schematic.layers(seen, naming))
        TelSchematic.merge(own.reverse.unwind(nested))

  // The layer a field's `@assign` annotation names, if it carries one. A plain method, so the
  // lookups happen outside the polymorphic lambda `contexts` types the fields under.
  private def layerOf(assignments: scala.List[(Text, Text)], label: Text): Optional[Text] =
    assignments.find(_(0) == label) match
      case scala.Some((_, layer)) => layer
      case scala.None             => Unset

  // The layers named by the fields' `@assign` annotations, and by the cases' `@retract`
  // annotations, read and checked at compile time.
  private[stratiform] inline def assignments[product]: scala.List[(Text, Text)] =
    ${stratiform.internal.assignments[product]}

  private[stratiform] inline def retractions[sum]: scala.List[(Text, Text)] =
    ${stratiform.internal.retractions[sum]}

  // The identity of the type a definition derives from, read at compile time.
  private[stratiform] inline def identityOf[derivation]: Identity =
    ${stratiform.internal.identityOf[derivation]}

  // One field's `Member`, built outside the polymorphic lambda `contexts` types the fields
  // under, where the closures it holds would be typed against live type variables.
  private def member
    ( schematic: TelSchematic, keyword: Text, layerName: Optional[Text] )
  :   Member =

    Member(keyword, layerName, schematic)

  inline def conjunction[derivation <: Product: ProductReflection]
  :   derivation is TelSchematic over Tels.Type =

    val identity: Identity = TelsDerivation.identityOf[derivation]
    val renames: Map[Text, Text] = relabelling[derivation, Tel]
    val grouping: scala.List[(Text, Text)] = TelsDerivation.assignments[derivation]

    def members: List[Member] =
      val array =
        contexts[derivation]():
          [field] => schematic =>
            val keyword: Text = renames(label).or(Tel.camelToKebab(label.s))
            TelsDerivation.member(schematic, keyword, TelsDerivation.layerOf(grouping, label))

      array.to[proscenium.List]

    productSchematic(identity, () => members)
    . asInstanceOf[derivation is TelSchematic over Tels.Type]

  // The schematic for a sum: `schema()` indirects to the named select; the select
  // itself is surfaced through `selectDefinitions` for registration, and the
  // definitions of its variants' types through `definitions`. A plain `def`
  // (not the inline body) so the anonymous class is compiled once, not per call site.
  private def selectSchematic
    ( identity: Identity, variants: List[(Text, TelSchematic)], retractions: scala.List[(Text, Text)] )
  :   TelSchematic over Tels.Type =

    new TelSchematic:
      private def select(naming: Naming): Tels.SelectDefinition =
        val declared: List[Tels.Variant] = variants.map: variant =>
          Tels.Variant(variant(0), variant(1).fieldType(naming))

        Tels.SelectDefinition(naming(identity), declared.to[Array], Array.empty)

      def schema(): Tels.Type = schema(Naming.simple)
      override def schema(naming: Naming): Tels.Type = Tels.Reference(naming(identity))

      override def selectDefinitions(naming: Naming): List[Tels.SelectDefinition] =
        List(select(naming))

      override def identities(seen: scala.collection.immutable.Set[Identity]): List[Identity] =
        if seen.contains(identity) then Nil
        else identity :: variants.bind(_(1).identities(seen + identity))

      // The layers that retract this sum's cases, and those its variants' types contribute.
      override def layers(seen: scala.collection.immutable.Set[Identity], naming: Naming)
      :   List[Tels.Layer] =

        if seen.contains(identity) then Nil
        else
          val own = retractionLayers(naming(identity), retractions)
          val below = variants.bind(_(1).layers(seen + identity, naming))
          TelSchematic.merge(own.reverse.unwind(below))

      override def definitions(seen: scala.collection.immutable.Set[Identity], naming: Naming)
      :   TelSchematic.Definitions =

        if seen.contains(identity) then TelSchematic.Definitions(Nil, Nil)
        else
          val below = variants.map(_(1).definitions(seen + identity, naming))
          TelSchematic.Definitions(below.bind(_.records), select(naming) :: below.bind(_.selects))

  // A sum derives to a named `SelectDefinition` (its variants) registered in the
  // namespace, with `schema()` returning a `Reference` to it — the indirected form
  // `Tel.Type.assign` and BinTEL resolve. Mirrors the codec's discriminator. A product
  // variant's type is a reference to its record, registered through `definitions`.
  // One variant's keyword and the schematic of its type, built outside the polymorphic lambda
  // `choices` types the variants under.
  private def variant(schematic: TelSchematic, keyword: Text): (Text, TelSchematic) =
    (keyword, schematic)

  // The layers a sum's `@retract` annotations form, in order of first appearance: each a `select`
  // of the sum's name which excludes the variants retracted from that layer.
  private def retractionLayers(select: Text, retractions: scala.List[(Text, Text)])
  :   List[Tels.Layer] =

    val layers: List[Text] =
      TelSchematic.distinct[Text](proscenium.List.from(retractions.map(_(1))), name => name)

    layers.map: layer =>
      val excludes: scala.List[Text] =
        retractions.filter(_(1) == layer).map: (label, _) => Tel.camelToKebab(label.s)

      val definition =
        Tels.SelectDefinition(select, Array.empty, Array.empty, excludes = Array.from(excludes))

      val selects: Array[Tels.SelectDefinition]^{} = Array(definition)
      Tels.Layer(layer, Tels.Struct(Array.empty, Array.empty), Array.empty, Array.empty, selects)

  inline def disjunction[derivation: SumReflection]
  :   derivation is TelSchematic over Tels.Type =

    val identity: Identity = TelsDerivation.identityOf[derivation]

    val variants =
      val array =
        choices:
          [variant <: derivation] => schematic =>
            TelsDerivation.variant(schematic, Tel.camelToKebab(label.s))

      array.to[proscenium.List]

    selectSchematic(identity, variants, TelsDerivation.retractions[derivation])
    . asInstanceOf[derivation is TelSchematic over Tels.Type]

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
import scala.collection.Factory

import adversaria.*
import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import panopticon.*
import prepositional.*
import vacuous.*
import wisteria.*

// Phase-2 encode/decode typeclasses and Wisteria derivation. Mirrors
// jacinta.Json2 / DecodableDerivation / EncodableDerivation but produces
// TEL presentation structures instead of JSON ASTs.
//
// Encoding model: a scalar value (Text, Int, ...) produces a Tel wrapping
// a single Compound with one inline atom carrying the text. A product
// type (case class) produces a Tel wrapping a Compound whose children
// list contains one Compound per field, keyed by the field's label.
// Decoding inverts these mappings.

trait Tel2:
  // Field-keyed lens: a name `<: Label` resolves to a Lens from `Tel`
  // onto `Tel`. The getter delegates to `selectDynamic`; the setter
  // routes through `Tel.modify`, which replaces an existing child
  // compound with the same kebab-case keyword in place or appends a
  // new one. Mirrors jacinta's lens given.
  given lens: [name <: Label: ValueOf] => (erased DynamicTelEnabler) => Tactic[TelError]
  =>  name is Lens from Tel onto Tel =
    Lens(_.selectField(valueOf[name]), _.modify(valueOf[name], _))

  // Positional optics over a node's child compounds (TEL has no positional arrays,
  // but a compound's children are ordered — this mirrors the read-side
  // `applyDynamic(field)(index)`). `Ordinal` addresses the n-th child; `Each` every
  // child. The transform's result keeps the original child's keyword, so a positional
  // update preserves the field identity while replacing its value/children.
  private def rewrap(original: Tel.Compound, replacement: Tel): Tel.Compound =
    replacement.subtree match
      case compound: Tel.Compound =>
        compound.copy(keyword = original.keyword)
      case document: Tel.Document =>
        original.copy(atoms = IArray.empty[Tel.Atom], remark = Unset, children = document.children)

  private def rebuild(origin: Tel, children: IArray[Tel.Block]): Tel = origin.subtree match
    case document: Tel.Document => Tel.make(document.copy(children = children))
    case compound: Tel.Compound => Tel.make(compound.copy(children = children))

  given ordinalOptical: [element] => Ordinal is Optical from Tel onto Tel = ordinal =>
    Optic: (origin, lambda) =>
      if ordinal.n0 < 0 || ordinal.n0 >= origin.childCompounds.length then origin
      else rebuild
       ( origin,
         Tel.withChildCompound
          (origin.subtree.children, ordinal.n0, c => rewrap(c, lambda(Tel.make(c)))) )

  given eachOptical: Each.type is Optical from Tel onto Tel = _ =>
    Optic: (origin, lambda) =>
      rebuild
       ( origin,
         Tel.mapChildCompounds(origin.subtree.children, c => rewrap(c, lambda(Tel.make(c)))) )

  // `tel"…"` interpolator: parses at compile time and substitutes typed
  // holes via Encodable in Tel.
  inline given interpolator: Tel is Interpolable:
    type Result = Tel

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
         (inline insertions: Any*)
    :     Tel =

      ${stratiform.internal.interpolator[parts, origins]('insertions)}

  // `tel"…"` extractor: parses the pattern at compile time and produces
  // a structural matcher that binds atom-text captures.
  inline given extrapolator: Tel is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Tel)
    :     Boolean | Option[Tuple | Tel] =

      ${stratiform.internal.extractor[parts, origins]('scrutinee)}

  inline given decodable: [value] => value is Decodable in Tel = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[TelError]](_.primaryAtom.decode[value])

    case given Reflection[`value`] => DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Tel = summonFrom:
    case given (`value` is Encodable in Text) =>
      v => Tel.scalar(v.encode)

    case given Reflection[`value`] => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Tel]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Tel =
      telVal =>
        provide[Tactic[TelError]]:
          // `@name[Tel]` / bare `@name` renames: field name -> keyword, used
          // verbatim; an unannotated field falls back to its camel→kebab form.
          val renames: Map[Text, Text] = relabelling[derivation, Tel]

          build: [field] =>
            ctx =>
              val keyword: Text = renames.getOrElse(label, Tel.camelToKebab(label.s))
              val match0 = telVal.field(keyword)
              if match0.absent then default.or(ctx.decoded(Tel.empty))
              else ctx.decoded(match0.vouch)

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Tel =
      telVal =>
        provide[Tactic[TelError]]:
          provide[Tactic[VariantError]]:
            val variantKeyword = telVal.primaryAtom
            delegate(variantKeyword): [variant <: derivation] =>
              ctx => ctx.decoded(telVal)

  object EncodableDerivation extends Derivable[Encodable in Tel]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Tel = value =>
      val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

      // `@name[Tel]` / bare `@name` renames: field name -> keyword, used
      // verbatim; an unannotated field falls back to its camel→kebab form.
      val renames: Map[Text, Text] = relabelling[derivation, Tel]

      fields(value): [field] =>
        fieldValue =>
          val encoded = contextual.encode(fieldValue)
          val keyword = renames.getOrElse(label, Tel.camelToKebab(label.s))
          encoded.subtree match
            case c: Tel.Compound =>
              compounds += c.copy(keyword = keyword)

            // A list-valued field encodes to a Document of element compounds;
            // flatten them as repeated fields, each re-keyed to the field label.
            case d: Tel.Document =>
              compounds ++= d.children.flatMap(_.compounds).map(_.copy(keyword = keyword))

      Tel.compound(t"", IArray.empty, IArray.from(compounds))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Tel =
      value =>
        variant(value): [variant <: derivation] =>
          v => contextual.encode(v)

  // Primitive instances: Text/Int/Long/Double/Boolean as Compound + inline
  // atom. These mirror jacinta.Json's primitive decoders but go through
  // the atom text rather than a JSON AST.

  given textDecodable: Tactic[TelError] => Text is Decodable in Tel = _.primaryAtom

  given stringDecodable: Tactic[TelError] => String is Decodable in Tel =
    _.primaryAtom.s

  given intDecodable: Tactic[TelError] => Int is Decodable in Tel = telVal =>
    try telVal.primaryAtom.s.toInt
    catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given longDecodable: Tactic[TelError] => Long is Decodable in Tel = telVal =>
    try telVal.primaryAtom.s.toLong
    catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given doubleDecodable: Tactic[TelError] => Double is Decodable in Tel = telVal =>
    try telVal.primaryAtom.s.toDouble
    catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given booleanDecodable: Tactic[TelError] => Boolean is Decodable in Tel = telVal =>
    telVal.primaryAtom.s match
      case "true"  => true
      case "false" => false
      case _       => abort(TelError(TelError.Reason.BadVersion))

  given telDecodable: Tel is Decodable in Tel = identity(_)

  given textEncodable: Text is Encodable in Tel = text => Tel.scalar(text)
  given stringEncodable: String is Encodable in Tel = s => Tel.scalar(Text(s))
  given intEncodable: Int is Encodable in Tel = i => Tel.scalar(Text(i.toString))
  given longEncodable: Long is Encodable in Tel = l => Tel.scalar(Text(l.toString))
  given doubleEncodable: Double is Encodable in Tel = d => Tel.scalar(Text(d.toString))
  given booleanEncodable: Boolean is Encodable in Tel = b => Tel.scalar(Text(b.toString))
  given telEncodable: Tel is Encodable in Tel = identity(_)

  // Optional / List support — repeatable scalar fields produce multiple
  // compounds with the same keyword; we return a Document-rooted Tel
  // containing the list elements as siblings, which the product encoder
  // recognises and flattens with the field's label.

  given optionalEncodable: [value: Encodable in Tel] => Optional[value] is Encodable in Tel =
    opt => opt.lay(Tel.empty)(v => v.encode)

  // A `List` encodes to a Document-rooted Tel whose children are the elements'
  // compounds; the product encoder recognises the Document and flattens those
  // compounds into repeated fields keyed by the list's label — TEL's
  // representation of a repeated field. `List[value]` is more specific than
  // `value`, so this is preferred over the `encodable` derivation without
  // ambiguity (exactly as `optionalEncodable` is for `Optional`).
  given listEncodable: [value: Encodable in Tel] => List[value] is Encodable in Tel = list =>
    val compounds: IArray[Tel.Compound] = IArray.from:
      list.flatMap: element =>
        element.encode.subtree match
          case compound: Tel.Compound => List(compound)
          case document: Tel.Document => document.children.flatMap(_.compounds).to(List)
          case _                      => Nil

    Tel(Tel.Document(Unset, Unset, Tel.LineEndings.Lf, IArray(Tel.Block(IArray.empty, Unset, compounds, 0))))

  given optionalDecodable: [value: Decodable in Tel] => Tactic[TelError]
  =>  Optional[value] is Decodable in Tel = telVal =>
    if telVal.childCompounds.isEmpty && telVal.atomTexts.isEmpty then Unset else value.decoded(telVal)

  // Collection support — every element is encoded as an `item` compound, and the
  // product encoder re-keys the wrapping compound with the field's label, so a
  // `List[T]` field `xs` becomes an `xs` compound with one `item` child per
  // element. Decoding reads back all `item` children, building the target
  // collection via its `Factory`.

  // Re-keys an encoded value's compound (or wraps a document) under `keyword`.
  private def reKey(tel: Tel, keyword: Text): Tel.Compound = tel.subtree match
    case c: Tel.Compound => c.copy(keyword = keyword)
    case d: Tel.Document => Tel.Compound(keyword, IArray.empty, Unset, d.children)

  private def collectionTel[value]
      (values: Iterable[value])(using encodable: value is Encodable in Tel)
  :     Tel =
    Tel.compound(t"", IArray.empty, IArray.from(values.map(v => reKey(encodable.encoded(v), t"item"))))

  given listEncodable: [list <: List, element] => (encodable: => element is Encodable in Tel)
  =>  list[element] is Encodable in Tel =
    values => collectionTel(values)(using encodable)

  given setEncodable: [set <: Set, element] => (encodable: => element is Encodable in Tel)
  =>  set[element] is Encodable in Tel =
    values => collectionTel(values)(using encodable)

  given seriesEncodable: [series <: Series, element] => (encodable: => element is Encodable in Tel)
  =>  series[element] is Encodable in Tel =
    values => collectionTel(values)(using encodable)

  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        decodable: => element is Decodable in Tel )
  =>  Tactic[TelError]
  =>  collection[element] is Decodable in Tel =
    telVal =>
      val builder = factory.newBuilder
      for item <- telVal.fields(t"item") do builder += decodable.decoded(item)
      builder.result()

  // A `Map` encodes as a series of `entries` compounds, each carrying a `key`
  // and a `value` child field. As with other collections the product encoder
  // re-keys the wrapping compound with the field's label.

  given mapEncodable: [key: Encodable in Tel, value: Encodable in Tel]
  =>  Map[key, value] is Encodable in Tel =
    map =>
      val entries = IArray.from:
        map.map: (k, v) =>
          val keyChild   = reKey(key.encoded(k), t"key")
          val valueChild = reKey(value.encoded(v), t"value")
          reKey(Tel.compound(t"", IArray.empty, IArray(keyChild, valueChild)), t"entries")

      Tel.compound(t"", IArray.empty, entries)

  given mapDecodable: [key: Decodable in Tel, value: Decodable in Tel] => Tactic[TelError]
  =>  Map[key, value] is Decodable in Tel =
    telVal =>
      var accumulator = Map.empty[key, value]

      for entry <- telVal.fields(t"entries") do
        val k = key.decoded(entry.field(t"key").or(Tel.empty))
        val v = value.decoded(entry.field(t"value").or(Tel.empty))
        accumulator = accumulator.updated(k, v)

      accumulator

  // Helpers used by encoders to construct Tel values.

  def scalar(text: Text): Tel =
    Tel(Tel.Compound(t"", IArray(Tel.Atom.Inline(text, 1)), Unset, IArray.empty))

  def compound
       (keyword: Text, atoms: IArray[Tel.Atom], compounds: IArray[Tel.Compound])
  :     Tel =
    val children =
      if compounds.isEmpty then IArray.empty[Tel.Block]
      else IArray(Tel.Block(IArray.empty, Unset, compounds, 0))

    Tel(Tel.Compound(keyword, atoms, Unset, children))

  def empty: Tel = Tel(Tel.Compound(t"", IArray.empty, Unset, IArray.empty))

// `value.encode` (provided by the Encodable typeclass extension defined in
// anticipation) is the idiomatic call site producing a Tel from any
// encodable value. A `.tel` alias may be added later for symmetry with
// jacinta's `.json`.

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
import rudiments.*
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

  inline given decodable: [value] => value is Tel.Decodable = summonFrom:
    case given (`value` is Decodable in Text) =>
      Tel.Decodable(Shape.Str)(provide[Tactic[TelError]](_.primaryAtom.decode[value]))

    case given Reflection[`value`] => DecodableDerivation.derived

  inline given encodable: [value] => value is Tel.Encodable = summonFrom:
    case given (`value` is Encodable in Text) =>
      Tel.Encodable(Shape.Str)(v => Tel.scalar(v.encode))

    case given Reflection[`value`] => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Tel.Decodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Decodable =
      // The object `Shape` is built from the field decoders' own shapes (a single
      // inlined `contexts` traversal — kept here, not factored out, so it does not
      // perturb the `build` traversal), keeping a fused `Decodable & Schematic`
      // coherent. Built by-name so recursive types compile.
      Tel.Decodable({
        val fields: List[(Text, Shape)] =
          contexts: [field] => context => (label, context.shape())
          . to(List)

        Shape.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
      }):
        telVal =>
          provide[Tactic[TelError]]:
            // `@name[Tel]` / bare `@name` renames: field name -> keyword, used
            // verbatim; an unannotated field falls back to its camel→kebab form.
            val renames: Map[Text, Text] = relabelling[derivation, Tel]

            build: [field] =>
              ctx =>
                val keyword: Text = renames.getOrElse(label, Tel.camelToKebab(label.s))

                // A `List`/`Set` field (`ctx.repeatable`) is encoded as repeated
                // keyword compounds, so gather them all into a Document for the
                // collection decoder. Every other field — scalar, nested product,
                // `Optional`, `Map` (a single `entries` compound) — reads one match.
                if ctx.repeatable then
                  val compounds = telVal.childCompounds.filter(_.keyword == keyword)
                  ctx.decoded:
                    Tel.make
                     (Tel.Document
                       (Unset, Unset, Tel.LineEndings.Lf,
                        IArray(Tel.Block(IArray.empty, Unset, compounds, 0))))
                else
                  val match0 = telVal.field(keyword)
                  if match0.absent then default.or(ctx.decoded(Tel.empty))
                  else ctx.decoded(match0.vouch)

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Decodable =
      // A sum is a single compound whose keyword is the variant's (kebab-cased) name;
      // dispatch on that keyword and decode the same compound as the chosen variant.
      // This is the discriminated form `Tel.Type.assign` and BinTEL also key on. The
      // codec-carried shape stays permissive (`Any`) — walking the variants
      // (`delegate`) is `fallible` and would leak a `Tactic[VariantError]` requirement
      // onto every codec; the precise select schema comes from the standalone
      // `Schematic` / `Tels.tels`.
      Tel.Decodable(Shape.Any):
        telVal =>
          provide[Tactic[TelError]]:
            provide[Tactic[VariantError]]:
              // The compound's keyword is the variant's kebab-cased name; map it back
              // to the variant label `delegate` dispatches on.
              val labels: Map[Text, Text] =
                variantLabels[derivation].map { label => Tel.camelToKebab(label.s) -> label }.to(Map)

              val variantKeyword: Text = labels.getOrElse(telVal.keyword, telVal.keyword)

              delegate(variantKeyword): [variant <: derivation] =>
                ctx => ctx.decoded(telVal)

  object EncodableDerivation extends Derivable[Tel.Encodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Encodable =
      Tel.Encodable({
        val fields: List[(Text, Shape)] =
          contexts: [field] => context => (label, context.shape())
          . to(List)

        Shape.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
      }):
        value =>
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

                // A list/set field encodes to a Document of element compounds;
                // flatten them as repeated fields, each re-keyed to the field label
                // (TEL's representation of a repeated field — see `#1291`).
                case d: Tel.Document =>
                  compounds ++= d.children.flatMap(_.compounds).map(_.copy(keyword = keyword))

          Tel.compound(t"", IArray.empty, IArray.from(compounds))

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Encodable =
      // A sum encodes as a single compound whose keyword is the variant's (kebab-cased)
      // name, with the variant's fields as its children — the discriminated form the
      // decoder, `Tel.Type.assign` and BinTEL all key on. The codec-carried shape stays
      // permissive (`Any`); the precise select schema comes from the standalone
      // `Schematic` / `Tels.tels`.
      Tel.Encodable(Shape.Any):
        value =>
          variant(value): [variant <: derivation] =>
            v =>
              val keyword: Text = Tel.camelToKebab(label.s)

              contextual.encode(v).subtree match
                case compound: Tel.Compound => Tel.make(compound.copy(keyword = keyword))
                case other                  => Tel.make(other)

  // Primitive instances: Text/Int/Long/Double/Boolean as Compound + inline
  // atom. These mirror jacinta.Json's primitive decoders but go through
  // the atom text rather than a JSON AST.

  given textDecodable: Tactic[TelError] => Text is Tel.Decodable =
    Tel.Decodable(Shape.Str)(_.primaryAtom)

  given stringDecodable: Tactic[TelError] => String is Tel.Decodable =
    Tel.Decodable(Shape.Str)(_.primaryAtom.s)

  given intDecodable: Tactic[TelError] => Int is Tel.Decodable =
    Tel.Decodable(Shape.Whole): telVal =>
      try telVal.primaryAtom.s.toInt
      catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given longDecodable: Tactic[TelError] => Long is Tel.Decodable =
    Tel.Decodable(Shape.Whole): telVal =>
      try telVal.primaryAtom.s.toLong
      catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given doubleDecodable: Tactic[TelError] => Double is Tel.Decodable =
    Tel.Decodable(Shape.Real): telVal =>
      try telVal.primaryAtom.s.toDouble
      catch case _: NumberFormatException => abort(TelError(TelError.Reason.BadVersion))

  given booleanDecodable: Tactic[TelError] => Boolean is Tel.Decodable =
    Tel.Decodable(Shape.Bool): telVal =>
      telVal.primaryAtom.s match
        case "true"  => true
        case "false" => false
        case _       => abort(TelError(TelError.Reason.BadVersion))

  given telDecodable: Tel is Tel.Decodable = Tel.Decodable(Shape.Any)(identity(_))

  given textEncodable: Text is Tel.Encodable = Tel.Encodable(Shape.Str)(text => Tel.scalar(text))
  given stringEncodable: String is Tel.Encodable =
    Tel.Encodable(Shape.Str)(s => Tel.scalar(Text(s)))
  given intEncodable: Int is Tel.Encodable =
    Tel.Encodable(Shape.Whole)(i => Tel.scalar(Text(i.toString)))
  given longEncodable: Long is Tel.Encodable =
    Tel.Encodable(Shape.Whole)(l => Tel.scalar(Text(l.toString)))
  given doubleEncodable: Double is Tel.Encodable =
    Tel.Encodable(Shape.Real)(d => Tel.scalar(Text(d.toString)))
  given booleanEncodable: Boolean is Tel.Encodable =
    Tel.Encodable(Shape.Bool)(b => Tel.scalar(Text(b.toString)))
  given telEncodable: Tel is Tel.Encodable = Tel.Encodable(Shape.Any)(identity(_))

  // Optional / List support — repeatable scalar fields produce multiple
  // compounds with the same keyword; we return a Document-rooted Tel
  // containing the list elements as siblings, which the product encoder
  // recognises and flattens with the field's label.

  given optionalEncodable: [value: Tel.Encodable] => Optional[value] is Tel.Encodable =
    Tel.Encodable(Shape.Opt(value.shape())): opt =>
      opt.lay(Tel.empty)(v => v.encode)

  given optionalDecodable: [value: Tel.Decodable] => Tactic[TelError]
  =>  Optional[value] is Tel.Decodable =
    Tel.Decodable(Shape.Opt(value.shape())): telVal =>
      if telVal.childCompounds.isEmpty && telVal.atomTexts.isEmpty then Unset
      else value.decoded(telVal)

  // Collection support (aligned with `#1291`) — a `List`/`Set` encodes to a
  // Document-rooted Tel whose children are the elements' compounds; the product
  // encoder (`conjunction`) flattens those into repeated fields, each re-keyed to
  // the field's label (TEL's representation of a repeated field). Decoding inverts
  // this: the product decoder gathers all sibling compounds sharing the field's
  // keyword into a Document and hands it here, where each child is decoded as an
  // element via the target's `Factory`.

  // Re-keys an encoded value's compound (or wraps a document) under `keyword`.
  private def reKey(tel: Tel, keyword: Text): Tel.Compound = tel.subtree match
    case c: Tel.Compound => c.copy(keyword = keyword)
    case d: Tel.Document => Tel.Compound(keyword, IArray.empty, Unset, d.children)

  private def collectionDocument[value]
      (values: Iterable[value])(using encodable: value is Encodable in Tel)
  :     Tel =
    val compounds: IArray[Tel.Compound] = IArray.from:
      values.flatMap: element =>
        encodable.encoded(element).subtree match
          case compound: Tel.Compound => List(compound)
          case document: Tel.Document => document.children.flatMap(_.compounds).to(List)

    Tel(Tel.Document(Unset, Unset, Tel.LineEndings.Lf,
        IArray(Tel.Block(IArray.empty, Unset, compounds, 0))))

  given listEncodable: [list <: List, element] => (encodable: => element is Tel.Encodable)
  =>  list[element] is Tel.Encodable =
    Tel.Encodable(Shape.Arr(encodable.shape())): values =>
      collectionDocument(values)(using encodable)

  given setEncodable: [set <: Set, element] => (encodable: => element is Tel.Encodable)
  =>  set[element] is Tel.Encodable =
    Tel.Encodable(Shape.Arr(encodable.shape())): values =>
      collectionDocument(values)(using encodable)

  given seriesEncodable: [series <: Series, element] => (encodable: => element is Tel.Encodable)
  =>  series[element] is Tel.Encodable =
    Tel.Encodable(Shape.Arr(encodable.shape())): values =>
      collectionDocument(values)(using encodable)

  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        element0:  => element is Tel.Decodable )
  =>  Tactic[TelError]
  =>  collection[element] is Tel.Decodable =
    new Tel.Decodable:
      type Self = collection[element]
      def shape(): Shape = Shape.Arr(element0.shape())
      override def repeatable: Boolean = true

      def decoded(telVal: Tel): collection[element] =
        val builder = factory.newBuilder

        telVal.subtree.absolve match
          case document: Tel.Document =>
            document.children.flatMap(_.compounds).each: compound =>
              builder += element0.decoded(Tel.make(compound))

          case compound: Tel.Compound =>
            builder += element0.decoded(telVal)

        builder.result()

  // A `Map` encodes as a series of `entries` compounds, each carrying a `key`
  // and a `value` child field. As with other collections the product encoder
  // re-keys the wrapping compound with the field's label.

  given mapEncodable: [key: Tel.Encodable, value: Tel.Encodable]
  =>  Map[key, value] is Tel.Encodable =
    Tel.Encodable(Shape.Dict(key.shape(), value.shape())): map =>
      val entries = IArray.from:
        map.map: (k, v) =>
          val keyChild   = reKey(key.encoded(k), t"key")
          val valueChild = reKey(value.encoded(v), t"value")
          reKey(Tel.compound(t"", IArray.empty, IArray(keyChild, valueChild)), t"entries")

      Tel.compound(t"", IArray.empty, entries)

  given mapDecodable: [key: Tel.Decodable, value: Tel.Decodable] => Tactic[TelError]
  =>  Map[key, value] is Tel.Decodable =
    Tel.Decodable(Shape.Dict(key.shape(), value.shape())): telVal =>
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

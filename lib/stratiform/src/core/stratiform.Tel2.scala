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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.collection.Factory
import scala.compiletime.*

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
import turbulence.*
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

// Register a decode error and continue with `sentinel` instead of aborting, so
// that sibling fields of a product can each accrue their own error under a
// `validate[Tel.Focus]` boundary. A field with no atom (the empty `Tel` handed
// to a primitive by `conjunction` for an absent field) raises `Absent`; an atom
// that fails to parse raises `NotScalar`, distinguishing "missing" from
// "wrong shape". Outside a `validate` boundary the ambient `ThrowTactic` makes
// `raise` throw, preserving fail-fast decoding.
// At file level (not a trait member) so the decoder SAMs that call it capture only their
// tactic, not the enclosing `Tel2` instance.
private[stratiform] def primitiveFault[value]
  ( tel: Tel, expected: Text, sentinel: value )
  ( parse: Text => Optional[value] )
  ( using Tactic[TelError] )
:   value =

  if tel.atomTexts.isEmpty then raise(TelError(TelError.Reason.Absent)) yet sentinel
  else parse(tel.primaryAtom).or:
    raise(TelError(TelError.Reason.NotScalar(tel.primaryAtom, expected))) yet sentinel

trait Tel2 extends Tel3:
  // Field-keyed lens: a name `<: Label` resolves to a Lens from `Tel`
  // onto `Tel`. The getter delegates to `selectDynamic`; the setter
  // routes through `Tel.modify`, which replaces an existing child
  // compound with the same kebab-case keyword in place or appends a
  // new one. Mirrors jacinta's lens given.
  given lens: [name <: Label: ValueOf] => (erased dynamicTelEnabler: DynamicTelEnabler) => Tactic[TelError]
  =>  name is Lens from Tel onto Tel =
    Lens(_.selectField(valueOf[name]), _.modify(valueOf[name], _))

  // Positional optics over a node's child compounds (TEL has no positional arrays,
  // but a compound's children are ordered — this mirrors the read-side
  // `applyDynamic(field)(index)`). `Ordinal` addresses the n-th child; `Each` every
  // child. The transform's result keeps the original child's keyword, so a positional
  // update preserves the field identity while replacing its value/children.
  // (`rewrap`/`rebuild` are package-level pure helpers — see `stratiform_core.scala`.)

  given ordinalOptical: [element] => Ordinal is Optical from Tel onto Tel = ordinal =>
    Optic: (origin, lambda) =>
      if ordinal.n0 < 0 || ordinal.n0 >= origin.childCompounds.length then origin
      else rebuild
        ( origin,
          Tel.withChildCompound
           ( origin.subtree.children, ordinal.n0, c => rewrap(c, lambda(Tel.make(c))) ) )

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
      ( inline insertions: Any* )
    :   Tel =

      ${stratiform.internal.interpolator[parts, origins]('insertions)}

  // `tel"…"` extractor: parses the pattern at compile time and produces
  // a structural matcher that binds atom-text captures.
  inline given extrapolator: Tel is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Tel)
    :   Boolean | Option[Tuple | Tel] =

      ${stratiform.internal.extractor[parts, origins]('scrutinee)}

  inline given decodable: [value] => value is Tel.Decodable = summonFrom:
    case given (`value` is Decodable in Text) =>
      Tel.Decodable(() => Morphology.Str)(provide[Tactic[TelError]](_.primaryAtom.as[value]))

    case given Reflection[`value`] => DecodableDerivation.derived

  inline given encodable: [value] => value is Tel.Encodable = summonFrom:
    case given (`value` is Encodable in Text) =>
      Tel.Encodable(() => Morphology.Str): v => Tel.scalar(v.encode)

    case given Reflection[`value`] => EncodableDerivation.derived

  // ── Direct parsing: element-wise `Tel.Field` instances and the read path ──

  // Element-wise `Tel.Field` for collections, resolved during derivation:
  // the element's own parser comes from the fallback chain, so nested
  // products still parse directly. This layer beats `Tel3`'s fallback, so
  // collection types never reach its `Reflection` case (a `List`'s own
  // `Mirror` would otherwise derive it as a sum). The instance is
  // repeatable: the product engine gathers every same-keyword occurrence,
  // exactly as the AST derivation collects all matching compounds.
  given fieldCollection: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[TelError] )
  =>  ( field: => (element is Tel.Field)^ )
  =>  collection[element] is Tel.Field =
    Tel.Field(Tel.Parsable.iterable[collection, element](field))

  // Element-wise `Tel.Field` for `Optional`, resolved during derivation:
  // the inner instance comes from the field fallback chain (by-name, so
  // recursive types resolve, exactly like collections), and the wrapper's
  // semantics mirror the AST `optionalDecodable`: an entry with neither an
  // inline atom nor a child compound reads as `Unset`, as does a missing
  // keyword. Above the blanket so an `Optional` field resolves here with
  // the same specificity preference the AST derivation's `Tel.Decodable`
  // summon exhibits.
  given fieldOptional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( tactic: Tactic[TelError] )
  =>  ( field: => (inner is Tel.Field)^ )
  =>  value is Tel.Field =
    Tel.Field(Tel.Parsable.optionality[inner, value](field))

  // The AST-materializing read path: `source.read[Foo in Tel]` shorthand for
  // `source.read[Tel].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect` for
  // `value in Json`. The `Form` type-tag is added by an `asInstanceOf` cast —
  // `value in Tel` is just `value { type Form = Tel }` so the cast is a no-op
  // at runtime. Lives at this priority so `object Tel`'s direct-parsing
  // `aggregableParsed` wins whenever the value has a `Tel.Parsable`; when it
  // does not (all pre-`Parsable` code), this resolves exactly as before.
  given aggregableIn: [value: distillate.Decodable in Tel] => (tactic: Tactic[TelError])
  =>  (((value in Tel) is Aggregable by Data)^{tactic}) =
    source => Tel.parse(Tel.concatenate(source)).as[value].asInstanceOf[value in Tel]

  object ParsableDerivation extends Derivable[Tel.Field]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Field =

      // Like `DecodableDerivation.conjunction`: the capabilities are summoned
      // at the derivation site and the instance is sealed per the codec-thunk
      // pattern. A single `contexts` traversal collects, per field, its wire
      // keyword (`@name[Tel]`-aware, camel→kebab otherwise — the same mapping
      // as the AST derivation), its parser (via the `Field` fallback chain)
      // and its declared default; `Tel.Parsable.product` owns the entry loop,
      // so no per-field lambda ever closes over the reader.
      caps.unsafe.unsafeAssumePure:
        val reflection = infer[ProductReflection[derivation]]

        Tel.Parsable.product[derivation](
          { () =>
            val renames: Map[Text, Text] = relabelling[derivation, Tel]

            contexts[derivation]():
              [field] => context =>
                ( renames.getOrElse(label, Tel.camelToKebab(label.s)).s,
                  context: Tel.Parsing,
                  default[Optional[field]]: Any )
          },
          values => Tel.Parsable.assemble(reflection, values))
          ( using infer[Foci[Tel.Focus]], infer[Tactic[TelError]] )

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Field =
      // A sum's wire form is a single child compound keyed by the variant's
      // name, which cannot be recognised before the entry itself is read —
      // so a sum always takes the AST bridge over its derived (or custom)
      // decoder, keeping the two paths identical by construction. Sealed per
      // the codec-thunk pattern: the instance captures a resolution-scoped
      // decoder.
      caps.unsafe.unsafeAssumePure:
        Tel.Field(Tel.Parsable.fromDecodable(infer[derivation is Tel.Decodable]))

  object DecodableDerivation extends Derivable[Tel.Decodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Decodable =

      // The object `Morphology` is built from the field decoders' own shapes (a single
      // inlined `contexts` traversal — kept here, not factored out, so it does not
      // perturb the `build` traversal), keeping a fused `Decodable & Schematic`
      // coherent. Built by-name so recursive types compile.
      Tel.Decodable({ () =>
        val fields: List[(Text, Morphology)] =
          contexts[derivation](): [field] => context => (label, context.shape())
          . to(List)

        Morphology.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
      }):
        telVal =>
          provide[Foci[Tel.Focus]]:
            provide[Tactic[TelError]]:
              // `@name[Tel]` / bare `@name` renames: field name -> keyword, used
              // verbatim; an unannotated field falls back to its camel→kebab form.
              val renames: Map[Text, Text] = relabelling[derivation, Tel]

              build[derivation]: [field] =>
                ctx =>
                  val keyword: Text = renames.getOrElse(label, Tel.camelToKebab(label.s))

                  // Tag every error registered while decoding this field with its
                  // keyword path, so that under a `validate[Tel.Focus]` boundary the
                  // primitives' `raise … yet sentinel` accrue per-field rather than the
                  // first malformed field aborting the whole record.
                  focus({
                    val base = prior.let(_.pointer).or(TelPath.Root)
                    Tel.Focus(base.prepend(keyword))
                  }):
                    // A `List`/`Set` field (`ctx.repeatable`) is encoded as repeated
                    // keyword compounds, so gather them all into a Document for the
                    // collection decoder. Every other field — scalar, nested product,
                    // `Optional`, `Map` (a single `entries` compound) — reads one match.
                    if ctx.repeatable then
                      val compounds = telVal.childCompounds.filter(_.keyword == keyword)

                      ctx.decoded:
                        Tel.make
                          ( Tel.Document
                            ( Unset, Unset, Tel.LineEndings.Lf,
                             IArray(Tel.Block(IArray.empty, Unset, compounds, 0)) ) )
                    else
                      val match0 = telVal.field(keyword)

                      if match0.absent then default.or(ctx.decoded(Tel.empty))
                      else ctx.decoded(match0.vouch)

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Decodable =
      // A sum is a document whose single child compound is the chosen variant, keyed by
      // the variant's (kebab-cased) name. Dispatch on that child's keyword and decode it
      // as the variant. This is the select-member form `Tel.Type.assign` and BinTEL key
      // on. The codec-carried shape stays permissive (`Any`) — walking the variants
      // (`delegate`) is `fallible` and would leak a `Tactic[VariantError]` requirement
      // onto every codec; the precise select schema comes from the standalone
      // `Schematic` / `Tels.tels`.
      // Kebab keyword → variant label (the label `delegate` dispatches on), a
      // per-derivation constant: built once here rather than on every decode
      // call, whose profile it dominated (map building plus generic-equality
      // lookups, per occurrence) — jacinta's map hoist.
      val labels: Map[Text, Text] =
        variantLabels.map: label => Tel.camelToKebab(label.s) -> label
        . to(Map)

      Tel.Decodable(() => Morphology.Any):
        telVal =>
          provide[Foci[Tel.Focus]]:
            provide[Tactic[TelError]]:
              provide[Tactic[VariantError]]:
                val variant: Tel = Tel.make(telVal.childCompounds.head)
                val variantKeyword: Text = labels.getOrElse(variant.keyword, variant.keyword)

                delegate(variantKeyword): [variant <: derivation] =>
                  ctx => ctx.decoded(variant)

  object EncodableDerivation extends Derivable[Tel.Encodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Encodable =

      Tel.Encodable({ () =>
        val fields: List[(Text, Morphology)] =
          contexts[derivation](): [field] => context => (label, context.shape())
          . to(List)

        Morphology.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
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
      // A sum encodes as a document whose single child compound is the chosen variant,
      // keyed by the variant's (kebab-cased) name with the variant's fields as its own
      // children. This is the select-member form `Tel.Type.assign` and BinTEL key on (the
      // variant is a member of the document, matched by the schema's `SelectRef`), and it
      // round-trips identically to the same document parsed from text. The codec-carried
      // shape stays permissive (`Any`); the precise select schema comes from the standalone
      // `Schematic` / `Tels.tels`.
      Tel.Encodable(() => Morphology.Any):
        value =>
          variant(value): [variant <: derivation] =>
            v =>
              val keyword: Text = Tel.camelToKebab(label.s)

              contextual.encode(v).subtree match
                case compound: Tel.Compound =>
                  Tel.compound(t"", IArray.empty, IArray(compound.copy(keyword = keyword)))

                case other =>
                  Tel.make(other)

  // Primitive instances: Text/Int/Long/Double/Boolean as Compound + inline
  // atom. These mirror jacinta.Json's primitive decoders but go through
  // the atom text rather than a JSON AST.


  given textDecodable: (tactic: Tactic[TelError]) => ((Text is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Str): tel =>
      primitiveFault(tel, t"Text", t""): atom =>
        atom

  given stringDecodable: (tactic: Tactic[TelError]) => ((String is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Str): tel =>
      primitiveFault(tel, t"String", ""): atom =>
        atom.s

  given intDecodable: (tactic: Tactic[TelError]) => ((Int is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Whole): tel =>
      primitiveFault(tel, t"Int", 0): atom =>
        try atom.s.toInt catch case _: NumberFormatException => Unset

  given longDecodable: (tactic: Tactic[TelError]) => ((Long is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Whole): tel =>
      primitiveFault(tel, t"Long", 0L): atom =>
        try atom.s.toLong catch case _: NumberFormatException => Unset

  given doubleDecodable: (tactic: Tactic[TelError]) => ((Double is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Real): tel =>
      primitiveFault(tel, t"Double", 0.0): atom =>
        try atom.s.toDouble catch case _: NumberFormatException => Unset

  given booleanDecodable: (tactic: Tactic[TelError]) => ((Boolean is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Bool): tel =>
      primitiveFault(tel, t"Boolean", false): atom =>
        atom.s match
          case "true"  => true
          case "false" => false
          case _       => Unset

  given telDecodable: Tel is Tel.Decodable = Tel.Decodable(() => Morphology.Any)(identity(_))

  given textEncodable: Text is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str): text => Tel.scalar(text)

  given stringEncodable: String is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str): s => Tel.scalar(Text(s))

  given intEncodable: Int is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole): i => Tel.scalar(Text(i.toString))

  given longEncodable: Long is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole): l => Tel.scalar(Text(l.toString))

  given doubleEncodable: Double is Tel.Encodable =
    Tel.Encodable(() => Morphology.Real): d => Tel.scalar(Text(d.toString))

  given booleanEncodable: Boolean is Tel.Encodable =
    Tel.Encodable(() => Morphology.Bool): b => Tel.scalar(Text(b.toString))

  given telEncodable: Tel is Tel.Encodable = Tel.Encodable(() => Morphology.Any)(identity(_))

  // Optional / List support — repeatable scalar fields produce multiple
  // compounds with the same keyword; we return a Document-rooted Tel
  // containing the list elements as siblings, which the product encoder
  // recognises and flattens with the field's label.

  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Tel.Encodable )
  =>  value is Tel.Encodable =
    Tel.Encodable(() => Morphology.Opt(encodable.shape())): opt =>
      opt.let(_.asInstanceOf[inner]).lay(emptyDocument)(_.encode)

  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  Tactic[TelError]
  =>  ( decodable: -> (inner is Tel.Decodable) )
  =>  value is Tel.Decodable =
    Tel.Decodable(() => Morphology.Opt(decodable.shape())): telVal =>
      if telVal.childCompounds.nil && telVal.atomTexts.nil then Unset
      else decodable.decoded(telVal)

  // Collection support (aligned with `#1291`) — a `List`/`Set` encodes to a
  // Document-rooted Tel whose children are the elements' compounds; the product
  // encoder (`conjunction`) flattens those into repeated fields, each re-keyed to
  // the field's label (TEL's representation of a repeated field). Decoding inverts
  // this: the product decoder gathers all sibling compounds sharing the field's
  // keyword into a Document and hands it here, where each child is decoded as an
  // element via the target's `Factory`.

  // Re-keys an encoded value's compound (or wraps a document) under `keyword`.
  given listEncodable: [list <: List, element] => (encodable: -> (element is Tel.Encodable))
  =>  list[element] is Tel.Encodable =
    Tel.Encodable(() => Morphology.Arr(encodable.shape())): values =>
      collectionDocument(values)(using encodable)

  given setEncodable: [set <: Set, element] => (encodable: -> (element is Tel.Encodable))
  =>  set[element] is Tel.Encodable =
    Tel.Encodable(() => Morphology.Arr(encodable.shape())): values =>
      collectionDocument(values)(using encodable)

  given seriesEncodable: [series <: Series, element] => (encodable: -> (element is Tel.Encodable))
  =>  series[element] is Tel.Encodable =
    Tel.Encodable(() => Morphology.Arr(encodable.shape())): values =>
      collectionDocument(values)(using encodable)

  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        element0:  -> (element is Tel.Decodable) )
  =>  Tactic[TelError]
  =>  collection[element] is Tel.Decodable =
    new Tel.Decodable:
      type Self = collection[element]
      def shape(): Morphology = Morphology.Arr(element0.shape())
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
    Tel.Encodable(() => Morphology.Dict(key.shape(), value.shape())): map =>
      val entries = IArray.from:
        map.map: (k, v) =>
          val keyChild   = reKey(key.encoded(k), t"key")
          val valueChild = reKey(value.encoded(v), t"value")
          reKey(Tel.compound(t"", IArray.empty, IArray(keyChild, valueChild)), t"entries")

      Tel.compound(t"", IArray.empty, entries)

  given mapDecodable: [key: Tel.Decodable, value: Tel.Decodable] => Tactic[TelError]
  =>  Map[key, value] is Tel.Decodable =
    Tel.Decodable(() => Morphology.Dict(key.shape(), value.shape())): telVal =>
      var accumulator = Map.empty[key, value]

      for entry <- telVal.fields(t"entries") do
        val k = key.decoded(entry.field(t"key").or(Tel.empty))
        val v = value.decoded(entry.field(t"value").or(Tel.empty))
        accumulator = accumulator.updated(k, v)

      accumulator

  // Helpers used by encoders to construct Tel values.

  def scalar(text: Text): Tel =
    Tel.make(Tel.Compound(t"", IArray(Tel.Atom.Inline(text, 1)), Unset, IArray.empty))

  def compound
    ( keyword: Text, atoms: IArray[Tel.Atom], compounds: IArray[Tel.Compound] )
  :   Tel =

    val children =
      if compounds.nil then IArray.empty[Tel.Block]
      else IArray(Tel.Block(IArray.empty, Unset, compounds, 0))

    Tel.make(Tel.Compound(keyword, atoms, Unset, children))

  def empty: Tel = Tel.make(Tel.Compound(t"", IArray.empty, Unset, IArray.empty))

// `value.encode` (provided by the Encodable typeclass extension defined in
// anticipation) is the idiomatic call site producing a Tel from any
// encodable value. A `.tel` alias may be added later for symmetry with
// jacinta's `.json`.

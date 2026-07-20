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

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

import TelError.Reason

// Schema data model per §20 of the TEL specification. The data is a
// straightforward translation of the TypeScript interfaces given in the
// spec; behavioural code (type assignment, layer merging, validators)
// lives in companion modules.
//
// Notes on naming:
// - `Schema.name` and `Layer.name` are kebab-case identifiers carried
//   as Text values; conformance to the kebab-case grammar of §20.7 is
//   enforced by validators at parse time, not by the type.
// - Definition names (record / scalar / select) are PascalCase
//   TypeName identifiers, also Text at the data level.

object Tels extends Tels2:

  // Per-axis polarity tristate from §20: "default" means no flag was
  // declared, "loose" means a loosening flag (optional / repeatable)
  // was declared, "tight" means a tightening flag (required /
  // irrepeatable) was declared. Effective booleans are derived as
  //   required   = (member.required   != "loose")
  //   repeatable = (member.repeatable == "loose")
  enum Polarity:
    case Implicit, Loose, Tight

  // A schema's member sequence is a list of Member kinds. Field carries
  // its keyword and type at the use site; SelectRef references a named
  // SelectDefinition; Exclude is a layer-only operation that removes a
  // variant from the merged SelectDefinition.
  sealed trait Member

  case class Field
    ( required:    Polarity,
      repeatable:  Polarity,
      keyword:     Text,
      fieldType:   Type,
      default:     Optional[Text],
      description: Optional[Text] = Unset )
  extends Member

  case class SelectRef
    ( required:   Polarity,
      repeatable: Polarity,
      reference:  Text )
  extends Member

  case class Exclude(keyword: Text) extends Member

  // A Variant of a SelectDefinition: a kebab-case keyword paired with
  // any Type, plus an optional free-text description (§20).
  case class Variant(keyword: Text, variantType: Type, description: Optional[Text] = Unset)

  // The four kinds of Type per §20:
  //   - Struct: an ordered Member list plus struct-level validators
  //   - Scalar: zero-or-more validators applied to the atom text
  //   - Flag:   value-less; identity from keyword alone
  //   - Reference: indirect to a named Definition by TypeName
  sealed trait Type

  case class Struct(members: IArray[Member], validators: IArray[Text]) extends Type

  case class Scalar(validators: IArray[Text]) extends Type

  case object Flag extends Type

  case class Reference(name: Text) extends Type

  // Definitions in the schema's namespace. They share a single namespace
  // (§20: E211 for cross-kind collisions). Each Definition optionally
  // carries validators applied to the entire instance, and an optional
  // free-text `description` (§20): semantic, survives BinTEL round-trips,
  // never validated.
  case class RecordDefinition
    ( name:        Text,
      members:     IArray[Member],
      validators:  IArray[Text],
      description: Optional[Text] = Unset )

  case class ScalarDefinition
    ( name:        Text,
      validators:  IArray[Text],
      description: Optional[Text] = Unset )

  case class SelectDefinition
    ( name:        Text,
      variants:    IArray[Variant],
      validators:  IArray[Text],
      description: Optional[Text] = Unset )

  // A Layer applies incremental refinements per §20.3. `overlay` is the
  // (possibly empty) Struct merged into the document root; the three
  // Definition lists are merged into the composed namespace.
  case class Layer
    ( name:    Text,
      overlay: Struct,
      records: IArray[RecordDefinition],
      scalars: IArray[ScalarDefinition],
      selects: IArray[SelectDefinition] )

  // Predefined built-in type names per §20.5 / §21.5. Used by the
  // schema-of-schemas and any user schema that references them via
  // `Reference(TypeName)`.
  object Builtin:
    val String:     Text = t"String"
    val Identifier: Text = t"Identifier"
    val TypeName:   Text = t"TypeName"
    val Sigil:      Text = t"Sigil"
    val Flag:       Text = t"Flag"

  // Hand-encoded `tel-schema` axiom per §20.5 of the TEL specification.
  // This Scala literal mirrors the canonical `tel-schema.tel` document
  // (saved at `res/test/stratiform/corpus/tel-schema.tel`) verbatim.
  object Axiom:
    import Polarity.*

    private inline def kebab(s: String): Text = Text(s)

    private inline def field
      ( keyword:    String,
        fieldType:  Type,
        required:   Polarity = Implicit,
        repeatable: Polarity = Implicit,
        default:    Optional[Text] = Unset )
    :   Field =

      Field(required, repeatable, kebab(keyword), fieldType, default)

    private inline def selectRef
      ( reference:  String,
        required:   Polarity = Implicit,
        repeatable: Polarity = Implicit )
    :   SelectRef =

      SelectRef(required, repeatable, kebab(reference))

    private inline def variant(keyword: String, variantType: Type): Variant =
      Variant(kebab(keyword), variantType)

    // A Definition's `description` mirrors a single-line source atom in
    // the canonical document. Under §14 "Convention A" a source atom's text
    // carries no trailing LF, so the description is stored verbatim.
    private inline def describe(text: String): Text = Text(text)

    private inline def record(name: String, description: String, members: Member*)
    :   RecordDefinition =

      RecordDefinition(kebab(name), IArray.from(members), IArray.empty, describe(description))

    private inline def scalar(name: String, validators: String*): ScalarDefinition =
      ScalarDefinition(kebab(name), IArray.from(validators.map(kebab)))

    private inline def select(name: String, description: String, variants: Variant*)
    :   SelectDefinition =

      SelectDefinition(kebab(name), IArray.from(variants), IArray.empty, describe(description))

    // Built-in scalar types referenced from member declarations.
    private val identifierRef: Type = Reference(kebab("Identifier"))
    private val typeNameRef:   Type = Reference(kebab("TypeName"))
    private val sigilRef:      Type = Reference(kebab("Sigil"))
    private val stringRef:     Type = Reference(kebab("String"))

    // The schema's root struct, mirroring the `document` block in the
    // canonical tel-schema.tel.
    private val documentStruct: Struct = Struct(
      members = IArray(
        field("name",     identifierRef),
        field("sigil",    sigilRef,                              required = Loose),
        field("record",   Reference(kebab("Record")), required = Loose, repeatable = Loose),
        field("scalar",   Reference(kebab("Scalar")), required = Loose, repeatable = Loose),
        field("select",   Reference(kebab("Select")), required = Loose, repeatable = Loose),
        field("document", Reference(kebab("Body"))),
        field("layer", Reference(kebab("Layer")), required = Loose, repeatable = Loose)),
      validators = IArray.empty)

    val tels: Tels = Tels(
      name     = kebab("tel-schema"),
      document = documentStruct,
      layers   = IArray.empty,
      sigil    = Unset,
      records  = IArray(
        record("Field",
          "A field declaration at a member position.",
          field("keyword",      identifierRef),
          field("type",         typeNameRef),
          field("optional",     Flag,       required = Loose),
          field("required",     Flag,       required = Loose),
          field("repeatable",   Flag,       required = Loose),
          field("irrepeatable", Flag,       required = Loose),
          field("default",      stringRef,  required = Loose),
          field("description",  stringRef,  required = Loose)),

        record("SelectRef",
          "A select declaration at a member position, referencing a top-level SelectDefinition.",
          field("reference",    typeNameRef),
          field("optional",     Flag, required = Loose),
          field("required",     Flag, required = Loose),
          field("repeatable",   Flag, required = Loose),
          field("irrepeatable", Flag, required = Loose)),

        record("Variant",
          "A variant declaration inside a Select body.",
          field("keyword",     identifierRef),
          field("type",        typeNameRef),
          field("description", stringRef, required = Loose)),

        record("Record",
          "A record declaration: a named struct definition.",
          field("name", typeNameRef),
          selectRef("Member", required = Loose, repeatable = Loose),
          field("description", stringRef, required = Loose)),

        record("Scalar",
          "A scalar declaration: a named scalar definition with one or more validators.",
          field("name",        typeNameRef),
          field("validate",    identifierRef, repeatable = Loose),
          field("description", stringRef, required = Loose)),

        record("Select",
          "A top-level select declaration: a named sum type.",
          field("name", typeNameRef),
          selectRef("SelectChild", repeatable = Loose),
          field("description", stringRef, required = Loose)),

        record("Body",
          "The shared struct shape used by document and overlay.",
          selectRef("Member", required = Loose, repeatable = Loose)),

        record("Layer",
          "A layer declaration: per-layer definitions and an optional overlay.",
          field("name",    identifierRef),
          field("record",  Reference(kebab("Record")), required = Loose, repeatable = Loose),
          field("scalar",  Reference(kebab("Scalar")), required = Loose, repeatable = Loose),
          field("select",  Reference(kebab("Select")), required = Loose, repeatable = Loose),
          field("overlay", Reference(kebab("Body")), required = Loose))),
      scalars  = IArray(
        scalar("Identifier", "identifier"),
        scalar("TypeName",   "type-name"),
        scalar("Sigil",      "sigil"),
        scalar("String",     "string")),
      selects  = IArray(
        select("Member",
          "Members admissible inside a struct-shaped body: a field, select, or validator.",
          variant("field",    Reference(kebab("Field"))),
          variant("select",   Reference(kebab("SelectRef"))),
          variant("validate", identifierRef)),
        select("SelectChild",
          "Children admissible inside a Select body: a variant, exclude, or validator.",
          variant("variant",  Reference(kebab("Variant"))),
          variant("exclude",  identifierRef),
          variant("validate", identifierRef))))

  // Bridges the schema-aware semantic model into the existing
  // presentation-model-driven Tel.as[T] decoder.
  object Decoder:
    extension (tel: Tel)
      // Validate `tel` against the schema in scope and return it for chaining.
      // Under the default fail-fast tactic this raises a TelError on the first
      // E2xx/E3xx violation; under a `validate[Tel.Focus]` boundary the document-
      // level violations (unknown keyword, missing required member, failed
      // validator, flag-with-content) accrue. Returns the same `tel` on success.
      def validate(using schema: Tels): Tel raises TelError tracks Tel.Focus =
        Tel.Type.assign(tel, schema)
        tel

      // Same as `validate` but also applies the registry's validators.
      def validate(using schema: Tels, validators: Tel.Validator.Registry)
      :   Tel raises TelError tracks Tel.Focus =

        Tel.Type.assign(tel, schema, validators)
        tel

      // Convenience: validate-then-decode in a single call.
      inline def asValidated[value: Decodable in Tel](using schema: Tels)
      :   value raises TelError tracks Tel.Focus =

        Tel.Type.assign(tel, schema)
        tel.as[value]

  // Layer composition per §20.3. Takes a base schema and applies its
  // ordered layer list, producing a flat composed Tels.
  object Layers:

    // Top-level entry: applies every layer in `schema.layers` to the
    // schema's base, returning a composed Schema with empty `layers`.
    def compose(schema: Tels): Tels raises TelError =
      if schema.layers.nil then schema
      else
        val seenLayerNames = scala.collection.mutable.HashSet.empty[Text]
        var composed = schema.copy(layers = IArray.empty)
        var i = 0

        while i < schema.layers.length do
          val layer = schema.layers(i)
          if !seenLayerNames.add(layer.name) then abort(TelError(Reason.DuplicateLayerName))
          composed = applyLayer(composed, layer)
          i += 1

        composed

    private def applyLayer(base: Tels, layer: Layer): Tels raises TelError =
      val mergedRecords = mergeRecordList(base.records, layer.records, base.scalars, base.selects)
      val mergedScalars = mergeScalarList(base.scalars, layer.scalars, mergedRecords, base.selects)
      val mergedSelects = mergeSelectList(base.selects, layer.selects, mergedRecords, mergedScalars)
      val mergedDocument = mergeStruct(base.document, layer.overlay)

      base.copy
        ( document = mergedDocument,
          records  = mergedRecords,
          scalars  = mergedScalars,
          selects  = mergedSelects )

    private def mergePolarity(base: Polarity, layer: Polarity, axis: PolarityAxis)
    :   Polarity raises TelError =

      (base, layer) match
        case (b, Polarity.Implicit)              => b
        case (_, Polarity.Tight)                 => Polarity.Tight
        case (Polarity.Loose, Polarity.Loose)    => Polarity.Loose

        case (_, Polarity.Loose) => axis match
          case PolarityAxis.Required   => abort(TelError(Reason.LayerLoosenRequired))
          case PolarityAxis.Repeatable => abort(TelError(Reason.LayerLoosenRepeatable))

    private enum PolarityAxis:
      case Required, Repeatable

    private def mergeStruct(base: Struct, layer: Struct): Struct raises TelError =
      val members = scala.collection.mutable.ArrayBuffer.from(base.members.toList)

      val keywordToIndex = scala.collection.mutable.HashMap.from(
        members.zipWithIndex.collect:
          case (f: Field, idx)     => f.keyword -> idx
          case (s: SelectRef, idx) => s.reference -> idx)

      var i = 0

      while i < layer.members.length do
        layer.members(i) match
          case f: Field =>
            keywordToIndex.get(f.keyword) match
              case Some(idx) =>
                members(idx) match
                  case existing: Field =>
                    members(idx) =
                      Field
                        ( required   = mergePolarity(existing.required, f.required,
                                        PolarityAxis.Required),
                         repeatable = mergePolarity(existing.repeatable, f.repeatable,
                                        PolarityAxis.Repeatable),
                         keyword     = f.keyword,
                         fieldType   = existing.fieldType,
                         default     = existing.default,
                         // §20.3: a layer's non-null description overrides
                         // the base's; otherwise the base's is inherited.
                         description = f.description.or(existing.description) )

                  case _ => abort(TelError(Reason.LayerFieldTypeMismatch))

              case None =>
                keywordToIndex(f.keyword) = members.length
                members += f

          case s: SelectRef =>
            keywordToIndex.get(s.reference) match
              case Some(idx) =>
                members(idx) match
                  case existing: SelectRef if existing.reference == s.reference =>
                    members(idx) =
                      SelectRef
                        ( required   = mergePolarity(existing.required, s.required,
                                         PolarityAxis.Required),
                          repeatable = mergePolarity(existing.repeatable, s.repeatable,
                                         PolarityAxis.Repeatable),
                          reference  = s.reference )

                  case _ => abort(TelError(Reason.LayerKeywordCollision))

              case None =>
                keywordToIndex(s.reference) = members.length
                members += s

          case _: Exclude => abort(TelError(Reason.ExcludeOutsideSelect))

        i += 1

      val mergedValidators = (base.validators ++ layer.validators).distinct
      Struct(IArray.from(members), IArray.from(mergedValidators))

    private def mergeRecordList
      ( base:     IArray[RecordDefinition],
       layer:    IArray[RecordDefinition],
       scalars:  IArray[ScalarDefinition],
       selects:  IArray[SelectDefinition] )
    :   IArray[RecordDefinition] raises TelError =

      val out = scala.collection.mutable.ArrayBuffer.from(base.toList)
      var i = 0

      while i < layer.length do
        val newDef = layer(i)
        val existing = out.indexWhere(_.name == newDef.name)

        if existing >= 0 then
          out(existing) = mergeRecord(out(existing), newDef)
        else
          if scalars.exists(_.name == newDef.name) || selects.exists(_.name == newDef.name)
          then abort(TelError(Reason.DuplicateDefinition))

          out += newDef

        i += 1

      IArray.from(out)

    private def mergeRecord(base: RecordDefinition, layer: RecordDefinition)
    :   RecordDefinition raises TelError =

      val baseStruct  = Struct(base.members, base.validators)
      val layerStruct = Struct(layer.members, layer.validators)
      val merged      = mergeStruct(baseStruct, layerStruct)

      RecordDefinition(base.name, merged.members, merged.validators,
          layer.description.or(base.description))

    private def mergeScalarList
      ( base:    IArray[ScalarDefinition],
       layer:   IArray[ScalarDefinition],
       records: IArray[RecordDefinition],
       selects: IArray[SelectDefinition] )
    :   IArray[ScalarDefinition] raises TelError =

      val out = scala.collection.mutable.ArrayBuffer.from(base.toList)
      var i = 0

      while i < layer.length do
        val newDef = layer(i)
        val existing = out.indexWhere(_.name == newDef.name)

        if existing >= 0 then
          val mergedValidators = (out(existing).validators ++ newDef.validators).distinct

          out(existing) = ScalarDefinition(newDef.name, IArray.from(mergedValidators),
              newDef.description.or(out(existing).description))
        else
          if records.exists(_.name == newDef.name) || selects.exists(_.name == newDef.name)
          then abort(TelError(Reason.DuplicateDefinition))

          out += newDef

        i += 1

      IArray.from(out)

    private def mergeSelectList
      ( base:    IArray[SelectDefinition],
       layer:   IArray[SelectDefinition],
       records: IArray[RecordDefinition],
       scalars: IArray[ScalarDefinition] )
    :   IArray[SelectDefinition] raises TelError =

      val out = scala.collection.mutable.ArrayBuffer.from(base.toList)
      var i = 0

      while i < layer.length do
        val newDef = layer(i)
        val existing = out.indexWhere(_.name == newDef.name)

        if existing >= 0 then
          out(existing) = mergeSelect(out(existing), newDef)
        else
          if records.exists(_.name == newDef.name) || scalars.exists(_.name == newDef.name)
          then abort(TelError(Reason.DuplicateDefinition))

          out += newDef

        i += 1

      IArray.from(out)

    private def mergeSelect(base: SelectDefinition, layer: SelectDefinition)
    :   SelectDefinition raises TelError =

      val variants = scala.collection.mutable.ArrayBuffer.from(base.variants.toList)
      var i = 0

      while i < layer.variants.length do
        val v = layer.variants(i)
        val existingIdx = variants.indexWhere(_.keyword == v.keyword)
        if existingIdx < 0 then abort(TelError(Reason.LayerVariantAddition))
        i += 1

      val mergedValidators = (base.validators ++ layer.validators).distinct

      SelectDefinition(base.name, IArray.from(variants), IArray.from(mergedValidators),
          layer.description.or(base.description))

  // Inverse of the §20.5 schema-of-schemas: given a Tel.Document whose
  // surface matches the canonical tel-schema vocabulary, reconstruct
  // a Tels value.
  object Reconstructor:

    // Deep structural equality for Tels values.
    def equivalent(a: Tels, b: Tels): Boolean =
      a.name == b.name &&
        a.sigil == b.sigil &&
        structEq(a.document, b.document) &&
        seqEq(a.records, b.records, recordEq) &&
        seqEq(a.scalars, b.scalars, scalarEq) &&
        seqEq(a.selects, b.selects, selectEq) &&
        seqEq(a.layers, b.layers, layerEq)

    private def seqEq[T](a: IArray[T], b: IArray[T], eq: (T, T) => Boolean): Boolean =
      a.length == b.length && (0 until a.length).forall: i => eq(a(i), b(i))

    private def structEq(a: Struct, b: Struct): Boolean =
      seqEq(a.members, b.members, memberEq) &&
        seqEq(a.validators, b.validators, textEq)

    private def textEq(a: Text, b: Text): Boolean = a == b

    private def memberEq(a: Member, b: Member): Boolean = (a, b) match
      case (a: Field, b: Field) =>
        a.required == b.required && a.repeatable == b.repeatable &&
          a.keyword == b.keyword && typeEq(a.fieldType, b.fieldType) &&
          a.default == b.default && a.description == b.description

      case (a: SelectRef, b: SelectRef) =>
        a.required == b.required && a.repeatable == b.repeatable && a.reference == b.reference

      case (a: Exclude, b: Exclude) => a.keyword == b.keyword
      case _                        => false

    private def typeEq(a: Type, b: Type): Boolean = (a, b) match
      case (a: Struct, b: Struct)         => structEq(a, b)
      case (a: Scalar, b: Scalar)         => seqEq(a.validators, b.validators, textEq)
      case (Flag, Flag)                   => true
      case (Reference(n1), Reference(n2)) => n1 == n2
      case _                              => false

    private def recordEq(a: RecordDefinition, b: RecordDefinition): Boolean =
      a.name == b.name && seqEq(a.members, b.members, memberEq) &&
        seqEq(a.validators, b.validators, textEq) &&
        a.description == b.description

    private def scalarEq(a: ScalarDefinition, b: ScalarDefinition): Boolean =
      a.name == b.name && seqEq(a.validators, b.validators, textEq) &&
        a.description == b.description

    private def selectEq(a: SelectDefinition, b: SelectDefinition): Boolean =
      a.name == b.name &&
        seqEq(a.variants, b.variants, (x, y) => x.keyword == y.keyword &&
          typeEq(x.variantType, y.variantType) && x.description == y.description) &&
        seqEq(a.validators, b.validators, textEq) &&
        a.description == b.description

    private def layerEq(a: Layer, b: Layer): Boolean =
      a.name == b.name && structEq(a.overlay, b.overlay) &&
        seqEq(a.records, b.records, recordEq) &&
        seqEq(a.scalars, b.scalars, scalarEq) &&
        seqEq(a.selects, b.selects, selectEq)

    def fromTel(tel: Tel): Tels raises TelError =
      val compounds: IArray[Tel.Compound] = tel.subtree.children.flatMap(_.compounds)

      var name: Optional[Text] = Unset
      var sigil: Optional[Char] = Unset
      var documentStruct: Optional[Struct] = Unset
      val records  = scala.collection.mutable.ArrayBuffer.empty[RecordDefinition]
      val scalars  = scala.collection.mutable.ArrayBuffer.empty[ScalarDefinition]
      val selects  = scala.collection.mutable.ArrayBuffer.empty[SelectDefinition]
      val layers   = scala.collection.mutable.ArrayBuffer.empty[Layer]

      var i = 0

      while i < compounds.length do
        val c = compounds(i)

        c.keyword.s match
          case "name"     => name = firstAtomText(c)

          case "sigil" =>
            val s = firstAtomText(c)
            sigil = if s.absent then Unset else Optional(s.vouch.s.charAt(0))

          case "record"   => records  += parseRecord(c)
          case "scalar"   => scalars  += parseScalar(c)
          case "select"   => selects  += parseSelect(c)
          case "document" => documentStruct = parseBody(c)
          case "layer"    => layers   += parseLayer(c)
          case _          => abort(TelError(Reason.UnknownKeyword))

        i += 1

      val builtinScalars =
        IArray
          ( ScalarDefinition(t"Identifier", IArray(t"identifier")),
            ScalarDefinition(t"TypeName",   IArray(t"type-name")),
            ScalarDefinition(t"Sigil",      IArray(t"sigil")),
            ScalarDefinition(t"String",     IArray(t"string")) )

      Tels
        ( name     = name.or(abort(TelError(Reason.RequiredMemberAbsent))),
          document = documentStruct.or(abort(TelError(Reason.RequiredMemberAbsent))),
          layers   = IArray.from(layers),
          sigil    = sigil,
          records  = IArray.from(records),
          scalars  = builtinScalars ++ IArray.from(scalars),
          selects  = IArray.from(selects) )

    private def firstAtomText(c: Tel.Compound): Optional[Text] =
      val texts = c.atoms.collect { case Tel.Atom.Inline(t, _) => t }
      if texts.nil then Unset else texts(0): Optional[Text]

    private def atomTexts(c: Tel.Compound): IArray[Text] =
      c.atoms.collect { case Tel.Atom.Inline(t, _) => t }

    private def childCompounds(c: Tel.Compound): IArray[Tel.Compound] =
      c.children.flatMap(_.compounds)

    private def parseType(name: Text): Type =
      if name == t"Flag" then Flag else Reference(name)

    // The text of a scalar-valued child compound, taking its first atom
    // (inline, source, or literal) — used for both `default` and the §20
    // `description` child, whose prose is typically a source atom (§14).
    private def scalarAtomText(c: Tel.Compound): Optional[Text] =
      if c.atoms.nil then Unset else c.atoms(0) match
        case Tel.Atom.Inline(t, _)  => t
        case Tel.Atom.Source(t)     => t
        case Tel.Atom.Literal(_, t) => t

    // The optional §20 `description` of a Definition/Field/Variant: the
    // text of its `description` child compound, or `Unset` if absent.
    private def descriptionOf(children: IArray[Tel.Compound]): Optional[Text] =
      children.find(_.keyword == t"description") match
        case Some(c) => scalarAtomText(c)
        case None    => Unset

    private def parseRecord(c: Tel.Compound): RecordDefinition raises TelError =
      val recName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
      val children = childCompounds(c)
      val (members, validators) = parseMembersAndValidators(children)
      RecordDefinition(recName, members, validators, descriptionOf(children))

    private def parseScalar(c: Tel.Compound): ScalarDefinition raises TelError =
      val scName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
      val children = childCompounds(c)

      val validators = children.flatMap: cc =>
        if cc.keyword == t"validate" then atomTexts(cc) else IArray.empty[Text]

      ScalarDefinition(scName, validators, descriptionOf(children))

    private def parseSelect(c: Tel.Compound): SelectDefinition raises TelError =
      val seName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
      val children   = childCompounds(c)
      val variants   = scala.collection.mutable.ArrayBuffer.empty[Variant]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]

      children.each: cc =>
        cc.keyword.s match
          case "validate"    => validators ++= atomTexts(cc)
          case "exclude"     => ()
          case "description" => ()

          case "variant" =>
            val ats = atomTexts(cc)
            if ats.length < 2 then abort(TelError(Reason.RequiredMemberAbsent))
            variants += Variant(ats(0), parseType(ats(1)), descriptionOf(childCompounds(cc)))

          case _ =>
            abort(TelError(Reason.UnknownKeyword))

      SelectDefinition
        ( seName, IArray.from(variants), IArray.from(validators), descriptionOf(children) )

    private def parseBody(c: Tel.Compound): Optional[Struct] raises TelError =
      val (members, validators) = parseMembersAndValidators(childCompounds(c))
      Optional(Struct(members, validators))

    private def parseLayer(c: Tel.Compound): Layer raises TelError =
      val lyName = firstAtomText(c).or(abort(TelError(Reason.RequiredMemberAbsent)))
      val recs = scala.collection.mutable.ArrayBuffer.empty[RecordDefinition]
      val scs  = scala.collection.mutable.ArrayBuffer.empty[ScalarDefinition]
      val sels = scala.collection.mutable.ArrayBuffer.empty[SelectDefinition]
      var overlay: Optional[Struct] = Struct(IArray.empty, IArray.empty)

      childCompounds(c).each: cc =>
        cc.keyword.s match
          case "record"  => recs += parseRecord(cc)
          case "scalar"  => scs  += parseScalar(cc)
          case "select"  => sels += parseSelect(cc)
          case "overlay" => overlay = parseBody(cc)
          case _         => abort(TelError(Reason.UnknownKeyword))

      Layer
        ( name    = lyName,
          overlay = overlay.or(Struct(IArray.empty, IArray.empty)),
          records = IArray.from(recs),
          scalars = IArray.from(scs),
          selects = IArray.from(sels) )

    private def parseMembersAndValidators(compounds: IArray[Tel.Compound])
    :   (IArray[Member], IArray[Text]) raises TelError =

      val members    = scala.collection.mutable.ArrayBuffer.empty[Member]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]

      compounds.each: cc =>
        cc.keyword.s match
          case "field"    => members += parseField(cc)
          case "select"   => members += parseSelectRef(cc)
          case "validate" => validators ++= atomTexts(cc)

          case "exclude" =>
            val ats = atomTexts(cc)
            if ats.length >= 1 then members += Exclude(ats(0))

          // The Definition's own `description` (§20); consumed by the
          // enclosing parseRecord/parseBody, not a member.
          case "description" => ()
          case _             => abort(TelError(Reason.UnknownKeyword))

      (IArray.from(members), IArray.from(validators))

    private def parseField(c: Tel.Compound): Field raises TelError =
      val ats = atomTexts(c)
      if ats.length < 2 then abort(TelError(Reason.RequiredMemberAbsent))
      val keyword = ats(0)
      val fieldType = parseType(ats(1))

      var required:   Polarity = Polarity.Implicit
      var repeatable: Polarity = Polarity.Implicit
      var default:    Optional[Text] = Unset

      var j = 2

      while j < ats.length do
        ats(j).s match
          case "optional"     => required   = Polarity.Loose
          case "required"     => required   = Polarity.Tight
          case "repeatable"   => repeatable = Polarity.Loose
          case "irrepeatable" => repeatable = Polarity.Tight

          case "default" =>
            if j + 1 < ats.length then
              default = ats(j + 1): Optional[Text]
              j += 1

          case _              => ()

        j += 1

      Field(required, repeatable, keyword, fieldType, default,
          descriptionOf(childCompounds(c)))

    private def parseSelectRef(c: Tel.Compound): SelectRef raises TelError =
      val ats = atomTexts(c)
      if ats.length < 1 then abort(TelError(Reason.RequiredMemberAbsent))
      val reference = ats(0)

      var required:   Polarity = Polarity.Implicit
      var repeatable: Polarity = Polarity.Implicit

      var j = 1

      while j < ats.length do
        ats(j).s match
          case "optional"     => required   = Polarity.Loose
          case "required"     => required   = Polarity.Tight
          case "repeatable"   => repeatable = Polarity.Loose
          case "irrepeatable" => repeatable = Polarity.Tight
          case _              => ()

        j += 1

      SelectRef(required, repeatable, reference)

  // Inverse of `Tel.Type.assign` for schema documents: reconstruct a
  // `Tels` from the type-assigned semantic model produced by decoding an
  // embedded schema body (BinTEL §6.2) under the hardwired `tel-schema`
  // axiom. The element children are in §7.2 canonical order — grouped by
  // member, source order within a member — so iterating them in order
  // rebuilds each member sequence. The flat keyword indices below mirror
  // the member layout of `Tels.Axiom` (the schema-of-schemas).
  object SemanticReconstructor:

    def fromElement(root: Tel.Element): Tels raises TelError =
      val ch = childrenOf(root)
      // Document struct: name=0, sigil=1, record=2, scalar=3, select=4,
      // document=5, layer=6.
      val name = textAt(ch, 0).or(abort(TelError(Reason.RequiredMemberAbsent)))

      val sigil: Optional[Char] = textAt(ch, 1) match
        case t: Text => if t.s.isEmpty then Unset else Optional(t.s.charAt(0))
        case _       => Unset

      val records  = nodesAt(ch, 2).map(recordFromElement)
      val scalars  = nodesAt(ch, 3).map(scalarFromElement)
      val selects  = nodesAt(ch, 4).map(selectFromElement)
      val document = nodeAt(ch, 5).let(bodyFromElement)
        .or(abort(TelError(Reason.RequiredMemberAbsent)))
      val layers   = nodesAt(ch, 6).map(layerFromElement)

      val builtinScalars =
        IArray
          ( ScalarDefinition(t"Identifier", IArray(t"identifier")),
            ScalarDefinition(t"TypeName",   IArray(t"type-name")),
            ScalarDefinition(t"Sigil",      IArray(t"sigil")),
            ScalarDefinition(t"String",     IArray(t"string")) )

      Tels(name, document, layers, sigil, records, builtinScalars ++ scalars, selects)

    private def typeFromText(name: Text): Type =
      if name == t"Flag" then Flag else Reference(name)

    private def childrenOf(element: Tel.Element): IArray[Tel.Element] = element match
      case Tel.Element.Node(_, _, c) => c
      case _                         => IArray.empty[Tel.Element]

    private def kidx(element: Tel.Element): Int = element match
      case Tel.Element.Node(i, _, _)  => i.or(0)
      case Tel.Element.Value(i, _, _) => i

    private def textAt(children: IArray[Tel.Element], idx: Int): Optional[Text] =
      var i = 0
      var result: Optional[Text] = Unset

      while i < children.length do
        children(i) match
          case Tel.Element.Value(j, _, t) if j == idx => result = t
          case _                                      => ()

        i += 1

      result

    private def nodesAt(children: IArray[Tel.Element], idx: Int): IArray[Tel.Element] =
      children.filter(kidx(_) == idx)

    private def nodeAt(children: IArray[Tel.Element], idx: Int): Optional[Tel.Element] =
      val found = nodesAt(children, idx)
      if found.nil then Unset else found(0)

    private def present(children: IArray[Tel.Element], idx: Int): Boolean =
      children.exists(kidx(_) == idx)

    // tight if the tightening flag is present, else loose if the loosening
    // flag is present, else implicit (§20 Polarity).
    private def polarity(children: IArray[Tel.Element], looseIdx: Int, tightIdx: Int): Polarity =
      if present(children, tightIdx) then Polarity.Tight
      else if present(children, looseIdx) then Polarity.Loose
      else Polarity.Implicit

    private def textsAt(children: IArray[Tel.Element], idx: Int): IArray[Text] =
      children.collect { case Tel.Element.Value(j, _, t) if j == idx => t }

    // Field meta: keyword=0, type=1, optional=2, required=3, repeatable=4,
    // irrepeatable=5, default=6, description=7.
    private def fieldFromElement(element: Tel.Element): Field =
      val ch = childrenOf(element)

      Field
        ( required    = polarity(ch, 2, 3),
          repeatable  = polarity(ch, 4, 5),
          keyword     = textAt(ch, 0).or(t""),
          fieldType   = typeFromText(textAt(ch, 1).or(t"")),
          default     = textAt(ch, 6),
          description = textAt(ch, 7) )

    // SelectRef meta: reference=0, optional=1, required=2, repeatable=3,
    // irrepeatable=4.
    private def selectRefFromElement(element: Tel.Element): SelectRef =
      val ch = childrenOf(element)
      SelectRef(polarity(ch, 1, 2), polarity(ch, 3, 4), textAt(ch, 0).or(t""))

    // Variant meta: keyword=0, type=1, description=2.
    private def variantFromElement(element: Tel.Element): Variant =
      val ch = childrenOf(element)
      Variant(textAt(ch, 0).or(t""), typeFromText(textAt(ch, 1).or(t"")), textAt(ch, 2))

    // Member group (field / select / validate at the given flat indices),
    // consumed in canonical order so members keep their source sequence.
    private def membersFromBody
      ( children: IArray[Tel.Element], fieldIdx: Int, selectIdx: Int, validateIdx: Int )
    :   (IArray[Member], IArray[Text]) =

      val members    = scala.collection.mutable.ArrayBuffer.empty[Member]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]
      var i = 0

      while i < children.length do
        val e = children(i)

        kidx(e) match
          case k if k == fieldIdx    => members += fieldFromElement(e)
          case k if k == selectIdx   => members += selectRefFromElement(e)

          case k if k == validateIdx => e match
            case Tel.Element.Value(_, _, t) => validators += t
            case _                          => ()

          case _ => ()

        i += 1

      (IArray.from(members), IArray.from(validators))

    // Record meta: name=0, Member{field=1, select=2, validate=3}, description=4.
    private def recordFromElement(element: Tel.Element): RecordDefinition =
      val ch = childrenOf(element)
      val (members, validators) = membersFromBody(ch, 1, 2, 3)
      RecordDefinition(textAt(ch, 0).or(t""), members, validators, textAt(ch, 4))

    // Scalar meta: name=0, validate=1, description=2.
    private def scalarFromElement(element: Tel.Element): ScalarDefinition =
      val ch = childrenOf(element)
      ScalarDefinition(textAt(ch, 0).or(t""), textsAt(ch, 1), textAt(ch, 2))

    // Select meta: name=0, SelectChild{variant=1, exclude=2, validate=3}, description=4.
    private def selectFromElement(element: Tel.Element): SelectDefinition =
      val ch = childrenOf(element)
      val variants   = scala.collection.mutable.ArrayBuffer.empty[Variant]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]
      var i = 0

      while i < ch.length do
        val e = ch(i)

        kidx(e) match
          case 1 => variants += variantFromElement(e)

          case 3 => e match
            case Tel.Element.Value(_, _, t) => validators += t
            case _                          => ()

          case _ => ()

        i += 1

      SelectDefinition(textAt(ch, 0).or(t""), IArray.from(variants), IArray.from(validators),
          textAt(ch, 4))

    // Body meta: Member{field=0, select=1, validate=2}.
    private def bodyFromElement(element: Tel.Element): Struct =
      val (members, validators) = membersFromBody(childrenOf(element), 0, 1, 2)
      Struct(members, validators)

    // Layer meta: name=0, record=1, scalar=2, select=3, overlay=4.
    private def layerFromElement(element: Tel.Element): Layer =
      val ch = childrenOf(element)

      Layer
        ( name    = textAt(ch, 0).or(t""),
          overlay = nodeAt(ch, 4).let(bodyFromElement).or(Struct(IArray.empty, IArray.empty)),
          records = nodesAt(ch, 1).map(recordFromElement),
          scalars = nodesAt(ch, 2).map(scalarFromElement),
          selects = nodesAt(ch, 3).map(selectFromElement) )

case class Tels
  ( name:     Text,
    document: Tels.Struct,
    layers:   IArray[Tels.Layer],
    sigil:    Optional[Char],
    records:  IArray[Tels.RecordDefinition],
    scalars:  IArray[Tels.ScalarDefinition],
    selects:  IArray[Tels.SelectDefinition] )

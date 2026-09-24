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

import scala.collection.immutable as sci

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import praxinoscope.Motif
import prepositional.*
import rudiments.*
import vacuous.*

import Tel.Error.Reason

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

  // `key` marks the identifying field of the enclosing Struct (§20:
  // E219–E221 at schema level, E314 at instance level; monotone under
  // layer merge). It is declared last for source compatibility with
  // positional construction — the TELS keyword order, in which `key`
  // precedes `default` (load-bearing for the atom phase, §20.5), is
  // fixed by the Axiom's member list, not by this parameter order.
  case class Field
    ( required:    Polarity,
      repeatable:  Polarity,
      keyword:     Text,
      fieldType:   Type,
      default:     Optional[Text],
      description: Optional[Text] = Unset,
      key:         Boolean = false )
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

  case class Struct(members: Array[Member]^{}, validators: Array[Text]^{}) extends Type

  // `encoding` names the codec (§21.7) defining the scalar's binary
  // representation in BinTEL, or is absent for UTF-8 text scalars.
  // `patterns` holds the RE2 pattern constraints of §21.8, in declaration
  // order; each matches the entire value text and they AND-conjoin, so the
  // accepted language is the intersection of theirs.
  case class Scalar
    ( validators: Array[Text]^{},
      encoding:   Optional[Text] = Unset,
      patterns:   Array[Text]^{} = Array.empty )
  extends Type

  case object Flag extends Type

  case class Reference(name: Text) extends Type

  // Definitions in the schema's namespace. They share a single namespace
  // (§20: E211 for cross-kind collisions). Each Definition optionally
  // carries validators applied to the entire instance, and an optional
  // free-text `description` (§20): semantic, survives BinTEL round-trips,
  // never validated.
  case class RecordDefinition
    ( name:        Text,
      members:     Array[Member]^{},
      validators:  Array[Text]^{},
      description: Optional[Text] = Unset )

  // §20: `validators` and `patterns` (§21.8) are each individually optional,
  // and a declaration carrying neither accepts every value, like the built-in
  // `String` — still a distinct named type for generated code, diagnostics and
  // later tightening (E224, which once forbade it, is withdrawn).
  case class ScalarDefinition
    ( name:        Text,
      validators:  Array[Text]^{},
      description: Optional[Text] = Unset,
      encoding:    Optional[Text] = Unset,
      patterns:    Array[Text]^{} = Array.empty )

  // `excludes` is layer-only: the variant keywords a layer's `exclude`
  // children remove from the merged SelectDefinition (§20.3). A base-side
  // SelectDefinition must have none (E216), and composition consumes them,
  // so a composed schema's excludes are always empty.
  case class SelectDefinition
    ( name:        Text,
      variants:    Array[Variant]^{},
      validators:  Array[Text]^{},
      description: Optional[Text] = Unset,
      excludes:    Array[Text]^{} = Array.empty )

  // A Layer applies incremental refinements per §20.3. `overlay` is the
  // (possibly empty) Struct merged into the document root; the three
  // Definition lists are merged into the composed namespace.
  case class Layer
    ( name:    Text,
      overlay: Struct,
      records: Array[RecordDefinition]^{},
      scalars: Array[ScalarDefinition]^{},
      selects: Array[SelectDefinition]^{} )

  // Predefined built-in type names per §20.5 / §21.5. Used by the
  // schema-of-schemas and any user schema that references them via
  // `Reference(TypeName)`.
  object Builtin:
    val String:     Text = t"String"
    val Identifier: Text = t"Identifier"
    val TypeName:   Text = t"TypeName"
    val Sigil:      Text = t"Sigil"
    val Flag:       Text = t"Flag"

  // Hand-encoded `tels` axiom per §20.5 of the TEL specification.
  // This Scala literal mirrors the canonical `tels.tel` document
  // (saved at `res/test/stratiform/corpus/tels.tel`) verbatim.
  object Axiom:
    import Polarity.*

    private inline def kebab(s: String): Text = Text(s)

    private inline def field
      ( keyword:    String,
        fieldType:  Type,
        required:   Polarity = Implicit,
        repeatable: Polarity = Implicit,
        default:    Optional[Text] = Unset,
        key:        Boolean = false )
    :   Field =

      Field(required, repeatable, kebab(keyword), fieldType, default, key = key)

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

      RecordDefinition(kebab(name), Array.from(members), Array.empty, describe(description))

    private inline def scalar(name: String, validators: String*): ScalarDefinition =
      ScalarDefinition(kebab(name), Array.from(validators.map(kebab)))

    private inline def select(name: String, description: String, variants: Variant*)
    :   SelectDefinition =

      SelectDefinition(kebab(name), Array.from(variants), Array.empty, describe(description))

    // Built-in scalar types referenced from member declarations.
    private val identifierRef: Type = Reference(kebab("Identifier"))
    private val typeNameRef:   Type = Reference(kebab("TypeName"))
    private val sigilRef:      Type = Reference(kebab("Sigil"))
    private val stringRef:     Type = Reference(kebab("String"))

    // The schema's root struct, mirroring the `document` block in the
    // canonical tels.tel.
    private val documentStruct: Struct = Struct(
      members = Array(
        field("name",     identifierRef),
        field("sigil",    sigilRef,                              required = Loose),
        field("record",   Reference(kebab("Record")), required = Loose, repeatable = Loose),
        field("scalar",   Reference(kebab("Scalar")), required = Loose, repeatable = Loose),
        field("select",   Reference(kebab("Select")), required = Loose, repeatable = Loose),
        field("document", Reference(kebab("Body"))),
        field("layer", Reference(kebab("Layer")), required = Loose, repeatable = Loose)),
      validators = Array.empty)

    val tels: Tels = Tels(
      name     = kebab("tels"),
      document = documentStruct,
      layers   = Array.empty,
      sigil    = Unset,
      records  = Array(
        record("Field",
          "A field declaration at a member position.",
          field("keyword",      identifierRef),
          field("type",         typeNameRef),
          field("optional",     Flag,       required = Loose),
          field("required",     Flag,       required = Loose),
          field("repeatable",   Flag,       required = Loose),
          field("irrepeatable", Flag,       required = Loose),
          // `key` must precede `default`, so a trailing `key` atom is
          // consumed as a flag rather than as a default value (§20.5).
          field("key",          Flag,       required = Loose),
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
          "A scalar declaration: a named scalar definition constrained by validators and/or RE2 patterns, with an optional encoding.",
          field("name",        typeNameRef),
          // §21.8 made `validate` optional and added `pattern`; both members are
          // optional, and a scalar may declare neither (E224 is withdrawn).
          field("validate",    identifierRef, required = Loose, repeatable = Loose),
          field("pattern",     stringRef,     required = Loose, repeatable = Loose),
          field("encoding",    identifierRef, required = Loose),
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
      scalars  = Array(
        scalar("Identifier", "identifier"),
        scalar("TypeName",   "type-name"),
        scalar("Sigil",      "sigil"),
        scalar("String",     "string")),
      selects  = Array(
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

    // The four scalars every schema document declares implicitly, in the
    // order `Reconstructor` prepends them, so that a hand-written axiom
    // is `Reconstructor.equivalent` to its reconstructed canonical source.
    private val builtinScalars: Array[ScalarDefinition]^{} = Array(
      scalar("Identifier", "identifier"),
      scalar("TypeName",   "type-name"),
      scalar("Sigil",      "sigil"),
      scalar("String",     "string"))

    // Hand-encoded `acceptance` schema per §8.4 of the BinTEL
    // specification, mirroring the canonical `acceptance.tel` (saved at
    // `res/test/stratiform/corpus/acceptance.tel`) verbatim. An
    // implementation that supports acceptances holds it built in, so that
    // a peer can parse an acceptance before any schema has been exchanged;
    // its two encodings are bound by `Tel.Codec.Bindings.builtins`.
    val acceptance: Tels = Tels(
      name     = kebab("acceptance"),
      document = Struct(
        members    = Array(field("accept", Reference(kebab("Alternative")), repeatable = Loose)),
        validators = Array.empty),
      layers   = Array.empty,
      sigil    = Unset,
      records  = Array(
        record("Alternative",
          "One composition the reader accepts, with the further components it can resolve.",
          field("schema",         Reference(kebab("Signature"))),
          field("self-contained", Flag, required = Loose),
          field("any-published",  Flag, required = Loose),
          field("component",      Reference(kebab("Component")), required = Loose,
            repeatable = Loose))),
      scalars  = Array.frozen(builtinScalars.readable ++ Array(
        ScalarDefinition
          ( kebab("Signature"),
            Array.empty,
            describe("A schema signature (BinTEL §8.2): a palimpsest of component hashes at the pinned parameters."),
            encoding = kebab("schema-signature") ),
        ScalarDefinition
          ( kebab("Component"),
            Array.empty,
            describe("A component hash, or a prefix of one at least four bytes long."),
            encoding = kebab("base-256"),
            patterns = Array(kebab(".{4,32}")) )).readable),
      selects  = Array.empty)

  // Bridges the schema-aware semantic model into the existing
  // presentation-model-driven Tel.as[T] decoder.
  object Decoder:
    extension (tel: Tel)
      // Validate `tel` against the schema in scope and return it for chaining.
      // Under the default fail-fast tactic this raises a Tel.Error on the first
      // E2xx/E3xx violation; under a `validate[Tel.Focus]` boundary the document-
      // level violations (unknown keyword, missing required member, failed
      // validator, flag-with-content) accrue. Returns the same `tel` on success.
      def validate(using schema: Tels): Tel raises Tel.Error tracks Tel.Focus =
        Tel.Type.assign(tel, schema)
        tel

      // Same as `validate` but also applies the registry's validators.
      def validate(using schema: Tels, validators: Tel.Validator.Registry)
      :   Tel raises Tel.Error tracks Tel.Focus =

        Tel.Type.assign(tel, schema, validators)
        tel

      // Same again, additionally checking declared encodings (§21.7)
      // against the codec bindings in scope.
      def validate
        ( using schema: Tels, validators: Tel.Validator.Registry, codecs: Tel.Codec.Bindings )
      :   Tel raises Tel.Error tracks Tel.Focus =

        Tel.Type.assign(tel, schema, validators, codecs)
        tel

      // Convenience: validate-then-decode in a single call.
      inline def asValidated[value: Decodable in Tel](using schema: Tels)
      :   value raises Tel.Error tracks Tel.Focus =

        Tel.Type.assign(tel, schema)
        tel.as[value]

  // The schema-resolution protocol of §8.2: the step taxonomy, the
  // content-addressed store abstraction serving steps 2–3, the step-4
  // delegate SPI (implemented by LIRA in reliquary), and the resolution
  // error, which sits outside the E1xx–E3xx taxonomy and identifies the
  // failing step. The engine itself lives in stratiform.binary
  // (`SchemaResolver`), since it needs signature computation.
  object Resolution:
    enum Step:
      case Embedded, Builtin, Cache, Library, Lira

    // Any content-addressed store may serve resolution steps 2–3: a
    // hash lookup is order-independent, because any store's answer for
    // a given signature is the right answer. Bare development
    // references resolve only through `reference`, against local state.
    trait Store:
      def apply(signature: Data): Optional[Data]
      def reference(domain: Text, name: Text): Optional[Data]
      def cache(signature: Data, body: Data): Unit

    object Store:
      // An in-memory content-addressed store: the process-lifetime
      // schema cache, and the test double. Signatures are keyed by
      // their BASE-256 rendering, since `Data` has no structural
      // equality.
      class Memory() extends Store:
        private val bySignature = scala.collection.concurrent.TrieMap.empty[Text, Data]
        private val byReference = scala.collection.concurrent.TrieMap.empty[Text, Data]

        def apply(signature: Data): Optional[Data] =
          bySignature.get(Base256.encode(signature)).getOrElse(Unset)

        def reference(domain: Text, name: Text): Optional[Data] =
          byReference.get(t"$domain/$name").getOrElse(Unset)

        def cache(signature: Data, body: Data): Unit =
          bySignature(Base256.encode(signature)) = body

        // Seed a bare development reference with a local working copy,
        // as `tel schema add` does for the developer's cache.
        def install(domain: Text, name: Text, body: Data): Unit =
          byReference(t"$domain/$name") = body

    // A resolved schema body. Wrapping the bytes keeps effectful
    // delegate results pure under capture checking (a bare
    // `Array[Byte]` result would carry a fresh read capability).
    case class Body(data: Data)

    // The step-4 SPI, replacing the deleted URL fetch: LIRA resolution
    // by identifier form. An implementation without network access
    // resolves from local state only and answers `Unset` otherwise.
    trait Delegate:
      // Signature form: resolve the signature's components by exact
      // identity and return the schema body serving it. If `reference`
      // is present, the resolved lineage must serve the signature — the
      // signature is authoritative, and the reference must agree.
      def bySignature(signature: Data, reference: Optional[Tel.Pragma.Reference])
      :   Optional[Body] raises Error

      // Selector form (`:version` or `:tag`): resolve a published,
      // signed release; the resolver must verify the release's
      // manifest signature rather than trusting a store index.
      def bySelector(reference: Tel.Pragma.Reference): Optional[Body] raises Error

    object Error:
      enum Reason:
        case UnknownLayer(layer: Text)
        case ComponentCount(expected: Int, found: Int)
        case BaseMismatch
        case LayerMismatch(layer: Text)
        case ReferenceDisagrees
        case Unresolved(step: Step, identifier: Text)
        case NotSchema(detail: Text)
        case Unverified(detail: Text)

      given communicable: Reason is Communicable =
        case Reason.UnknownLayer(layer) =>
          m"the selected layer '$layer' is not declared by the schema"

        case Reason.ComponentCount(expected, found) =>
          m"the signature has ${found.toString} components where ${expected.toString} were expected"

        case Reason.BaseMismatch =>
          m"the signature's base component does not match the schema's base hash"

        case Reason.LayerMismatch(layer) =>
          m"the signature's component for layer '$layer' does not match its hash"

        case Reason.ReferenceDisagrees =>
          m"the reference's lineage does not serve the authoritative signature"

        case Reason.Unresolved(step, identifier) =>
          m"'$identifier' was not resolved (failing at the ${step.toString.toLowerCase.nn.tt} step)"

        case Reason.NotSchema(detail) =>
          m"the resolved body is not a valid schema document: $detail"

        case Reason.Unverified(detail) =>
          m"signature verification failed: $detail"

    case class Error(reason: Resolution.Error.Reason)(using Diagnostics)
    extends fulminate.Error(611, reason.ordinal + 1)
      (m"the schema does not resolve because $reason")

  // Layer composition per §20.3. Takes a base schema and applies its
  // ordered layer list, producing a flat composed Tels.
  object Layers:

    // §8.1: compose the base plus exactly the named layer selection.
    // Unknown names and order violations are raised by `select`.
    def compose(schema: Tels, selection: List[Text])
    :   Tels raises Tel.Error raises Resolution.Error =

      val chosen = select(schema, selection)
      var composed = schema.copy(layers = Array.empty)

      chosen.each: layer =>
        composed = applyLayer(composed, layer)

      composed

    // The selected `Layer` values, validated: each name must be
    // declared by the schema (else `Resolution.Error`), and the
    // selection must be a subsequence of the declaration order — an
    // out-of-order or duplicate selection is E124, so each selected
    // layer set has exactly one canonical pragma spelling.
    def select(schema: Tels, selection: List[Text])
    :   List[Tels.Layer] raises Tel.Error raises Resolution.Error =

      val declared = schema.layers.readable
      val chosen = scala.collection.mutable.ListBuffer.empty[Tels.Layer]
      var cursor = 0

      selection.each: name =>
        if !declared.exists(_.name == name)
        then abort(Resolution.Error(Resolution.Error.Reason.UnknownLayer(name)))
        else
          var found = false

          while !found && cursor < declared.length do
            if declared(cursor).name == name then
              chosen += declared(cursor)
              found = true
            cursor += 1

          if !found then abort(Tel.Error(Reason.LayerOrderMismatch))

      chosen.toList.to(List)

    // Top-level entry: applies every layer in `schema.layers` to the
    // schema's base, returning a composed Schema with empty `layers`.
    def compose(schema: Tels): Tels raises Tel.Error =
      if schema.layers.nil then schema
      else
        val seenLayerNames = scala.collection.mutable.HashSet.empty[Text]
        var composed = schema.copy(layers = Array.empty)
        var i = 0

        while i < schema.layers.readable.length do
          val layer = schema.layers.readable(i)
          if !seenLayerNames.add(layer.name) then abort(Tel.Error(Reason.DuplicateLayerName))
          composed = applyLayer(composed, layer)
          i += 1

        composed

    // Composes a base with an ordered sequence of components — whole layers or the virtual
    // single-atom layers of `Atoms` — as §20.3 composes a component sequence: each is applied in
    // turn, and the constraints that concern the composed schema as a whole are checked once on
    // the result. Unlike `compose`, no name check applies: atoms carry no name, and an atom
    // listed twice applies twice, harmlessly.
    def composeComponents(base: Tels, components: List[Layer]): Tels raises Tel.Error =
      var composed = base.copy(layers = Array.empty)
      components.each { layer => composed = applyLayer(composed, layer) }
      Validation.checkComposed(composed)

    private[stratiform] def applyLayer(base: Tels, layer: Layer): Tels raises Tel.Error =
      val mergedRecords = mergeRecordList(base.records, layer.records, base.scalars, base.selects)
      val mergedScalars = mergeScalarList(base.scalars, layer.scalars, mergedRecords, base.selects)
      val mergedSelects = mergeSelectList(base.selects, layer.selects, mergedRecords, mergedScalars)
      val mergedDocument = mergeStruct(base.document, layer.overlay, mergedSelects)

      base.copy
        ( document = mergedDocument,
          records  = mergedRecords,
          scalars  = mergedScalars,
          selects  = mergedSelects )

    private def mergePolarity(base: Polarity, layer: Polarity, axis: PolarityAxis)
    :   Polarity raises Tel.Error =

      (base, layer) match
        case (b, Polarity.Implicit)              => b
        case (_, Polarity.Tight)                 => Polarity.Tight
        case (Polarity.Loose, Polarity.Loose)    => Polarity.Loose

        case (_, Polarity.Loose) => axis match
          case PolarityAxis.Required   => abort(Tel.Error(Reason.LayerLoosenRequired))
          case PolarityAxis.Repeatable => abort(Tel.Error(Reason.LayerLoosenRepeatable))

    private enum PolarityAxis:
      case Required, Repeatable

    private def mergeStruct(base: Struct, layer: Struct, selects: Array[SelectDefinition]^{})
    :   Struct raises Tel.Error =

      val members = scala.collection.mutable.ArrayBuffer.from(base.members.readable)

      // §20.1/§20.3: a Struct's keyword space is its Fields' keywords together
      // with the variant keywords its SelectRefs contribute, so a layer member
      // is matched against both. A SelectRef restated by name is found through
      // `referenceToIndex`.
      val keywordToIndex   = scala.collection.mutable.HashMap.empty[Text, Int]
      val referenceToIndex = scala.collection.mutable.HashMap.empty[Text, Int]

      def variantKeywords(reference: Text): scala.collection.immutable.List[Text] =
        selects.seek(_.name == reference).lay(scala.collection.immutable.Nil): definition =>
          var keywords = scala.collection.immutable.List.empty[Text]
          var v = definition.variants.length - 1

          while v >= 0 do
            keywords = definition.variants.readable(v).keyword :: keywords
            v -= 1

          keywords

      def register(index: Int, member: Member): Unit = member match
        case f: Field => keywordToIndex(f.keyword) = index

        case s: SelectRef =>
          referenceToIndex(s.reference) = index
          variantKeywords(s.reference).foreach { keyword => keywordToIndex(keyword) = index }

        case _: Exclude => ()

      var k = 0
      while k < members.length do { register(k, members(k)); k += 1 }

      var i = 0

      while i < layer.members.readable.length do
        layer.members.readable(i) match
          case f: Field =>
            keywordToIndex.get(f.keyword) match
              case Some(idx) =>
                members(idx) match
                  case existing: Field =>
                    // §20.3: a restated field's type must be structurally
                    // equal to the base's (a polarity-only refinement) or
                    // both must be Structs, which merge recursively;
                    // anything else is E206.
                    val mergedType = (existing.fieldType, f.fieldType) match
                      case (baseType: Struct, layerType: Struct) =>
                        mergeStruct(baseType, layerType, selects)

                      case (baseType, layerType) =>
                        if Reconstructor.typeEq(baseType, layerType) then baseType
                        else abort(Tel.Error(Reason.LayerFieldTypeMismatch))

                    members(idx) =
                      Field
                        ( required   = mergePolarity(existing.required, f.required,
                                        PolarityAxis.Required),
                         repeatable = mergePolarity(existing.repeatable, f.repeatable,
                                        PolarityAxis.Repeatable),
                         keyword     = f.keyword,
                         fieldType   = mergedType,
                         default     = existing.default,
                         // §20.3: a layer's non-null description overrides
                         // the base's; otherwise the base's is inherited.
                         description = f.description.or(existing.description),
                         // §20.3: monotone OR — a layer may mark a field
                         // as key; nothing can clear it.
                         key         = existing.key || f.key )

                  // §20.3 MergeStruct: the keyword matches a variant keyword
                  // contributed by an existing SelectRef — a collision (E205),
                  // not a type mismatch.
                  case _ => abort(Tel.Error(Reason.LayerKeywordCollision))

              case None =>
                register(members.length, f)
                members += f

          case s: SelectRef =>
            referenceToIndex.get(s.reference) match
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

                  case _ => abort(Tel.Error(Reason.LayerKeywordCollision))

              case None =>
                // §20.1: the SelectRef brings its variant keywords into the
                // Struct's keyword space; one already present is E205.
                variantKeywords(s.reference).foreach: keyword =>
                  if keywordToIndex.contains(keyword)
                  then abort(Tel.Error(Reason.LayerKeywordCollision))

                register(members.length, s)
                members += s

          case _: Exclude => abort(Tel.Error(Reason.ExcludeOutsideSelect))

        i += 1

      val mergedValidators = Array.frozen((base.validators.readable ++ layer.validators.readable).distinct)
      Struct(Array.from(members), mergedValidators)

    private def mergeRecordList
      ( base:     Array[RecordDefinition]^{},
       layer:    Array[RecordDefinition]^{},
       scalars:  Array[ScalarDefinition]^{},
       selects:  Array[SelectDefinition]^{} )
    :   Array[RecordDefinition]^{} raises Tel.Error =

      val out = scala.collection.mutable.ArrayBuffer.from(base.readable)
      var i = 0

      while i < layer.readable.length do
        val newDef = layer.readable(i)
        val existing = out.indexWhere(_.name == newDef.name)

        if existing >= 0 then
          out(existing) = mergeRecord(out(existing), newDef, selects)
        else
          if scalars.exists(_.name == newDef.name) || selects.exists(_.name == newDef.name)
          then abort(Tel.Error(Reason.DuplicateDefinition))

          out += newDef

        i += 1

      Array.from(out)

    private def mergeRecord
      ( base: RecordDefinition, layer: RecordDefinition, selects: Array[SelectDefinition]^{} )
    :   RecordDefinition raises Tel.Error =

      val baseStruct  = Struct(base.members, base.validators)
      val layerStruct = Struct(layer.members, layer.validators)
      val merged      = mergeStruct(baseStruct, layerStruct, selects)

      RecordDefinition(base.name, merged.members, merged.validators,
          layer.description.or(base.description))

    private def mergeScalarList
      ( base:    Array[ScalarDefinition]^{},
       layer:   Array[ScalarDefinition]^{},
       records: Array[RecordDefinition]^{},
       selects: Array[SelectDefinition]^{} )
    :   Array[ScalarDefinition]^{} raises Tel.Error =

      val out = scala.collection.mutable.ArrayBuffer.from(base.readable)
      var i = 0

      while i < layer.readable.length do
        val newDef = layer.readable(i)
        val existing = out.indexWhere(_.name == newDef.name)

        if existing >= 0 then
          val mergedValidators = Array.frozen((out(existing).validators.readable ++ newDef.validators.readable).distinct)

          // §20.3/§21.7: an encoding, once declared, cannot be changed
          // (E218). Restating the base's encoding is a benign no-op;
          // declaring one where the base has none adds it. Removal has
          // no syntax.
          val mergedEncoding = out(existing).encoding.lay(newDef.encoding): base =>
            newDef.encoding.let { layer => if layer != base then abort(Tel.Error(Reason.EncodingConflict)) }
            base

          val mergedPatterns = mergePatterns(out(existing).patterns, newDef.patterns)

          out(existing) = ScalarDefinition(newDef.name, mergedValidators,
              newDef.description.or(out(existing).description), mergedEncoding,
              mergedPatterns)
        else
          if records.exists(_.name == newDef.name) || selects.exists(_.name == newDef.name)
          then abort(Tel.Error(Reason.DuplicateDefinition))

          out += newDef

        i += 1

      Array.from(out)

    // §20.3's checked replacement: a layer's `pattern` lines *replace* the
    // inherited list rather than appending to it, subject to the containment
    // premise `L(⋂new) ⊆ L(⋂old)`. Patterns are the one constraint whose
    // semantics the composition rules can inspect — validator names are opaque
    // and therefore append-only — and RE2's decidable containment is what makes
    // inspecting them sound.
    //
    // The first three cases avoid asking the (potentially budget-exhausting)
    // containment question at all.
    private def mergePatterns(inherited: Array[Text]^{}, replacing: Array[Text]^{})
    :   Array[Text]^{} raises Tel.Error =

      // A layer with no `pattern` lines inherits the base's list unchanged.
      if replacing.nil then inherited

      // Restating a textually identical list is a benign no-op, and needs no
      // containment decision — which is what lets a layer restate a pattern the
      // analysis could not decide.
      else if sameTexts(replacing, inherited) then inherited

      // An inherited empty list denotes Σ*, so any first patterns are contained.
      else if inherited.nil then replacing

      else if contained(replacing, inherited) then replacing

      // Fail closed, retaining the inherited list. `raise` rather than `abort`
      // so that under an accrual boundary §20.3's "retain the inherited list"
      // is literally what happens; under the default fail-fast tactic this
      // aborts exactly as the neighbouring E218 check does.
      else raise(Tel.Error(Reason.PatternNotContained)) yet inherited

    private def sameTexts(left: Array[Text]^{}, right: Array[Text]^{}): Boolean =
      left.length == right.length && (0 until left.length).forall: index =>
        left.readUnchecked(index) == right.readUnchecked(index)

    // `∀ Pᵢ ∈ inherited : L(⋂replacing) ⊆ L(Pᵢ)`, which §20.3 gives as the way
    // to decide `L(⋂new) ⊆ L(⋂old)`.
    //
    // Every failure mode is treated as *not proven* and so reports E223: a
    // budget exhaustion (§21.8 requires exactly this), a word boundary the
    // analysis cannot model, or a pattern that does not compile — the last
    // being unreachable once `checkBase` has run, but fail-closed regardless.
    private[stratiform] def contained(replacing: Array[Text]^{}, inherited: Array[Text]^{}): Boolean =
      val motifs = scala.collection.mutable.ArrayBuffer.empty[Motif]
      var compiled = true

      replacing.each: pattern =>
        Patterns.compile(pattern) match
          case motif: Motif => motifs += motif
          case _            => compiled = false

      if !compiled then false else
        val candidates = motifs.to(List)
        var holds = true

        inherited.each: pattern =>
          if holds then
            holds = Patterns.compile(pattern).lay(false): cover =>
              safely[Motif.Error](cover.subsumes(candidates)).or(false)

        holds

    private def mergeSelectList
      ( base:    Array[SelectDefinition]^{},
       layer:   Array[SelectDefinition]^{},
       records: Array[RecordDefinition]^{},
       scalars: Array[ScalarDefinition]^{} )
    :   Array[SelectDefinition]^{} raises Tel.Error =

      val out = scala.collection.mutable.ArrayBuffer.from(base.readable)
      var i = 0

      while i < layer.readable.length do
        val newDef = layer.readable(i)
        val existing = out.indexWhere(_.name == newDef.name)

        if existing >= 0 then
          out(existing) = mergeSelect(out(existing), newDef)
        else
          if records.exists(_.name == newDef.name) || scalars.exists(_.name == newDef.name)
          then abort(Tel.Error(Reason.DuplicateDefinition))

          // A layer-introduced fresh SelectDefinition has no base to
          // exclude from, so any exclude it carries is E211.
          if !newDef.excludes.nil then abort(Tel.Error(Reason.ExcludeMissingVariant))
          out += newDef

        i += 1

      Array.from(out)

    private def mergeSelect(base: SelectDefinition, layer: SelectDefinition)
    :   SelectDefinition raises Tel.Error =

      val variants = scala.collection.mutable.ArrayBuffer.from(base.variants.readable)
      var i = 0

      while i < layer.variants.readable.length do
        val v = layer.variants.readable(i)
        val existingIdx = variants.indexWhere(_.keyword == v.keyword)
        if existingIdx < 0 then abort(Tel.Error(Reason.LayerVariantAddition))
        i += 1

      // §20.3: apply the layer's excludes, removing each named variant
      // from the merged SelectDefinition. An exclude naming no variant of
      // the base is E211; whether the removals empty a SelectDefinition
      // that a required SelectRef references (E212) is checked against
      // the composed schema, where the referencing members are known.
      layer.excludes.each: keyword =>
        val idx = variants.indexWhere(_.keyword == keyword)
        if idx < 0 then abort(Tel.Error(Reason.ExcludeMissingVariant))
        variants.remove(idx)

      val mergedValidators = Array.frozen((base.validators.readable ++ layer.validators.readable).distinct)

      SelectDefinition(base.name, Array.from(variants), mergedValidators,
          layer.description.or(base.description))

  // Post-composition schema validity (§20.1). The key-field constraints
  // (E219–E221) are checked against each *composed* Struct, after all
  // layers have been applied, since a layer may both key a field and
  // tighten its polarity (§20.3).
  object Validation:

    // Check the base-side constraints, compose `schema`'s layers, then
    // check the composed schema; returns the composed schema for
    // further use.
    def validate(schema: Tels): Tels raises Tel.Error =
      checkBase(schema)
      checkComposed(Layers.compose(schema))

    // §8.1: validate under a pragma layer selection, composing only the
    // selected layers; the post-composition checks run against the
    // composition in use.
    def validate(schema: Tels, selection: List[Text])
    :   Tels raises Tel.Error raises Resolution.Error =

      checkBase(schema)
      checkComposed(Layers.compose(schema, selection))

    private def checkBase(schema: Tels): Unit raises Tel.Error =
      // E207: the schema sigil must be sigil-valid per §6. (An invalid
      // *pragma* sigil is E105 at parse time; this covers the schema
      // model itself, however it was constructed.)
      schema.sigil.let: sigil =>
        if !sigilValid(sigil) then abort(Tel.Error(Reason.BadSchemaSigil))

      // E210: definition names are unique across a schema's records,
      // scalars and selects, in the base and within each layer (a layer's
      // same-name definition *refines* the base's, which composition
      // handles, so names are checked per declaration site).
      def checkNames
        ( records: Array[RecordDefinition]^{}, scalars: Array[ScalarDefinition]^{},
          selects: Array[SelectDefinition]^{} )
      :   Unit raises Tel.Error =

        val names = scala.collection.mutable.HashSet.empty[Text]
        records.each { r => if !names.add(r.name) then abort(Tel.Error(Reason.DuplicateDefinition)) }
        scalars.each { s => if !names.add(s.name) then abort(Tel.Error(Reason.DuplicateDefinition)) }
        selects.each { s => if !names.add(s.name) then abort(Tel.Error(Reason.DuplicateDefinition)) }

      checkNames(schema.records, schema.scalars, schema.selects)
      schema.layers.each { layer => checkNames(layer.records, layer.scalars, layer.selects) }

      // Base-side select constraints, checked before composition: a
      // *declared* SelectDefinition must have at least one variant
      // (E202) and no excludes (E216 — exclude is layer-only). Neither
      // applies to the composed result: a layer's excludes may
      // legitimately empty a select (E212 below covers the required
      // case), and composition consumes them.
      schema.selects.each: select =>
        if select.variants.nil then abort(Tel.Error(Reason.EmptySelectVariants))
        if !select.excludes.nil then abort(Tel.Error(Reason.ExcludeOutsideSelect))

      // §20.1: E202 is a check on the declaration wherever it appears, so a
      // layer's select declared with neither variants nor excludes is E202
      // too. A layer select carrying only excludes is a refinement of the
      // base's (§20.3), not an empty declaration.
      schema.layers.each: layer =>
        layer.selects.each: select =>
          if select.variants.nil && select.excludes.nil
          then abort(Tel.Error(Reason.EmptySelectVariants))

      // §21.8: every declared pattern, base-side or layer-side, must be valid
      // RE2 (E222). Checking here — before `Layers.compose` runs — is what
      // guarantees E222 precedes E223: the containment decision of §20.3 must
      // never be asked about a pattern that does not compile. Like an
      // unresolved encoding (E313), an unparseable pattern is never treated as
      // satisfied; the schema is invalid instead.
      checkPatterns(schema.scalars)
      schema.layers.each { layer => checkPatterns(layer.scalars) }

    private def checkPatterns(scalars: Array[ScalarDefinition]^{}): Unit raises Tel.Error =
      scalars.each: definition =>
        definition.patterns.each: pattern =>
          if Patterns.compile(pattern).absent then abort(Tel.Error(Reason.InvalidPattern))

    private[stratiform] def checkComposed(composed: Tels): Tels raises Tel.Error =
      checkStruct(composed.document, composed)

      composed.records.each: record =>
        checkStruct(Struct(record.members, record.validators), composed)

      composed.selects.each: select =>
        // E201 also covers duplicates within one SelectDefinition's
        // variants, and E208 reserves `tel` among variant keywords.
        val seen = scala.collection.mutable.HashSet.empty[Text]

        select.variants.each: variant =>
          if variant.keyword == t"tel" then abort(Tel.Error(Reason.TelKeywordReserved))
          if !seen.add(variant.keyword) then abort(Tel.Error(Reason.DuplicateKeywordInStruct))

      composed

    // §6 sigil validity: a single character that is not whitespace, not
    // a letter or digit, not a parenthetical symbol, and not `+` (which
    // exclusively introduces pragma layer selections). Mirrors the
    // built-in `sigil` validator.
    private def sigilValid(sigil: Char): Boolean =
      !(sigil == ' ' || sigil == '\n' || sigil == '\r' || sigil == '\t')
        && !sigil.isLetterOrDigit
        && "()[]{}<>".indexOf(sigil.toInt) < 0
        && sigil != '+'

    // The Scalar a type resolves to through the composed namespace and
    // the built-ins (§20.5), or Unset for any non-Scalar resolution.
    private def scalarOf(t: Type, schema: Tels): Optional[Scalar] = t match
      case scalar: Scalar => scalar

      case Reference(name) =>
        schema.scalars.readable.find(_.name == name) match
          case scala.Some(definition) =>
            Scalar(definition.validators, definition.encoding, definition.patterns)

          case scala.None =>
            if builtinScalar(name) then Scalar(Array.empty) else Unset

      case _ => Unset

    // The built-in scalar TypeNames of §20.5. (`Flag` is not among them:
    // it parses to the Flag type directly, never to a Reference.)
    private def builtinScalar(name: Text): Boolean =
      name == Builtin.String || name == Builtin.Identifier
        || name == Builtin.TypeName || name == Builtin.Sigil

    private def checkStruct(struct: Struct, schema: Tels): Unit raises Tel.Error =
      val keywords = scala.collection.mutable.HashSet.empty[Text]
      var keys = 0

      // E201: keyword uniqueness spans the Field keywords and the variant
      // keywords of SelectRef-referenced SelectDefinitions alike.
      def claim(keyword: Text): Unit raises Tel.Error =
        if keyword == t"tel" then abort(Tel.Error(Reason.TelKeywordReserved))
        if !keywords.add(keyword) then abort(Tel.Error(Reason.DuplicateKeywordInStruct))

      struct.members.each:
        case field: Field =>
          claim(field.keyword)

          // Merge-produced nested Structs are checked regardless of `key`.
          field.fieldType match
            case nested: Struct => checkStruct(nested, schema)

            // E209/E217: a Field's Reference must resolve, through the
            // composed namespace or the §20.5 built-ins, to a record or
            // scalar; a SelectDefinition can only be the target of a
            // SelectRef.
            case Reference(name) =>
              if !schema.records.readable.exists(_.name == name)
                && !schema.scalars.readable.exists(_.name == name)
                && !builtinScalar(name)
              then
                if schema.selects.readable.exists(_.name == name)
                then abort(Tel.Error(Reason.ReferenceKindMismatch))
                else abort(Tel.Error(Reason.UnresolvedReference))

            case _ => ()

          val required   = field.required != Polarity.Loose
          val repeatable = field.repeatable == Polarity.Loose

          // E203: a default is only permitted on a required, Scalar-typed
          // member (it supplies the value of a required-but-elided field).
          if field.default.present && (!required || scalarOf(field.fieldType, schema).absent)
          then abort(Tel.Error(Reason.DefaultOnOptional))

          if field.key then
            keys += 1
            if keys > 1 then abort(Tel.Error(Reason.MultipleKeyFields))
            if scalarOf(field.fieldType, schema).absent
            then abort(Tel.Error(Reason.KeyOnNonScalar))
            if !required || repeatable then abort(Tel.Error(Reason.KeyOnLooseMember))

        case select: SelectRef =>
          schema.selects.readable.find(_.name == select.reference) match
            case scala.Some(definition) =>
              definition.variants.each { variant => claim(variant.keyword) }

              // E212: a layer's excludes must not empty a
              // SelectDefinition that an effectively required SelectRef
              // references — the member could then never be filled.
              // Checked against the composed schema, where both the
              // post-exclusion variant lists and the referencing members
              // are known.
              if definition.variants.nil && select.required != Polarity.Loose
              then abort(Tel.Error(Reason.ExcludeEmptiesRequired))

            // E209/E217: a SelectRef must resolve to a SelectDefinition;
            // a record, scalar or built-in name is a kind mismatch, and
            // anything else is unresolved.
            case scala.None =>
              if schema.records.readable.exists(_.name == select.reference)
                || schema.scalars.readable.exists(_.name == select.reference)
                || builtinScalar(select.reference)
              then abort(Tel.Error(Reason.ReferenceKindMismatch))
              else abort(Tel.Error(Reason.UnresolvedReference))

        case _ => ()

  // Inverse of the §20.5 schema-of-schemas: given a Tel.Document whose
  // surface matches the canonical tels vocabulary, reconstruct
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

    private def seqEq[T](a: Array[T]^{}, b: Array[T]^{}, eq: (T, T) => Boolean): Boolean =
      a.length == b.length && (0 until a.length).forall: i => eq(a.readUnchecked(i), b.readUnchecked(i))

    private def structEq(a: Struct, b: Struct): Boolean =
      seqEq(a.members, b.members, memberEq) && seqEq(a.validators, b.validators, textEq)

    private def textEq(a: Text, b: Text): Boolean = a == b

    private def memberEq(a: Member, b: Member): Boolean = (a, b) match
      case (a: Field, b: Field) =>
        a.required == b.required && a.repeatable == b.repeatable && a.key == b.key &&
          a.keyword == b.keyword && typeEq(a.fieldType, b.fieldType) &&
          a.default == b.default && a.description == b.description

      case (a: SelectRef, b: SelectRef) =>
        a.required == b.required && a.repeatable == b.repeatable && a.reference == b.reference

      case (a: Exclude, b: Exclude) => a.keyword == b.keyword
      case _                        => false

    private[Tels] def typeEq(a: Type, b: Type): Boolean = (a, b) match
      case (a: Struct, b: Struct)         => structEq(a, b)

      case (a: Scalar, b: Scalar) =>
        seqEq(a.validators, b.validators, textEq) && a.encoding == b.encoding &&
          seqEq(a.patterns, b.patterns, textEq)
      case (Flag, Flag)                   => true
      case (Reference(n1), Reference(n2)) => n1 == n2
      case _                              => false

    private def recordEq(a: RecordDefinition, b: RecordDefinition): Boolean =
      a.name == b.name && seqEq(a.members, b.members, memberEq) &&
        seqEq(a.validators, b.validators, textEq) &&
        a.description == b.description

    // Patterns compare textually and in order: §20.3 makes textual identity the
    // benign-no-op test for layer merge, and schema *identity* is spelling-based
    // throughout — only the merge rule of §20.3 inspects a pattern's meaning.
    private def scalarEq(a: ScalarDefinition, b: ScalarDefinition): Boolean =
      a.name == b.name && seqEq(a.validators, b.validators, textEq) &&
        a.description == b.description && a.encoding == b.encoding &&
        seqEq(a.patterns, b.patterns, textEq)

    private def selectEq(a: SelectDefinition, b: SelectDefinition): Boolean =
      a.name == b.name &&
        seqEq(a.variants, b.variants, (x, y) => x.keyword == y.keyword &&
          typeEq(x.variantType, y.variantType) && x.description == y.description) &&
        seqEq(a.validators, b.validators, textEq) &&
        a.description == b.description &&
        seqEq(a.excludes, b.excludes, textEq)

    private def layerEq(a: Layer, b: Layer): Boolean =
      a.name == b.name && structEq(a.overlay, b.overlay) &&
        seqEq(a.records, b.records, recordEq) &&
        seqEq(a.scalars, b.scalars, scalarEq) &&
        seqEq(a.selects, b.selects, selectEq)

    def fromTel(tel: Tel): Tels raises Tel.Error =
      val compounds: Array[Tel.Compound]^{} = tel.subtree.children.bind(_.compounds)

      var name: Optional[Text] = Unset
      var sigil: Optional[Char] = Unset
      var documentStruct: Optional[Struct] = Unset
      val records  = scala.collection.mutable.ArrayBuffer.empty[RecordDefinition]
      val scalars  = scala.collection.mutable.ArrayBuffer.empty[ScalarDefinition]
      val selects  = scala.collection.mutable.ArrayBuffer.empty[SelectDefinition]
      val layers   = scala.collection.mutable.ArrayBuffer.empty[Layer]

      compounds.extent.each: i =>
        val c = compounds(i)

        c.keyword.s match
          case "name"     => name = firstAtomText(c)

          case "sigil" =>
            val s = firstAtomText(c)
            sigil = s.let { text => if text.s.isEmpty then Unset else text.s.charAt(0) }

          case "record"   => records  += parseRecord(c)
          case "scalar"   => scalars  += parseScalar(c)
          case "select"   => selects  += parseSelect(c)
          case "document" => documentStruct = parseBody(c)
          case "layer"    => layers   += parseLayer(c)
          case _          => abort(Tel.Error(Reason.UnknownKeyword))

      val builtinScalars =
        Array
          ( ScalarDefinition(t"Identifier", Array(t"identifier")),
            ScalarDefinition(t"TypeName",   Array(t"type-name")),
            ScalarDefinition(t"Sigil",      Array(t"sigil")),
            ScalarDefinition(t"String",     Array(t"string")) )

      Tels
        ( name     = name.or(abort(Tel.Error(Reason.RequiredMemberAbsent))),
          document = documentStruct.or(abort(Tel.Error(Reason.RequiredMemberAbsent))),
          layers   = Array.from(layers),
          sigil    = sigil,
          records  = Array.from(records),
          scalars  = Array.frozen(builtinScalars.readable ++ Array.from(scalars).readable),
          selects  = Array.from(selects) )

    private def firstAtomText(c: Tel.Compound): Optional[Text] =
      val texts = c.atoms.sweep { case Tel.Atom.Inline(t, _) => t }
      if texts.nil then Unset else texts.readUnchecked(0): Optional[Text]

    private def atomTexts(c: Tel.Compound): Array[Text]^{} =
      c.atoms.sweep { case Tel.Atom.Inline(t, _) => t }

    private def childCompounds(c: Tel.Compound): Array[Tel.Compound]^{} =
      c.children.bind(_.compounds)

    private def parseType(name: Text): Type =
      if name == t"Flag" then Flag else Reference(name)

    // A Definition's name: the first inline atom, or (per the §20.5
    // atom/compound interchangeability rule) an explicit `name <value>`
    // child compound.
    private def nameOf(c: Tel.Compound): Optional[Text] =
      firstAtomText(c).or:
        childCompounds(c).seek(_.keyword == t"name").let(scalarAtomText(_))

    // The text of a scalar-valued child compound, taking its first atom
    // (inline, source, or literal) — used for both `default` and the §20
    // `description` child, whose prose is typically a source atom (§14).
    private def scalarAtomText(c: Tel.Compound): Optional[Text] =
      if c.atoms.nil then Unset else c.atoms.readUnchecked(0) match
        case Tel.Atom.Inline(t, _)  => t
        case Tel.Atom.Source(t)     => t
        case Tel.Atom.Literal(_, t) => t

    // The optional §20 `description` of a Definition/Field/Variant: the
    // text of its `description` child compound, or `Unset` if absent.
    private def descriptionOf(children: Array[Tel.Compound]^{}): Optional[Text] =
      children.seek(_.keyword == t"description").let(scalarAtomText(_))

    private def parseRecord(c: Tel.Compound): RecordDefinition raises Tel.Error =
      val recName = nameOf(c).or(abort(Tel.Error(Reason.RequiredMemberAbsent)))
      val children = childCompounds(c)
      val (members, validators) = parseMembersAndValidators(children)
      RecordDefinition(recName, members, validators, descriptionOf(children))

    private def parseScalar(c: Tel.Compound): ScalarDefinition raises Tel.Error =
      val scName = nameOf(c).or(abort(Tel.Error(Reason.RequiredMemberAbsent)))
      val children = childCompounds(c)

      val validators = children.bind: cc =>
        if cc.keyword == t"validate" then atomTexts(cc) else Array.empty[Text]

      // §21.8: one RE2 pattern per `pattern` child, in declaration order. The
      // value is read with `scalarAtomText` rather than `atomTexts` because
      // §20.5 makes `pattern` a compound child whose regex may be carried as a
      // source atom (§14) when it contains a hard-space run.
      val patterns = children.bind: cc =>
        if cc.keyword == t"pattern"
        then scalarAtomText(cc).lay(Array.empty[Text])(Array(_))
        else Array.empty[Text]

      var encoding: Optional[Text] = Unset

      children.each: cc =>
        if cc.keyword == t"encoding" && encoding.absent then encoding = firstAtomText(cc)

      ScalarDefinition(scName, validators, descriptionOf(children), encoding, patterns)

    private def parseSelect(c: Tel.Compound): SelectDefinition raises Tel.Error =
      val seName = nameOf(c).or(abort(Tel.Error(Reason.RequiredMemberAbsent)))
      val children   = childCompounds(c)
      val variants   = scala.collection.mutable.ArrayBuffer.empty[Variant]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]
      val excludes   = scala.collection.mutable.ArrayBuffer.empty[Text]

      children.each: cc =>
        cc.keyword.s match
          case "validate"    => validators ++= atomTexts(cc).readable
          case "exclude"     => scalarAtomText(cc).let { keyword => excludes += keyword }
          case "description" => ()

          case "variant" =>
            val ats = atomTexts(cc)
            if ats.length < 2 then abort(Tel.Error(Reason.RequiredMemberAbsent))
            variants += Variant(ats.readUnchecked(0), parseType(ats.readUnchecked(1)), descriptionOf(childCompounds(cc)))

          case _ =>
            abort(Tel.Error(Reason.UnknownKeyword))

      SelectDefinition
        ( seName, Array.from(variants), Array.from(validators), descriptionOf(children),
          Array.from(excludes) )

    private def parseBody(c: Tel.Compound): Optional[Struct] raises Tel.Error =
      val (members, validators) = parseMembersAndValidators(childCompounds(c))
      Optional(Struct(members, validators))

    private def parseLayer(c: Tel.Compound): Layer raises Tel.Error =
      // §20.5: the name is carried either as the first inline atom
      // (`layer auth`) or as an explicit `name <value>` child compound.
      var name: Optional[Text] = firstAtomText(c)
      val recs = scala.collection.mutable.ArrayBuffer.empty[RecordDefinition]
      val scs  = scala.collection.mutable.ArrayBuffer.empty[ScalarDefinition]
      val sels = scala.collection.mutable.ArrayBuffer.empty[SelectDefinition]
      var overlay: Optional[Struct] = Struct(Array.empty, Array.empty)

      childCompounds(c).each: cc =>
        cc.keyword.s match
          case "name"    => if name.absent then name = scalarAtomText(cc)
          case "record"  => recs += parseRecord(cc)
          case "scalar"  => scs  += parseScalar(cc)
          case "select"  => sels += parseSelect(cc)
          case "overlay" => overlay = parseBody(cc)
          case _         => abort(Tel.Error(Reason.UnknownKeyword))

      val lyName = name.or(abort(Tel.Error(Reason.RequiredMemberAbsent)))

      Layer
        ( name    = lyName,
          overlay = overlay.or(Struct(Array.empty, Array.empty)),
          records = Array.from(recs),
          scalars = Array.from(scs),
          selects = Array.from(sels) )

    private def parseMembersAndValidators(compounds: Array[Tel.Compound]^{})
    :   (Array[Member]^{}, Array[Text]^{}) raises Tel.Error =

      val members    = scala.collection.mutable.ArrayBuffer.empty[Member]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]

      compounds.each: cc =>
        cc.keyword.s match
          case "field"    => members += parseField(cc)
          case "select"   => members += parseSelectRef(cc)
          case "validate" => validators ++= atomTexts(cc).readable

          case "exclude" =>
            val ats = atomTexts(cc)
            if ats.length >= 1 then members += Exclude(ats.readUnchecked(0))

          // The Definition's own `description` (§20); consumed by the
          // enclosing parseRecord/parseBody, not a member.
          case "description" => ()
          case _             => abort(Tel.Error(Reason.UnknownKeyword))

      (Array.from(members), Array.from(validators))

    private def parseField(c: Tel.Compound): Field raises Tel.Error =
      val ats = atomTexts(c)
      if ats.length < 2 then abort(Tel.Error(Reason.RequiredMemberAbsent))
      val keyword = ats.readUnchecked(0)
      val fieldType = parseType(ats.readUnchecked(1))

      var required:   Polarity = Polarity.Implicit
      var repeatable: Polarity = Polarity.Implicit
      var key:        Boolean = false
      var default:    Optional[Text] = Unset

      var j = 2

      // Atom phase against the Field record's member order (§20.5):
      // flag-matching atoms set their flag; the first non-flag atom fills
      // `default`, which follows `key` in member order — a trailing `key`
      // atom is therefore a flag, never a default value.
      while j < ats.length do
        ats.readUnchecked(j).s match
          case "optional"     => required   = Polarity.Loose
          case "required"     => required   = Polarity.Tight
          case "repeatable"   => repeatable = Polarity.Loose
          case "irrepeatable" => repeatable = Polarity.Tight
          case "key"          => key        = true
          case text           => if default.absent then default = Text(text)

        j += 1

      // Child compounds may supply the same members in compound form.
      childCompounds(c).each: cc =>
        cc.keyword.s match
          case "optional"     => required   = Polarity.Loose
          case "required"     => required   = Polarity.Tight
          case "repeatable"   => repeatable = Polarity.Loose
          case "irrepeatable" => repeatable = Polarity.Tight
          case "key"          => key        = true
          case "default"      => firstAtomText(cc).let { text => default = text }
          case _              => ()

      Field(required, repeatable, keyword, fieldType, default,
          descriptionOf(childCompounds(c)), key)

    private def parseSelectRef(c: Tel.Compound): SelectRef raises Tel.Error =
      val ats = atomTexts(c)
      if ats.length < 1 then abort(Tel.Error(Reason.RequiredMemberAbsent))
      val reference = ats.readUnchecked(0)

      var required:   Polarity = Polarity.Implicit
      var repeatable: Polarity = Polarity.Implicit

      var j = 1

      while j < ats.length do
        ats.readUnchecked(j).s match
          case "optional"     => required   = Polarity.Loose
          case "required"     => required   = Polarity.Tight
          case "repeatable"   => repeatable = Polarity.Loose
          case "irrepeatable" => repeatable = Polarity.Tight
          case _              => ()

        j += 1

      SelectRef(required, repeatable, reference)

  // Inverse of `Tel.Type.assign` for schema documents: reconstruct a
  // `Tels` from the type-assigned semantic model produced by decoding an
  // embedded schema body (BinTEL §6.2) under the hardwired `tels`
  // axiom. The element children are in §7.2 canonical order — grouped by
  // member, source order within a member — so iterating them in order
  // rebuilds each member sequence. The flat keyword indices below mirror
  // the member layout of `Tels.Axiom` (the schema-of-schemas).
  object SemanticReconstructor:

    def fromElement(root: Tel.Element): Tels raises Tel.Error =
      val ch = childrenOf(root)
      // Document struct: name=0, sigil=1, record=2, scalar=3, select=4,
      // document=5, layer=6.
      val name = textAt(ch, 0).or(abort(Tel.Error(Reason.RequiredMemberAbsent)))

      val sigil: Optional[Char] = textAt(ch, 1) match
        case t: Text => if t.s.isEmpty then Unset else Optional(t.s.charAt(0))
        case _       => Unset

      val records  = nodesAt(ch, 2).remap(recordFromElement)
      val scalars  = nodesAt(ch, 3).remap(scalarFromElement)
      val selects  = nodesAt(ch, 4).remap(selectFromElement)
      val document = nodeAt(ch, 5).let(bodyFromElement)
        .or(abort(Tel.Error(Reason.RequiredMemberAbsent)))
      val layers   = nodesAt(ch, 6).remap(layerFromElement)

      val builtinScalars =
        Array
          ( ScalarDefinition(t"Identifier", Array(t"identifier")),
            ScalarDefinition(t"TypeName",   Array(t"type-name")),
            ScalarDefinition(t"Sigil",      Array(t"sigil")),
            ScalarDefinition(t"String",     Array(t"string")) )

      Tels(name, document, layers, sigil, records, Array.frozen(builtinScalars.readable ++ scalars.readable), selects)

    private def typeFromText(name: Text): Type =
      if name == t"Flag" then Flag else Reference(name)

    private def childrenOf(element: Tel.Element): Array[Tel.Element]^{} = element match
      case Tel.Element.Node(_, _, c) => c
      case _                         => Array.empty[Tel.Element]

    private def kidx(element: Tel.Element): Int = element match
      case Tel.Element.Node(i, _, _)  => i.or(0)
      case Tel.Element.Value(i, _, _) => i

    private def textAt(children: Array[Tel.Element]^{}, idx: Int): Optional[Text] =
      var i = 0
      var result: Optional[Text] = Unset

      while i < children.length do
        children.readUnchecked(i) match
          case Tel.Element.Value(j, _, t) if j == idx => result = t
          case _                                      => ()

        i += 1

      result

    private def nodesAt(children: Array[Tel.Element]^{}, idx: Int): Array[Tel.Element]^{} =
      children.filter(kidx(_) == idx)

    private def nodeAt(children: Array[Tel.Element]^{}, idx: Int): Optional[Tel.Element] =
      val found = nodesAt(children, idx)
      if found.nil then Unset else found.readUnchecked(0)

    private def present(children: Array[Tel.Element]^{}, idx: Int): Boolean =
      children.exists(kidx(_) == idx)

    // tight if the tightening flag is present, else loose if the loosening
    // flag is present, else implicit (§20 Polarity).
    private def polarity(children: Array[Tel.Element]^{}, looseIdx: Int, tightIdx: Int): Polarity =
      if present(children, tightIdx) then Polarity.Tight
      else if present(children, looseIdx) then Polarity.Loose
      else Polarity.Implicit

    private def textsAt(children: Array[Tel.Element]^{}, idx: Int): Array[Text]^{} =
      children.sweep { case Tel.Element.Value(j, _, t) if j == idx => t }

    // Field meta: keyword=0, type=1, optional=2, required=3, repeatable=4,
    // irrepeatable=5, key=6, default=7, description=8.
    private def fieldFromElement(element: Tel.Element): Field =
      val ch = childrenOf(element)

      Field
        ( required    = polarity(ch, 2, 3),
          repeatable  = polarity(ch, 4, 5),
          keyword     = textAt(ch, 0).or(t""),
          fieldType   = typeFromText(textAt(ch, 1).or(t"")),
          default     = textAt(ch, 7),
          description = textAt(ch, 8),
          key         = present(ch, 6) )

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
      ( children: Array[Tel.Element]^{}, fieldIdx: Int, selectIdx: Int, validateIdx: Int )
    :   (Array[Member]^{}, Array[Text]^{}) =

      val members    = scala.collection.mutable.ArrayBuffer.empty[Member]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]
      children.extent.each: i =>
        val e = children(i)

        kidx(e) match
          case k if k == fieldIdx    => members += fieldFromElement(e)
          case k if k == selectIdx   => members += selectRefFromElement(e)

          case k if k == validateIdx => e match
            case Tel.Element.Value(_, _, t) => validators += t
            case _                          => ()

          case _ => ()

      (Array.from(members), Array.from(validators))

    // Record meta: name=0, Member{field=1, select=2, validate=3}, description=4.
    private def recordFromElement(element: Tel.Element): RecordDefinition =
      val ch = childrenOf(element)
      val (members, validators) = membersFromBody(ch, 1, 2, 3)
      RecordDefinition(textAt(ch, 0).or(t""), members, validators, textAt(ch, 4))

    // Scalar meta: name=0, validate=1, pattern=2, encoding=3, description=4.
    // These indices track the member order of the `Scalar` record in the
    // Axiom; inserting `pattern` at 2 shifted `encoding` and `description`.
    private def scalarFromElement(element: Tel.Element): ScalarDefinition =
      val ch = childrenOf(element)

      ScalarDefinition
        ( textAt(ch, 0).or(t""), textsAt(ch, 1), textAt(ch, 4), textAt(ch, 3),
          textsAt(ch, 2) )

    // Select meta: name=0, SelectChild{variant=1, exclude=2, validate=3}, description=4.
    private def selectFromElement(element: Tel.Element): SelectDefinition =
      val ch = childrenOf(element)
      val variants   = scala.collection.mutable.ArrayBuffer.empty[Variant]
      val validators = scala.collection.mutable.ArrayBuffer.empty[Text]
      val excludes   = scala.collection.mutable.ArrayBuffer.empty[Text]
      ch.extent.each: i =>
        val e = ch(i)

        kidx(e) match
          case 1 => variants += variantFromElement(e)

          case 2 => e match
            case Tel.Element.Value(_, _, t) => excludes += t
            case _                          => ()

          case 3 => e match
            case Tel.Element.Value(_, _, t) => validators += t
            case _                          => ()

          case _ => ()

      SelectDefinition(textAt(ch, 0).or(t""), Array.from(variants), Array.from(validators),
          textAt(ch, 4), Array.from(excludes))

    // Body meta: Member{field=0, select=1, validate=2}.
    private def bodyFromElement(element: Tel.Element): Struct =
      val (members, validators) = membersFromBody(childrenOf(element), 0, 1, 2)
      Struct(members, validators)

    // Layer meta: name=0, record=1, scalar=2, select=3, overlay=4.
    private def layerFromElement(element: Tel.Element): Layer =
      val ch = childrenOf(element)

      Layer
        ( name    = textAt(ch, 0).or(t""),
          overlay = nodeAt(ch, 4).let(bodyFromElement).or(Struct(Array.empty, Array.empty)),
          records = nodesAt(ch, 1).remap(recordFromElement),
          scalars = nodesAt(ch, 2).remap(scalarFromElement),
          selects = nodesAt(ch, 3).remap(selectFromElement) )

  // Renders a schema value as the typed semantic model of its schema document under the `tels`
  // axiom — the inverse of `SemanticReconstructor.fromElement` — so that a schema built in Scala
  // (derived from a type, composed, or decomposed into atoms) is encoded, hashed and signed
  // exactly as a parsed `.tel` document is. The keyword indices are the flat indices of the
  // axiom's member layout, the same ones the reconstructor reads; `Bintel.encode` puts the
  // children into §7.2 canonical order itself.
  object Renderer:
    // The four scalars every schema declares implicitly, which `Reconstructor` prepends and a
    // canonical `.tel` source never declares, so a rendered document omits them too.
    private val builtinNames: scala.collection.immutable.Set[Text] =
      scala.collection.immutable.Set(t"Identifier", t"TypeName", t"Sigil", t"String")

    private[Tels] def builtin(scalar: ScalarDefinition): Boolean =
      builtinNames.contains(scalar.name) && scalar.encoding.absent && scalar.patterns.nil &&
        scalar.description.absent

    // The schema document's root element: `name`, `sigil`, the definitions, the `document` body
    // and the layers, in the axiom's keyword order (name=0, sigil=1, record=2, scalar=3, select=4,
    // document=5, layer=6).
    def element(schema: Tels, axiom: Tels = Axiom.tels): Tel.Element.Node raises Error =
      val context = Context(axiom)
      import context.*

      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "Identifier", schema.name)
      schema.sigil.let: sigil => buffer += value(1, "Sigil", Text(sigil.toString))
      schema.records.each: record => buffer += node(2, "Record", recordChildren(context, record))

      schema.scalars.each: scalar =>
        if !builtin(scalar) then buffer += node(3, "Scalar", scalarChildren(context, scalar))

      schema.selects.each: select => buffer += node(4, "Select", selectChildren(context, select))
      buffer += node(5, "Body", bodyChildren(context, schema.document, 0, 1, 2))
      schema.layers.each: layer0 => buffer += node(6, "Layer", layerChildren(context, layer0))

      Tel.Element.Node(Unset, axiom.document, Array.from(buffer))

    // A layer as a document root of its own, under the axiom's `Layer` definition: the form BinTEL
    // §8.1 hashes for a layer component, and — with an empty name and a single atom — for an atom.
    def layer(layer: Layer, axiom: Tels = Axiom.tels): Tel.Element.Node raises Error =
      val context = Context(axiom)
      Tel.Element.Node(Unset, context.structOf("Layer"), layerChildren(context, layer))

    // The axiom's definitions, resolved once per rendering: a Definition's struct for the nodes,
    // and a scalar for the values.
    private class Context(axiom: Tels):
      def structOf(name: String): Struct =
        val record = axiom.records.seek(_.name == Text(name)).or:
          panic(m"the axiom declares no record ${Text(name)}")

        Struct(record.members, record.validators)

      def scalarOf(name: String): Scalar =
        val definition = axiom.scalars.seek(_.name == Text(name)).or:
          panic(m"the axiom declares no scalar ${Text(name)}")

        Scalar(definition.validators, definition.encoding, definition.patterns)

      def node(index: Int, record: String, children: Array[Tel.Element]^{}): Tel.Element =
        Tel.Element.Node(index, structOf(record), children)

      def value(index: Int, scalarName: String, text: Text): Tel.Element =
        Tel.Element.Value(index, scalarOf(scalarName), text)

      def flag(index: Int): Tel.Element = Tel.Element.Node(index, Flag, Array.empty)

    // A member type's `TypeName`. Only a reference, `Flag`, or an inline scalar that is one of
    // the built-in scalars can be written in a schema document; a derived schema's inline struct
    // or constrained inline scalar has no textual form, and is `Error`.
    private def typeName(fieldType: Type, keyword: Text): Text raises Error = fieldType match
      case Reference(name) => name
      case Flag            => t"Flag"

      case Scalar(validators, encoding, patterns) =>
        if encoding.present || !patterns.nil then abort(Error(Error.Reason.InlineScalar(keyword)))
        else validators.readable.toList match
          case scala.Nil => t"String"

          case scala.List(single) => single.s match
            case "identifier" => t"Identifier"
            case "type-name"  => t"TypeName"
            case "sigil"      => t"Sigil"
            case "string"     => t"String"
            case _            => abort(Error(Error.Reason.InlineScalar(keyword)))

          case _ => abort(Error(Error.Reason.InlineScalar(keyword)))

      case _: Struct => abort(Error(Error.Reason.InlineStruct(keyword)))

    // Field meta: keyword=0, type=1, optional=2, required=3, repeatable=4, irrepeatable=5, key=6,
    // default=7, description=8.
    private def fieldChildren(context: Context, field: Field): Array[Tel.Element]^{} raises Error =
      import context.*
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "Identifier", field.keyword)
      buffer += value(1, "TypeName", typeName(field.fieldType, field.keyword))
      polarityFlags(context, buffer, field.required, 2, 3)
      polarityFlags(context, buffer, field.repeatable, 4, 5)
      if field.key then buffer += flag(6)
      field.default.let: default => buffer += value(7, "String", default)
      field.description.let: description => buffer += value(8, "String", description)
      Array.from(buffer)

    private def polarityFlags
      ( context: Context, buffer: scala.collection.mutable.ArrayBuffer[Tel.Element],
        polarity: Polarity, looseIndex: Int, tightIndex: Int )
    :   Unit =

      polarity match
        case Polarity.Implicit => ()
        case Polarity.Loose    => buffer += context.flag(looseIndex)
        case Polarity.Tight    => buffer += context.flag(tightIndex)

    // SelectRef meta: reference=0, optional=1, required=2, repeatable=3, irrepeatable=4.
    private def selectRefChildren(context: Context, select: SelectRef): Array[Tel.Element]^{} =
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += context.value(0, "TypeName", select.reference)
      polarityFlags(context, buffer, select.required, 1, 2)
      polarityFlags(context, buffer, select.repeatable, 3, 4)
      Array.from(buffer)

    // Variant meta: keyword=0, type=1, description=2.
    private def variantChildren(context: Context, variant: Variant)
    :   Array[Tel.Element]^{} raises Error =

      import context.*
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "Identifier", variant.keyword)
      buffer += value(1, "TypeName", typeName(variant.variantType, variant.keyword))
      variant.description.let: description => buffer += value(2, "String", description)
      Array.from(buffer)

    // A struct-shaped body's members and validators at the given flat indices.
    private def bodyChildren
      ( context: Context, struct: Struct, fieldIndex: Int, selectIndex: Int, validateIndex: Int )
    :   Array[Tel.Element]^{} raises Error =

      import context.*
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]

      struct.members.each:
        case field: Field =>
          buffer += node(fieldIndex, "Field", fieldChildren(context, field))

        case select: SelectRef =>
          buffer += node(selectIndex, "SelectRef", selectRefChildren(context, select))

        case _: Exclude => ()

      struct.validators.each: validator => buffer += value(validateIndex, "Identifier", validator)
      Array.from(buffer)

    // Record meta: name=0, Member{field=1, select=2, validate=3}, description=4.
    private def recordChildren(context: Context, record: RecordDefinition)
    :   Array[Tel.Element]^{} raises Error =

      import context.*
      val body = bodyChildren(context, Struct(record.members, record.validators), 1, 2, 3)
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "TypeName", record.name)
      body.each: child => buffer += child
      record.description.let: description => buffer += value(4, "String", description)
      Array.from(buffer)

    // Scalar meta: name=0, validate=1, pattern=2, encoding=3, description=4.
    private def scalarChildren(context: Context, scalar: ScalarDefinition): Array[Tel.Element]^{} =
      import context.*
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "TypeName", scalar.name)
      scalar.validators.each: validator => buffer += value(1, "Identifier", validator)
      scalar.patterns.each: pattern => buffer += value(2, "String", pattern)
      scalar.encoding.let: encoding => buffer += value(3, "Identifier", encoding)
      scalar.description.let: description => buffer += value(4, "String", description)
      Array.from(buffer)

    // Select meta: name=0, SelectChild{variant=1, exclude=2, validate=3}, description=4.
    private def selectChildren(context: Context, select: SelectDefinition)
    :   Array[Tel.Element]^{} raises Error =

      import context.*
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "TypeName", select.name)

      select.variants.each: variant =>
        buffer += node(1, "Variant", variantChildren(context, variant))

      select.excludes.each: exclude => buffer += value(2, "Identifier", exclude)
      select.validators.each: validator => buffer += value(3, "Identifier", validator)
      select.description.let: description => buffer += value(4, "String", description)
      Array.from(buffer)

    // Layer meta: name=0, record=1, scalar=2, select=3, overlay=4. An overlay with no members and
    // no validators is not written, as a layer refining only definitions omits it.
    private def layerChildren(context: Context, layer: Layer): Array[Tel.Element]^{} raises Error =
      import context.*
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      buffer += value(0, "Identifier", layer.name)
      layer.records.each: record => buffer += node(1, "Record", recordChildren(context, record))
      layer.scalars.each: scalar => buffer += node(2, "Scalar", scalarChildren(context, scalar))
      layer.selects.each: select => buffer += node(3, "Select", selectChildren(context, select))

      if !layer.overlay.members.nil || !layer.overlay.validators.nil
      then buffer += node(4, "Body", bodyChildren(context, layer.overlay, 0, 1, 2))

      Array.from(buffer)

    object Error:
      object Reason:
        given communicable: Reason is Communicable =
          case InlineStruct(keyword) =>
            m"the member $keyword has an inline struct type, which a schema document cannot name"

          case InlineScalar(keyword) =>
            m"the member $keyword has a constrained inline scalar type, which no document can name"

      enum Reason(val number: Int) extends Clarification:
        case InlineStruct(keyword: Text) extends Reason(1)
        case InlineScalar(keyword: Text) extends Reason(2)

    case class Error(reason: Renderer.Error.Reason)(using Diagnostics)
    extends fulminate.Error(613, reason.number)(m"the schema cannot be rendered because $reason")

  // §20.3 atoms: the canonical decomposition of a schema's components into the smallest
  // modifications that can appear on their own in a composition. Every atom but the base's head
  // is represented as a virtual layer with an empty name containing exactly that atom under its
  // path, which is both how BinTEL §8.1 hashes it and how composition applies it; the head atom
  // (the base's `name` and `sigil`) is the schema document with every definition removed and an
  // empty `document` body.
  object Atoms:
    enum Path:
      case Body
      case Record(name: Text)
      case Scalar(name: Text)
      case Select(name: Text)

    enum Atom:
      case Head(name: Text, sigil: Optional[Char])
      case Part(path: Path, layer: Layer)

      // The atom as the schema or layer it renders as: the head as a definition-less schema, a
      // part as its virtual layer.
      def element(axiom: Tels = Axiom.tels): Tel.Element.Node raises Renderer.Error = this match
        case Head(name, sigil) =>
          val head = Tels(name, Struct(Array.empty, Array.empty), Array.empty, sigil,
              Array.empty, Array.empty, Array.empty)

          Renderer.element(head, axiom)

        case Part(_, layer) => Renderer.layer(layer, axiom)

    // A group — the base (`name` absent) or a layer — with its atoms in canonical order: the
    // head (base only); the atoms of each record, scalar and select in source order; then the
    // body's members and validators in source order.
    case class Group(name: Optional[Text], atoms: List[Atom])

    // The schema's groups: the base, then each layer in declaration order.
    def decompose(schema: Tels): List[Group] =
      base(schema) :: schema.layers.readable.toList.to(List).map(layer)

    def base(schema: Tels): Group =
      val head = Atom.Head(schema.name, schema.sigil)
      val rest = atomsOf(schema.records, schema.scalars, schema.selects, schema.document)
      Group(Unset, head :: rest)

    def layer(layer: Layer): Group =
      Group(layer.name, atomsOf(layer.records, layer.scalars, layer.selects, layer.overlay))

    // The atomic expansion of a component sequence: each group replaced by its atoms.
    def expansion(groups: List[Group]): List[Atom] = groups.bind(_.atoms)

    private def empty: Struct = Struct(Array.empty, Array.empty)

    private def part(path: Path, records: Array[RecordDefinition]^{} = Array.empty,
        scalars: Array[ScalarDefinition]^{} = Array.empty,
        selects: Array[SelectDefinition]^{} = Array.empty, overlay: Struct = empty)
    :   Atom =

      Atom.Part(path, Layer(t"", overlay, records, scalars, selects))

    private def atomsOf
      ( records: Array[RecordDefinition]^{}, scalars: Array[ScalarDefinition]^{},
        selects: Array[SelectDefinition]^{}, body: Struct )
    :   List[Atom] =

      val buffer = scala.collection.mutable.ListBuffer.empty[Atom]

      // `record N`: description first, then each member and each validator; an empty record is
      // itself one atom, so that a fresh, empty definition is not lost.

      records.each: record =>
        val path = Path.Record(record.name)

        def at(members: Array[Member]^{}, validators: Array[Text]^{}, description: Optional[Text]) =
          val definition = RecordDefinition(record.name, members, validators, description)
          part(path, records = Array(definition))

        if record.members.nil && record.validators.nil && record.description.absent
        then buffer += at(Array.empty, Array.empty, Unset)
        else
          record.description.let: description => buffer += at(Array.empty, Array.empty, description)
          record.members.each: member => buffer += at(Array(member), Array.empty, Unset)
          record.validators.each: validator => buffer += at(Array.empty, Array(validator), Unset)

      // `scalar N`: each validator; all patterns together; the encoding; the description. The
      // four built-in scalars are never atoms, being declared by no document.

      scalars.each: scalar =>
        if !Renderer.builtin(scalar) then
          val path = Path.Scalar(scalar.name)

          def at
            ( validators: Array[Text]^{}, description: Optional[Text], encoding: Optional[Text],
              patterns: Array[Text]^{} )
          :   Atom =

            val definition =
              ScalarDefinition(scalar.name, validators, description, encoding, patterns)

            part(path, scalars = Array(definition))

          if scalar.validators.nil && scalar.patterns.nil && scalar.encoding.absent &&
            scalar.description.absent
          then buffer += at(Array.empty, Unset, Unset, Array.empty)
          else
            scalar.validators.each: validator =>
              buffer += at(Array(validator), Unset, Unset, Array.empty)

            if !scalar.patterns.nil then buffer += at(Array.empty, Unset, Unset, scalar.patterns)
            scalar.encoding.let: encoding => buffer += at(Array.empty, Unset, encoding, Array.empty)

            scalar.description.let: description =>
              buffer += at(Array.empty, description, Unset, Array.empty)

      // `select N`: all variants together; each exclude; each validator; the description.

      selects.each: select =>
        val path = Path.Select(select.name)

        def at
          ( variants: Array[Variant]^{}, validators: Array[Text]^{}, description: Optional[Text],
            excludes: Array[Text]^{} )
        :   Atom =

          val definition =
            SelectDefinition(select.name, variants, validators, description, excludes)

          part(path, selects = Array(definition))

        if select.variants.nil && select.excludes.nil && select.validators.nil &&
          select.description.absent
        then buffer += at(Array.empty, Array.empty, Unset, Array.empty)
        else
          if !select.variants.nil
          then buffer += at(select.variants, Array.empty, Unset, Array.empty)

          select.excludes.each: exclude =>
            buffer += at(Array.empty, Array.empty, Unset, Array(exclude))

          select.validators.each: validator =>
            buffer += at(Array.empty, Array(validator), Unset, Array.empty)

          select.description.let: description =>
            buffer += at(Array.empty, Array.empty, description, Array.empty)

      // The body: each member and each validator.

      body.members.each: member =>
        buffer += part(Path.Body, overlay = Struct(Array(member), Array.empty))

      body.validators.each: validator =>
        buffer += part(Path.Body, overlay = Struct(Array.empty, Array(validator)))

      buffer.toList.to(List)

  // The subtype relation of §24.3, which §8.2 makes normative for compatibility: a document
  // composed under `sub` can be read by a consumer expecting `sup` when `sub <: sup`. Records
  // are subtyped by extension, selects by narrowing and scalars by tightening; references are
  // unfolded coinductively, an assumption set of definition-name pairs cutting off repeated
  // unfoldings, so the decision terminates over cyclic definitions. `check` reports the first
  // premise that fails, as §8.2 says a resolution error SHOULD.
  object Subtyping:
    object Failure:
      given communicable: Failure is Communicable = failure =>
        def at(path: List[Text]): Text =
          if path.nil then t"the document root" else path.reverse.join(t"/")

        failure match
          case MissingRequired(path, keyword) =>
            m"${at(path)} lacks the required member $keyword"

          case MemberOrder(path, keyword) =>
            m"${at(path)} places the member $keyword out of order"

          case MemberKind(path, keyword) =>
            m"${at(path)} has a member $keyword of a different kind"

          case Validators(path) =>
            m"${at(path)} lacks a validator of the supertype"

          case Patterns(path) =>
            m"${at(path)} has a pattern language not within the supertype's"

          case Encoding(path) =>
            m"${at(path)} does not keep the supertype's encoding"

          case Required(path, keyword) =>
            m"${at(path)} does not require the member $keyword"

          case Repeatable(path, keyword) =>
            m"${at(path)} repeats the member $keyword"

          case Key(path, keyword) =>
            m"${at(path)} does not key the member $keyword"

          case VariantMissing(path, keyword) =>
            m"${at(path)} offers the variant $keyword, which the supertype does not"

          case TypeKind(path) =>
            m"${at(path)} has a type of a different kind"

          case Unresolved(path, name) =>
            m"${at(path)} references $name, which does not resolve"

    enum Failure:
      case MissingRequired(path: List[Text], keyword: Text)
      case MemberOrder(path: List[Text], keyword: Text)
      case MemberKind(path: List[Text], keyword: Text)
      case Validators(path: List[Text])
      case Patterns(path: List[Text])
      case Encoding(path: List[Text])
      case Required(path: List[Text], keyword: Text)
      case Repeatable(path: List[Text], keyword: Text)
      case Key(path: List[Text], keyword: Text)
      case VariantMissing(path: List[Text], keyword: Text)
      case TypeKind(path: List[Text])
      case Unresolved(path: List[Text], name: Text)

    def subtype(sub: Tels, sup: Tels): Boolean = check(sub, sup).absent

    def check(sub: Tels, sup: Tels): Optional[Failure] =
      Checker(sub, sup).struct(sub.document, sup.document, Nil, sci.Set())

    // The always-valid built-in validator (§21.5): declared by the implicit `String` scalar of a
    // parsed document but by no derived one, so it is disregarded when validator sets are
    // compared.
    private def constraining(validators: Array[Text]^{}): sci.Set[Text] =
      validators.readable.toList.filter(_ != t"string").to(sci.Set)

    private def required(polarity: Polarity): Boolean = polarity != Polarity.Loose
    private def repeatable(polarity: Polarity): Boolean = polarity == Polarity.Loose

    private class Checker(sub: Tels, sup: Tels):
      // A member's keywords: a field's own, or every variant keyword of a select member.
      private def keywords(member: Member, schema: Tels): sci.Set[Text] = member match
        case field: Field => sci.Set(field.keyword)

        case select: SelectRef =>
          schema.selects.seek(_.name == select.reference).lay(sci.Set()): definition =>
            definition.variants.readable.toList.map(_.keyword).to(sci.Set)

        case _: Exclude => sci.Set()

      // A type resolved through its schema's namespace: a record's struct, a scalar, or a
      // built-in scalar; `Unset` when the name is unbound or names a select.
      private def resolve(fieldType: Type, schema: Tels): Optional[Type] = fieldType match
        case Reference(name) =>
          schema.records.seek(_.name == name).lay(scalarOf(name, schema)): record =>
            Struct(record.members, record.validators)

        case other => other

      private def scalarOf(name: Text, schema: Tels): Optional[Type] =
        schema.scalars.seek(_.name == name).lay(builtinScalar(name)): definition =>
          Scalar(definition.validators, definition.encoding, definition.patterns)

      private def builtinScalar(name: Text): Optional[Type] =
        if name == Builtin.String then Scalar(Array.empty)
        else if name == Builtin.Identifier then Scalar(Array(t"identifier"))
        else if name == Builtin.TypeName then Scalar(Array(t"type-name"))
        else if name == Builtin.Sigil then Scalar(Array(t"sigil"))
        else Unset

      def struct(left: Struct, right: Struct, path: List[Text], assumed: sci.Set[(Text, Text)])
      :   Optional[Failure] =

        if !constraining(right.validators).subsetOf(constraining(left.validators))
        then Failure.Validators(path)
        else
          val members = left.members.readable.toList.map: member => (member, keywords(member, sub))
          var last = -1
          var failure: Optional[Failure] = Unset

          right.members.each: member =>
            if failure.absent then
              val keys = keywords(member, sup)
              val index = members.indexWhere(_(1).exists(keys.contains))
              val keyword = keys.headOption.getOrElse(t"")

              if index < 0 then
                if requiredMember(member) then failure = Failure.MissingRequired(path, keyword)
              else if index <= last then
                failure = Failure.MemberOrder(path, keyword)
              else
                last = index
                failure = this.member(members(index)(0), member, path, assumed)

          failure

      private def requiredMember(member: Member): Boolean = member match
        case field: Field       => required(field.required)
        case select: SelectRef  => required(select.required)
        case _: Exclude         => false

      private def member
        ( left: Member, right: Member, path: List[Text], assumed: sci.Set[(Text, Text)] )
      :   Optional[Failure] =

        (left, right) match
          case (left: Field, right: Field) =>
            val keyword = right.keyword

            if required(right.required) && !required(left.required)
            then Failure.Required(path, keyword)
            else if repeatable(left.repeatable) && !repeatable(right.repeatable)
            then Failure.Repeatable(path, keyword)
            else if right.key && !left.key then Failure.Key(path, keyword)
            else fieldType(left.fieldType, right.fieldType, keyword :: path, assumed)

          case (left: SelectRef, right: SelectRef) =>
            val keyword = right.reference

            if required(right.required) && !required(left.required)
            then Failure.Required(path, keyword)
            else if repeatable(left.repeatable) && !repeatable(right.repeatable)
            then Failure.Repeatable(path, keyword)
            else select(left.reference, right.reference, keyword :: path, assumed)

          case (_, right: Field)     => Failure.MemberKind(path, right.keyword)
          case (_, right: SelectRef) => Failure.MemberKind(path, right.reference)
          case _                     => Unset

      // [Sub-Select]: every variant of the subtype's definition is a variant of the supertype's,
      // with a subtype-compatible type.
      private def select(left: Text, right: Text, path: List[Text], assumed: sci.Set[(Text, Text)])
      :   Optional[Failure] =

        val leftDefinition = sub.selects.seek(_.name == left)
        val rightDefinition = sup.selects.seek(_.name == right)

        (leftDefinition, rightDefinition) match
          case (leftDefinition: SelectDefinition, rightDefinition: SelectDefinition) =>
            val rightValidators = constraining(rightDefinition.validators)
            val leftValidators = constraining(leftDefinition.validators)

            if !rightValidators.subsetOf(leftValidators) then Failure.Validators(path)
            else
              var failure: Optional[Failure] = Unset

              leftDefinition.variants.each: variant =>
                if failure.absent then
                  rightDefinition.variants.seek(_.keyword == variant.keyword) match
                    case counterpart: Variant =>
                      failure = fieldType(variant.variantType, counterpart.variantType,
                          variant.keyword :: path, assumed)

                    case _ =>
                      failure = Failure.VariantMissing(path, variant.keyword)

              failure

          case (_: SelectDefinition, _) => Failure.Unresolved(path, right)
          case _                        => Failure.Unresolved(path, left)

      private def fieldType
        ( left: Type, right: Type, path: List[Text], assumed: sci.Set[(Text, Text)] )
      :   Optional[Failure] =

        (left, right) match
          case (Reference(l), Reference(r)) if assumed.contains((l, r)) => Unset

          case _ =>
            val assumed2 = (left, right) match
              case (Reference(leftName), Reference(rightName)) => assumed + ((leftName, rightName))
              case _                                           => assumed

            (resolve(left, sub), resolve(right, sup)) match
              case (Flag, Flag)                  => Unset
              case (left: Scalar, right: Scalar) => scalar(left, right, path)
              case (left: Struct, right: Struct) => struct(left, right, path, assumed2)
              case (Unset, _)                    => Failure.Unresolved(path, nameOf(left))
              case (_, Unset)                    => Failure.Unresolved(path, nameOf(right))
              case _                             => Failure.TypeKind(path)

      private def nameOf(fieldType: Type): Text = fieldType match
        case Reference(name) => name
        case _               => t""

      // [Sub-Scalar]: the subtype has at least the supertype's validators, its pattern language
      // lies within the supertype's (an empty pattern set denoting Σ*), and it keeps the
      // supertype's encoding, or adds one where there was none.
      private def scalar(left: Scalar, right: Scalar, path: List[Text]): Optional[Failure] =
        val validators = constraining(right.validators).subsetOf(constraining(left.validators))

        val patterns =
          right.patterns.nil ||
            (!left.patterns.nil && Layers.contained(left.patterns, right.patterns))

        val encoding = right.encoding.absent || right.encoding == left.encoding

        if !validators then Failure.Validators(path)
        else if !patterns then Failure.Patterns(path)
        else if !encoding then Failure.Encoding(path)
        else Unset

  // The projection of §24.5: an element valid under a subtype schema, restricted to the members
  // its supertype can address. At every struct, children whose keyword the supertype's struct
  // does not declare are dropped, and the rest are re-indexed to the supertype's flat keyword
  // positions — BinTEL keyword indices are positional, so a projection that kept the subtype's
  // indices would encode a different document — and typed by the supertype's member. Scalars and
  // flags pass through unchanged. `from <: to` is the caller's responsibility (`Subtyping`); a
  // member whose kinds disagree is dropped rather than mistyped.
  object Projection:
    def project(element: Tel.Element, from: Tels, to: Tels): Tel.Element raises Bintel.Error =
      element match
        case Tel.Element.Node(_, source: Struct, children) =>
          val projected = projectChildren(children, source, from, to.document, to)
          Tel.Element.Node(Unset, to.document, projected)

        case other => other

    private def projectChildren
      ( children: Array[Tel.Element]^{}, source: Struct, from: Tels, target: Struct, to: Tels )
    :   Array[Tel.Element]^{} raises Bintel.Error =

      val sourceKeywords = Bintel.flattenKeywords(source, from)
      val targetKeywords = Bintel.flattenKeywords(target, to)
      val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]

      def indexOf(element: Tel.Element): Int = element match
        case Tel.Element.Node(index, _, _)  => index.or(0)
        case Tel.Element.Value(index, _, _) => index

      children.each: child =>
        val index = indexOf(child)

        if index >= 0 && index < sourceKeywords.length then
          val keyword = sourceKeywords.readable(index)(0)
          val position = targetKeywords.readable.indexWhere(_(0) == keyword)

          if position >= 0 then
            (child, resolve(targetKeywords.readable(position)(1), to)) match
              case (Tel.Element.Value(_, _, text), scalar: Scalar) =>
                buffer += Tel.Element.Value(position, scalar, text)

              case (Tel.Element.Node(_, Flag, _), Flag) =>
                buffer += Tel.Element.Node(position, Flag, Array.empty)

              case (Tel.Element.Node(_, nested: Struct, grandchildren), struct: Struct) =>
                buffer += Tel.Element.Node(position, struct,
                    projectChildren(grandchildren, nested, from, struct, to))

              case _ => ()

      Array.from(buffer)

    private def resolve(fieldType: Type, schema: Tels): Optional[Type] = fieldType match
      case Reference(name) =>
        schema.records.seek(_.name == name).lay(scalarOf(name, schema)): record =>
          Struct(record.members, record.validators)

      case other => other

    private def scalarOf(name: Text, schema: Tels): Optional[Type] =
      val builtinName =
        name == Builtin.String || name == Builtin.Identifier || name == Builtin.TypeName ||
          name == Builtin.Sigil

      val builtin: Optional[Type] = if builtinName then Scalar(Array.empty) else Unset

      schema.scalars.seek(_.name == name).lay(builtin): definition =>
        Scalar(definition.validators, definition.encoding, definition.patterns)

case class Tels
  ( name:     Text,
    document: Tels.Struct,
    layers:   Array[Tels.Layer]^{},
    sigil:    Optional[Char],
    records:  Array[Tels.RecordDefinition]^{},
    scalars:  Array[Tels.ScalarDefinition]^{},
    selects:  Array[Tels.SelectDefinition]^{} )

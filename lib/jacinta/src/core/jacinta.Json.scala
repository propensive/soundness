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
package jacinta

import language.dynamics
import language.experimental.pureFunctions

import scala.collection.Factory
import scala.collection.mutable as scm
import scala.compiletime.*

import adversaria.*
import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import panopticon.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import JsonError.Reason

// Base mixin for Jacinta's decoder instances. Fixes the focus type to
// `Json.Focus` and provides the position-enrichment hook that `Json#as[T]` runs
// over the accumulated `Foci[Json.Focus]` after decoding.
private[jacinta] trait JsonDecodable[T] extends Decodable:
  type Self = T
  type Form = Json
  type Locus = Json.Focus

  override def position(value: Json, focus: Json.Focus): Json.Focus =
    focus.withPosition(value)

// Lowest-priority layer (extended by `Json2`), holding only the focus adapter,
// which lifts any `Decodable in Json` into one carrying `type Locus = Json.Focus`
// (used by `as[T]`). Keeping it below the carrying decoders matters: its result
// (`… at Json.Focus`) is itself a subtype of a plain `Decodable in Json`, so at
// equal priority it would compete with — and be incomparable to (`Locus` vs
// `shape`) — the carrying decoders for a plain `summon[T is Decodable in Json]`,
// yielding an ambiguity. At lower priority it is ignored for plain/carrier
// summons, yet `as[T]` still finds it because it is the *only* given producing the
// `at Json.Focus` form. Deliberately broad (`Decodable in Json`, not
// `Json.Decodable`) so generic `as[T]` callers bounded on `Decodable in Json`
// still resolve.
trait Json3:
  // The universal fallback for `Json.Field` — the typeclass the product
  // derivation resolves per field — mirroring `Json2.decodable`'s dispatch
  // order so a field's wire format is identical on both paths: an explicit
  // (or derived) direct parser; a string codec; structural derivation; or
  // the AST bridge over any remaining `Json.Decodable` (opaque leaf types
  // like `Instant`). A case class with both a `Reflection` and a custom
  // hand-written `Json.Decodable` derives here, diverging from its custom
  // decoder — the documented remedy is one line:
  // `given MyType is Json.Parsable = Json.Parsable.fromDecodable(...)`.
  // Lowest priority so `Json2`'s element-wise field givens match collection
  // types first (a `List`'s own `Mirror` would otherwise derive it as a
  // sum).
  inline given field: [value] => value is Json.Field = summonFrom:
    case parsable: (`value` is Json.Parsable) =>
      Json.Field(parsable)

    case given (`value` is distillate.Decodable in Text) =>
      // Laundered pure per the codec-thunk seal pattern: the parse lambda
      // closes over the resolution-scoped text decodable.
      caps.unsafe.unsafeAssumePure:
        Json.Field(Json.Parsable(Morphology.Str)(_.string().as[value]))

    case given Reflection[`value`] =>
      Json.ParsableDerivation.derived

    case given (`value` is Json.Decodable) =>
      caps.unsafe.unsafeAssumePure:
        Json.Field(Json.Parsable.fromDecodable(infer[`value` is Json.Decodable]))

  given decodableAtFocus: [value]
  =>  ( inner: (value is Decodable in Json)^ )
  =>  ((value is Decodable in Json at Json.Focus)^{inner, caps.any}) =

    new JsonDecodable[value]:
      def decoded(json: Json): value = inner.decoded(json)

trait Json2 extends Json3:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Json.Encodable )
  =>  value is Json.Encodable =

    // Sealed lazily: the shape must stay by-name (recursive derivation
    // depends on deferral), and its thunk may not capture the evidence.
    val shape: () -> Morphology =
      caps.unsafe.unsafeAssumePure(() => Morphology.Opt(encodable.shape()))


    Json.Encodable(shape): value =>
      value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Json.ast(Json.Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[JsonError]
  =>  ( decodable: => (inner is Json.Decodable)^ )
  =>  value is Json.Decodable =

    // HONESTY BLOCKED BY THE CHECKER, not by design (Jon's 2026-07-12 ruling wants
    // this instance to be a capability): a by-name parameter cannot be named in a
    // capture set, so the honest result type must illegally hide `decodable`, and
    // the synthesized shape thunk aliases the tactic argument under separation
    // checking. Sealed until the checker can express it; see rep/DECISIONS.md
    // (upstream candidate: nameable by-name captures).
    caps.unsafe.unsafeAssumePure:
      val shape: () -> Morphology =
        caps.unsafe.unsafeAssumePure(() => Morphology.Opt(decodable.shape()))

      Json.Decodable(shape()): json =>
        // An `Optional` field reads `Unset` from an absent key *and* from an
        // explicit JSON `null`: both mean "no value" on the wire. (Missing keys
        // arrive here as `Unset`; a present `null` arrives as the `JsonNull`
        // sentinel.)
        if json.root.isAbsent || json.root.isNull then Unset else decodable.decoded(json)


  // An honest capability: the instance retains the resolution-scoped tactic
  // (every given that includes a tactic is a capability; Jon, 2026-07-12).
  given bytes: (tactic: Tactic[JsonError])
  =>  ((Bytes is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Whole)(_.root.long.b)

  // Element-wise `Json.Field` instances resolved during derivation: the
  // element's own parser comes from the fallback chain, so nested products
  // still parse directly. This layer beats `Json3`'s fallback, so collection
  // types never reach its `Reflection` case (a `List`'s own `Mirror` would
  // otherwise derive it as a sum).
  given fieldOptional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  Tactic[JsonError]
  =>  ( field: => (inner is Json.Field)^ )
  =>  value is Json.Field =
    Json.Field(Json.Parsable.optionality[inner, value](field))

  given fieldOption: [value] => (field: => (value is Json.Field)^)
  =>  Option[value] is Json.Field =
    Json.Field(Json.Parsable.boxed(field))

  given fieldArray: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[JsonError],
        foci:    Foci[Json.Focus] )
  =>  ( field: => (element is Json.Field)^ )
  =>  collection[element] is Json.Field =
    Json.Field(Json.Parsable.iterable[collection, element](field))

  given fieldMap: [key: distillate.Decodable in Text, element]
  =>  Tactic[JsonError]
  =>  ( field: => (element is Json.Field)^ )
  =>  Map[key, element] is Json.Field =
    Json.Field(Json.Parsable.dictionary[key, element](field))

  // The nominal counterpart of `fieldOptional`: an `Optional` reads
  // directly only when its element has opted in.
  given optionalParsable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  Tactic[JsonError]
  =>  ( parsable: => (inner is Json.Parsable)^ )
  =>  value is Json.Parsable =
    Json.Parsable.optionality[inner, value](parsable)

  // The AST-materializing read path: parse the whole input into a `Json`,
  // then decode. Lives at this priority so `object Json`'s direct-parsing
  // `aggregableParsed` wins whenever the value has a `Json.Parsable`; when it
  // does not (all pre-`Parsable` code), this resolves exactly as before.
  // Sealed like `Json.aggregable`; see the comment there.
  given aggregableDirect: [value: distillate.Decodable in Json]
  =>  (tactic: Tactic[ParseError], jsonTactic: Tactic[JsonError], tracking: PositionTracking)
  =>  ((value in Json) is Aggregable by Data) =

    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = value in Json
        type Operand = Data

        def aggregate(bytes: LazyList[Data]): value in Json =
          Json.readJson(bytes.iterator).as[value].asInstanceOf[value in Json]

        override def accept(stream: (Stream[Data] over Credit)^): value in Json =
          Json.readJson(stream).as[value].asInstanceOf[value in Json]

  inline given decodable: [value] => value is Json.Decodable = summonFrom:
    // `Json` decodes to itself. Handled here (not as a separate carrier given) so it
    // does not compete — as a `Json.Decodable` subtype — with the plain
    // `jsonDecodable` for a `Json is Decodable in Json` summon (which must beat
    // distillate's `generic`).
    case _: (`value` =:= Json) =>
      Json.Decodable[Json](Morphology.Any)(identity(_)).asInstanceOf[value is Json.Decodable]

    case given (`value` is distillate.Decodable in Text) =>
      // The decode lambda closes over the `provide`-summoned tactic, which shares the
      // instance's given-resolution lifetime; laundered pure per the codec-thunk seal
      // pattern (see rep/DECISIONS.md), like the primitive codecs.
      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str)(provide[Tactic[JsonError]](_.root.string.as[value]))

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Json.Encodable = summonFrom:
    case given (`value` is anticipation.Encodable in Text) =>
      Json.Encodable(() => Morphology.Str): value => Json.ast(Json.Ast(value.encode.s))

    case given Reflection[`value`] =>
      EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Json.Decodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   (derivation is Json.Decodable)^ =

      // The object `Morphology` is built from the *field decoders'* own shapes, so it
      // describes exactly what this decoder reads (a `… in Text`-branch field
      // contributes a string shape, not its derived object shape), keeping a fused
      // `Decodable & Schematic` coherent. A single `contexts` traversal (rather than
      // one each for fields and required) keeps the inlined codegen within JVM class
      // limits; it must be inlined here (not factored into a helper) so it does not
      // perturb the `build` traversal below. Built by-name so recursive types compile.
      // The capabilities are summoned at the derivation site and supplied explicitly to
      // `decodeObject`, rather than re-summoned inside the decoder body via nested `provide`s
      // (under capture checking those minted distinct root capabilities that failed to unify
      // with `build`'s per-field lambda). The decode SAM closes over the resolution-scoped
      // tactic, which the fresh (`^`) result honestly admits — no seal.
      Json.Decodable({
        val fields: List[(Text, Morphology)] =
          contexts[derivation](): [field] => context => (label, context.shape())
          . to(List)

        Morphology.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
      }):
        json =>
          decodeObject[derivation](json)
            ( using infer[ProductReflection[derivation]],
                    infer[Foci[Json.Focus]],
                    infer[Tactic[JsonError]] )

    private inline def decodeObject[derivation <: Product]
      ( json: Json )
      ( using ProductReflection[derivation], Foci[Json.Focus], Tactic[JsonError] )
    :   derivation =

      val root = json.root
      val n = root.objectSize

      // Built immutably: `build`'s per-field lambda is polymorphic (`[field] => …`) and must be
      // pure, so it may only close over pure values — a mutable map would be a capability.
      val values: Map[String, Json.Ast] =
        val builder = Map.newBuilder[String, Json.Ast]
        var i = 0

        while i < n do
          builder += root.objectKey(i) -> root.objectValue(i)
          i += 1

        builder.result()

      // `@name[Json]` / bare `@name` renames: field name -> JSON key, read
      // back the same way they are written.
      val renames: Map[Text, Text] = relabelling[derivation, Json]

      build[derivation]: [field] =>
        context =>
          val key: Text = renames.at(label).or(label)

          focus({
            val base = prior.let(_.pointer).or(JsonPointer())

            val newPointer =
              JsonPointer
                ( base.url,
                  Path[JsonPointer, JsonPointer.type, Tuple]
                    ( base.path.root, base.path.descent :+ key ) )

            Json.Focus(newPointer)
          }):
            values.get(key.s) match
              case Some(value) => context.decoded(new Json(value))
              case None        => default.or(context.decoded(new Json(Json.Ast(Unset))))

    inline def disjunction[derivation: SumReflection]: derivation is Json.Decodable =
      // A sum encodes as a discriminated object. Its precise per-variant schema is
      // available from the standalone `Schematic` / `JsonSchema.derived`; the
      // codec-carried shape is kept permissive (`Any`) because the only way to walk
      // the variants here (`delegate`) is `fallible` and would leak a
      // `Tactic[VariantError]` requirement onto every codec.
      // The shape test happens once, outside the decode lambda: a reference
      // to the shape class inside the nested context functions poisons
      // their capture sets.
      val fieldShaped: Boolean =
        infer[derivation is Discriminable in Json].isInstanceOf[Json.DiscriminantField[?]]

      Json.Decodable(Morphology.Any):
        json =>
          provide[Tactic[JsonError]]:
            provide[Tactic[VariantError]]:
              val discriminable = infer[derivation is Discriminable in Json]

              // `@name[Json]` / bare `@name` variant renames: map the serialized
              // discriminator back to the variant name before delegating.
              val variantNames: Map[Text, Text] =
                variantRelabelling[derivation, Json].map: (variant, wire) => wire -> variant

              val wire: Text = discriminable.discriminate(json).or:
                focus(prior.or(Json.Focus(JsonPointer())))(abort(JsonError(Reason.Absent)))

              val discriminant: Text = variantNames.getOrElse(wire, wire)

              // The variant decodes the whole value for the internal-field
              // shape (its tag is simply skipped as an unknown key — no need
              // to rebuild the object without it), and the extracted payload
              // for every other shape.
              val payload = if fieldShaped then json else discriminable.variant(json)

              delegate(discriminant): [variant <: derivation] =>
                context => context.decoded(payload)

  object ParsableDerivation extends Derivable[Json.Field]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   (derivation is Json.Field)^ =

      // Like `DecodableDerivation.conjunction`: the capabilities are summoned
      // at the derivation site, and the fresh (`^`) result honestly admits the
      // instance's capture of them — no seal. A single `contexts` traversal
      // collects, per field, its wire key (`@name`-aware), its parser (via the
      // `Field` fallback chain) and its declared default; `Json.Parsable.product`
      // owns the parse loop, so no per-field lambda ever closes over the reader.
      val reflection = infer[ProductReflection[derivation]]

      Json.Parsable.product[derivation](
        { () =>
          val renames: Map[Text, Text] = relabelling[derivation, Json]

          contexts[derivation]():
            [field] => context =>
              ( renames.at(label).or(label).s,
                context: Json.Parsing,
                default[Optional[field]]: Any )
        },
        values => Json.Parsable.assemble(reflection, values))
        ( using infer[Foci[Json.Focus]], infer[Tactic[JsonError]] )

    inline def disjunction[derivation: SumReflection]: (derivation is Json.Field)^ =
      // Dispatch strategy by wire shape: a wrapper's tag is its first token,
      // so it streams with no lookahead at all; an envelope and an internal
      // field locate the tag with a bounded scan-ahead (`discriminant`
      // skips values without materializing them, then rewinds), after which
      // the chosen variant parses directly from the tokens. Any custom
      // `Discriminable` falls back to materializing one value's AST and
      // dispatching through the decoder. The fresh (`^`) result honestly
      // admits each instance's capture of its resolution-scoped tactics.
      locally:
        // Wire tag → variant label, a per-derivation constant: built once
        // here rather than on every `parse` call, whose profile it dominated
        // (map building plus generic-equality lookups, per occurrence).
        val variantNames: Map[Text, Text] =
          variantRelabelling[derivation, Json].map: (variant, wire) =>
            wire -> variant

        infer[derivation is Discriminable in Json] match
          case fielded: Json.DiscriminantField[?] =>
            new Json.Field:
              type Self = derivation
              def shape(): Morphology = Morphology.Any

              def parse(reader: JsonReader^): derivation =
                provide[Tactic[JsonError]]:
                  provide[Tactic[VariantError]]:
                    val wire: Text = reader.discriminant(fielded.field).or:
                      abort(JsonError(Reason.Absent))

                    // The variant re-reads the whole object, skipping the
                    // tag as an unknown key.
                    delegate(variantNames.getOrElse(wire, wire)):
                      [variant <: derivation] => context => context.parse(reader)

          case wrapper: Json.DiscriminantWrapper[?] =>
            new Json.Field:
              type Self = derivation
              def shape(): Morphology = Morphology.Any

              def parse(reader: JsonReader^): derivation =
                provide[Tactic[JsonError]]:
                  provide[Tactic[VariantError]]:
                    reader.openObject()
                    val wire: Text = reader.key().or(abort(JsonError(Reason.Absent)))

                    val result =
                      delegate(variantNames.getOrElse(wire, wire)):
                        [variant <: derivation] => context => context.parse(reader)

                    // A wrapper is a single-key object; anything more means
                    // no tag is identifiable, as on the AST path.
                    if !reader.key().absent then abort(JsonError(Reason.Absent))
                    result

          case envelope: Json.DiscriminantEnvelope[?] =>
            new Json.Field:
              type Self = derivation
              def shape(): Morphology = Morphology.Any

              def parse(reader: JsonReader^): derivation =
                provide[Tactic[JsonError]]:
                  provide[Tactic[VariantError]]:
                    val wire: Text = reader.discriminant(envelope.tagField).or:
                      abort(JsonError(Reason.Absent))

                    val name = variantNames.getOrElse(wire, wire)
                    reader.openObject()
                    var result: Optional[derivation] = Unset
                    var continue = true

                    while continue do
                      val key = reader.key()

                      if key.absent then continue = false
                      else if key.or(t"") == envelope.valueField && result.absent then
                        result = delegate(name):
                          [variant <: derivation] => context => context.parse(reader)
                      else reader.skipValue()

                    result.or(abort(JsonError(Reason.Absent)))

          case other =>
            Json.Field(Json.Parsable.fromDecodable(infer[derivation is Json.Decodable]))

  object EncodableDerivation extends Derivable[Json.Encodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Json.Encodable =

      Json.Encodable({ () =>
        val fields: List[(Text, Morphology)] =
          contexts[derivation](): [field] => context => (label, context.shape())
          . to(List)

        Morphology.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
      }):
        value =>
          provide[Foci[Json.Focus]]:
            val labels: scm.ArrayBuffer[String] = scm.ArrayBuffer()
            val values: scm.ArrayBuffer[Json.Ast] = scm.ArrayBuffer()

            // `@name[Json]` / bare `@name` renames: field name -> JSON key.
            val renames: Map[Text, Text] = relabelling[derivation, Json]

            fields(value): [field] =>
              field =>
                val key: Text = renames.at(label).or(label)

                focus({
                  val base = prior.let(_.pointer).or(JsonPointer())

                  val newPointer =
                    JsonPointer
                      ( base.url,
                        Path[JsonPointer, JsonPointer.type, Tuple]
                          ( base.path.root, base.path.descent :+ key ) )

                  Json.Focus(newPointer)
                }):
                  contextual.encode(field).root.tap: encoded =>
                    if !encoded.isAbsent then
                      labels += key.s
                      values += encoded

            Json.ast
              ( Json.Ast.obj
                  ( unsafely(labels.toArray.immutable), unsafely(values.toArray.immutable) ) )

    inline def disjunction[derivation: SumReflection]: derivation is Json.Encodable =
      // See the decoder disjunction: the codec-carried sum shape is permissive
      // (`Any`); precise `oneOf` schemas come from the standalone `Schematic`.
      Json.Encodable(() => Morphology.Any):
        value =>
          val discriminable = infer[derivation is Discriminable in Json]

          // `@name[Json]` / bare `@name` variant renames: variant name -> wire
          // discriminator, read back the same way by the decoder.
          val variantNames: Map[Text, Text] = variantRelabelling[derivation, Json]

          variant(value): [variant <: derivation] =>
            value =>
              discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

object Json extends Json2, Dynamic:
  // Controls how a `Json` value is serialized. `indent` is the whitespace unit per nesting level;
  // `Unset` (the default) produces minimal, single-line JSON. `trailingNewline` appends a final
  // newline. Bundled as `formatting.compactJsonFormatting` / `formatting.indentedJsonFormatting`.
  object Formatting:
    def apply(indent: Optional[Text], trailingNewline: Boolean): Formatting =
      Basic(indent, trailingNewline)

    private case class Basic(indent: Optional[Text], trailingNewline: Boolean) extends Formatting

  trait Formatting extends zephyrine.Formatting:
    def indent: Optional[Text]
    def trailingNewline: Boolean

  type JsonString  = String
  type JsonNumber  = Long | Double | Bcd | Int
  type JsonBoolean = Boolean
  // A distinct sentinel for a JSON `null` value, kept disjoint from the null-backed
  // `Unset` (absent): both would otherwise be the JVM `null` and collide in `Ast`.
  case object JsonNull
  type JsonNull    = JsonNull.type
  type JsonObject  = IArray[Any]
  type JsonArray   = IArray[Any] | Array[Long] | Array[Int]

  object Encodable:
    def apply[value](shape0: () => Morphology)(lambda: (value -> Json)^)
    :   ((value is Json.Encodable)^{shape0, lambda}) =

      // An honest capability: the instance retains the encode lambda and the shape
      // thunk, which may capture the resolution-scoped tactics of field/element
      // codecs (every given that includes a tactic is a capability; Jon,
      // 2026-07-13). The shape is an EXPLICIT thunk — nameable in the capture set,
      // unlike a by-name — so pure call sites yield pure instances, and deferral
      // (which recursive derivation depends on) is the caller's one-liner.
      new Json.Encodable:
        type Self = value
        def encoded(value: value): Json = lambda(value)
        def shape(): Morphology = shape0()

  // A JSON encoder that also carries the format-neutral `Morphology` describing exactly
  // what it produces. Making the shape travel *with* the codec is what lets a fused
  // `Encodable & Schematic` (built by `jsonSchematics.encodable`) be coherent by
  // construction — the shape is never resolved independently of the codec, so a
  // gated or `… in Text`-branch encoder always pairs with its own matching shape.
  // The `Morphology` is reified into a concrete `JsonSchema` downstream (in the schema
  // module), so the codec — and `jacinta.core` — need not know `JsonSchema` at all.
  // It is deliberately *not* `Schematic` (that subtyping is what caused the earlier
  // resolution ambiguity); it merely *has* a `shape()`.
  trait Encodable extends anticipation.Encodable:
    type Form = Json
    def shape(): Morphology

  object Decodable:
    def apply[value](shape0: => Morphology)(decoder: (value is distillate.Decodable in Json)^)
    :   ((value is Json.Decodable)^{decoder}) =
      // Same shape-thunk laundering as `Encodable.apply`; see the comment there.
      val shape1: () -> Morphology = caps.unsafe.unsafeAssumePure { () => shape0 }

      new Json.Decodable:
        type Self = value
        def decoded(json: Json): value = decoder.decoded(json)
        def shape(): Morphology = shape1()

  // The decoding counterpart of `Json.Encodable`: a `Decodable in Json` that also
  // carries the `Morphology` of exactly what it reads, so `jsonSchematics.decodable` and
  // `verify` get a schema that is guaranteed coherent with the decoder.
  trait Decodable extends distillate.Decodable:
    type Form = Json
    def shape(): Morphology

  object Parsable:
    def apply[value](shape0: => Morphology)(parser: (reader: JsonReader^) => value)
    :   ((value is Json.Parsable)^{parser}) =
      // Same shape-thunk laundering as `Encodable.apply`; see the comment there.
      val shape1: () -> Morphology = caps.unsafe.unsafeAssumePure { () => shape0 }

      new Json.Parsable:
        type Self = value
        def parse(reader: JsonReader^): value = parser(reader)
        def shape(): Morphology = shape1()

    // The universal bridge from the AST world: parse one whole value into a
    // `Json` and decode it. Field types with only a `Decodable in Json` keep
    // working through this, and it is the user's one-line escape hatch when a
    // custom decoder must beat a derived direct parser.
    def fromDecodable[value](decodable: (value is Json.Decodable)^)
    :   ((value is Json.Parsable)^{decodable}) =

      new Json.Parsable:
        type Self = value
        def parse(reader: JsonReader^): value = decodable.decoded(reader.value())
        def shape(): Morphology = decodable.shape()

        override def absent()(using Tactic[JsonError]): value =
          decodable.decoded(Json.ast(Json.Ast(Unset)))

    // The one-line opt-in to direct parsing for a structural type:
    // `given MyType is Json.Parsable = Json.Parsable.derived` — a
    // Wisteria-derived direct parser, wrapped as a *nominal* `Parsable` so
    // it participates in the read trigger. Deliberately a method, not a
    // blanket given: a type parses directly only once it has opted in.
    inline def derived[value](using Reflection[value]): value is Json.Parsable =
      fromField(ParsableDerivation.derivedOne[value])

    // Generates a monomorphic parser at compile time — the staged
    // counterpart of `derived`, with identical semantics but no interpretive
    // machinery at runtime. A case class parses through typed local slots
    // and packed-literal key dispatch; a sealed sum (whose `Discriminable`
    // must be a field-discriminated shape, like `jsonByKindDiscriminable`)
    // dispatches its scan-ahead tag through a monomorphic comparison chain
    // into per-variant `Json.Field` instances. Method-local classes,
    // curried case classes, singleton variants and generic sums stay on
    // `derived`.
    inline def staged[value]: value is Json.Parsable = summonFrom:
      case given SumReflection[`value`] => stagedSum[value]
      case _                            => stagedProduct[value]

    private inline def stagedProduct[value]: value is Json.Parsable =
      ${ jacinta.internal.stagedParsable[value]('{ relabelling[value, Json] }) }

    private inline def stagedSum[value]: value is Json.Parsable =
      ${ jacinta.internal.stagedSum[value]('{ variantRelabelling[value, Json] }) }

    // The scan-ahead tag field of a field-discriminated sum, for staged
    // parsers, which dispatch on it monomorphically. The other shapes have
    // no equivalent single step, so a staged sum whose `Discriminable` is
    // not a `DiscriminantField` fails fast at instance construction. Public
    // because staged parsers are generated into user modules.
    def discriminantField(discriminable: Discriminable in Json): Text =
      discriminable match
        case fielded: Json.DiscriminantField[?] => fielded.field

        case other =>
          panic
            (m"""staged sum parsing requires a field-discriminated sum (a `DiscriminantField`
                 `Discriminable`, like `jsonByKindDiscriminable`); other shapes use
                 `Json.Parsable.derived`""")

    def fromField[value](field0: (value is Json.Parsing)^)
    :   ((value is Json.Parsable)^{field0}) =

      new Json.Parsable:
        type Self = value
        def parse(reader: JsonReader^): value = field0.parse(reader)
        def shape(): Morphology = field0.shape()
        override def absent()(using Tactic[JsonError]): value = field0.absent()

    // Shared element-wise implementations, used by the nominal
    // `Json.Parsable` givens (elements must themselves be nominally
    // `Parsable`, so the read trigger stays opt-in) and by the `Field`
    // givens above (elements resolve through the fallback chain). Each is
    // sealed per the codec-thunk pattern: a by-name parameter cannot be
    // named in a capture set (see `Json2.optional`).
    def optionality[inner <: value, value >: Unset.type]
      ( field: => (inner is Json.Parsing)^ )
      ( using tactic: Tactic[JsonError] )
    :   value is Json.Parsable =

      caps.unsafe.unsafeAssumePure:
        new Json.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Opt(field.shape())

          // A wire `null` reads as `Unset`, and so does an absent key: both
          // mean "no value", exactly as the AST decoder's `optional`.
          def parse(reader: JsonReader^): value =
            if reader.hasNull then
              reader.nullValue()
              Unset
            else field.parse(reader)

          override def absent()(using Tactic[JsonError]): value = Unset

    def boxed[value](field: => (value is Json.Parsing)^)
    :   Option[value] is Json.Parsable =

      caps.unsafe.unsafeAssumePure:
        new Json.Parsable:
          type Self = Option[value]
          def shape(): Morphology = Morphology.Opt(field.shape())

          // A present key always reads the value — a wire `null` flows to
          // the element, preserving the AST decoder's `Option` asymmetry —
          // while an absent key yields `None`.
          def parse(reader: JsonReader^): Option[value] = Some(field.parse(reader))
          override def absent()(using Tactic[JsonError]): Option[value] = None

    def iterable[collection <: Iterable, element]
      ( field: => (element is Json.Parsing)^ )
      ( using factory: Factory[element, collection[element]],
              tactic:  Tactic[JsonError],
              foci:    Foci[Json.Focus] )
    :   collection[element] is Json.Parsable =

      caps.unsafe.unsafeAssumePure:
        new Json.Parsable:
          type Self = collection[element]
          def shape(): Morphology = Morphology.Arr(field.shape())

          def parse(reader: JsonReader^): collection[element] =
            val builder = factory.newBuilder
            // See the derived product parser: skip inert focus wrapping.
            val focused = foci.active
            reader.openArray()
            var index = 0

            while reader.element() do
              builder +=
                ( if focused
                  then focus(descend(prior, index.toString.tt))(field.parse(reader))
                  else field.parse(reader) )
              index += 1

            builder.result()

    def dictionary[key: distillate.Decodable in Text, element]
      ( field: => (element is Json.Parsing)^ )
      ( using tactic: Tactic[JsonError] )
    :   Map[key, element] is Json.Parsable =

      caps.unsafe.unsafeAssumePure:
        new Json.Parsable:
          type Self = Map[key, element]
          def shape(): Morphology = Morphology.Dict(Morphology.Str, field.shape())

          def parse(reader: JsonReader^): Map[key, element] =
            var entries = Map.empty[key, element]
            reader.openObject()
            var name: String | Null = reader.keyName()

            while name != null do
              entries = entries.updated(name.nn.tt.as[key], field.parse(reader))
              name = reader.keyName()

            entries

    // Support points for staged parsers, which are generated into user
    // modules and so may only reference public members.

    // The wire keys of a product's fields, `@name` renames applied.
    def wireKeys(names: IArray[String], renames: Map[Text, Text]): IArray[String] =
      names.map { name => renames.at(name.tt).or(name.tt).s }

    // A required field whose key was absent from the object.
    def missing[value]()(using Tactic[JsonError]): value = abort(JsonError(Reason.Absent))

    // Focus bookkeeping for one field read, compiled away when the ambient
    // `Foci` is the inert default — the same short-circuit as the derived
    // parser's loop.
    inline def focusing[result](foci: Foci[Json.Focus], key: Text)(inline block: => result)
    :   result =
      if foci.active then focus(using foci)(descend(prior, key))(block) else block

    // The prior focus's pointer, extended by one step. Called inside
    // `focus`'s transform context, so it only runs at error-registration
    // time — the success path pays nothing.
    private def descend(base0: Optional[Json.Focus], key: Text): Json.Focus =
      val base = base0.let(_.pointer).or(JsonPointer())

      val pointer =
        JsonPointer
          ( base.url,
            Path[JsonPointer, JsonPointer.type, Tuple]
              ( base.path.root, base.path.descent :+ key ) )

      Json.Focus(pointer)

    // Wire kinds of the builtin primitive instances, so the derived product
    // parser can read them with a direct call on the reader instead of three
    // megamorphic hops (adapter, instance, lambda). Each case mirrors the
    // corresponding singleton's lambda exactly.
    private[jacinta] inline final val KindOther = 0
    private[jacinta] inline final val KindInt = 1
    private[jacinta] inline final val KindLong = 2
    private[jacinta] inline final val KindDouble = 3
    private[jacinta] inline final val KindFloat = 4
    private[jacinta] inline final val KindText = 5
    private[jacinta] inline final val KindString = 6
    private[jacinta] inline final val KindBoolean = 7

    private[jacinta] def kindOf(parsing: Json.Parsing): Byte =
      val actual = parsing match
        case adapter: Json.Field.Adapter[?] => adapter.source
        case other                          => other

      if actual eq Json.intParsable then KindInt
      else if actual eq Json.longParsable then KindLong
      else if actual eq Json.doubleParsable then KindDouble
      else if actual eq Json.floatParsable then KindFloat
      else if actual eq Json.textParsable then KindText
      else if actual eq Json.stringParsable then KindString
      else if actual eq Json.booleanParsable then KindBoolean
      else KindOther

    // Sentinel for the derived product parser's value buffer: a slot still
    // `AbsentSlot` after the key loop had no key on the wire (`null`-checking
    // would be unsound — `Optional` fields legitimately store a null-backed
    // `Unset`, and a bitmask would cap the field count).
    private[jacinta] val AbsentSlot: AnyRef = new Object

    // Positional construction through the threaded `Mirror`, from the value
    // buffer the parse loop filled. `fromProduct` is the only construction
    // form that works for method-local and object-nested case classes; see
    // the note on `ProductDerivation.build`.
    def assemble[derivation <: Product]
      ( reflection: ProductReflection[derivation], values: IArray[Any] )
    :   derivation =

      reflection.fromProduct(ArrayProduct(values))

    private final class ArrayProduct(values: IArray[Any]) extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int = values.length
      def productElement(index: Int): Any = values(index)

    // The derived product parser's engine. `fields0` is an explicit thunk
    // (nameable in the capture set, unlike a by-name) evaluated lazily, so
    // recursive derivation can defer sibling resolution; it yields, per
    // field, the wire key, the field's parser and its declared default (or
    // `Unset`). The parse loop lives here, in an ordinary method body —
    // no Wisteria per-field lambda ever closes over the reader, which is
    // what lets the whole construction respect the checker.
    def product[derivation]
      ( fields0: () => IArray[(String, Json.Parsing, Any)],
        make:    IArray[Any] -> derivation )
      ( using foci: Foci[Json.Focus], tactic: Tactic[JsonError] )
    :   ((derivation is Json.Field)^{fields0, tactic}) =

      new Json.Field:
        type Self = derivation

        private lazy val fields: IArray[(String, Json.Parsing, Any)] = fields0()
        private lazy val keys: IArray[String] = fields.map(_(0))
        private lazy val table: Json.KeyTable = Json.KeyTable(keys)
        private lazy val kinds: IArray[Byte] = fields.map { field => kindOf(field(1)) }

        def shape(): Morphology =
          val entries: List[(Text, Morphology)] =
            fields.map { (key, parser, _) => (key.tt, parser.shape()) }.to(List)

          Morphology.Obj
            ( entries, entries.collect { case (key, shape) if !shape.optional => key } )

        // Reference equality first: object keys of up to 16 bytes come out
        // of the tokenizer's per-thread intern cache, so a steady-state
        // lookup is a scan of pointer compares. The `equals` pass covers
        // cache evictions, longer keys and escaped keys.
        private def indexOf(key: String): Int =
          val named = keys
          val count = named.length
          var index = 0

          while index < count do
            if named(index) eq key then return index
            index += 1

          index = 0

          while index < count do
            if named(index) == key then return index
            index += 1

          -1

        def parse(reader: JsonReader^): derivation =
          val entries = fields
          val count = entries.length
          val values = new Array[Any](count)
          var index = 0

          while index < count do
            values(index) = AbsentSlot
            index += 1

          // With the inert default `Foci`, per-field `focus` wrapping (a
          // try/finally and two closures per field) would observably do
          // nothing, so the hot loop skips it.
          val focused = foci.active

          reader.openObject()
          var found = reader.keyIndex(table)

          while found != Json.KeyTable.End do
            if found < 0 then reader.skipValue()
            else values(found) =
              if focused
              then focus(descend(prior, keys(found).tt))(entries(found)(1).parse(reader))
              else
                (kinds(found): @scala.annotation.switch) match
                  case KindInt     => reader.long().toInt
                  case KindLong    => reader.long()
                  case KindDouble  => reader.double()
                  case KindFloat   => reader.double().toFloat
                  case KindText    => reader.string()
                  case KindString  => reader.string().s
                  case KindBoolean => reader.boolean()
                  case _           => entries(found)(1).parse(reader)

            found = reader.keyIndex(table)

          index = 0

          while index < count do
            if values(index).asInstanceOf[AnyRef] eq AbsentSlot then
              val fallback = entries(index)(2).asInstanceOf[Optional[Any]]

              values(index) =
                if fallback.present then fallback
                else if focused
                then focus(descend(prior, keys(index).tt))(entries(index)(1).absent())
                else entries(index)(1).absent()

            index += 1

          make(values.immutable(using Unsafe))

  // The direct-parsing counterpart of `Json.Decodable`: consumes JSON tokens
  // from a `JsonReader` instead of walking a materialized `Json`, so
  // `read[value in Json]` can instantiate values without building the AST.
  // Carries the same `Morphology` so a direct parser stays coherent with the
  // schema machinery. `Parsable` is the opt-in surface: explicit instances,
  // `Json.Parsable.derived`, and the read trigger. It has no blanket
  // fallback given, so no read changes behavior until a type opts in; the
  // fallback belongs to its operational sibling, `Json.Field`.
  trait Parsable extends Parsing

  // The shared substance of `Json.Parsable` and `Json.Field`. The two
  // subtraits add nothing: they exist so that neither is a subtype of the
  // other. A subtype relation in either direction would make one family's
  // givens candidates for the other's queries — nominal instances would
  // clash ambiguously with the field fallback, and Wisteria's codec probe
  // would find a generic type's own `Parsable` given while deriving it, a
  // lazy-val self-reference.
  trait Parsing extends distillate.Parsable:
    type Transport = Json
    type Reader = JsonReader
    def shape(): Morphology

    // What a field of this type yields when its key is absent from the
    // object: an error unless overridden (`Unset` for `Optional`s, `None`
    // for `Option`s; the bridge delegates to its decoder). A wire `null` is
    // never routed here: the parse method consumes it itself.
    def absent()(using Tactic[JsonError]): Self = abort(JsonError(Reason.Absent))

  object KeyTable:
    inline final val Unknown = -1
    inline final val End = -2

    // Fibonacci hashing multiplier (2^64 / phi), spreading low-byte entropy
    // across the sign-discarded upper bits of the product.
    private[jacinta] inline final val Scramble = -7046029254386353131L

    def apply(keys: IArray[String]): KeyTable =
      val count = keys.length
      val lows = new Array[Long](count)
      val highs = new Array[Long](count)
      val packable = new Array[Boolean](count)
      var index = 0

      while index < count do
        val key = keys(index)
        val length = key.length
        var fits = length > 0 && length <= 16
        var position = 0

        while fits && position < length do
          val char = key.charAt(position)
          if char < ' ' || char >= 127 then fits = false
          position += 1

        if fits then
          var low = 0L
          var high = 0L
          position = 0

          while position < length do
            val byte = key.charAt(position).toLong & 0xFF
            if position < 8 then low |= byte << (position*8)
            else high |= byte << ((position - 8)*8)
            position += 1

          lows(index) = low
          highs(index) = high
          packable(index) = true

        index += 1

      // Direct-mapped hash over the packed forms: capacity is the smallest
      // power of two holding all keys at load factor <= 1/2, retried at
      // double the size when two keys collide. Lookups are then one
      // multiply-shift and one slot probe. `slots(slot)` holds the key's
      // index + 1 (0 = empty). Degenerates to `capacity = 0` (linear scan)
      // only if collisions persist at 4x — practically never for the small,
      // distinct key sets of a case class.
      var capacity = Integer.highestOneBit(count.max(1))*4
      var slots: Array[Int] | Null = null

      while slots == null && capacity <= count*16 do
        val attempt = new Array[Int](capacity)
        var clash = false
        index = 0

        while index < count && !clash do
          if packable(index) then
            val slot = (((lows(index)*KeyTable.Scramble) ^ highs(index)).toInt
              & (capacity - 1)).abs

            if attempt(slot) == 0 then attempt(slot) = index + 1 else clash = true

          index += 1

        if clash then capacity *= 2 else slots = attempt

      if slots == null then capacity = 0

      new KeyTable
        ( keys,
          lows.immutable(using Unsafe),
          highs.immutable(using Unsafe),
          packable.immutable(using Unsafe),
          (if slots == null then new Array[Int](0) else slots.nn).immutable(using Unsafe),
          capacity )

  // Precomputed packed-byte forms of a fixed key set (a derived product's
  // wire keys): matching a wire key against the table needs no `String`
  // materialization, no intern-cache probe and no equality scan — a
  // multiply-shift hash into a direct-mapped slot table, then two `Long`
  // compares, against bytes packed straight from the input window. Only
  // non-empty ASCII keys of up to 16 bytes pack (the same packing as the
  // tokenizer's intern cache); any other key, or any wire key the fast scan
  // cannot handle, falls back to string comparison.
  final class KeyTable private
    ( keys:     IArray[String],
      lows:     IArray[Long],
      highs:    IArray[Long],
      packable: IArray[Boolean],
      slots:    IArray[Int],
      capacity: Int ):

    private val count = keys.length

    def indexOf(low: Long, high: Long): Int =
      if capacity != 0 then
        val slot = (((low*KeyTable.Scramble) ^ high).toInt & (capacity - 1)).abs
        val found = slots(slot) - 1

        if found >= 0 && lows(found) == low && highs(found) == high then found
        else KeyTable.Unknown
      else
        var index = 0

        while index < count do
          if packable(index) && lows(index) == low && highs(index) == high then return index
          index += 1

        KeyTable.Unknown

    def indexOfName(name: String): Int =
      var index = 0

      while index < count do
        if keys(index) eq name then return index
        index += 1

      index = 0

      while index < count do
        if keys(index) == name then return index
        index += 1

      KeyTable.Unknown

  object Field:
    // Adapts an opted-in nominal instance (or any other `Parsing`) for use
    // as a field parser. A named class (with the wrapped instance held as a
    // neutral carrier and reasserted at the rim, preserving the declared
    // capture) so the derived product parser can recognize builtin
    // primitives through it and devirtualize them.
    private[jacinta] final class Adapter[value](source0: AnyRef) extends Json.Field:
      type Self = value

      private[jacinta] def source: value is Json.Parsing =
        source0.asInstanceOf[value is Json.Parsing]

      def parse(reader: JsonReader^): value = source.parse(reader)
      def shape(): Morphology = source.shape()
      override def absent()(using Tactic[JsonError]): value = source.absent()

    def apply[value](parsing: (value is Json.Parsing)^)
    :   ((value is Json.Field)^{parsing}) =
      Adapter[value](parsing.asInstanceOf[AnyRef])
      . asInstanceOf[(value is Json.Field)^{parsing}]

  // The operational face of direct parsing: how a value is read from a
  // `JsonReader`, whether directly or through the AST bridge. This is the
  // typeclass the product derivation resolves per field: `Json2` carries its
  // element-wise instances and `Json3` the universal fallback — declared as
  // members of this companion (like `Json2.decodable`) so Wisteria's wrapper
  // detection excludes the fallback during codec probing.
  trait Field extends Parsing

  // All internal references in a `PositionIndex` are stored as offsets
  // relative to the start of the containing node descriptor, so any slice
  // extracted at a descriptor boundary is itself a valid `PositionIndex`.
  opaque type PositionIndex = IArray[Int]

  object PositionIndex:
    private[jacinta] def apply(data: IArray[Int]): PositionIndex = data

  extension (positionIndex: PositionIndex)
    private[jacinta] def ints: IArray[Int] = positionIndex

  // Focus value tracked by Jacinta's decoders / encoders. `pointer` is the
  // JSON-pointer path to the current node. `position` is initially `Unset`
  // and filled in by `withPosition(json)` against the root `Json` that
  // produced the focus — typically at error-render time so the success
  // path pays nothing for `locate` lookups. Constructed lazily inside
  // `Foci.supplement`, only for actually-registered errors.
  case class Focus
    ( pointer:  JsonPointer,
      position: Optional[Json.Ast.Position] = Unset )
  derives CanEqual:

    def withPosition(json: Json): Focus = copy(position = json.locate(pointer))

  opaque type Ast =
    JsonString | JsonNumber | JsonBoolean | JsonNull | JsonObject | JsonArray | Unset

  object Ast extends Format:
    def name: Text = "JSON"

    case class Position
      ( line:                Int,
        column:              Int,
        override val offset: Optional[Int] = Unset,
        override val length: Optional[Int] = Unset )
    extends Format.Position:
      def describe: Text = ("line "+line+", column "+column).tt

      // `line`/`column` are 1-based here; the public span is 0-based.
      override def span: Span =
        Span.line((line - 1).max(0).z, (column - 1).max(0).z, length.or(0))

    enum Issue extends Format.Issue:
      case EmptyInput
      case UnexpectedChar(found: Char)
      case ExpectedTrue
      case ExpectedFalse
      case ExpectedNull
      case ExpectedSomeValue(char: Char)
      case ExpectedColon(found: Char)
      case InvalidWhitespace
      case ExpectedString(found: Char)
      case ExpectedHexDigit(found: Char)
      case PrematureEnd
      case NumberHasLeadingZero
      case SpuriousContent(found: Char)
      case LeadingDecimalPoint
      case NotEscaped(char: Char)
      case IncorrectEscape(char: Char)
      case MultipleDecimalPoints
      case ExpectedDigit(found: Char)

      // Raised only on the direct-parsing path (`Json.Parsable`), where a
      // typed reader knows which kind of value it expects next.
      case ExpectedNumber(found: Char)
      case ExpectedBoolean(found: Char)
      case ExpectedObject(found: Char)
      case ExpectedArray(found: Char)

      def describe: Message = this match
        case EmptyInput              => m"the input was empty"
        case UnexpectedChar(found)   => m"the character $found was not expected here"
        case ExpectedTrue            => m"true was expected"
        case ExpectedFalse           => m"false was expected"
        case ExpectedNull            => m"null was expected"
        case ExpectedSomeValue(char) => m"a value was expected but $char was found instead"
        case ExpectedColon(found)    => m"a colon was expected but $found was found instead"
        case InvalidWhitespace       => m"invalid whitespace was found"
        case ExpectedString(found)   => m"a string was expected but $found was found instead"
        case ExpectedHexDigit(found) => m"a hexadecimal digit was expected"
        case PrematureEnd            => m"the content ended prematurely"
        case SpuriousContent(found)  => m"$found was found after the full JSON value was read"
        case LeadingDecimalPoint     => m"a number cannot start with a decimal point"
        case NotEscaped(char)        => m"the character $char must be escaped with a backslash"
        case ExpectedDigit(found)    => m"a digit was expected but $found was found instead"
        case MultipleDecimalPoints   => m"a number cannot contain more than one decimal point"
        case ExpectedNumber(found)   => m"a number was expected but $found was found instead"
        case ExpectedBoolean(found)  => m"a boolean was expected but $found was found instead"
        case ExpectedObject(found)   => m"an object was expected but $found was found instead"
        case ExpectedArray(found)    => m"an array was expected but $found was found instead"

        case NumberHasLeadingZero =>
          m"a number cannot start with a zero except when followed by a decimal point"

        case IncorrectEscape(char) =>
          m"the character $char was escaped with a backslash unnecessarily"

    object AsciiByte:
      inline final val Tab:          9   = 9   // '\t'
      inline final val Newline:      10  = 10  // '\n'
      inline final val Return:       13  = 13  // '\r'
      inline final val Space:        32  = 32  // ' '
      inline final val Comma:        44  = 44  // ','
      inline final val Quote:        34  = 34  // '"'
      inline final val Minus:        45  = 45  // '-'
      inline final val Plus:         43  = 43  // '+'
      inline final val Slash:        47  = 47  // '/'
      inline final val Period:       46  = 46  // '.'
      inline final val Num0:         48  = 48  //'0'
      inline final val Num1:         49  = 49  //'1'
      inline final val Num2:         50  = 50  //'2'
      inline final val Num3:         51  = 51  //'3'
      inline final val Num4:         52  = 52  //'4'
      inline final val Num5:         53  = 53  //'5'
      inline final val Num6:         54  = 54  //'6'
      inline final val Num7:         55  = 55  //'7'
      inline final val Num8:         56  = 56  //'8'
      inline final val Num9:         57  = 57  //'9'
      inline final val Colon:        58  = 58  // ':'
      inline final val UpperA:       65  = 65  // 'A'
      inline final val UpperB:       66  = 66  // 'B'
      inline final val UpperC:       67  = 67  // 'C'
      inline final val UpperD:       68  = 68  // 'D'
      inline final val UpperE:       69  = 69  // 'E'
      inline final val UpperF:       70  = 70  // 'F'
      inline final val OpenBracket:  91  = 91  // '['
      inline final val CloseBracket: 93  = 93  // ']'
      inline final val Backslash:    92  = 92  // '\\'
      inline final val LowerA:       97  = 97  // 'a'
      inline final val LowerB:       98  = 98  // 'b'
      inline final val LowerC:       99  = 99  // 'c'
      inline final val LowerD:       100 = 100 // 'd'
      inline final val LowerE:       101 = 101 // 'e'
      inline final val LowerF:       102 = 102 // 'f'
      inline final val LowerL:       108 = 108 // 'l'
      inline final val LowerN:       110 = 110 // 'n'
      inline final val LowerR:       114 = 114 // 'r'
      inline final val LowerS:       115 = 115 // 's'
      inline final val LowerT:       116 = 116 // 't'
      inline final val LowerU:       117 = 117 // 'u'
      inline final val OpenBrace:    123 = 123 // '{'
      inline final val CloseBrace:   125 = 125 // '}'

    // Sentinel used to pad a heterogeneous array whose original length is
    // even, so that all such arrays have odd `IArray[Any]` length and can be
    // distinguished from objects (encoded as alternating `key, value, …` and
    // therefore always even length). The sentinel only ever appears as the
    // last element of a padded array and is never part of the user-visible
    // contents. (Pure number arrays use `Array[Double]` and need no
    // padding.)
    val arrayPad: AnyRef = new Object

    def apply
      ( value
      : JsonString | JsonNumber | JsonBoolean | JsonNull | JsonObject | JsonArray | Unset )
    :   Ast =

      // Asserted: `JsonArray`'s `Array` members decorate the union read-only under
      // separation checking, but AST arrays are internal, never-mutated data.
      value.asInstanceOf[Ast]

    // Build an object node from parallel `keys` and `values` arrays. The result
    // is stored as a single `IArray[Any]` of length `2 * keys.length`, with keys
    // at even indices and values at odd indices.
    def obj(keys: IArray[String], values: IArray[Any]): Ast =
      val n = keys.length
      val arr = new Array[Any](n*2)
      var i = 0

      while i < n do
        arr(i*2) = keys(i)
        arr(i*2 + 1) = values(i)
        i += 1

      arr.asInstanceOf[IArray[Any]]

    // Build a heterogeneous array node. If the element count is even, a
    // single sentinel `arrayPad` is appended so the stored `IArray[Any]` has
    // odd length, distinguishing it from objects (always even length).
    def arr(elements: IArray[Any]): Ast =
      val n = elements.length

      if (n & 1) == 1 then elements
      else
        val padded = new Array[Any](n + 1)
        System.arraycopy(elements.asInstanceOf[Array[Any]], 0, padded, 0, n)
        padded(n) = arrayPad
        padded.asInstanceOf[IArray[Any]]

    // Build a number-only array node using the single-Long BCD encoding
    // (see `Bcd.packBcdLong`). Each `Long` element carries one number's
    // sign + count + nibbles inline — no per-element `Double` materialisation
    // and no per-element heap allocation.
    def bcdArr(values: Array[Long]): Ast = values.immutable(using Unsafe)

    // Build a number-only array node using the single-Int small-BCD
    // encoding (see `Bcd.packBcdInt`). For arrays where every number
    // fits in 7 nibbles — the half-memory variant of `bcdArr`.
    def smallBcdArr(values: Array[Int]): Ast = values.immutable(using Unsafe)

    // Accessors over the opaque AST representation. They live in the `Ast`
    // companion so they are in implicit scope wherever a `Json.Ast` is used,
    // without being top-level (and thus without entering the `soundness`
    // umbrella, where their generic names would clash).
    extension (json: Ast)
      inline def isNumber: Boolean = isDouble || isLong || isBcd || isSmallBcd
      inline def isAbsent: Boolean = json == Unset
      inline def isLong: Boolean = json.isInstanceOf[Long]
      inline def isDouble: Boolean = json.isInstanceOf[Double]

      // Small-BCD number node — a single number with up to 7 nibbles packed
      // into one `Int` (see `Bcd.packBcdInt`). The boxed runtime class is
      // `java.lang.Integer`, distinct from `java.lang.Long` (`isLong`), so
      // the type test reliably separates the two.
      inline def isSmallBcd: Boolean = json.isInstanceOf[Int]

      // High-precision number node. `Bcd` is `Array[Double]` (`[D`) at
      // runtime, with each Double's raw bits packing 13 nibbles in its
      // mantissa — distinct from `Array[Int]` (`[I`, arrays of small BCDs),
      // `Array[Long]` (`[J`, arrays of larger BCDs), and `Array[AnyRef]`
      // (`[Ljava/lang/Object;`).
      inline def isBcd: Boolean = json.isInstanceOf[Array[Double]]
      inline def isString: Boolean = json.isInstanceOf[String]
      inline def isBoolean: Boolean = json.isInstanceOf[Boolean]
      inline def isNull: Boolean = json.asInstanceOf[AnyRef] eq Json.JsonNull

      // Objects and heterogeneous arrays share a runtime representation
      // (`IArray[Any]` = `[Ljava/lang/Object;`) and are distinguished by
      // length parity: even = object (alternating `key, value, …`), odd =
      // array (with sentinel padding when the logical element count is even).
      // Number-only arrays are stored unboxed as `Array[Double]` (`[D`),
      // a distinct runtime class.
      inline def isObject: Boolean =
        json.isInstanceOf[Array[AnyRef]] &&
          (json.asInstanceOf[Array[?]].length & 1) == 0

      inline def isArray: Boolean =
        json.isInstanceOf[Array[Long]] ||
          json.isInstanceOf[Array[Int]] ||
          (json.isInstanceOf[Array[AnyRef]] &&
            (json.asInstanceOf[Array[?]].length & 1) == 1)

      // True when the array is in either unboxed number-only form (BCD-packed).
      inline def isNumberArray: Boolean =
        json.isInstanceOf[Array[Long]] || json.isInstanceOf[Array[Int]]

      // True when the array is in the small-BCD `Array[Int]` form.
      inline def isBcdIntArray:  Boolean = json.isInstanceOf[Array[Int]]

      // True when the array is in the larger-BCD `Array[Long]` form.
      inline def isBcdLongArray: Boolean = json.isInstanceOf[Array[Long]]

      private def expected(jsonPrimitive: JsonPrimitive): Unit raises JsonError =
        val reason = if isAbsent then Reason.Absent else Reason.NotType(primitive, jsonPrimitive)
        raise(JsonError(reason))

      // The number of user-visible elements in an array node (excludes the
      // sentinel pad of a parity-padded heterogeneous array, if present).
      def arrayLength: Int = (json: @unchecked) match
        case bcds: IArray[Long] @unchecked   => bcds.length
        case smalls: IArray[Int] @unchecked  => smalls.length

        case _ =>
          val arr = json.asInstanceOf[Array[?]]
          val n = arr.length
          if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq arrayPad) then n - 1 else n

      // Materialise an array element as a `Json.Ast` node. Three cases:
      //   - `Array[Long]`: a single-Long BCD-packed number — decoded back to
      //     `Long` if it represents an exact integer that fits, else to
      //     `Double` via the canonical text form.
      //   - `Array[Int]`: a single-Int small BCD — the raw `Int` *is* a
      //     small-BCD `JsonNumber`, so it surfaces directly via `Json.Ast(_)`.
      //   - boxed `IArray[Any]`: direct indexed lookup.
      def arrayElement(index: Int): Json.Ast = (json: @unchecked) match
        case bcds: IArray[Long] @unchecked =>
          val v = bcds(index)
          val text = Bcd.bcdLongText(v)

          try Json.Ast(java.lang.Long.parseLong(text))
          catch case _: NumberFormatException => Json.Ast(java.lang.Double.parseDouble(text))

        case smalls: IArray[Int] @unchecked =>
          Json.Ast(smalls(index))

        case _ =>
          json.asInstanceOf[IArray[Json.Ast]](index)

      // The number of key/value pairs in an object node.
      inline def objectSize: Int = json.asInstanceOf[IArray[Any]].length/2

      inline def objectKey(index: Int): String =
        json.asInstanceOf[IArray[Any]](index*2).asInstanceOf[String]

      inline def objectValue(index: Int): Json.Ast =
        json.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[Json.Ast]

      // Linear scan for a key. Returns the value index (in pair units) or -1.
      def objectIndexOf(key: String): Int =
        val arr = json.asInstanceOf[IArray[Any]]
        val len = arr.length
        var i = 0

        while i < len do
          if arr(i) == key then return i/2
          i += 2
        -1

      def array(using Tactic[JsonError]): IArray[Json.Ast] = (json: @unchecked) match
        // Freshly built, frozen on return; the seal strips the tactic-capture decoration
        // that `arrayElement`'s raising result leaves on the elements.
        case bcds: IArray[Long] @unchecked =>
          val out = new Array[Json.Ast](bcds.length)
          var i = 0
          while i < out.length do { out(i) = json.arrayElement(i); i += 1 }
          out.immutable(using Unsafe).asInstanceOf[IArray[Json.Ast]]

        case smalls: IArray[Int] @unchecked =>
          val out = new Array[Json.Ast](smalls.length)
          var i = 0
          while i < out.length do { out(i) = json.arrayElement(i); i += 1 }
          out.immutable(using Unsafe).asInstanceOf[IArray[Json.Ast]]

        case _ =>
          if isArray then
            val full = json.asInstanceOf[IArray[Json.Ast]]
            val n = json.arrayLength

            if n == full.length then full
            else IArray.tabulate(n)(full(_))
          else
            // hoisted: a fresh array built inside `yet`'s by-name operand (which
            // captures the ambient Tactic) could not escape it
            val empty = IArray[Json.Ast]().asInstanceOf[IArray[Json.Ast]]
            expected(JsonPrimitive.Array) yet empty

      def double: Double raises JsonError = json.asMatchable match
        case value: Double                   => value
        case value: Long                     => value.toDouble
        case value: Int                      => Bcd.bcdIntToDouble(value)
        case value: Array[Double] @unchecked => value.asInstanceOf[Bcd].toDouble
        case _                               => expected(JsonPrimitive.Number) yet 0.0

      def bcd: Bcd raises JsonError = json.asMatchable match
        case value: Array[Double] @unchecked => value.asInstanceOf[Bcd]
        case value: Long                     => caps.unsafe.unsafeAssumePure(Bcd(BigDecimal(value)))
        case value: Double                   => caps.unsafe.unsafeAssumePure(Bcd(BigDecimal(value)))

        case value: Int =>
          caps.unsafe.unsafeAssumePure:
            Bcd.fromString(Bcd.bcdIntText(value).stripPrefix("-"), value < 0)

        case _ =>
          expected(JsonPrimitive.Number) yet caps.unsafe.unsafeAssumePure(Bcd(BigDecimal(0L)))

      def long: Long raises JsonError = json.asMatchable match
        case value: Long                     => value
        case value: Double                   => value.toLong
        case value: Int                      => Bcd.bcdIntToDouble(value).toLong
        case value: Array[Double] @unchecked => value.asInstanceOf[Bcd].toLong.or(0L)
        case _                               => expected(JsonPrimitive.Number) yet 0L

      def primitive: JsonPrimitive =
        if isNumber then JsonPrimitive.Number
        else if isBoolean then JsonPrimitive.Boolean
        else if isString then JsonPrimitive.String
        else if isObject then JsonPrimitive.Object
        else if isArray then JsonPrimitive.Array
        else JsonPrimitive.Null

      def string: Text raises JsonError =
        if isString then json.asInstanceOf[Text]
        else expected(JsonPrimitive.String) yet "".tt

      def boolean: Boolean raises JsonError =
        if isBoolean then json.asInstanceOf[Boolean]
        else expected(JsonPrimitive.Boolean) yet false

      // Returns a (keys, values) view over an object node. This *materialises*
      // two new IArrays from the flat alternating layout, so prefer
      // `objectKey`/`objectValue` when you only need a few entries.
      def obj(using Tactic[JsonError]): (IArray[String], IArray[Json.Ast]) =
        if !isObject
        then
          val empty = (IArray[String](), IArray[Json.Ast]()).asInstanceOf[(IArray[String], IArray[Json.Ast])]
          expected(JsonPrimitive.Object) yet empty
        else
          val arr = json.asInstanceOf[IArray[Any]]
          val n = arr.length/2
          val keys = new Array[String](n)
          val values = new Array[Json.Ast](n)
          var i = 0

          while i < n do
            keys(i) = arr(i*2).asInstanceOf[String]
            values(i) = arr(i*2 + 1).asInstanceOf[Json.Ast]
            i += 1

          (keys.asInstanceOf[IArray[String]], values.asInstanceOf[IArray[Json.Ast]])

      def number: Long | Double | Bcd raises JsonError =
        if isLong then long
        else if isDouble then double
        else if isBcd then bcd
        else if isSmallBcd then long
        else expected(JsonPrimitive.Number) yet 0L

    // Low-level parsers building an `Ast` directly from input. Public reading
    // goes through `source.read[Json]` (the `Aggregable` instances); these are
    // the underlying engine and stay scoped to the `Ast` companion.
    def parse(source: Data)(using mode: NumberMode): Json.Ast raises ParseError =
      Json.Ast(Parser.parse(source, mode))

    def parse(source: Data, holes: Boolean)(using mode: NumberMode): Json.Ast raises ParseError =
      Json.Ast(Parser.parse(source, holes, mode))

    def parse(input: Iterator[Data])(using mode: NumberMode): Json.Ast raises ParseError =
      Json.Ast(Parser.parse(input, mode))

    def parse(input: Iterator[Data], holes: Boolean)(using mode: NumberMode)
    :   Json.Ast raises ParseError =

      Json.Ast(Parser.parse(input, holes, mode))

    def parseTracked(source: Data)(using mode: NumberMode)
    :   (Json.Ast, Json.PositionIndex) raises ParseError =

      val (raw, ints) = Parser.parseTracked(source, mode)
      (Json.Ast(raw), Json.PositionIndex(ints))

    def parseTracked(input: Iterator[Data])(using mode: NumberMode)
    :   (Json.Ast, Json.PositionIndex) raises ParseError =

      val (raw, ints) = Parser.parseTracked(input, mode)
      (Json.Ast(raw), Json.PositionIndex(ints))

    private[jacinta] def parse(consume input: (Stream[Data] over Credit)^)
      ( using mode: NumberMode, tactic: Tactic[ParseError] )
    :   Json.Ast =

      Json.Ast(Parser.parse(input, mode))

    private[jacinta] def parseTracked(consume input: (Stream[Data] over Credit)^)
      ( using mode: NumberMode, tactic: Tactic[ParseError] )
    :   (Json.Ast, Json.PositionIndex) =

      val (raw, ints) = Parser.parseTracked(input, mode)
      (Json.Ast(raw), Json.PositionIndex(ints))

  def ast(value: Json.Ast): Json = new Json(value)

  // `object Json` extends `Dynamic`, which suppresses the universal-apply
  // synthesis for `Json(...)`; these forward to the constructor manually.
  def apply(value: Any): Json = new Json(value)
  def apply(value: Any, positions: Optional[Json.PositionIndex]): Json = new Json(value, positions)

  // Defined on the companion directly (not as a `Json.type` extension) because
  // the companion's `Dynamic` parentage intercepts `Json.parseTracked(...)`
  // before extension-method resolution gets a chance.
  def parseTracked(source: Data)(using NumberMode): Json raises ParseError =
    val (ast, index) = Json.Ast.parseTracked(source)
    new Json(ast, index)

  def parseTracked(input: Iterator[Data])(using NumberMode): Json raises ParseError =
    val (ast, index) = Json.Ast.parseTracked(input)
    new Json(ast, index)

  def parseTracked(source: Text)(using NumberMode, CharEncoder): Json raises ParseError =
    parseTracked(source.in[Data])

  // Parse a byte-chunk iterator into a `Json`, honouring the in-scope
  // `PositionTracking` toggle (`parsing.trackPositions`): when on, source
  // positions are recorded and retained on the `Json` (locatable via
  // `Positionable`); when off, the throughput path is unchanged. This is the
  // single place the `read`/`load` givens branch on tracking.
  private[jacinta] def readJson(input: Iterator[Data])(using Tactic[ParseError], PositionTracking)
  :   Json =
    summon[PositionTracking] match
      case PositionTracking.On =>
        val (ast, index) = Json.Ast.parseTracked(input)
        new Json(ast, index)

      case PositionTracking.Off =>
        Json(Json.Ast.parse(input))

  // As above, but pulling from a demand-aware stream rather than a chunk
  // iterator.
  // The parameter is not `consume`: `Aggregable.accept`'s signature is pinned
  // non-consuming by overrides in modules outside separation checking, so the
  // stream crosses to the consuming parser as a neutral reference — each accept
  // call delivers a stream that is used exactly once, by construction.
  private[jacinta] def readJson(input: (Stream[Data] over Credit)^)
    ( using Tactic[ParseError], PositionTracking )
  :   Json =

    val ref: AnyRef = input.asInstanceOf[AnyRef]

    summon[PositionTracking] match
      case PositionTracking.On =>
        val (ast, index) =
          Json.Ast.parseTracked(ref.asInstanceOf[(Stream[Data] over Credit)^])
        new Json(ast, index)

      case PositionTracking.Off =>
        Json(Json.Ast.parse(ref.asInstanceOf[(Stream[Data] over Credit)^]))

  // Direct-parsing counterpart of `readJson`: drives a `Json.Parsable`
  // instance over the input through a `JsonReader`, so no AST is built for
  // the values the instance reads directly. Under `parsing.trackPositions`
  // the parser's lineation is enabled so parse errors carry real source
  // coordinates; there is no `PositionIndex` (nothing to index — the result
  // is the caller's value, not a `Json`).
  private def parseDirect[value]
    ( input: Iterator[Data], parsable: (value is Json.Parsable)^ )
    ( using tactic: Tactic[ParseError], tracking: PositionTracking, mode: NumberMode )
  :   value =

    val parser = Parser.borrow()
    parser.tracking = tracking == PositionTracking.On
    parser.resetIterator(input)
    parser.holes = false
    parser.numberMode = mode
    parser.directBegin()
    val result = parsable.parse(JsonReader(parser, tactic))
    parser.directEnd()
    result

  // As above, but parsing a single in-memory block: no stream, no filler,
  // no credit — the cursor reads the block in place.
  private def parseDirect[value]
    ( input: Data, parsable: (value is Json.Parsable)^ )
    ( using tactic: Tactic[ParseError], tracking: PositionTracking, mode: NumberMode )
  :   value =

    val parser = Parser.borrow()
    parser.tracking = tracking == PositionTracking.On
    parser.resetData(input)
    parser.holes = false
    parser.numberMode = mode
    parser.directBegin()
    val result = parsable.parse(JsonReader(parser, tactic))
    parser.directEnd()
    result

  // As above, but pulling from a demand-aware stream; see `readJson`'s note
  // on the neutral-reference hop.
  private def parseDirect[value]
    ( input: (Stream[Data] over Credit)^, parsable: (value is Json.Parsable)^ )
    ( using tactic: Tactic[ParseError], tracking: PositionTracking, mode: NumberMode )
  :   value =

    val ref: AnyRef = input.asInstanceOf[AnyRef]
    val parser = Parser.borrow()
    parser.tracking = tracking == PositionTracking.On
    parser.resetStream(ref.asInstanceOf[(Stream[Data] over Credit)^])
    parser.holes = false
    parser.numberMode = mode
    parser.directBegin()
    val result = parsable.parse(JsonReader(parser, tactic))
    parser.directEnd()
    result

  // Canonical external accessor for the underlying AST. The `root`
  // method on `class Json` is package-private so that breaking through
  // the `Json` abstraction is a deliberate, named action.
  def unseal(json: Json): Json.Ast = json.root

  // Resolves a `JsonPointer` to the source `Position` recorded in a tracked
  // `Json`'s `PositionIndex`. Exposed uniformly as `json.locate(pointer)` /
  // `json.locateKey(pointer)` through zephyrine's `Positionable`.
  given positionable: Json is Positionable by JsonPointer to Json.Ast.Position =
    new Positionable:
      type Self    = Json
      type Operand = JsonPointer
      type Result  = Json.Ast.Position

      def locate(value: Json, path: JsonPointer): Optional[Json.Ast.Position] =
        value.positionIndex.let: posIndex =>
          walkIndex(value.root, posIndex.ints, 0, path.path.descent.toIndexedSeq, 0, false)

      def locateKey(value: Json, path: JsonPointer): Optional[Json.Ast.Position] =
        value.positionIndex.let: posIndex =>
          walkIndex(value.root, posIndex.ints, 0, path.path.descent.toIndexedSeq, 0, true)

  private def walkIndex
    ( ast:      Json.Ast,
      data:     IArray[Int],
      offset:   Int,
      segments: IndexedSeq[Text],
      i:        Int,
      keyMode:  Boolean )
  :   Optional[Json.Ast.Position] =

    if i >= segments.length then
      if keyMode then Unset
      else Json.Ast.Position
        ( line   = data(offset + 1),
          column = data(offset + 2),
          length = data(offset + 3) )
    else
      // `JsonPointer.path.descent` is stored leaf-first (Serpentine's `/`
      // prepends), so iterate it in reverse to walk root-to-leaf.
      val seg = segments(segments.length - 1 - i).s

      if ast.isObject then
        val k = ast.objectIndexOf(seg)

        if k < 0 then Unset else
          val entryOff = data(offset + 5 + k)
          val isLast = i == segments.length - 1

          if isLast && keyMode then
            Json.Ast.Position
              ( line   = data(offset + entryOff),
                column = data(offset + entryOff + 1),
                length = data(offset + entryOff + 2) )
          else
            walkIndex
              ( ast.objectValue(k), data, offset + entryOff + 3, segments, i + 1, keyMode )
      else if ast.isArray then
        try
          val k = Integer.parseInt(seg)

          if k < 0 || k >= ast.arrayLength then Unset
          else
            val childOff = data(offset + 5 + k)

            walkIndex
              ( ast.arrayElement(k), data, offset + childOff, segments, i + 1, keyMode )
        catch case _: NumberFormatException => Unset
      else
        Unset

  inline given interpolator: Json is Interpolable:
    type Result = Json

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Json =

      ${jacinta.internal.interpolator[parts, origins]('insertions)}


  inline given extrapolator: Json is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Json)
    :   Boolean | Option[Tuple | Json] =

      ${jacinta.internal.extractor[parts, origins]('scrutinee)}


  given lens: [name <: Label: ValueOf] => (erased dynamicJsonEnabler: DynamicJsonEnabler) => (tactic: Tactic[JsonError])
  =>  ((name is Lens from Json onto Json)^{tactic}) =

    Lens(_.selectField(valueOf[name]), (json, value) => json.modify(valueOf[name], value))


  given ordinalOptical: [element] => Ordinal is Optical from Json onto Json =
    ordinal =>
      Optic: (origin, lambda) =>
        if origin.root.isArray then
          val n = origin.root.arrayLength

          if n <= ordinal.n0 then origin else Json.ast:
            val updated = new Array[Any](n)
            var i = 0

            while i < n do
              updated(i) =
                if i == ordinal.n0
                then lambda(Json.ast(origin.root.arrayElement(i))).root
                else origin.root.arrayElement(i)

              i += 1

            Json.Ast.arr(updated.asInstanceOf[IArray[Any]])
        else
          origin

  // `Each` applies the transform to every array element; `Filter` to those matching
  // its predicate. Both rebuild the array immutably and no-op on non-arrays.
  given eachOptical: Each.type is Optical from Json onto Json = _ =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Json.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            updated(i) = lambda(Json.ast(origin.root.arrayElement(i))).root
            i += 1

          Json.Ast.arr(updated.asInstanceOf[IArray[Any]])
      else
        origin

  // The `predicate` laundering is for the Scala.js pipeline, which — unlike the JVM
  // pipeline — rejects the `Optic`'s capture of `filter.predicate` against the required
  // pure `Optic` type. (Compiler divergence; see #1520 and `caesura`'s `rowFilter`.)
  given filterOptical: Filter[Json] is Optical from Json onto Json = filter =>
    val predicate: Json -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Json.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            val element = Json.ast(origin.root.arrayElement(i))
            updated(i) = (if predicate(element) then lambda(element) else element).root
            i += 1

          Json.Ast.arr(updated.asInstanceOf[IArray[Any]])
      else
        origin

  // A `Json` value decodes to itself. Typed as the plain `Decodable in Json` (not
  // the `Json.Decodable` carrier) so it is *exactly* the queried type and strictly
  // beats distillate's universal `value is Decodable in value` identity given for a
  // `Json is Decodable in Json` summon (e.g. `someJson.as[Json]`); a carrier subtype
  // would be incomparable to `generic` and therefore ambiguous. `Json is
  // Json.Decodable` (needed when `Json` nests in a collection/`Optional`) comes from
  // the `Json` case in the `decodable` summonFrom above.
  given jsonDecodable: Json is distillate.Decodable in Json = identity(_)

  // The primitive codecs are laundered pure: their resolution-scoped tactic shares each
  // instance's given-resolution lifetime, and both the product derivation and the inline
  // bodies expanded inside staged quotes (superlunary) summon them against pure expected
  // types (honest capturing forms return with wisteria capture-polymorphism; see
  // rep/DECISIONS.md).
  given boolean: (tactic: Tactic[JsonError])
  =>  ((Boolean is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Bool)(_.root.boolean)

  given double: (tactic: Tactic[JsonError])
  =>  ((Double is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Real)(_.root.double)

  given float: (tactic: Tactic[JsonError])
  =>  ((Float is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Real)(_.root.double.toFloat)

  given long: (tactic: Tactic[JsonError])
  =>  ((Long is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Whole)(_.root.long)

  given int: (tactic: Tactic[JsonError])
  =>  ((Int is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Whole)(_.root.long.toInt)

  given ordinalDecodable: (tactic: Tactic[JsonError])
  =>  ((Ordinal is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Whole)(_.root.long.toInt.z)

  given text: (tactic: Tactic[JsonError])
  =>  ((Text is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Str)(_.root.string)

  given string: (tactic: Tactic[JsonError])
  =>  ((String is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Str)(_.root.string.s)

  given unit: (tactic: Tactic[JsonError])
  =>  ((Unit is Json.Decodable)^{tactic, caps.any}) =
    Json.Decodable(Morphology.Empty): value =>
      if value.root.isNull then ()
      else
        val reason =
          if value.root.isAbsent then Reason.Absent
          else Reason.NotType(value.root.primitive, JsonPrimitive.Null)

        raise(JsonError(reason))


  // Direct-parsing primitives. Genuinely pure — no tactic and no seal: all
  // raising happens inside the `JsonReader`, through the tactic it carries.
  given booleanParsable: Boolean is Json.Parsable =
    Json.Parsable(Morphology.Bool)(_.boolean())

  given doubleParsable: Double is Json.Parsable =
    Json.Parsable(Morphology.Real)(_.double())

  given floatParsable: Float is Json.Parsable =
    Json.Parsable(Morphology.Real)(_.double().toFloat)

  given longParsable: Long is Json.Parsable =
    Json.Parsable(Morphology.Whole)(_.long())

  given intParsable: Int is Json.Parsable =
    Json.Parsable(Morphology.Whole)(_.long().toInt)

  given textParsable: Text is Json.Parsable =
    Json.Parsable(Morphology.Str)(_.string())

  given stringParsable: String is Json.Parsable =
    Json.Parsable(Morphology.Str)(_.string().s)

  given jsonParsable: Json is Json.Parsable =
    Json.Parsable(Morphology.Any)(_.value())

  // Nominal counterparts of the `Json.Field` element-wise givens:
  // a collection reads directly only when its element type has opted in.
  given optionParsable: [value] => (parsable: => (value is Json.Parsable)^)
  =>  Option[value] is Json.Parsable =
    Json.Parsable.boxed(parsable)

  given arrayParsable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[JsonError],
        foci:    Foci[Json.Focus] )
  =>  ( parsable: => (element is Json.Parsable)^ )
  =>  collection[element] is Json.Parsable =
    Json.Parsable.iterable[collection, element](parsable)

  given mapParsable: [key: distillate.Decodable in Text, element]
  =>  Tactic[JsonError]
  =>  ( parsable: => (element is Json.Parsable)^ )
  =>  Map[key, element] is Json.Parsable =
    Json.Parsable.dictionary[key, element](parsable)

  given option: [value: Json.Decodable] => Tactic[JsonError]
  =>  Option[value] is Json.Decodable =

    // Sealed lazily: the shape must stay by-name (recursive derivation
    // depends on deferral), and its thunk may not capture the evidence.
    val shape: () -> Morphology =
      caps.unsafe.unsafeAssumePure(() => Morphology.Opt(value.shape()))


    Json.Decodable(shape()): json =>
      if json.root.isAbsent then None else Some(value.decoded(json))


  given optionEncodable: [value] => (encodable: value is Json.Encodable)
  =>  Option[value] is Json.Encodable =

    // Sealed lazily: the shape must stay by-name (recursive derivation
    // depends on deferral), and its thunk may not capture the evidence.
    val shape: () -> Morphology =
      caps.unsafe.unsafeAssumePure(() => Morphology.Opt(encodable.shape()))


    Json.Encodable(shape):
      case None        => Json.ast(Json.Ast(Unset))
      case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Json.Encodable =
    Json.Encodable(() => Morphology.Whole): int => Json.ast(Json.Ast(integral.toLong(int)))

  given textEncodableInJson: Text is Json.Encodable =
    Json.Encodable(() => Morphology.Str): text => Json.ast(Json.Ast(text.s))

  given stringEncodable: String is Json.Encodable =
    Json.Encodable(() => Morphology.Str): string => Json.ast(Json.Ast(string))

  given doubleEncodable: Double is Json.Encodable =
    Json.Encodable(() => Morphology.Real): double => Json.ast(Json.Ast(double))

  given intEncodable: Int is Json.Encodable =
    Json.Encodable(() => Morphology.Whole): int => Json.ast(Json.Ast(int.toLong))

  given unitEncodable: Unit is Json.Encodable =
    Json.Encodable(() => Morphology.Empty): unit => Json.ast(Json.Ast(JsonNull))

  given ordinalEncodable: Ordinal is Json.Encodable =
    Json.Encodable(() => Morphology.Whole): ordinal => Json.ast(Json.Ast(ordinal.n0.toLong))

  given longEncodable: Long is Json.Encodable =
    Json.Encodable(() => Morphology.Whole): long => Json.ast(Json.Ast(long))

  given booleanEncodable: Boolean is Json.Encodable =
    Json.Encodable(() => Morphology.Bool): boolean => Json.ast(Json.Ast(boolean))

  given jsonEncodable: Json is Json.Encodable = Json.Encodable(() => Morphology.Any)(identity(_))


  given listEncodable: [list <: List, element] => (encodable: => (element is Json.Encodable))
  =>  list[element] is Json.Encodable =

    // Laundered pure per the codec-thunk seal pattern; see `optional`'s comment above.
    caps.unsafe.unsafeAssumePure:
      // Sealed lazily: the shape must stay by-name (recursive derivation
      // depends on deferral), and its thunk may not capture the evidence.
      val shape: () -> Morphology =
        caps.unsafe.unsafeAssumePure(() => Morphology.Arr(encodable.shape()))

      Json.Encodable(shape):
        values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root)).asInstanceOf[IArray[Any]]))


  given setEncodable: [set <: Set, element] => (encodable: => (element is Json.Encodable))
  =>  set[element] is Json.Encodable =

    // Laundered pure per the codec-thunk seal pattern; see `optional`'s comment above.
    caps.unsafe.unsafeAssumePure:
      // Sealed lazily: the shape must stay by-name (recursive derivation
      // depends on deferral), and its thunk may not capture the evidence.
      val shape: () -> Morphology =
        caps.unsafe.unsafeAssumePure(() => Morphology.Arr(encodable.shape()))

      Json.Encodable(shape):
        values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root)).asInstanceOf[IArray[Any]]))


  given seriesEncodable: [series <: Series, element] => (encodable: => (element is Json.Encodable))
  =>  series[element] is Json.Encodable =

    // Laundered pure per the codec-thunk seal pattern; see `optional`'s comment above.
    caps.unsafe.unsafeAssumePure:
      // Sealed lazily: the shape must stay by-name (recursive derivation
      // depends on deferral), and its thunk may not capture the evidence.
      val shape: () -> Morphology =
        caps.unsafe.unsafeAssumePure(() => Morphology.Arr(encodable.shape()))

      Json.Encodable(shape):
        values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root)).asInstanceOf[IArray[Any]]))


  given array: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[JsonError],
        foci:    Foci[Json.Focus] )
  =>  ( decodable: => (element is Json.Decodable)^ )
  =>  collection[element] is Json.Decodable =

    // HONESTY BLOCKED BY THE CHECKER, not by design (Jon's 2026-07-12 ruling wants
    // this instance to be a capability): a by-name parameter cannot be named in a
    // capture set, so the honest result type must illegally hide `decodable`, and
    // the synthesized shape thunk aliases the tactic argument under separation
    // checking. Sealed until the checker can express it; see rep/DECISIONS.md
    // (upstream candidate: nameable by-name captures).
    caps.unsafe.unsafeAssumePure:
      val shape: () -> Morphology =
        caps.unsafe.unsafeAssumePure(() => Morphology.Arr(decodable.shape()))

      Json.Decodable(shape()): value =>
        val builder = factory.newBuilder

        value.root.array.each: json =>
          focus({
            val base = prior.let(_.pointer).or(JsonPointer())

            val newPointer =
              JsonPointer
                ( base.url,
                  Path[JsonPointer, JsonPointer.type, Tuple]
                    ( base.path.root, base.path.descent :+ ordinal.n0.toString.tt ) )

            Json.Focus(newPointer)
          }):
            builder += decodable.decoded(Json.ast(json))

        builder.result()


  given map: [key: distillate.Decodable in Text, element]
  =>  ( decodable: => (element is Json.Decodable)^ )
  =>  Tactic[JsonError]
  =>  Map[key, element] is Json.Decodable =

    // HONESTY BLOCKED BY THE CHECKER, not by design (Jon's 2026-07-12 ruling wants
    // this instance to be a capability): a by-name parameter cannot be named in a
    // capture set, so the honest result type must illegally hide `decodable`, and
    // the synthesized shape thunk aliases the tactic argument under separation
    // checking. Sealed until the checker can express it; see rep/DECISIONS.md
    // (upstream candidate: nameable by-name captures).
    caps.unsafe.unsafeAssumePure:
      val shape: () -> Morphology =
        caps.unsafe.unsafeAssumePure(() => Morphology.Dict(Morphology.Str, decodable.shape()))

      Json.Decodable(shape()): value =>
        val root = value.root
        val n = root.objectSize
        var i = 0
        var acc = Map.empty[key, element]

        while i < n do
          acc =
            acc.updated
              ( root.objectKey(i).tt.as,
                decodable.decoded(Json.ast(root.objectValue(i))) )

          i += 1

        acc


  given mapEncodable: [key: anticipation.Encodable in Text, element]
  =>  ( encodable: element is Json.Encodable )
  =>  Map[key, element] is Json.Encodable =

    // Sealed lazily: the shape must stay by-name (recursive derivation
    // depends on deferral), and its thunk may not capture the evidence.
    val shape: () -> Morphology =
      caps.unsafe.unsafeAssumePure(() => Morphology.Dict(Morphology.Str, encodable.shape()))


    Json.Encodable(shape): map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      Json.ast(Json.Ast.obj(IArray.from(keys.map(_.encode.s)).asInstanceOf[IArray[String]], values.asInstanceOf[IArray[Any]]))


  given jsonEncodableInText: Json is anticipation.Encodable in Text = json =>
    given Formatting = Formatting(Unset, false)
    json.root.show

  // Laundered pure per the codec-thunk seal pattern (see the primitive codecs above and
  // rep/DECISIONS.md): the resolution-scoped tactic shares the instance's given-resolution
  // lifetime, and a capturing instance cannot be expressed via `new Aggregable` (its pure
  // base class forbids captured references in method bodies).
  given aggregable: (tactic: Tactic[ParseError], tracking: PositionTracking)
  =>  Json is Aggregable by Data =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = Json
        type Operand = Data

        def aggregate(bytes: LazyList[Data]): Json = readJson(bytes.iterator)
        override def accept(stream: (Stream[Data] over Credit)^): Json = readJson(stream)


  // Direct parsing: when the value knows how to consume JSON tokens itself,
  // the AST is never materialized. Declared here (not in `Json2`, where the
  // `Decodable`-based `aggregableDirect` lives) so it wins whenever a
  // `Json.Parsable` exists, and is otherwise inapplicable — existing code
  // resolves exactly as before. Sealed like `aggregable` above.
  given aggregableParsed: [value]
  =>  (parsable: (value is Json.Parsable)^)
  =>  (tactic: Tactic[ParseError], tracking: PositionTracking)
  =>  ((value in Json) is Aggregable by Data) =

    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = value in Json
        type Operand = Data

        def aggregate(bytes: LazyList[Data]): value in Json =
          // A single in-memory block — the common case — skips the iterator
          // plumbing entirely.
          if !bytes.isEmpty && bytes.tail.isEmpty
          then parseDirect(bytes.head, parsable).asInstanceOf[value in Json]
          else parseDirect(bytes.iterator, parsable).asInstanceOf[value in Json]

        override def accept(stream: (Stream[Data] over Credit)^): value in Json =
          parseDirect(stream, parsable).asInstanceOf[value in Json]


  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream —
  // the `Readable.dataData` precedent. Concrete in `Data`, so it beats the
  // composed `dataToData` pipeline by specificity. Sealed like
  // `aggregableParsed` above.
  given readableParsed: [value]
  =>  (parsable: (value is Json.Parsable)^)
  =>  (tactic: Tactic[ParseError], tracking: PositionTracking)
  =>  (Data is Readable to (value in Json)) =

    caps.unsafe.unsafeAssumePure:
      data => parseDirect(data, parsable).asInstanceOf[value in Json]

  given showable: Formatting => Json is Showable = _.root.show


  given abstractable: (encoder: CharEncoder, formatting: Formatting)
  =>  Json is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Json
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(json: Json): HttpStreams.Content =
        (t"application/json; charset=${encoder.encoding.name}", HttpStreams.Body(json.show.in[Data]))


  // Laundered pure like the primitive codecs above; additionally, this instance is
  // summoned inside inline bodies that are expanded within staged quotes (superlunary's
  // `Stageable.json`), where a capturing instance cannot be pickled.
  given decodable: (tactic: Tactic[ParseError])
  =>  Json is distillate.Decodable in Text =
    caps.unsafe.unsafeAssumePure:
      text => LazyList(text.in[Data](using charEncoders.utf8Encoder)).read[Json]

  given instantiable: (tactic: Tactic[ParseError])
  =>  Json is Instantiable across HttpRequests from Text =
    caps.unsafe.unsafeAssumePure:
      text => LazyList(text.in[Data](using charEncoders.utf8Encoder)).read[Json]

  def applyDynamicNamed(methodName: "make")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0))).asInstanceOf[IArray[String]]
    val values: IArray[Json.Ast] = IArray.from(elements.map(_(1).root)).asInstanceOf[IArray[Json.Ast]]
    Json(Json.Ast.obj(keys, values.asInstanceOf[IArray[Any]]))

  def discriminatedUnion[value](label: Text): value is Discriminable in Json =
    DiscriminantField(label)

  // The three standard shapes a discriminated sum takes on the wire. Each is
  // a NAMED class so both derivations can recognize the shape: the AST
  // decoder knows whether to decode the whole value or the variant payload,
  // and the direct parser picks the most streaming-friendly token-level
  // dispatch for each — a wrapper's tag is always its first token, an
  // envelope's usually is, and an internal field needs a bounded scan-ahead.
  // Any other (custom) `Discriminable` still works through the AST bridge.

  // `{"radius": 1.0, "type": "Circle"}` — the tag is a field of the
  // variant's own object, under `label`.
  class DiscriminantField[value](label: Text) extends Discriminable:
    type Form = Json
    type Self = value

    private[jacinta] def field: Text = label
    protected def key: String = label.s

    import dynamicJsonAccess.enabled

    def rewrite(kind: Text, json: Json): Json = unsafely(json.updateDynamic(key)(kind))
    // Reads the discriminator through direct AST access rather than a `Decodable` summon:
    // the summoned instance would capture `safely`'s scoped tactic, which the summon's
    // invariant `Self` bound cannot absorb.
    def discriminate(json: Json): Optional[Text] = safely(json.selectField(key).root.string)
    def variant(json: Json): Json = unsafely(json.updateDynamic(key)(Unset))

  // `{"Circle": {"radius": 1.0}}` — a single-key object whose key is the
  // tag and whose value is the variant's payload.
  class DiscriminantWrapper[value]() extends Discriminable:
    type Form = Json
    type Self = value

    def rewrite(kind: Text, json: Json): Json =
      Json.ast(Json.Ast.obj(IArray(kind.s), IArray(json.root)))

    def discriminate(json: Json): Optional[Text] =
      if json.root.isObject && json.root.objectSize == 1
      then json.root.objectKey(0).tt
      else Unset

    def variant(json: Json): Json = Json.ast(json.root.objectValue(0))

  // `{"type": "Circle", "value": {"radius": 1.0}}` — the tag and the
  // variant's payload are adjacent fields of an enclosing object.
  class DiscriminantEnvelope[value](tagLabel: Text, valueLabel: Text) extends Discriminable:
    type Form = Json
    type Self = value

    private[jacinta] def tagField: Text = tagLabel
    private[jacinta] def valueField: Text = valueLabel

    def rewrite(kind: Text, json: Json): Json =
      Json.ast(Json.Ast.obj(IArray(tagLabel.s, valueLabel.s), IArray(kind.s, json.root)))

    def discriminate(json: Json): Optional[Text] =
      safely(json.selectField(tagLabel.s).root.string)

    def variant(json: Json): Json = json.selectField(valueLabel.s)

class Json(rootValue: Any, positions: Optional[Json.PositionIndex] = Unset)
extends Dynamic, Topical, Original derives CanEqual:
  private[jacinta] def root: Json.Ast = rootValue.asInstanceOf[Json.Ast]
  def positionIndex: Optional[Json.PositionIndex] = positions

  // Total field access used by the schema-typed navigation macros and by
  // internal optics: an absent `Json` for a missing key, never raising.
  private[jacinta] def selectField(field: String): Json =
    if root.isAbsent then Json.ast(Json.Ast(Unset))
    else root.objectIndexOf(field) match
      case -1    => Json.ast(Json.Ast(Unset))
      case index => Json(root.objectValue(index))

  // Total array access: an absent `Json` for an out-of-bounds index.
  private[jacinta] def selectIndex(index: Int): Json =
    if root.isArray && index >= 0 && index < root.arrayLength then Json(root.arrayElement(index))
    else Json.ast(Json.Ast(Unset))

  // Raising array access, preserving the behaviour of plain `json(i)`.
  private[jacinta] def indexValue(index: Int): Json raises JsonError = Json(root.array(index))

  // Array indexing. For a schema-typed `Json of List[E] from R` the navigation
  // macro yields `Json of E from R`; for a plain `Json` it indexes at runtime
  // (raising on a non-array or out-of-bounds index), exactly as before.
  transparent inline def apply(index: Int): Json = ${jacinta.internal.index('this, 'index)}

  // Dynamic field selection. For a schema-typed `Json of P from R` the macro
  // checks `P` has the field and yields `Json of <field-type> from R`; for a
  // plain `Json` it requires a `DynamicJsonEnabler` (as before) and reads the
  // field at runtime.
  transparent inline def selectDynamic(field: String): Json =
    ${jacinta.internal.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(index: Int): Json =
    ${jacinta.internal.applied('this, 'field, 'index)}


  def update[value: anticipation.Encodable in Json](index: Int, value: value)
    (using erased dynamicJsonEnabler: DynamicJsonEnabler)
  :   Json raises JsonError =

    if !root.isArray then raise(JsonError(Reason.NotType(root.primitive, JsonPrimitive.Array)))
    val n = root.arrayLength
    val updated = new Array[Any](n)
    var i = 0

    while i < n do
      updated(i) =
        if i == index then value.encode.root
        else root.arrayElement(i)

      i += 1

    Json.ast(Json.Ast.arr(updated.asInstanceOf[IArray[Any]]))


  def updateDynamic(field: String)[value: anticipation.Encodable in Json](value: value)
    (using erased dynamicJsonEnabler: DynamicJsonEnabler)
  :   Json raises JsonError =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased dynamicJsonEnabler: DynamicJsonEnabler)
  :   Json raises JsonError =

    delete(field)


  private[jacinta] def modify(field: String, value: Json): Json raises JsonError =
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length
    val n = len/2

    root.objectIndexOf(field) match
      case -1 =>
        val out = new Array[Any](len + 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(len) = field
        out(len + 1) = value.root
        Json.ast(Json.Ast(out.asInstanceOf[IArray[Any]]))

      case index =>
        val out = new Array[Any](len)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(index*2 + 1) = value.root
        Json.ast(Json.Ast(out.asInstanceOf[IArray[Any]]))

  private[jacinta] def delete(field: String): Json raises JsonError =
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length

    root.objectIndexOf(field) match
      case -1 =>
        Json.ast(root)

      case index =>
        val out = new Array[Any](len - 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, index*2)

        System.arraycopy
          ( arr.asInstanceOf[Array[Any]],
            index*2 + 2,
            out,
            index*2,
            len - index*2 - 2 )

        Json.ast(Json.Ast(out.asInstanceOf[IArray[Any]]))

  def apply(field: Text): Json raises JsonError =
    if root.isAbsent then Json.ast(Json.Ast(Unset))
    else root.objectIndexOf(field.s) match
      case -1    => Json.ast(Json.Ast(Unset))
      case index => Json(root.objectValue(index))

  override def hashCode: Int =
    def recur(value: Json.Ast): Int = value.asMatchable match
      case value: Long       => value.hashCode
      case value: Double     => value.hashCode
      case value: String     => value.hashCode
      case value: Boolean    => value.hashCode

      case value: Int =>
        // Small-BCD number — hash through the BigDecimal projection so
        // it stays consistent with the `Bcd` / `Long` / `Double` paths.
        BigDecimal(Bcd.bcdIntText(value)).hashCode

      case value: Array[Long] @unchecked =>
        // BCD-packed number array — same recursion as `Array[Double]` so
        // arrays of equal values hash identically regardless of which
        // backing representation the parser picked.
        val ast = value.asInstanceOf[Json.Ast]
        val n = value.length
        var acc = n.hashCode
        var i = 0

        while i < n do
          acc = acc*31 ^ recur(ast.arrayElement(i))
          i += 1

        acc

      case value: Array[Double] @unchecked =>
        // High-precision number (`Bcd`) — hash via the BigDecimal
        // projection so a Bcd whose value equals a BigDecimal literal has
        // a consistent hash.
        value.asInstanceOf[Bcd].toBigDecimal.hashCode

      case value: Array[Int] @unchecked =>
        // Number array in single-Int small-BCD form — recurse per element
        // for cross-form equality with the boxed/Double/Long array shapes.
        val ast = value.asInstanceOf[Json.Ast]
        val n = value.length
        var acc = n.hashCode
        var i = 0

        while i < n do
          acc = acc*31 ^ recur(ast.arrayElement(i))
          i += 1

        acc

      case value: IArray[Any] @unchecked =>
        // Heterogeneous array or object, distinguished by parity.
        val ast = value.asInstanceOf[Json.Ast]

        if ast.isObject then
          val n = ast.objectSize
          var acc = Map.empty[String, Int]
          var i = 0

          while i < n do
            acc = acc.updated(ast.objectKey(i), recur(ast.objectValue(i)))
            i += 1

          acc.hashCode
        else
          val n = ast.arrayLength
          var acc = n.hashCode
          var i = 0

          while i < n do
            acc = acc*31 ^ recur(ast.arrayElement(i))
            i += 1

          acc

      case _ =>
        0

    recur(root)

  override def equals(right: Any): Boolean = right.asMatchable match
    case right: Json =>
      def arrayEq(leftAst: Json.Ast, rightAst: Json.Ast): Boolean =
        val rn = rightAst.arrayLength
        val ln = leftAst.arrayLength

        rn == ln &&
          { var i = 0
            var eq = true

            while i < rn && eq do
              if !recur(leftAst.arrayElement(i), rightAst.arrayElement(i)) then eq = false
              i += 1

            eq }

      def objectEq(leftAst: Json.Ast, rightAst: Json.Ast): Boolean =
        val rn = rightAst.objectSize
        val ln = leftAst.objectSize

        if rn != ln then false
        else
          var leftMap = Map.empty[String, Json.Ast]
          var i = 0

          while i < ln do
            leftMap = leftMap.updated(leftAst.objectKey(i), leftAst.objectValue(i))
            i += 1

          var rightMap = Map.empty[String, Json.Ast]
          i = 0

          while i < rn do
            rightMap = rightMap.updated(rightAst.objectKey(i), rightAst.objectValue(i))
            i += 1

          leftMap.keySet == rightMap.keySet && leftMap.keySet.forall: key =>
            recur(leftMap(key), rightMap(key))

      def recur(left: Json.Ast, right: Json.Ast): Boolean = right.asMatchable match
        case right: Long => left.asMatchable match
          case left: Long                    => left == right
          case left: Int                     => left == right.toInt && left.toLong == right
          case left: Double                  => left == right

          case left: Array[Double] @unchecked =>
            left.asInstanceOf[Bcd].toBigDecimal == BigDecimal(right)

          case _                             => false

        case right: Int => left.asMatchable match
          case left: Int    => left == right
          case left: Long   => BigDecimal(Bcd.bcdIntText(right)) == BigDecimal(left)
          case left: Double => BigDecimal(Bcd.bcdIntText(right)) == BigDecimal(left)

          case left: Array[Double] @unchecked =>
            left.asInstanceOf[Bcd].toBigDecimal == BigDecimal(Bcd.bcdIntText(right))

          case _                             => false

        case right: Double => left.asMatchable match
          case left: Long   => left == right
          case left: Int    => BigDecimal(Bcd.bcdIntText(left)) == BigDecimal(right)
          case left: Double => left == right

          case left: Array[Double] @unchecked =>
            left.asInstanceOf[Bcd].toBigDecimal == BigDecimal(right)

          case _                             => false

        case right: String => left.asMatchable match
          case left: String => left == right
          case _            => false

        case right: Boolean => left.asMatchable match
          case left: Boolean => left == right
          case _             => false

        case right: Array[Long] @unchecked =>
          // BCD-Long-packed number array.
          val rightAst = right.asInstanceOf[Json.Ast]

          left.asMatchable match
            case _: Array[Long] @unchecked => arrayEq(left, rightAst)
            case _: Array[Int] @unchecked  => arrayEq(left, rightAst)

            case _: Array[AnyRef] @unchecked if left.asInstanceOf[Json.Ast].isArray =>
              arrayEq(left, rightAst)

            case _ => false

        case right: Array[Double] @unchecked =>
          // High-precision number (`Bcd`).
          val rb = right.asInstanceOf[Bcd]

          left.asMatchable match
            case left: Long   => BigDecimal(left) == rb.toBigDecimal
            case left: Int    => BigDecimal(Bcd.bcdIntText(left)) == rb.toBigDecimal
            case left: Double => BigDecimal(left) == rb.toBigDecimal

            case left: Array[Double] @unchecked =>
              left.asInstanceOf[Bcd].toBigDecimal == rb.toBigDecimal

            case _                             => false

        case right: Array[Int] @unchecked =>
          // Number array in single-Int small-BCD form.
          val rightAst = right.asInstanceOf[Json.Ast]

          left.asMatchable match
            case _: Array[Int] @unchecked  => arrayEq(left, rightAst)
            case _: Array[Long] @unchecked => arrayEq(left, rightAst)

            case _: Array[AnyRef] @unchecked if left.asInstanceOf[Json.Ast].isArray =>
              arrayEq(left, rightAst)

            case _ => false

        case right: IArray[Any] @unchecked =>
          // Heterogeneous array or object, distinguished by parity.
          val rightAst = right.asInstanceOf[Json.Ast]
          val rightIsObject = rightAst.isObject

          left.asMatchable match
            case _: Array[AnyRef] @unchecked =>
              val leftAst = left.asInstanceOf[Json.Ast]

              if rightIsObject then
                if leftAst.isObject then objectEq(leftAst, rightAst) else false
              else if leftAst.isArray then
                arrayEq(leftAst, rightAst)
              else
                false

            case _: Array[Long] @unchecked if !rightIsObject =>
              arrayEq(left, rightAst)

            case _: Array[Int] @unchecked if !rightIsObject =>
              arrayEq(left, rightAst)

            case _ => false

        case _ =>
          false

      recur(root, right.root)

    case _ =>
      false

  // Plain using-parameters, de-sugared from `raises`/`tracks`: a context-function
  // result may not hide the capability-typed evidence.
  def as[value](using decodable: (value is distillate.Decodable in Json at Json.Focus)^)
    ( using Foci[Json.Focus], Tactic[JsonError] )
  :   value =

    val result = decodable.decoded(this)
    val foci = summon[Foci[Json.Focus]]
    foci.supplement(foci.length, _.let(decodable.position(this, _)).vouch)
    result

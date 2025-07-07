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
  given decodableAtFocus: [value]
  =>  ( inner: value is Decodable in Json )
  =>  value is Decodable in Json at Json.Focus =

    new JsonDecodable[value]:
      def decoded(json: Json): value = inner.decoded(json)

trait Json2 extends Json3:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Json.Encodable )
  =>  value is Json.Encodable =

    Json.Encodable(Morphology.Opt(encodable.shape())): value =>
      value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Json.ast(Json.Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[JsonError]
  =>  ( decodable: => (inner is Json.Decodable) )
  =>  value is Json.Decodable =

    // The inner codec may capture the consumer's `Tactic` (primitive codecs are tactic-taking
    // givens), so the by-name argument synthesized by given resolution is not capture-free. A
    // codec instance is pure by design, and its lifetime is governed by the same given
    // resolution that bound the tactic, so the whole instance is laundered pure rather than
    // making every codec instance a capability. (The honest alternative — capturing instance
    // types throughout — is the given-captures-context design question; see rep/DECISIONS.md.)
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Opt(decodable.shape())): json =>
        // An `Optional` field reads `Unset` from an absent key *and* from an
        // explicit JSON `null`: both mean "no value" on the wire. (Missing keys
        // arrive here as `Unset`; a present `null` arrives as the `JsonNull`
        // sentinel.)
        if json.root.isAbsent || json.root.isNull then Unset else decodable.decoded(json)


  // Laundered pure per the codec-thunk seal pattern, like the primitive codecs in
  // `object Json` (see the comment there and rep/DECISIONS.md).
  given bytes: (tactic: Tactic[JsonError])
  =>  Bytes is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Whole)(_.root.long.b)

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
        Json.Decodable(Morphology.Str)(provide[Tactic[JsonError]](_.root.string.decode[value]))

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Json.Encodable = summonFrom:
    case given (`value` is anticipation.Encodable in Text) =>
      Json.Encodable(Morphology.Str): value => Json.ast(Json.Ast(value.encode.s))

    case given Reflection[`value`] =>
      EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Json.Decodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Json.Decodable =

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
      // with `build`'s per-field lambda). The decode SAM therefore closes over the
      // resolution-scoped tactic, which shares the instance's given-resolution lifetime, so
      // the instance is laundered pure per the codec-thunk seal pattern (see the primitive
      // codecs in `object Json` and rep/DECISIONS.md).
      caps.unsafe.unsafeAssumePure:
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

              delegate(discriminant): [variant <: derivation] =>
                context => context.decoded(json)

  object EncodableDerivation extends Derivable[Json.Encodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Json.Encodable =

      Json.Encodable({
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
      Json.Encodable(Morphology.Any):
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
    def apply[value](shape0: => Morphology)(lambda: (value -> Json)^)
    :   ((value is Json.Encodable)^{lambda}) =

      // The shape thunk may close over a `Tactic` when field/element codecs are summoned from
      // tactic-taking givens, but `shape()` only reads static metadata and never encodes or
      // raises, so the capture is behaviourally inert; laundered pure to keep codec instances
      // pure. Invariant: a `shape()` implementation must never invoke its tactic.
      val shape1: () -> Morphology = caps.unsafe.unsafeAssumePure { () => shape0 }

      new Json.Encodable:
        type Self = value
        def encoded(value: value): Json = lambda(value)
        def shape(): Morphology = shape1()

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

      value

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
    def bcdArr(values: Array[Long]): Ast = values

    // Build a number-only array node using the single-Int small-BCD
    // encoding (see `Bcd.packBcdInt`). For arrays where every number
    // fits in 7 nibbles — the half-memory variant of `bcdArr`.
    def smallBcdArr(values: Array[Int]): Ast = values

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
        case bcds: Array[Long] @unchecked   => bcds.length
        case smalls: Array[Int] @unchecked  => smalls.length

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
        case bcds: Array[Long] @unchecked =>
          val v = bcds(index)
          val text = Bcd.bcdLongText(v)

          try Json.Ast(java.lang.Long.parseLong(text))
          catch case _: NumberFormatException => Json.Ast(java.lang.Double.parseDouble(text))

        case smalls: Array[Int] @unchecked =>
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

      def array: IArray[Json.Ast] raises JsonError = (json: @unchecked) match
        case bcds: Array[Long] @unchecked =>
          IArray.tabulate(bcds.length)(json.arrayElement(_))

        case smalls: Array[Int] @unchecked =>
          IArray.tabulate(smalls.length)(json.arrayElement(_))

        case _ =>
          if isArray then
            val full = json.asInstanceOf[IArray[Json.Ast]]
            val n = json.arrayLength

            if n == full.length then full
            else IArray.tabulate(n)(full(_))
          else
            expected(JsonPrimitive.Array) yet IArray[Json.Ast]()

      def double: Double raises JsonError = json.asMatchable match
        case value: Double                   => value
        case value: Long                     => value.toDouble
        case value: Int                      => Bcd.bcdIntToDouble(value)
        case value: Array[Double] @unchecked => value.asInstanceOf[Bcd].toDouble
        case _                               => expected(JsonPrimitive.Number) yet 0.0

      def bcd: Bcd raises JsonError = json.asMatchable match
        case value: Array[Double] @unchecked => value.asInstanceOf[Bcd]
        case value: Long                     => Bcd(BigDecimal(value))
        case value: Double                   => Bcd(BigDecimal(value))

        case value: Int =>
          Bcd.fromString(Bcd.bcdIntText(value).stripPrefix("-"), value < 0)

        case _ =>
          expected(JsonPrimitive.Number) yet Bcd(BigDecimal(0L))

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
      def obj: (IArray[String], IArray[Json.Ast]) raises JsonError =
        if !isObject
        then expected(JsonPrimitive.Object) yet (IArray[String]() -> IArray[Json.Ast]())
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

    private[jacinta] def parse(input: Stream[Data] over Credit)(using mode: NumberMode)
    :   Json.Ast raises ParseError =

      Json.Ast(Parser.parse(input, mode))

    private[jacinta] def parseTracked(input: Stream[Data] over Credit)(using mode: NumberMode)
    :   (Json.Ast, Json.PositionIndex) raises ParseError =

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
    parseTracked(source.data)

  // Parse a byte-chunk iterator into a `Json`, honouring the in-scope
  // `PositionTracking` toggle (`parsing.trackPositions`): when on, source
  // positions are recorded and retained on the `Json` (locatable via
  // `Positionable`); when off, the throughput path is unchanged. This is the
  // single place the `read`/`load` givens branch on tracking.
  private def readJson(input: Iterator[Data])(using Tactic[ParseError], PositionTracking): Json =
    summon[PositionTracking] match
      case PositionTracking.On =>
        val (ast, index) = Json.Ast.parseTracked(input)
        new Json(ast, index)

      case PositionTracking.Off =>
        Json(Json.Ast.parse(input))

  // As above, but pulling from a demand-aware stream rather than a chunk
  // iterator.
  private def readJson(input: Stream[Data] over Credit)
    ( using Tactic[ParseError], PositionTracking )
  :   Json =

    summon[PositionTracking] match
      case PositionTracking.On =>
        val (ast, index) = Json.Ast.parseTracked(input)
        new Json(ast, index)

      case PositionTracking.Off =>
        Json(Json.Ast.parse(input))

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

  given filterOptical: Filter[Json] is Optical from Json onto Json = filter =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Json.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            val element = Json.ast(origin.root.arrayElement(i))
            updated(i) = (if filter.predicate(element) then lambda(element) else element).root
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
  =>  Boolean is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Bool)(_.root.boolean)

  given double: (tactic: Tactic[JsonError])
  =>  Double is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Real)(_.root.double)

  given float: (tactic: Tactic[JsonError])
  =>  Float is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Real)(_.root.double.toFloat)

  given long: (tactic: Tactic[JsonError])
  =>  Long is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Whole)(_.root.long)

  given int: (tactic: Tactic[JsonError])
  =>  Int is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Whole)(_.root.long.toInt)

  given ordinalDecodable: (tactic: Tactic[JsonError])
  =>  Ordinal is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Whole)(_.root.long.toInt.z)

  given text: (tactic: Tactic[JsonError])
  =>  Text is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Str)(_.root.string)

  given string: (tactic: Tactic[JsonError])
  =>  String is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Str)(_.root.string.s)

  given unit: (tactic: Tactic[JsonError])
  =>  Unit is Json.Decodable =
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Empty): value =>
        if value.root.isNull then ()
        else
          val reason =
            if value.root.isAbsent then Reason.Absent
            else Reason.NotType(value.root.primitive, JsonPrimitive.Null)

          raise(JsonError(reason))


  given option: [value: Json.Decodable] => Tactic[JsonError]
  =>  Option[value] is Json.Decodable =

    Json.Decodable(Morphology.Opt(value.shape())): json =>
      if json.root.isAbsent then None else Some(value.decoded(json))


  given optionEncodable: [value] => (encodable: value is Json.Encodable)
  =>  Option[value] is Json.Encodable =

    Json.Encodable(Morphology.Opt(encodable.shape())):
      case None        => Json.ast(Json.Ast(Unset))
      case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Json.Encodable =
    Json.Encodable(Morphology.Whole): int => Json.ast(Json.Ast(integral.toLong(int)))

  given textEncodableInJson: Text is Json.Encodable =
    Json.Encodable(Morphology.Str): text => Json.ast(Json.Ast(text.s))

  given stringEncodable: String is Json.Encodable =
    Json.Encodable(Morphology.Str): string => Json.ast(Json.Ast(string))

  given doubleEncodable: Double is Json.Encodable =
    Json.Encodable(Morphology.Real): double => Json.ast(Json.Ast(double))

  given intEncodable: Int is Json.Encodable =
    Json.Encodable(Morphology.Whole): int => Json.ast(Json.Ast(int.toLong))

  given unitEncodable: Unit is Json.Encodable =
    Json.Encodable(Morphology.Empty): unit => Json.ast(Json.Ast(JsonNull))

  given ordinalEncodable: Ordinal is Json.Encodable =
    Json.Encodable(Morphology.Whole): ordinal => Json.ast(Json.Ast(ordinal.n0.toLong))

  given longEncodable: Long is Json.Encodable =
    Json.Encodable(Morphology.Whole): long => Json.ast(Json.Ast(long))

  given booleanEncodable: Boolean is Json.Encodable =
    Json.Encodable(Morphology.Bool): boolean => Json.ast(Json.Ast(boolean))

  given jsonEncodable: Json is Json.Encodable = Json.Encodable(Morphology.Any)(identity(_))


  given listEncodable: [list <: List, element] => (encodable: => (element is Json.Encodable))
  =>  list[element] is Json.Encodable =

    // Laundered pure per the codec-thunk seal pattern; see `optional`'s comment above.
    caps.unsafe.unsafeAssumePure:
      Json.Encodable(Morphology.Arr(encodable.shape())):
        values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given setEncodable: [set <: Set, element] => (encodable: => (element is Json.Encodable))
  =>  set[element] is Json.Encodable =

    // Laundered pure per the codec-thunk seal pattern; see `optional`'s comment above.
    caps.unsafe.unsafeAssumePure:
      Json.Encodable(Morphology.Arr(encodable.shape())):
        values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given seriesEncodable: [series <: Series, element] => (encodable: => (element is Json.Encodable))
  =>  series[element] is Json.Encodable =

    // Laundered pure per the codec-thunk seal pattern; see `optional`'s comment above.
    caps.unsafe.unsafeAssumePure:
      Json.Encodable(Morphology.Arr(encodable.shape())):
        values => Json.ast(Json.Ast.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given array: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[JsonError],
        foci:    Foci[Json.Focus] )
  =>  ( decodable: => (element is Json.Decodable) )
  =>  collection[element] is Json.Decodable =

    // The by-name inner codec and the resolution-scoped tactic share this instance's
    // given-resolution lifetime; the whole instance is laundered pure (the codec-thunk
    // seal pattern; see rep/DECISIONS.md).
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Arr(decodable.shape())): value =>
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
  =>  ( decodable: => (element is Json.Decodable) )
  =>  Tactic[JsonError]
  =>  Map[key, element] is Json.Decodable =

    // Laundered pure as for `array` above.
    caps.unsafe.unsafeAssumePure:
      Json.Decodable(Morphology.Dict(Morphology.Str, decodable.shape())): value =>
        val root = value.root
        val n = root.objectSize
        var i = 0
        var acc = Map.empty[key, element]

        while i < n do
          acc =
            acc.updated
              ( root.objectKey(i).tt.decode,
                decodable.decoded(Json.ast(root.objectValue(i))) )

          i += 1

        acc


  given mapEncodable: [key: anticipation.Encodable in Text, element]
  =>  ( encodable: element is Json.Encodable )
  =>  Map[key, element] is Json.Encodable =

    Json.Encodable(Morphology.Dict(Morphology.Str, encodable.shape())): map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      Json.ast(Json.Ast.obj(IArray.from(keys.map(_.encode.s)), values))


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
        override def accept(stream: Stream[Data] over Credit): Json = readJson(stream)


  // Sealed like `aggregable` above.
  given aggregableDirect: [value: distillate.Decodable in Json]
  =>  (tactic: Tactic[ParseError], jsonTactic: Tactic[JsonError], tracking: PositionTracking)
  =>  ((value in Json) is Aggregable by Data) =

    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = value in Json
        type Operand = Data

        def aggregate(bytes: LazyList[Data]): value in Json =
          readJson(bytes.iterator).as[value].asInstanceOf[value in Json]

        override def accept(stream: Stream[Data] over Credit): value in Json =
          readJson(stream).as[value].asInstanceOf[value in Json]


  given showable: Formatting => Json is Showable = _.root.show


  given abstractable: (encoder: CharEncoder, formatting: Formatting)
  =>  Json is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Json
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(json: Json): HttpStreams.Content =
        (t"application/json; charset=${encoder.encoding.name}", HttpStreams.Body(json.show.data))


  // Laundered pure like the primitive codecs above; additionally, this instance is
  // summoned inside inline bodies that are expanded within staged quotes (superlunary's
  // `Stageable.json`), where a capturing instance cannot be pickled.
  given decodable: (tactic: Tactic[ParseError])
  =>  Json is distillate.Decodable in Text =
    caps.unsafe.unsafeAssumePure:
      text => LazyList(text.data(using charEncoders.utf8Encoder)).read[Json]

  given instantiable: (tactic: Tactic[ParseError])
  =>  Json is Instantiable across HttpRequests from Text =
    caps.unsafe.unsafeAssumePure:
      text => LazyList(text.data(using charEncoders.utf8Encoder)).read[Json]

  def applyDynamicNamed(methodName: "make")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[Json.Ast] = IArray.from(elements.map(_(1).root))
    Json(Json.Ast.obj(keys, values.asInstanceOf[IArray[Any]]))

  def discriminatedUnion[value](label: Text): value is Discriminable in Json = new Discriminable:
    type Form = Json
    type Self = value

    protected def key: String = label.s

    import dynamicJsonAccess.enabled

    def rewrite(kind: Text, json: Json): Json = unsafely(json.updateDynamic(key)(kind))
    // Reads the discriminator through direct AST access rather than a `Decodable` summon:
    // the summoned instance would capture `safely`'s scoped tactic, which the summon's
    // invariant `Self` bound cannot absorb.
    def discriminate(json: Json): Optional[Text] = safely(json.selectField(key).root.string)
    def variant(json: Json): Json = unsafely(json.updateDynamic(key)(Unset))

  private[jacinta] object Parser:
    private[jacinta] type Raw =
      Long | Int | Double | Bcd | String | IArray[Any] |
        Array[Long] | Array[Int] |
        Boolean | Json.JsonNull.type | Unset

    private inline val NumZero       = 0
    private inline val NumInt        = 1
    private inline val NumAfterDot   = 2
    private inline val NumFrac       = 3
    private inline val NumAfterE     = 4
    private inline val NumAfterESign = 5
    private inline val NumExp        = 6

    // `parseArray` number-container modes (see comment on `parseArray`).
    private[jacinta] inline val ModeUndecided = 0
    private[jacinta] inline val ModeBoxed     = 1
    private[jacinta] inline val ModeBcdLong   = 2
    private[jacinta] inline val ModeBcdInt    = 3

    // ASCII bytes of the JSON keyword literals, packed little-endian: byte at
    // offset i goes into bits (i*8)..(i*8+7). Four-byte literals (`true`,
    // `null`) fit an `Int`; `false` spans 5 bytes and uses a `Long` whose
    // top 24 bits are unused (the parser reads exactly 5 bytes, so those
    // bits aren't loaded). Allows the keyword check to collapse from a
    // four-or-five-step `expect` chain into a single integer compare.
    //
    //   't' 'r' 'u' 'e'  → 0x65_75_72_74  (read LSB-first)
    //   'n' 'u' 'l' 'l'  → 0x6C_6C_75_6E
    //   'f' 'a' 'l' 's' 'e' (low 40 bits) → 0x0000_0065_736C_6166
    private inline val TrueWord  = 0x6575_7274
    private inline val NullWord  = 0x6C6C_756E
    private inline val FalseWord = 0x0000_0065_736C_6166L

    private val TenPow: Array[Double] = Array.tabulate(23): i =>
      var p = 1.0
      var n = i
      while n > 0 do { p *= 10.0; n -= 1 }
      p

    // Byte-class table for `parseString`'s fast-scan loop. Entry `i` is
    // 1 when byte `i` keeps the scan going (printable ASCII other than
    // `"` and `\`), 0 when it ends it (`"`, `\`, any control byte, or
    // any byte ≥ 128 — UTF-8 continuation or lead). Single load + compare
    // per byte beats the three-compare chain
    // `b >= 32 && b != Quote && b != Backslash`. The 128–255 range stops
    // for the same reason the original signed-Byte `b >= 32` test did:
    // those bytes appear as negative when read as signed and need the
    // multi-byte UTF-8 decoder in `tail`, not the fast slice path.
    private val StringScanContinue: Array[Byte] =
      val arr = new Array[Byte](256)
      var i = 0

      while i < 256 do
        arr(i) =
          if i >= 32 && i < 128 && i != 0x22 && i != 0x5C
          then 1.toByte
          else 0.toByte

        i += 1

      arr

    private val pool: ThreadLocal[Parser] =
      new ThreadLocal[Parser]:
        override def initialValue(): Parser = new Parser

    def parse(source: Data, mode: NumberMode = NumberMode.Full): Raw raises ParseError =
      val parser = pool.get.nn
      parser.tracking = false
      parser.resetData(source)
      parser.holes = false
      parser.numberMode = mode
      parser.parse()

    def parse(source: Data, holes: Boolean, mode: NumberMode): Raw raises ParseError =
      val parser = pool.get.nn
      parser.tracking = false
      parser.resetData(source)
      parser.holes = holes
      parser.numberMode = mode
      parser.parse()

    def parse(input: Iterator[Data], mode: NumberMode): Raw raises ParseError =
      val parser = pool.get.nn
      parser.tracking = false
      parser.resetIterator(input)
      parser.holes = false
      parser.numberMode = mode
      parser.parse()

    def parse(input: Iterator[Data], holes: Boolean, mode: NumberMode): Raw raises ParseError =
      val parser = pool.get.nn
      parser.tracking = false
      parser.resetIterator(input)
      parser.holes = holes
      parser.numberMode = mode
      parser.parse()

    def parseTracked(source: Data, mode: NumberMode = NumberMode.Full)
    :   (Raw, IArray[Int]) raises ParseError =

      val parser = pool.get.nn
      parser.tracking = true
      parser.resetData(source)
      parser.holes = false
      parser.numberMode = mode
      val raw = parser.parse()
      (raw, parser.rootIndex.nn)

    def parseTracked(input: Iterator[Data], mode: NumberMode)
    :   (Raw, IArray[Int]) raises ParseError =

      val parser = pool.get.nn
      parser.tracking = true
      parser.resetIterator(input)
      parser.holes = false
      parser.numberMode = mode
      val raw = parser.parse()
      (raw, parser.rootIndex.nn)

    def parse(input: Stream[Data] over Credit, mode: NumberMode): Raw raises ParseError =
      val parser = pool.get.nn
      parser.tracking = false
      parser.resetStream(input)
      parser.holes = false
      parser.numberMode = mode
      parser.parse()

    def parseTracked(input: Stream[Data] over Credit, mode: NumberMode)
    :   (Raw, IArray[Int]) raises ParseError =

      val parser = pool.get.nn
      parser.tracking = true
      parser.resetStream(input)
      parser.holes = false
      parser.numberMode = mode
      val raw = parser.parse()
      (raw, parser.rootIndex.nn)

  private[jacinta] final class Parser:
    import scala.annotation.switch
    import scala.collection.mutable.ArrayBuffer
    import Json.Ast.AsciiByte.*
    import Json.Ast.{Issue, Position}

    import Parser.*

    // The cursor remains the source of truth at refill, mark, slice and error
    // points, but for the per-byte hot loops (`peek`, `advance`, `more`) the
    // parser maintains its own snapshot of the current buffer reference and
    // read position. Keeping `pos` and `bytes` as parser fields (rather than
    // accessing them through the cursor on every byte) gives the JIT the
    // freedom to keep both in registers across long inner loops, recovering
    // most of the per-byte cost of the Direct/Streaming split removed during
    // the substrate unification.
    //
    // Invariant: between `syncTo()` and `syncFrom()` calls, `pos` is the
    // authoritative read position; `cursor.unsafePos` is allowed to lag.
    // Whenever a cursor operation that depends on `pos` is performed (refill
    // via `more`'s slow path, mark, slice, error reporting, BOM probing) the
    // parser pushes `pos` to the cursor first, then refreshes its snapshot
    // from the cursor afterwards — refill may compact the buffer, reallocate
    // it, or reset `pos`.
    private var cursor:    Cursor[Data, ?]      = null.asInstanceOf[Cursor[Data, ?]]
    private var heldToken: Cursor.Held | Null = null

    // Parser-local snapshot (see comment above).
    private var bytes:  Array[Byte] = null.asInstanceOf[Array[Byte]]
    private var pos:    Int = 0
    private var bufEnd: Int = 0

    protected[jacinta] var holes: Boolean = false

    // When true, the cursor is built with a line-feed-tracking `Lineation` so
    // `cursor.line` / `cursor.column` reflect real source coordinates, and the
    // parser emits a parallel `PositionIndex`.
    protected[jacinta] var tracking: Boolean = false

    // Storage shape `parseNumber` uses for each parsed JSON number. See
    // `NumberMode` for semantics. Reset before each `parse()` call.
    protected[jacinta] var numberMode: NumberMode = NumberMode.Full

    protected var arraySize:           Int = 16
    protected var chars:               Array[Char] = new Array(arraySize)
    protected var stringCursor:        Int = 0
    protected var arrayBufferId:       Int = -1
    protected val arrayBuffers:        ArrayBuffer[ArrayBuffer[Any]]  = ArrayBuffer.empty
    protected var bcdLongBufferId:     Int = -1
    protected val bcdLongBuffers:      ArrayBuffer[ArrayBuffer[Long]] = ArrayBuffer.empty
    protected var bcdIntBufferId:      Int = -1
    protected val bcdIntBuffers:       ArrayBuffer[ArrayBuffer[Int]]  = ArrayBuffer.empty
    protected var indexBufferId:       Int = -1
    protected val indexBuffers:        ArrayBuffer[ArrayBuffer[Int]]  = ArrayBuffer.empty

    // Finalised root-level position index produced by the previous `parse()`
    // call when `tracking` was on. Reset to `null` at the start of every parse.
    protected[jacinta] var rootIndex: IArray[Int] | Null = null

    // Local-buffer offset up to which `cursor.lineNo` / `cursor.columnNo`
    // have been brought up to date. The hot-loop `syncTo()` bypasses the
    // cursor's lineation tracking via `unsafeAdvanceBy`, so the parser
    // keeps track of how far ahead `cursor.pos` has been pushed and
    // catches lineation up here only at tracking-mode capture points and
    // before any refill discards consumed bytes.
    private var lineationPos: Int = 0

    // Small open-addressed cache of recently-seen object keys. Index is
    // `hash & (KeyCacheSize - 1)`; on a hash collision the existing entry is
    // overwritten. Persisted across parses (per-thread, since the parser
    // pool is `ThreadLocal`) so repeating JSON shapes hit the cache from
    // the first key onward. Only consulted on the fast ASCII path with no
    // escapes — escaped keys still go through `parseString`'s slow tail.
    // Small open-addressed object-key cache. Keys of up to 16 ASCII bytes are
    // packed losslessly into a pair of Longs (low = bytes 0–7, high = bytes
    // 8–15, both LSB-first, with trailing positions zero) — and since JSON
    // object keys can't contain `\0` (the parser fast-path rejects bytes
    // < 32), every distinct ASCII key produces a distinct (low, high) pair.
    // Lookup is then a pair of Long equality checks — *no* byte-by-byte
    // comparison, and *no* hash-collision false positives. Keys longer than
    // 16 bytes (rare for typical record-shape JSON) bypass the cache and
    // allocate normally.
    private inline val KeyCacheSize = 256
    private inline val KeyCacheMaxBytes = 16
    private val keyCache:    Array[String | Null] = new Array(KeyCacheSize)
    private val keyCacheLow:  Array[Long]         = new Array(KeyCacheSize)
    private val keyCacheHigh: Array[Long]         = new Array(KeyCacheSize)

    def resetData(input: Data): Unit =
      cursor = makeCursor(input)
      syncFrom()
      stringCursor = 0
      arrayBufferId = -1
      bcdLongBufferId = -1
      bcdIntBufferId = -1
      indexBufferId = -1
      rootIndex = null
      heldToken = null

    def resetIterator(input: Iterator[Data]): Unit =
      cursor = makeCursor(input)
      syncFrom()
      stringCursor = 0
      arrayBufferId = -1
      bcdLongBufferId = -1
      bcdIntBufferId = -1
      indexBufferId = -1
      rootIndex = null
      heldToken = null

    def resetStream(input: Stream[Data] over Credit): Unit =
      cursor = makeCursor(input)
      syncFrom()
      stringCursor = 0
      arrayBufferId = -1
      bcdLongBufferId = -1
      bcdIntBufferId = -1
      indexBufferId = -1
      rootIndex = null
      heldToken = null

    private def makeCursor(input: Data): Cursor[Data, ?] =
      if tracking then
        import zephyrine.lineation.linefeedByte
        Cursor[Data](input)
      else
        import Lineation.untrackedData
        Cursor[Data](input)

    private def makeCursor(input: Iterator[Data]): Cursor[Data, ?] =
      if tracking then
        import zephyrine.lineation.linefeedByte
        Cursor[Data](input)
      else
        import Lineation.untrackedData
        Cursor[Data](input)

    private def makeCursor(input: Stream[Data] over Credit): Cursor[Data, ?] =
      if tracking then
        import zephyrine.lineation.linefeedByte
        Cursor[Data](input)
      else
        import Lineation.untrackedData
        Cursor[Data](input)

    // ──────────────────────────────────────────────────────────────────────────
    // Substrate.

    // Push the parser's local `pos` back to the cursor. Required before any
    // cursor operation that consults `pos` (mark, slice, refill, position).
    // This must stay zero-branch on the hot path; tracking-mode callers
    // separately invoke `reconcileLineation()` when they need accurate
    // `cursor.line` / `cursor.column`.
    private inline def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    // Walk the buffer bytes between the last lineation-reconciled position
    // and `cursor.unsafePos`, bumping `cursor.lineNo` / `cursor.columnNo`
    // accordingly. Called at tracking-mode capture points (`parseValue`,
    // `parseObject`) and before any refill in `moreSlow()` so that
    // consumed bytes are accounted for before they're discarded.
    private def reconcileLineation(): Unit =
      val end = cursor.unsafePos(using Unsafe)

      if lineationPos < end then
        var i = lineationPos
        var newlines = 0
        var lastNewlineAt = -1

        while i < end do
          if bytes(i) == Newline then
            newlines += 1
            lastNewlineAt = i

          i += 1

        if newlines > 0 then
          cursor.unsafeBumpLine(newlines)(using Unsafe)
          cursor.unsafeSetColumn(end - lastNewlineAt - 1)(using Unsafe)
        else
          cursor.unsafeBumpColumn(end - lineationPos)(using Unsafe)

        lineationPos = end

    // Refresh the parser's snapshot from the cursor. Required after any
    // cursor operation that may have changed the buffer reference, the read
    // position, or the write end (refill, cue).
    //
    // `lineationPos` is re-anchored to the cursor's current position because
    // refill compacts the buffer (bytes 0..old-pos are discarded; subsequent
    // walks would read different data).
    private inline def syncFrom(): Unit =
      bytes  = cursor.buffer(using Unsafe)
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)
      lineationPos = pos

    protected inline def more: Boolean = pos < bufEnd || moreSlow()

    // Out-of-line slow path so the inline budget for `more` stays small enough
    // for the JIT to keep `pos < bufEnd` as a single register comparison in
    // hot loops.
    //
    // In tracking mode the parser later reads `cursor.position` to compute
    // descriptor lengths; if `cursor.refill()` compacts the buffer here we
    // must re-anchor the parser's local `pos` against the new buffer scale
    // even when there's no more data, otherwise subsequent `syncTo()` calls
    // would advance the cursor by a now-stale delta. Lineation also needs
    // to be reconciled before compaction discards the consumed bytes.
    private def moreSlow(): Boolean =
      syncTo()
      if tracking then reconcileLineation()

      if cursor.more then { syncFrom(); true }
      else
        if tracking then syncFrom()
        false

    protected inline def peek: Byte = bytes(pos)

    protected inline def advance(): Unit = pos += 1

    protected def errorAt(issue: Issue, start: Optional[Cursor.Mark] = Unset)
      ( using Tactic[ParseError] )
    :   Nothing =

      syncTo()
      val end = cursor.position.n0
      val offset: Optional[Int] = start.let(_.absolute.toInt)
      val length: Optional[Int] = start.let: mark => end - mark.absolute.toInt
      abort(ParseError(Json.Ast, Position(0, end, offset = offset, length = length), issue))

    // A `Region` is just a `Cursor.Mark` (an absolute `Long` position). With
    // the single-buffer model there's no need to remember the starting block
    // for boundary detection.
    type Region = Cursor.Mark

    protected inline def begin(): Cursor.Mark =
      syncTo()
      cursor.mark(using heldToken.nn)

    protected inline def slice(start: Cursor.Mark): String =
      syncTo()
      val end = cursor.mark(using heldToken.nn)

      cursor.slice(start, end): (storage, off, len) =>
        val arr = storage.asInstanceOf[Array[Byte]]
        new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)

    protected inline def appendRegionToBuffer(start: Cursor.Mark): Unit =
      syncTo()
      val end = cursor.mark(using heldToken.nn)

      cursor.slice(start, end): (storage, off, len) =>
        if len > 0 then
          val arr = storage.asInstanceOf[Array[Byte]]
          ensureStringSpace(len)
          var i = 0

          while i < len do
            chars(stringCursor + i) = (arr(off + i) & 0xFF).toChar
            i += 1

          stringCursor += len

    // BOM probing runs once per parse, and almost no JSON inputs actually
    // start with one. The fast path peeks the first byte directly from the
    // parser-local snapshot — if it isn't `0xEF` (the BOM lead byte), no
    // valid BOM is possible and we return immediately, skipping the
    // `cursor.hold` token allocation, the mark/cue, and the
    // `syncTo`/`syncFrom` round-trip. The slow path retains full BOM
    // semantics (including rolling back if only the first byte matches).
    protected def bom(): Unit =
      if !more || peek != -17.toByte then return
      syncTo()

      cursor.hold:
        val mk = cursor.mark

        val bom =
          cursor.more && cursor.datum(using Unsafe) == -17.toByte &&
            { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte } &&
            { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

        if bom then cursor.next() else cursor.cue(mk)

      syncFrom()

    protected inline def holding[result](inline action: => result): result =
      syncTo()

      cursor.hold:
        heldToken = summon[Cursor.Held]
        try action finally heldToken = null

    // ──────────────────────────────────────────────────────────────────────────
    // String buffer plumbing (unchanged).

    protected inline def resetString(): Unit = stringCursor = 0

    protected inline def ensureStringSpace(n: Int): Unit =
      while stringCursor + n > arraySize do arraySize *= 2

      if chars.length < arraySize then
        val newArr = new Array[Char](arraySize)
        System.arraycopy(chars, 0, newArr, 0, stringCursor)
        chars = newArr

    protected inline def appendChar(char: Char): Unit =
      if stringCursor == arraySize then
        arraySize *= 2
        val newArray = new Array[Char](arraySize)
        System.arraycopy(chars, 0, newArray, 0, stringCursor)
        chars = newArray

      chars(stringCursor) = char
      stringCursor += 1

    protected inline def getString(): String = String(chars, 0, stringCursor)

    protected inline def getArrayBuffer(): ArrayBuffer[Any] =
      arrayBufferId += 1

      if arrayBuffers.length <= arrayBufferId then
        val newBuffer = ArrayBuffer.empty[Any]
        arrayBuffers += newBuffer
        newBuffer
      else
        val buffer = arrayBuffers(arrayBufferId)
        buffer.clear()
        buffer

    protected inline def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

    protected inline def getBcdLongBuffer(): ArrayBuffer[Long] =
      bcdLongBufferId += 1

      if bcdLongBuffers.length <= bcdLongBufferId then
        val newBuffer = ArrayBuffer.empty[Long]
        bcdLongBuffers += newBuffer
        newBuffer
      else
        val buffer = bcdLongBuffers(bcdLongBufferId)
        buffer.clear()
        buffer

    protected inline def relinquishBcdLongBuffer(): Unit = bcdLongBufferId -= 1

    protected inline def getBcdIntBuffer(): ArrayBuffer[Int] =
      bcdIntBufferId += 1

      if bcdIntBuffers.length <= bcdIntBufferId then
        val newBuffer = ArrayBuffer.empty[Int]
        bcdIntBuffers += newBuffer
        newBuffer
      else
        val buffer = bcdIntBuffers(bcdIntBufferId)
        buffer.clear()
        buffer

    protected inline def relinquishBcdIntBuffer(): Unit = bcdIntBufferId -= 1

    protected inline def getIndexBuffer(): ArrayBuffer[Int] =
      indexBufferId += 1

      if indexBuffers.length <= indexBufferId then
        val newBuffer = ArrayBuffer.empty[Int]
        indexBuffers += newBuffer
        newBuffer
      else
        val buffer = indexBuffers(indexBufferId)
        buffer.clear()
        buffer

    protected inline def relinquishIndexBuffer(): Unit = indexBufferId -= 1

    // Assemble a composite (array or object) descriptor in `indexOut`. `scratch`
    // holds the concatenated entry descriptors back-to-back (an entry is a
    // child for arrays, or `[keyLine, keyColumn, keyLength, value descriptor]`
    // for objects). `ends(i)` is the position in `scratch` immediately after
    // the i-th entry, so the i-th entry's size is `ends(i) - ends(i-1)`.
    protected def emitCompositeDescriptor
      ( indexOut:    ArrayBuffer[Int],
        scratch:     ArrayBuffer[Int],
        ends:        ArrayBuffer[Int],
        startLine:   Int,
        startColumn: Int,
        startMark:   Long )
    :   Unit =

      syncTo()
      val n = ends.length
      val sourceLength = (cursor.position.n0 - startMark).toInt
      val sizeSlot = indexOut.length

      indexOut += 0
      indexOut += startLine
      indexOut += startColumn
      indexOut += sourceLength
      indexOut += n

      val headerSize = 5 + n
      var i = 0

      while i < n do
        val childStart = if i == 0 then 0 else ends(i - 1)
        indexOut += headerSize + childStart
        i += 1

      indexOut ++= scratch
      indexOut(sizeSlot) = indexOut.length - sizeSlot

    // ──────────────────────────────────────────────────────────────────────────
    // Parser body (unchanged from the previous abstract base).

    protected inline def must()(using Tactic[ParseError]): Byte =
      if more then peek else errorAt(Issue.PrematureEnd)

    protected inline def next()(using Tactic[ParseError]): Byte =
      advance()
      if more then peek else errorAt(Issue.PrematureEnd)

    private def skip(): Unit =
      while
        more && {
          val ch = peek
          ch == Space || ch == Tab || ch == Newline || ch == Return
        }
      do advance()

    private def fromHex(ch: Byte)(using Tactic[ParseError]): Int =
      if ch <= Num9 && ch >= Num0 then ch - Num0
      else if ch <= UpperF && ch >= UpperA then ch - UpperA + 10
      else if ch <= LowerF && ch >= LowerA then ch - LowerA + 10
      else errorAt(Issue.ExpectedHexDigit(ch.toChar))

    private def parseUnicode()(using Tactic[ParseError]): Char =
      var acc = fromHex(next()) << 12
      acc |= fromHex(next()) << 8
      acc |= fromHex(next()) << 4
      acc |= fromHex(next())
      acc.toChar

    private def parseString()(using Tactic[ParseError]): String = holding:
      val region = begin()

      // Fast scan for plain printable ASCII that needs no escape handling.
      // The 256-entry `StringScanContinue` table collapses the three
      // comparisons (`>= 32`, `!= "`, `!= \`) into a single load + compare.
      while more && StringScanContinue(peek & 0xFF) != 0 do advance()

      if !more then errorAt(Issue.PrematureEnd, region)

      if peek == Quote then slice(region).also(advance())
      else tail(region)

    // Like `parseString`, but routes the no-escape result through `keyCache`.
    // The key's bytes are packed losslessly into a `(packedLow, packedHigh)`
    // Long pair *after* the scan, inside the `cursor.slice` callback where
    // the byte array is directly addressable — this keeps the per-byte
    // inner loop identical to `parseString` (no hash/packing overhead per
    // byte) and confines the cache work to a single per-key step.
    //
    // Every distinct ≤16-byte ASCII key produces a distinct (low, high)
    // pair (JSON keys can't contain `\0`, so trailing-zero padding can't
    // alias another key), so the lookup needs only two Long equality
    // checks — no byte-by-byte comparison and no hash-collision false
    // positives. Keys longer than 16 bytes bypass the cache.
    private def parseObjectKey()(using Tactic[ParseError]): String = holding:
      val region = begin()

      while more && StringScanContinue(peek & 0xFF) != 0 do advance()

      if !more then errorAt(Issue.PrematureEnd, region)

      if peek != Quote then tail(region)
      else
        syncTo()
        val end = cursor.mark(using heldToken.nn)

        val out = cursor.slice(region, end): (storage, off, len) =>
          val arr = storage.asInstanceOf[Array[Byte]]

          if len > KeyCacheMaxBytes then
            new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)
          else
            val packedLow  = packBytes(arr, off, math.min(len, 8))
            val packedHigh = if len > 8 then packBytes(arr, off + 8, len - 8) else 0L

            val idx        = ((packedLow.toInt ^ (packedLow >>> 32).toInt) ^
              (packedHigh.toInt ^ (packedHigh >>> 32).toInt)) & (KeyCacheSize - 1)

            val cached     = keyCache(idx)

            if cached != null && keyCacheLow(idx) == packedLow &&
              keyCacheHigh(idx) == packedHigh
            then cached
            else
              val fresh = new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)
              keyCache(idx)     = fresh
              keyCacheLow(idx)  = packedLow
              keyCacheHigh(idx) = packedHigh
              fresh

        advance()
        out

    // Pack up to 8 bytes from `arr[off..off+n)` into a Long, LSB-first
    // (byte at offset `i` goes to bit position `i*8`). Bytes beyond `n` are
    // zero — combined with the fact that JSON keys can't contain `\0`,
    // this means two distinct byte sequences of length ≤ 8 always pack to
    // distinct Longs.
    private inline def packBytes(arr: Array[Byte], off: Int, n: Int): Long =
      var out: Long = 0L
      var i = 0

      while i < n do
        out = out | ((arr(off + i) & 0xFFL) << (i << 3))
        i += 1

      out

    private def tail(start: Region): String raises ParseError =
      resetString()
      appendRegionToBuffer(start)

      var continue = true

      while continue do
        if !more then errorAt(Issue.PrematureEnd, start)
        val ch = peek

        ch match
          case Quote =>
            continue = false

          case Tab | Newline | Return =>
            errorAt(Issue.InvalidWhitespace, start)

          case Backslash =>
            advance()
            if !more then errorAt(Issue.PrematureEnd, start)

            (peek: @switch) match
              case Quote     => appendChar('"')
              case Slash     => appendChar('/')
              case Backslash => appendChar('\\')
              case LowerB    => appendChar('\b')
              case LowerF    => appendChar('\f')
              case LowerN    => appendChar('\n')
              case LowerR    => appendChar('\r')
              case LowerT    => appendChar('\t')
              case LowerU    => appendChar(parseUnicode())
              case bad       => errorAt(Issue.IncorrectEscape(bad.toChar), start)

          case _ =>
            if ch == 0 && holes then appendChar(' ')
            else ((ch >> 5): @switch) match
              case 0                 => errorAt(Issue.NotEscaped(ch.toChar), start)
              case 1 | 2 | 3 | 4 | 5 => appendChar(ch.toChar)

              case _ =>
                if (ch & 0xE0) == 0xC0 then
                  var char: Int = (ch & 0x1F) << 6
                  char |= next() & 0x3F
                  appendChar(char.toChar)
                else if (ch & 0xF0) == 0xE0 then
                  var char: Int = (ch & 0x0F) << 12
                  char |= (next() & 0x3F) << 6
                  char |= next() & 0x3F
                  appendChar(char.toChar)
                else if (ch & 0xF8) == 0xF0 then
                  var char: Int = (ch & 0x07) << 18
                  char |= (next() & 0x3F) << 12
                  char |= (next() & 0x3F) << 6
                  char |= next() & 0x3F
                  appendChar(char.toChar)

        if continue then advance()

      advance()
      getString()

    protected inline def expect(byte: Byte, issue: Issue): Unit raises ParseError =
      if next() != byte then errorAt(issue)

    private def parseFalse(): false raises ParseError =
      // Fast path: 5 bytes available in the local buffer. Pack them into a
      // Long and compare against `FalseWord` in one step. Falls back to the
      // byte-by-byte `expect` chain only at a buffer boundary.
      if pos + 5 <= bufEnd then
        val word: Long =
          (bytes(pos)     & 0xFFL)         |
            ((bytes(pos + 1) & 0xFFL) <<  8) |
            ((bytes(pos + 2) & 0xFFL) << 16) |
            ((bytes(pos + 3) & 0xFFL) << 24) |
            ((bytes(pos + 4) & 0xFFL) << 32)

        if word != FalseWord then errorAt(Issue.ExpectedFalse)
        pos += 5
        false
      else
        expect(LowerA, Issue.ExpectedFalse)
        expect(LowerL, Issue.ExpectedFalse)
        expect(LowerS, Issue.ExpectedFalse)
        expect(LowerE, Issue.ExpectedFalse)
        advance()
        false

    private def parseTrue(): true raises ParseError =
      if pos + 4 <= bufEnd then
        val word: Int =
          (bytes(pos)     & 0xFF)         |
            ((bytes(pos + 1) & 0xFF) <<  8) |
            ((bytes(pos + 2) & 0xFF) << 16) |
            ((bytes(pos + 3) & 0xFF) << 24)

        if word != TrueWord then errorAt(Issue.ExpectedTrue)
        pos += 4
        true
      else
        expect(LowerR, Issue.ExpectedTrue)
        expect(LowerU, Issue.ExpectedTrue)
        expect(LowerE, Issue.ExpectedTrue)
        advance()
        true

    private def parseNull(): Json.JsonNull.type raises ParseError =
      if pos + 4 <= bufEnd then
        val word: Int =
          (bytes(pos)     & 0xFF)         |
            ((bytes(pos + 1) & 0xFF) <<  8) |
            ((bytes(pos + 2) & 0xFF) << 16) |
            ((bytes(pos + 3) & 0xFF) << 24)

        if word != NullWord then errorAt(Issue.ExpectedNull)
        pos += 4
        Json.JsonNull
      else
        expect(LowerU, Issue.ExpectedNull)
        expect(LowerL, Issue.ExpectedNull)
        expect(LowerL, Issue.ExpectedNull)
        advance()
        Json.JsonNull

    private def parseNumber(first: Int, negative: Boolean, bcdOnly: Boolean = false)
    :   Double | Long | Bcd raises ParseError =

      var content: Long = first.toLong
      var nibbles: Int = 1
      var bcdValid: Boolean = true
      var floating: Boolean = false
      var continue: Boolean = true
      var state: Int = if first == 0 then NumZero else NumInt
      var bcdBuilder: Bcd.Builder | Null = null

      // When the in-Long fast path overflows (the 16th nibble is about to be
      // appended), behavior depends on `numberMode`:
      //   - Full   : seed a `Bcd.Builder` with the existing 15 nibbles plus the
      //              overflowing one and continue accumulation there. Preserves
      //              full precision; one heap allocation per overflowing number.
      //   - Bcd    : drop the overflowing nibble and continue scanning the
      //              number's remaining digits without recording them. The
      //              accumulator stays the truncated first 15 nibbles. No
      //              heap allocation.
      //   - Double : same drop-on-overflow behavior as `Bcd`; the construction
      //              site then derives a `Double` from the truncated nibbles.
      inline def fallback(extraNibble: Int): Unit =
        if bcdOnly || numberMode == NumberMode.Full then
          val b = new Bcd.Builder
          b.seedFromLong(content, nibbles)
          b.add(extraNibble)
          bcdBuilder = b
          bcdValid = false

      inline def appendNibble(n: Int): Unit =
        if bcdValid then
          if nibbles >= 15 then fallback(n)
          else
            content = (content << 4) | n.toLong
            nibbles += 1
        else
          bcdBuilder.nn.add(n)

      inline def rewriteEAsNeg(): Unit =
        if bcdValid then content = (content & ~0xFL) | 0xCL
        else bcdBuilder.nn.overwriteLast(0xC)

      while continue && more do
        val ch = peek

        (state: @switch) match
          case NumZero =>
            ch match
              case Period =>
                // Drop the leading "0" — it's redundant for "0.xxx" and the
                // BCD printer reinserts it on emit. Saves one nibble per such
                // input, which can shift a value from `Array[Long]` to the
                // tighter `Array[Int]` shape when it lands right at the
                // 7-nibble boundary.
                nibbles = 0
                appendNibble(0xA); floating = true; state = NumAfterDot; advance()

              case UpperE | LowerE =>
                appendNibble(0xB); floating = true; state = NumAfterE; advance()

              case _ =>
                continue = false

          case NumInt =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); advance()

              case Period =>
                appendNibble(0xA); floating = true; state = NumAfterDot; advance()

              case UpperE | LowerE =>
                appendNibble(0xB); floating = true; state = NumAfterE; advance()

              case _ =>
                continue = false

          case NumAfterDot =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); state = NumFrac; advance()

              case _ =>
                errorAt(Issue.ExpectedDigit(ch.toChar))

          case NumFrac =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); advance()

              case UpperE | LowerE =>
                appendNibble(0xB); state = NumAfterE; advance()

              case _ =>
                continue = false

          case NumAfterE =>
            ch match
              case Plus =>
                advance(); state = NumAfterESign

              case Minus =>
                rewriteEAsNeg(); advance(); state = NumAfterESign

              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); state = NumExp; advance()

              case _ =>
                errorAt(Issue.ExpectedDigit(ch.toChar))

          case NumAfterESign =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); state = NumExp; advance()

              case _ =>
                errorAt(Issue.ExpectedDigit(ch.toChar))

          case NumExp =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); advance()

              case _ =>
                continue = false

          case _ => ()

      (state: @switch) match
        case NumAfterDot | NumAfterE | NumAfterESign =>
          errorAt(Issue.PrematureEnd)

        case _ => ()

      if bcdOnly then
        // Array-element fast path: skip the decode loop and the `Double`
        // materialisation. Return a single-Long BCD value if it fits the
        // 14-nibble cap, allocate a 2-word `Bcd` directly for the 15-nibble
        // boundary case, and use the Builder only when the in-Long path
        // already overflowed.
        if bcdValid then
          if nibbles <= Bcd.MaxBcdLongNibbles then
            Bcd.packBcdLong(content, nibbles, negative)
          else
            Bcd.fromContent15(content, negative)
        else
          bcdBuilder.nn.finish(negative): Bcd
      else if bcdValid then
        if numberMode == NumberMode.Bcd then
          // BCD mode: skip the decode loop and hand back the raw in-Long BCD
          // accumulator. Sign and numeric value aren't recoverable from this
          // Long — consumers under this mode treat it as opaque.
          content: Long
        else
          var mantissa: Long = 0L
          var decimalDigits: Int = 0
          var explicitExp: Int = 0
          var expSign: Int = 1
          var inFraction: Boolean = false
          var inExponent: Boolean = false

          var i = nibbles - 1

          while i >= 0 do
            val n = ((content >>> (i * 4)) & 0xFL).toInt

            if n <= 9 then
              if inExponent then explicitExp = explicitExp*10 + n
              else
                mantissa = mantissa*10 + n
                if inFraction then decimalDigits += 1
            else if n == 0xA then
              inFraction = true
            else if n == 0xB then
              inExponent = true
            else if n == 0xC then
              inExponent = true
              expSign = -1

            i -= 1

          if !floating then
            val signed: Long = if negative then -mantissa else mantissa
            if numberMode == NumberMode.Double then signed.toDouble else signed
          else
            val totalExp = expSign * explicitExp - decimalDigits

            val mag =
              if mantissa == 0L then 0.0
              else if mantissa < (1L << 53) && totalExp >= 0 && totalExp <= 22 then
                mantissa.toDouble * TenPow(totalExp)
              else if mantissa < (1L << 53) && totalExp < 0 && totalExp >= -22 then
                mantissa.toDouble / TenPow(-totalExp)
              else
                java.math.BigDecimal.valueOf(mantissa).nn.scaleByPowerOfTen(totalExp).nn.doubleValue

            if negative then -mag else mag
      else
        // High-precision path: hand back the `Bcd` directly. Reached only in
        // `NumberMode.Full`; `Bcd` and `Double` modes leave `bcdValid` true
        // and truncate on overflow rather than allocating a `Bcd.Builder`.
        bcdBuilder.nn.finish(negative): Bcd

    // `bcdOnly` propagates through the `Minus` recursion so that `-3.14`
    // parsed from an array context still short-circuits the Double path.
    // The flag has no effect on non-number values, so we don't need to
    // propagate it to `parseObject` / `parseArray` / `parseString`.
    private def parseValue(minus: Boolean = false, bcdOnly: Boolean = false)
      ( using Tactic[ParseError] )
    :   Raw =

      if !more then errorAt(Issue.PrematureEnd)
      val ch = peek

      if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
        advance()
        parseNumber(ch & 0x0F, minus, bcdOnly)
      else if minus then
        errorAt(Issue.ExpectedDigit(ch.toChar))
      else if holes && ch == 0 then
        advance()
        Unset
      else
        (ch: @switch) match
          case Quote       => advance() yet parseString()
          case Minus       => advance() yet parseValue(true, bcdOnly)
          case OpenBracket => advance() yet parseArray()
          case LowerF      => parseFalse()
          case LowerN      => parseNull()
          case LowerT      => parseTrue()
          case OpenBrace   => advance() yet parseObject()
          case other       => errorAt(Issue.ExpectedSomeValue(other.toChar))

    // Tracked-mode `parseValue`. Captures source position before dispatch,
    // delegates composite cases to the `*Tracked` variants (so they emit
    // their own descriptors into `indexOut`), and writes a 4-int primitive
    // descriptor afterwards for everything else. The `Minus` recursion
    // routes to the untracked variant since the outer call already owns
    // the descriptor for the full `-N` span.
    private def parseValueTracked
      ( indexOut: ArrayBuffer[Int],
        minus:    Boolean = false,
        bcdOnly:  Boolean = false )
      ( using Tactic[ParseError] )
    :   Raw =

      if !more then errorAt(Issue.PrematureEnd)

      syncTo()
      reconcileLineation()
      val startLine   = cursor.line.n1
      val startColumn = cursor.column.n1
      val startMark   = cursor.position.n0.toLong

      val ch = peek

      val result: Raw =
        if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
          advance()
          parseNumber(ch & 0x0F, minus, bcdOnly)
        else if minus then
          errorAt(Issue.ExpectedDigit(ch.toChar))
        else if holes && ch == 0 then
          advance()
          Unset
        else
          (ch: @switch) match
            case Quote       => advance() yet parseString()
            case Minus       => advance() yet parseValue(true, bcdOnly)

            case OpenBracket =>
              advance()
              parseArrayTracked(indexOut, startLine, startColumn, startMark)

            case LowerF      => parseFalse()
            case LowerN      => parseNull()
            case LowerT      => parseTrue()

            case OpenBrace =>
              advance()
              parseObjectTracked(indexOut, startLine, startColumn, startMark)

            case other       => errorAt(Issue.ExpectedSomeValue(other.toChar))

      if ch != OpenBracket && ch != OpenBrace then
        syncTo()
        val length = (cursor.position.n0 - startMark).toInt
        indexOut += 4
        indexOut += startLine
        indexOut += startColumn
        indexOut += length

      result

    private def parseArray()(using Tactic[ParseError]): Raw =
      // The array starts in "undecided" mode. Each element is parsed with
      // `bcdOnly = true`, so a number comes back as either a single-Long
      // BCD packing (count + sign + nibbles encoded in the Long itself) or
      // a heap-allocated `Bcd` if it overflows the 14-nibble cap. The mode
      // is decided from the first element and tightens / widens as we go:
      //
      //   `Array[Int]`  if every element fits 7 nibbles  (4 bytes / element)
      //   `Array[Long]` if every element fits 14 nibbles (8 bytes / element)
      //   `IArray[Any]` otherwise                        (boxed)
      //
      // Subsequent elements either fit the current container (cheap append)
      // or trigger a one-step migration to the next-wider form, with the
      // existing elements re-packed in place. We never widen past the
      // boxed form; once boxed we stay boxed.
      var mode: Int = ModeUndecided
      var intItems:  ArrayBuffer[Int]  | Null = null
      var longItems: ArrayBuffer[Long] | Null = null
      var anyItems:  ArrayBuffer[Any]  | Null = null
      var first    = true
      var continue = true

      inline def migrateIntToLong(): Unit =
        val src = intItems.nn
        val dst = getBcdLongBuffer()
        val n = src.length
        var i = 0

        while i < n do
          dst += Bcd.repackBcdIntAsLong(src(i))
          i += 1

        relinquishBcdIntBuffer()
        intItems = null
        longItems = dst
        mode = ModeBcdLong

      inline def migrateIntToBoxed(): Unit =
        val src = intItems.nn
        val dst = getArrayBuffer()
        val n = src.length
        var i = 0

        // Each Int *is* a small-BCD `JsonNumber` variant — append directly.
        while i < n do
          dst += src(i)
          i += 1

        relinquishBcdIntBuffer()
        intItems = null
        anyItems = dst
        mode = ModeBoxed

      inline def migrateLongToBoxed(): Unit =
        val src = longItems.nn
        val dst = getArrayBuffer()
        val n = src.length
        var i = 0

        // Decode each BCD-Long to its scalar JsonNumber form. Long if the
        // value is an exact integer that fits Long, else Double.
        while i < n do
          val v = src(i)
          val text = Bcd.bcdLongText(v)

          val ast: Any =
            try java.lang.Long.parseLong(text)
            catch case _: NumberFormatException => java.lang.Double.parseDouble(text)

          dst += ast
          i += 1

        relinquishBcdLongBuffer()
        longItems = null
        anyItems = dst
        mode = ModeBoxed

      while continue do
        skip()

        must() match
          case CloseBracket =>
            if !first then errorAt(Issue.ExpectedSomeValue(']'))
            continue = false

          case _ =>
            val value = parseValue(bcdOnly = true)
            skip()

            val terminator: Byte = must()

            terminator match
              case Comma | CloseBracket => ()
              case char                 => errorAt(Issue.ExpectedSomeValue(char.toChar))

            // Classify the parsed value:
            //   bcdLong:    a Long return — parseNumber's bcdOnly fast path
            //               packed sign+count+nibbles into a single Long.
            //   nonNumber:  anything else (Bcd, String, Boolean, Null,
            //               IArray[Any], etc.) — goes to boxed mode.
            value match
              case bcdLong: Long =>
                val nibbles = ((bcdLong >>> 56) & 0x7FL).toInt

                if first then
                  first = false

                  if nibbles <= Bcd.MaxBcdIntNibbles then
                    mode = ModeBcdInt
                    val buf = getBcdIntBuffer()
                    intItems = buf
                    buf += Bcd.packBcdIntFromLong(bcdLong)
                  else
                    mode = ModeBcdLong
                    val buf = getBcdLongBuffer()
                    longItems = buf
                    buf += bcdLong
                else
                  (mode: @switch) match
                    case ModeBcdInt =>
                      if nibbles <= Bcd.MaxBcdIntNibbles then
                        intItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                      else
                        migrateIntToLong()
                        longItems.nn += bcdLong

                    case ModeBcdLong =>
                      longItems.nn += bcdLong

                    case _ =>
                      // Boxed mode — surface the value as a JsonNumber.
                      // For ≤7 nibbles, the small-BCD `Int` is the right
                      // shape; otherwise the BCD-Long itself is fine.
                      if nibbles <= Bcd.MaxBcdIntNibbles then
                        anyItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                      else
                        anyItems.nn += bcdLong

              case _ =>
                if first then
                  first = false
                  mode = ModeBoxed
                  val buf = getArrayBuffer()
                  anyItems = buf
                  buf += value
                else
                  (mode: @switch) match
                    case ModeBcdInt =>
                      migrateIntToBoxed()
                      anyItems.nn += value

                    case ModeBcdLong =>
                      migrateLongToBoxed()
                      anyItems.nn += value

                    case _ =>
                      anyItems.nn += value

            if terminator == CloseBracket then continue = false

        advance()

      if first then
        // Empty array — no buffer was ever allocated. The empty case has
        // even (zero) length so the sentinel pad is required to keep arrays
        // distinguishable from objects.
        val out = new Array[Any](1)
        out(0) = Json.Ast.arrayPad
        out.asInstanceOf[IArray[Any]]
      else
        (mode: @switch) match
          case ModeBcdInt =>
            val src = intItems.nn
            val out = new Array[Int](src.length)
            src.copyToArray(out)
            relinquishBcdIntBuffer()
            out

          case ModeBcdLong =>
            val src = longItems.nn
            val out = new Array[Long](src.length)
            src.copyToArray(out)
            relinquishBcdLongBuffer()
            out

          case _ =>
            // Mixed/boxed array — stored as `IArray[Any]` and parity-padded
            // when the element count is even, so arrays always have odd
            // length and can be distinguished from objects (always even).
            val src = anyItems.nn
            val n = src.length

            val out =
              if (n & 1) == 1 then
                val arr = new Array[Any](n)
                src.copyToArray(arr)
                arr
              else
                val arr = new Array[Any](n + 1)
                src.copyToArray(arr)
                arr(n) = Json.Ast.arrayPad
                arr

            relinquishArrayBuffer()
            out.asInstanceOf[IArray[Any]]

    // Tracked-mode `parseArray`. Mirrors `parseArray` exactly but uses
    // `parseValueTracked` for children and emits a composite descriptor
    // into `indexOut` at close.
    private def parseArrayTracked
      ( indexOut:    ArrayBuffer[Int],
        startLine:   Int,
        startColumn: Int,
        startMark:   Long )
      ( using Tactic[ParseError] )
    :   Raw =

      var mode: Int = ModeUndecided
      var intItems:  ArrayBuffer[Int]  | Null = null
      var longItems: ArrayBuffer[Long] | Null = null
      var anyItems:  ArrayBuffer[Any]  | Null = null
      var first    = true
      var continue = true

      val indexScratch = getIndexBuffer()
      val indexEnds    = getIndexBuffer()

      inline def migrateIntToLong(): Unit =
        val src = intItems.nn
        val dst = getBcdLongBuffer()
        val n = src.length
        var i = 0

        while i < n do
          dst += Bcd.repackBcdIntAsLong(src(i))
          i += 1

        relinquishBcdIntBuffer()
        intItems = null
        longItems = dst
        mode = ModeBcdLong

      inline def migrateIntToBoxed(): Unit =
        val src = intItems.nn
        val dst = getArrayBuffer()
        val n = src.length
        var i = 0

        while i < n do
          dst += src(i)
          i += 1

        relinquishBcdIntBuffer()
        intItems = null
        anyItems = dst
        mode = ModeBoxed

      inline def migrateLongToBoxed(): Unit =
        val src = longItems.nn
        val dst = getArrayBuffer()
        val n = src.length
        var i = 0

        while i < n do
          val v = src(i)
          val text = Bcd.bcdLongText(v)

          val ast: Any =
            try java.lang.Long.parseLong(text)
            catch case _: NumberFormatException => java.lang.Double.parseDouble(text)

          dst += ast
          i += 1

        relinquishBcdLongBuffer()
        longItems = null
        anyItems = dst
        mode = ModeBoxed

      while continue do
        skip()

        must() match
          case CloseBracket =>
            if !first then errorAt(Issue.ExpectedSomeValue(']'))
            continue = false

          case _ =>
            val value = parseValueTracked(indexScratch, bcdOnly = true)
            indexEnds += indexScratch.length
            skip()

            val terminator: Byte = must()

            terminator match
              case Comma | CloseBracket => ()
              case char                 => errorAt(Issue.ExpectedSomeValue(char.toChar))

            value match
              case bcdLong: Long =>
                val nibbles = ((bcdLong >>> 56) & 0x7FL).toInt

                if first then
                  first = false

                  if nibbles <= Bcd.MaxBcdIntNibbles then
                    mode = ModeBcdInt
                    val buf = getBcdIntBuffer()
                    intItems = buf
                    buf += Bcd.packBcdIntFromLong(bcdLong)
                  else
                    mode = ModeBcdLong
                    val buf = getBcdLongBuffer()
                    longItems = buf
                    buf += bcdLong
                else
                  (mode: @switch) match
                    case ModeBcdInt =>
                      if nibbles <= Bcd.MaxBcdIntNibbles then
                        intItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                      else
                        migrateIntToLong()
                        longItems.nn += bcdLong

                    case ModeBcdLong =>
                      longItems.nn += bcdLong

                    case _ =>
                      if nibbles <= Bcd.MaxBcdIntNibbles then
                        anyItems.nn += Bcd.packBcdIntFromLong(bcdLong)
                      else
                        anyItems.nn += bcdLong

              case _ =>
                if first then
                  first = false
                  mode = ModeBoxed
                  val buf = getArrayBuffer()
                  anyItems = buf
                  buf += value
                else
                  (mode: @switch) match
                    case ModeBcdInt =>
                      migrateIntToBoxed()
                      anyItems.nn += value

                    case ModeBcdLong =>
                      migrateLongToBoxed()
                      anyItems.nn += value

                    case _ =>
                      anyItems.nn += value

            if terminator == CloseBracket then continue = false

        advance()

      val astValue: Raw =
        if first then
          val out = new Array[Any](1)
          out(0) = Json.Ast.arrayPad
          out.asInstanceOf[IArray[Any]]
        else
          (mode: @switch) match
            case ModeBcdInt =>
              val src = intItems.nn
              val out = new Array[Int](src.length)
              src.copyToArray(out)
              relinquishBcdIntBuffer()
              out

            case ModeBcdLong =>
              val src = longItems.nn
              val out = new Array[Long](src.length)
              src.copyToArray(out)
              relinquishBcdLongBuffer()
              out

            case _ =>
              val src = anyItems.nn
              val n = src.length

              val out =
                if (n & 1) == 1 then
                  val arr = new Array[Any](n)
                  src.copyToArray(arr)
                  arr
                else
                  val arr = new Array[Any](n + 1)
                  src.copyToArray(arr)
                  arr(n) = Json.Ast.arrayPad
                  arr

              relinquishArrayBuffer()
              out.asInstanceOf[IArray[Any]]

      emitCompositeDescriptor
        ( indexOut, indexScratch, indexEnds, startLine, startColumn, startMark )

      relinquishIndexBuffer()
      relinquishIndexBuffer()

      astValue

    // Parse an object directly into the flat alternating-key/value layout. The
    // buffer always grows in pairs, so its length stays even, which is the
    // object/array parity invariant.
    private def parseObject()(using Tactic[ParseError]): IArray[Any] =
      val items: ArrayBuffer[Any] = getArrayBuffer()
      var continue = true

      while continue do
        skip()

        must() match
          case Quote =>
            advance()
            val string = parseObjectKey()
            skip()

            must() match
              case Colon =>
                advance()
                skip()
                val value = parseValue()
                skip()

                must() match
                  case Comma =>
                    advance()
                    items += string
                    items += value
                    skip()

                  case CloseBrace =>
                    advance()
                    items += string
                    items += value
                    continue = false

                  case ch  => errorAt(Issue.UnexpectedChar(ch.toChar))

              case ch => errorAt(Issue.ExpectedColon(ch.toChar))

          case 0 if holes =>
            advance()
            skip()

            must() match
              case Colon =>
                advance()
                skip()
                val value = parseValue()
                skip()

                must() match
                  case Comma =>
                    advance()
                    items += " "
                    items += value
                    skip()

                  case CloseBrace =>
                    advance()
                    items += " "
                    items += value
                    continue = false

                  case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

              case Comma =>
                advance()
                items += " "
                items += Unset
                skip()

              case CloseBrace =>
                advance()
                items += " "
                items += Unset
                continue = false

              case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

          case CloseBrace =>
            if !items.nil then errorAt(Issue.ExpectedSomeValue('}'))
            advance()
            continue = false

          case ch =>
            errorAt(Issue.ExpectedString(ch.toChar))

      val out = new Array[Any](items.length)
      items.copyToArray(out)
      relinquishArrayBuffer()
      out.asInstanceOf[IArray[Any]]

    // Tracked-mode `parseObject`. Mirrors `parseObject` exactly but captures
    // key positions, runs values through `parseValueTracked`, and emits a
    // composite descriptor into `indexOut` at close.
    private def parseObjectTracked
      ( indexOut:    ArrayBuffer[Int],
        startLine:   Int,
        startColumn: Int,
        startMark:   Long )
      ( using Tactic[ParseError] )
    :   IArray[Any] =

      val items: ArrayBuffer[Any] = getArrayBuffer()
      var continue = true

      val indexScratch = getIndexBuffer()
      val indexEnds    = getIndexBuffer()

      var keyLine:   Int  = 0
      var keyColumn: Int  = 0
      var keyMark:   Long = 0L

      while continue do
        skip()
        syncTo()
        reconcileLineation()
        keyLine   = cursor.line.n1
        keyColumn = cursor.column.n1
        keyMark   = cursor.position.n0.toLong

        must() match
          case Quote =>
            advance()
            val string = parseObjectKey()
            syncTo()
            val keyLength = (cursor.position.n0 - keyMark).toInt
            skip()

            must() match
              case Colon =>
                advance()
                skip()
                indexScratch += keyLine
                indexScratch += keyColumn
                indexScratch += keyLength
                val value = parseValueTracked(indexScratch)
                indexEnds += indexScratch.length
                skip()

                must() match
                  case Comma =>
                    advance()
                    items += string
                    items += value
                    skip()

                  case CloseBrace =>
                    advance()
                    items += string
                    items += value
                    continue = false

                  case ch  => errorAt(Issue.UnexpectedChar(ch.toChar))

              case ch => errorAt(Issue.ExpectedColon(ch.toChar))

          case 0 if holes =>
            advance()
            val keyLength = 1
            skip()

            must() match
              case Colon =>
                advance()
                skip()
                indexScratch += keyLine
                indexScratch += keyColumn
                indexScratch += keyLength
                val value = parseValueTracked(indexScratch)
                indexEnds += indexScratch.length
                skip()

                must() match
                  case Comma =>
                    advance()
                    items += " "
                    items += value
                    skip()

                  case CloseBrace =>
                    advance()
                    items += " "
                    items += value
                    continue = false

                  case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

              case Comma =>
                syncTo()
                reconcileLineation()
                val unsetLine   = cursor.line.n1
                val unsetColumn = cursor.column.n1
                indexScratch += keyLine
                indexScratch += keyColumn
                indexScratch += keyLength
                indexScratch += 4
                indexScratch += unsetLine
                indexScratch += unsetColumn
                indexScratch += 0
                indexEnds += indexScratch.length
                advance()
                items += " "
                items += Unset
                skip()

              case CloseBrace =>
                syncTo()
                reconcileLineation()
                val unsetLine   = cursor.line.n1
                val unsetColumn = cursor.column.n1
                indexScratch += keyLine
                indexScratch += keyColumn
                indexScratch += keyLength
                indexScratch += 4
                indexScratch += unsetLine
                indexScratch += unsetColumn
                indexScratch += 0
                indexEnds += indexScratch.length
                advance()
                items += " "
                items += Unset
                continue = false

              case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

          case CloseBrace =>
            if !items.nil then errorAt(Issue.ExpectedSomeValue('}'))
            advance()
            continue = false

          case ch =>
            errorAt(Issue.ExpectedString(ch.toChar))

      val out = new Array[Any](items.length)
      items.copyToArray(out)
      relinquishArrayBuffer()

      emitCompositeDescriptor
        ( indexOut, indexScratch, indexEnds, startLine, startColumn, startMark )

      relinquishIndexBuffer()
      relinquishIndexBuffer()

      out.asInstanceOf[IArray[Any]]

    def parse()(using Tactic[ParseError]): Raw =
      bom()
      skip()
      if !more then abort(ParseError(Json.Ast, Position(0, 0), Issue.EmptyInput))

      val result =
        if tracking then
          val rootBuf = getIndexBuffer()
          val r = parseValueTracked(rootBuf)
          rootIndex = IArray.from(rootBuf)
          relinquishIndexBuffer()
          r
        else
          parseValue()

      while more do
        peek match
          case Tab | Return | Newline | Space => advance()
          case char                           => errorAt(Issue.SpuriousContent(char.toChar))

      result


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

  def as[value: distillate.Decodable in Json at Json.Focus]
  :   value raises JsonError tracks Json.Focus =

    val decodable = summon[value is distillate.Decodable in Json at Json.Focus]
    val result = decodable.decoded(this)
    val foci = summon[Foci[Json.Focus]]
    foci.supplement(foci.length, _.let(decodable.position(this, _)).vouch)
    result

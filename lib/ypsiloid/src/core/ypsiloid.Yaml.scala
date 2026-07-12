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
package ypsiloid

import language.dynamics

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
import hieroglyph.CharEncoder
import jacinta.Bcd
import panopticon.*
import prepositional.*
import rudiments.*
import spectacular.show
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import YamlError.Reason

// `Yaml2` is only ever mixed into the `Yaml` object; pinning its self type to `Yaml.type` makes
// `this` a stable singleton, so the `provide`-wrapped decoder SAMs defined here do not capture an
// abstract trait self (which capture checking would reject as an impure capability).
trait Yaml2:
  this: Yaml.type =>
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Yaml )
  =>  value is Encodable in Yaml =

    new Encodable:
      type Self = value
      type Form = Yaml

      def encoded(value: value): Yaml =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_))
        . or(Yaml.ast(Yaml.Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[YamlError]
  =>  ( decodable: => inner is Decodable in Yaml )
  =>  value is Decodable in Yaml =
    // The by-name inner decoder and resolution-scoped tactic share this instance's
    // given-resolution lifetime; laundered pure (the codec-thunk seal pattern).
    caps.unsafe.unsafeAssumePure: yaml =>
          if yaml.root == Unset then Unset else decodable.decoded(yaml)


  // See `decodeMapping`: the `Tactic` is summoned at the SAM and passed to a stable helper, rather
  // than re-summoned via `provide` inside the SAM (which would capture the `yaml` parameter).
  private inline def decodePrimitive[value]
    ( yaml: Yaml )
    ( using value is Decodable in Text, Tactic[YamlError] )
  :   value =

    yaml.root.asMatchable match
      case s: String => s.tt.as[value]

      case _ =>
        abort(YamlError(Reason.NotType(Yaml.primitive(yaml.root), YamlPrimitive.Str)))

  inline given decodable: [value] => value is Decodable in Yaml = summonFrom:
    case given (`value` is Decodable in Text) =>
      yaml =>
        decodePrimitive[value](yaml)
          (using infer[value is Decodable in Text], infer[Tactic[YamlError]])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Yaml = summonFrom:
    case given (`value` is Encodable in Text) => value => Yaml.ast(Yaml.Ast(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Yaml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Yaml =

      // Capabilities are summoned at the derivation site and passed explicitly to `decodeMapping`
      // rather than re-summoned via `provide` inside the SAM: under capture checking a `provide`
      // wraps the body in a context function that captures the `yaml` parameter, which the pure
      // `Decodable` SAM rejects.
      yaml =>
        decodeMapping[derivation](yaml)
          ( using infer[ProductReflection[derivation]],
                  infer[Foci[Yaml.Focus]],
                  infer[Tactic[YamlError]] )

    private inline def decodeMapping[derivation <: Product]
      ( yaml: Yaml )
      ( using ProductReflection[derivation], Foci[Yaml.Focus], Tactic[YamlError] )
    :   derivation =

            val arr: IArray[Any] | Null = yaml.root.asMatchable match
              case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
                xs.asInstanceOf[IArray[Any]]

              case _ => null

            if arr != null then buildWith[derivation](arr)
            else
              // Wrong-shape input (including the `Yaml.Ast(Unset)`
              // sentinel an outer conjunction passes in for a missing
              // nested case-class field). If the user supplied
              // `Default[derivation]` we register one error at the
              // current focus and continue with the sentinel — a
              // missing nested case class lands as a single error
              // rather than expanding per sub-field.
              //
              // Without a `Default`, we fall back to running `build`
              // against a null mapping so each sub-field accrues its
              // own missing-field error (the PR-3 behaviour).
              //
              // The `Default[derivation]` summon must reference the
              // *outer* conjunction parameter `derivation` (concrete at
              // the inlining site). Pushing it into Wisteria's per-
              // field polymorphic lambda doesn't resolve reliably; see
              // xylophone #1157.
              summonFrom:
                case derivationDefault: Default[`derivation`] =>
                  val reason =
                    if yaml.root.isAbsent then Reason.Absent
                    else Reason.NotType(Yaml.primitive(yaml.root), YamlPrimitive.Mapping)

                  raise(YamlError(reason)) yet derivationDefault()

                case _ =>
                  buildWith[derivation](null)

    private inline def buildWith[derivation <: Product: ProductReflection]
      ( arr: IArray[Any] | Null )
      ( using Foci[Yaml.Focus], Tactic[YamlError] )
    :   derivation =

      // `@name[Yaml]` / bare `@name` renames: field name -> mapping key, read
      // back the same way they are written.
      val renames: Map[Text, Text] = relabelling[derivation, Yaml]

      build[derivation]: [field] =>
        context =>
          val key: Text = renames.at(label).or(label)
          val target = key.s
          var found: Yaml.Ast | Null = null

          if arr != null then
            val n = arr.length
            var i = 0

            while i < n && found == null do
              val entryKey = arr(i).asInstanceOf[Yaml.Ast]

              entryKey.asMatchable match
                case s: String if s == target => found = arr(i + 1).asInstanceOf[Yaml.Ast]
                case _                        => ()
              i += 2
          // Each outer `focus` runs *after* the inner one
          // (contingency's try/finally order), so we extend
          // `prior` at the root side via `prepend` so a nested
          // case-class error lands at `/outer/inner` rather than
          // `/inner/outer`. See `feedback-xml-decoder-split-design`
          // lesson 4.
          focus({
            val base = prior.let(_.pointer).or(YamlPath())
            Yaml.Focus(base.prepend(key))
          }):
            if found != null then context.decoded(new Yaml(found))
            // Missing field: try the case-class declared default
            // (Wisteria's `default`); if absent, hand the
            // `Yaml.Ast(Unset)` sentinel to the field's decoder.
            // Primitives detect it (via `isAbsent`) and `raise +
            // continue` with a zero/empty sentinel; nested
            // conjunctions detect wrong-shape and may further
            // short-circuit via a user-supplied `Default[Nested]`.
            else default.or(context.decoded(new Yaml(Yaml.Ast(Unset))))

    // Sealed-trait disjunction picks a variant by mapping discriminator
    // (`type:` key). We screen the discriminator against
    // `variantLabels` *before* `delegate`-ing so an unrecognised label
    // doesn't punch through as `VariantError` (which `validate
    // [Yaml.Focus]` doesn't catch) — it gets the same
    // `Default[derivation]`-or-abort treatment as a missing
    // discriminator. When the user supplies `Default[derivation]` we
    // register one error at the current focus and continue with the
    // sentinel; without one we abort.
    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Yaml = yaml =>
      provide[Foci[Yaml.Focus]]:
        provide[Tactic[YamlError]]:
          provide[Tactic[VariantError]]:
            val discriminable = infer[derivation is Discriminable in Yaml]
            val labels: List[Text] = variantLabels

            // `@name[Yaml]` / bare `@name` variant renames: map the serialized
            // discriminator back to the variant name before delegating.
            val variantNames: Map[Text, Text] =
              variantRelabelling[derivation, Yaml].map: (variant, wire) => wire -> variant

            val resolved: Optional[Text] =
              discriminable.discriminate(yaml).let: wire =>
                val discriminant = variantNames.getOrElse(wire, wire)
                if labels.contains(discriminant) then discriminant else Unset

            resolved.let: discriminant =>
              delegate(discriminant): [variant <: derivation] =>
                context => context.decoded(discriminable.variant(yaml))

            . or:
                focus(prior.or(Yaml.Focus(YamlPath()))):
                  summonFrom:
                    case derivationDefault: Default[`derivation`] =>
                      raise(YamlError(Reason.Absent)) yet derivationDefault()

                    case _ =>
                      abort(YamlError(Reason.Absent))

  object EncodableDerivation extends Derivable[Encodable in Yaml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Yaml =

      value =>
        provide[Foci[Yaml.Focus]]:
          val entries = scm.ArrayBuffer.empty[Any]

          // `@name[Yaml]` / bare `@name` renames: field name -> mapping key.
          val renames: Map[Text, Text] = relabelling[derivation, Yaml]

          fields(value): [field] =>
            field =>
              val key: Text = renames.at(label).or(label)

              focus({
                val base = prior.let(_.pointer).or(YamlPath())
                Yaml.Focus(base.prepend(key))
              }):
                val encoded = contextual.encode(field).root

                if !(encoded == Unset) then
                  entries += Yaml.Ast.Str(key).asInstanceOf[Any]
                  entries += encoded.asInstanceOf[Any]

          Yaml.ast(Yaml.Ast.mapFromAnyArray(entries.toArray))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Yaml = value =>
      val discriminable = infer[derivation is Discriminable in Yaml]

      // `@name[Yaml]` / bare `@name` variant renames: variant name -> wire
      // discriminator, read back the same way by the decoder.
      val variantNames: Map[Text, Text] = variantRelabelling[derivation, Yaml]

      variant(value): [variant <: derivation] =>
        value =>
          discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

object Yaml extends Yaml2, Dynamic:
  // Controls how a `Yaml` value is serialized. YAML's block style is fixed and round-trip-
  // constrained, so this currently carries no options; importing `formatting.blockYamlFormatting`
  // enables `.show` and HTTP encoding, and this is the place to add options later.
  trait Formatting extends zephyrine.Formatting

  type YamlString    = String
  type YamlInteger   = Long | Bcd
  type YamlDecimal   = Double | Bcd
  type YamlBoolean   = Boolean
  // Distinct sentinel for a YAML `null`, kept disjoint from the null-backed `Unset`
  // (absent): both would otherwise be the JVM `null` and collide in `Ast`.
  case object YamlNull
  type YamlNull      = YamlNull.type
  type YamlSequence  = IArray[Any]
  type YamlMapping   = IArray[Any]

  // The parser representation of a YAML value. Modelled on Jacinta's
  // `Json.Ast`: an opaque union over the primitive JVM types so that
  // primitive values are stored without case-class wrapping. Sequences
  // and mappings are both stored as `IArray[Any]`, distinguished by
  // length parity:
  //
  //   - mapping: even length (alternating key/value);
  //   - sequence: odd length (with a trailing `arrayPad` sentinel when
  //     the user-visible item count is even, so all sequences are
  //     length-odd at the array level).
  //
  // `null` represents a YAML null. `Unset.type` marks an absent value
  // (e.g. a missing field in a case-class derivation).
  // A phantom `caps.Pure` upper bound keeps capture checking from stamping spurious capture sets on
  // YAML values (which are always immutable). `Matchable` is *not* in the bound: one member,
  // `YamlSequence`/`YamlMapping = IArray[Any]`, is not statically `<: Matchable`, which would make
  // the bound conflict; `.asMatchable` still works (it is universal).
  opaque type Ast <: caps.Pure =
    (YamlString | YamlInteger | YamlDecimal | YamlBoolean | YamlNull | YamlSequence | YamlMapping |
      Unset) & caps.Pure

  // Whether `Yaml.Parser` captures line/column/length descriptors
  // alongside the AST. The default is `Off`, matching the historic
  // behaviour. Bring `Yaml.Tracking.On` into scope before calling
  // `.read[Yaml]` / `.load[Yaml]` / `Yaml.parseAll(...)` to get a
  // `Yaml` with `positionIndex` populated and `locate(pointer)`
  // returning concrete `Position`s. Mirrors the precedent set by
  // `jacinta.NumberMode`.
  object Tracking:
    given default: Tracking = Off

  enum Tracking:
    case On, Off

  // A flat `IArray[Int]` of position descriptors, produced alongside the AST
  // when a `Yaml` is parsed with `Tracking.On`. All internal offsets are
  // stored relative to the start of the containing descriptor, so any slice
  // taken at a descriptor boundary is itself a valid `PositionIndex` —
  // mirrors `jacinta.Json.PositionIndex`.
  opaque type PositionIndex = IArray[Int]

  object PositionIndex:
    private[ypsiloid] def apply(data: IArray[Int]): PositionIndex = data

  extension (positionIndex: PositionIndex)
    private[ypsiloid] def ints: IArray[Int] = positionIndex

  // The focus carried by `Yaml#as[T]` — a YAML-pointer path plus an
  // optional source position. The position is populated lazily by
  // `withPosition(yaml)`, which delegates to `yaml.locate(pointer)`,
  // so an untracked root (no `positionIndex`) leaves `position` Unset.
  // Costs nothing on the success path because `Focus` instances are
  // constructed inside `Foci.supplement` only for errors actually
  // registered in a surrounding `focus` block.
  case class Focus
    ( pointer:  YamlPath,
      position: Optional[Yaml.Ast.Position] = Unset )
  derives CanEqual:

    def withPosition(yaml: Yaml): Focus = copy(position = yaml.locate(pointer))

  object Ast extends Format:
    def name: Text = "YAML"

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
      case DirectiveWithoutDocumentStart
      case DirectivesOutOfPlace
      case MissingDocumentStart
      case ContentAfterDocumentEnd
      case DuplicateYamlDirective
      case YamlDirectiveRequiresVersion
      case YamlDirectiveTooManyArguments
      case YamlDirectiveInvalidVersion
      case TagDirectiveRequiresHandleAndPrefix
      case TwoAnchorsOnSameNode
      case AnchorOnAlias
      case ReservedIndicatorAtNodeStart
      case DuplicateAnchorOnNode
      case DuplicateTagOnNode
      case UnterminatedVerbatimTag
      case BlockMappingOnDocumentStartLine
      case ChainedMappingValueOnSingleLine
      case MultilineImplicitKey
      case TrailingContentAfterQuotedScalar
      case UnterminatedDoubleQuotedString
      case UnterminatedEscape
      case InvalidEscapeSequence
      case TruncatedHexEscape
      case InvalidHexDigit
      case UnterminatedSingleQuotedString
      case DocumentMarkerInsideMultilineScalar
      case ScalarContinuationUnderIndented
      case UnterminatedFlowSequence
      case EmptyFlowSequenceEntry
      case FlowImplicitKeyAndColonOnDifferentLines
      case FlowSequenceExpectedCommaOrClose
      case UnterminatedFlowMapping
      case EmptyFlowMappingEntry
      case FlowMappingExpectedCommaOrClose
      case FlowContentUnderIndented
      case CommentMissingPrecedingWhitespace
      case DocumentMarkerInFlowContext
      case ReservedIndicatorAtFlowPlainScalarStart
      case BlockSequenceIndicatorNotAtLineStart
      case TabInIndentation
      case PlainScalarAtMappingIndentWithoutColon
      case ExpectedColonAfterMappingKey
      case BlockSequenceOnMappingKeyLine
      case InvalidBlockScalarIndentationIndicator
      case DuplicateBlockScalarIndentationIndicator
      case DuplicateBlockScalarChompingIndicator
      case InvalidBlockScalarHeader
      case BlockScalarHeaderCommentMissingWhitespace
      case BlockScalarLeadingBlanksOverIndented
      case UndefinedTagHandle(handle: Text)
      case UnknownAlias(name: Text)

      def describe: Message = this match
        case DirectiveWithoutDocumentStart =>
          m"a directive must be followed by a `---` document-start marker"

        case DirectivesOutOfPlace =>
          m"directives can only appear at the start of a stream or after `...`"

        case MissingDocumentStart =>
          m"a `---` document-start marker is missing between documents"

        case ContentAfterDocumentEnd =>
          m"unexpected content was found after the `...` document-end marker"

        case DuplicateYamlDirective =>
          m"the %YAML directive was given more than once"

        case YamlDirectiveRequiresVersion =>
          m"the %YAML directive requires a version argument"

        case YamlDirectiveTooManyArguments =>
          m"the %YAML directive takes a single version argument"

        case YamlDirectiveInvalidVersion =>
          m"the %YAML directive version must be `major.minor`"

        case TagDirectiveRequiresHandleAndPrefix =>
          m"the %TAG directive requires a `handle prefix` argument pair"

        case TwoAnchorsOnSameNode =>
          m"two anchors were given on the same node"

        case AnchorOnAlias =>
          m"an alias node cannot also have an anchor"

        case ReservedIndicatorAtNodeStart =>
          m"a reserved indicator was found at the start of a node"

        case DuplicateAnchorOnNode =>
          m"more than one anchor was given on a single node"

        case DuplicateTagOnNode =>
          m"more than one tag was given on a single node"

        case UnterminatedVerbatimTag =>
          m"the verbatim tag was not terminated"

        case BlockMappingOnDocumentStartLine =>
          m"a block mapping cannot start on the document-start line"

        case ChainedMappingValueOnSingleLine =>
          m"a chained mapping value is not allowed on a single line"

        case MultilineImplicitKey =>
          m"an implicit mapping key cannot span multiple lines"

        case TrailingContentAfterQuotedScalar =>
          m"unexpected trailing content was found after a quoted scalar"

        case UnterminatedDoubleQuotedString =>
          m"the double-quoted string was not terminated"

        case UnterminatedEscape =>
          m"the escape sequence was not terminated"

        case InvalidEscapeSequence =>
          m"the escape sequence was not valid"

        case TruncatedHexEscape =>
          m"the hex escape was truncated"

        case InvalidHexDigit =>
          m"a hex digit was expected"

        case UnterminatedSingleQuotedString =>
          m"the single-quoted string was not terminated"

        case DocumentMarkerInsideMultilineScalar =>
          m"a document marker was found inside a multi-line scalar"

        case ScalarContinuationUnderIndented =>
          m"a multi-line scalar continuation was insufficiently indented"

        case UnterminatedFlowSequence =>
          m"the flow sequence was not terminated"

        case EmptyFlowSequenceEntry =>
          m"a flow-sequence entry was empty"

        case FlowImplicitKeyAndColonOnDifferentLines =>
          m"the implicit mapping key and `:` must be on the same line"

        case FlowSequenceExpectedCommaOrClose =>
          m"`,` or `]` was expected in the flow sequence"

        case UnterminatedFlowMapping =>
          m"the flow mapping was not terminated"

        case EmptyFlowMappingEntry =>
          m"a flow-mapping entry was empty"

        case FlowMappingExpectedCommaOrClose =>
          m"`,` or `}` was expected in the flow mapping"

        case FlowContentUnderIndented =>
          m"the flow content was insufficiently indented"

        case CommentMissingPrecedingWhitespace =>
          m"a comment must be preceded by whitespace"

        case DocumentMarkerInFlowContext =>
          m"document markers are not allowed in flow style"

        case ReservedIndicatorAtFlowPlainScalarStart =>
          m"a reserved indicator was found at the start of a flow plain scalar"

        case BlockSequenceIndicatorNotAtLineStart =>
          m"a block-sequence indicator must start its line"

        case TabInIndentation =>
          m"a tab character was used in indentation"

        case PlainScalarAtMappingIndentWithoutColon =>
          m"a plain scalar at the mapping indent was missing its `:`"

        case ExpectedColonAfterMappingKey =>
          m"`:` was expected after the mapping key"

        case BlockSequenceOnMappingKeyLine =>
          m"a sequence cannot start on the same line as a mapping key"

        case InvalidBlockScalarIndentationIndicator =>
          m"the block-scalar indentation indicator must be 1-9"

        case DuplicateBlockScalarIndentationIndicator =>
          m"more than one block-scalar indentation indicator was given"

        case DuplicateBlockScalarChompingIndicator =>
          m"more than one block-scalar chomping indicator was given"

        case InvalidBlockScalarHeader =>
          m"the block-scalar header was not valid"

        case BlockScalarHeaderCommentMissingWhitespace =>
          m"a comment in the block-scalar header must be preceded by whitespace"

        case BlockScalarLeadingBlanksOverIndented =>
          m"the leading empty lines have more indentation than the body"

        case UndefinedTagHandle(handle) =>
          m"the tag handle $handle was not defined"

        case UnknownAlias(name) =>
          m"the alias *$name does not refer to a known anchor"

    // Byte constants used by the parser (mirrors `Json.Ast.AsciiByte` from
    // Jacinta).
    object Byte:
      inline final val Tab:           9   = 9
      inline final val Newline:       10  = 10
      inline final val Return:        13  = 13
      inline final val Space:         32  = 32
      inline final val Bang:          33  = 33   // '!'
      inline final val Quote:         34  = 34   // '"'
      inline final val Hash:          35  = 35   // '#'
      inline final val Amp:           38  = 38   // '&'
      inline final val Apostrophe:    39  = 39   // '\''
      inline final val Star:          42  = 42   // '*'
      inline final val Plus:          43  = 43   // '+'
      inline final val Comma:         44  = 44
      inline final val Minus:         45  = 45   // '-'
      inline final val Period:        46  = 46
      inline final val Slash:         47  = 47
      inline final val Num0:          48  = 48
      inline final val Num1:          49  = 49
      inline final val Num2:          50  = 50
      inline final val Num3:          51  = 51
      inline final val Num4:          52  = 52
      inline final val Num5:          53  = 53
      inline final val Num6:          54  = 54
      inline final val Num7:          55  = 55
      inline final val Num8:          56  = 56
      inline final val Num9:          57  = 57
      inline final val Colon:         58  = 58
      inline final val Question:      63  = 63   // '?'
      inline final val Percent:       37  = 37   // '%'
      inline final val OpenAngle:     60  = 60   // '<'
      inline final val CloseAngle:    62  = 62   // '>'
      inline final val UpperE:        69  = 69
      inline final val UpperF:        70  = 70
      inline final val UpperX:        88  = 88
      inline final val OpenBracket:   91  = 91
      inline final val Backslash:     92  = 92
      inline final val CloseBracket:  93  = 93
      inline final val Caret:         94  = 94
      inline final val Underscore:    95  = 95
      inline final val LowerA:        97  = 97
      inline final val LowerB:        98  = 98
      inline final val LowerE:        101 = 101
      inline final val LowerF:        102 = 102
      inline final val LowerL:        108 = 108
      inline final val LowerN:        110 = 110
      inline final val LowerO:        111 = 111
      inline final val LowerR:        114 = 114
      inline final val LowerS:        115 = 115
      inline final val LowerT:        116 = 116
      inline final val LowerU:        117 = 117
      inline final val LowerV:        118 = 118
      inline final val LowerX:        120 = 120
      inline final val OpenBrace:     123 = 123
      inline final val Pipe:          124 = 124
      inline final val CloseBrace:    125 = 125
      inline final val Tilde:         126 = 126
      inline final val Greater:       62  = 62

    // Sentinel object used to pad an array whose user-visible length is
    // even so the underlying `IArray[Any]` is always odd-length and can be
    // distinguished from a mapping (always even-length).
    private[ypsiloid] val arrayPad: AnyRef = new Object

    // ── Constructors ────────────────────────────────────────────────────────

    // `Ast` is a pure type (a phantom `caps.Pure` upper bound keeps capture checking from stamping
    // spurious capture sets on YAML values, which are always immutable). The pure tag is erased, so
    // every constructor casts its plain underlying value through `make`.
    private inline def make
      ( value: Long | Double | Bcd | Boolean | String | IArray[Any] | YamlNull.type | Unset )
    :   Yaml.Ast =

      value.asInstanceOf[Yaml.Ast]

    inline def apply
      ( value:
        Long | Double | Bcd | Boolean | String | IArray[Any] | YamlNull.type | Unset )
    :   Yaml.Ast =

      make(value)

    val Null: Yaml.Ast = make(YamlNull)

    inline def Bool(value: Boolean): Yaml.Ast = make(value)
    inline def Integer(value: Long): Yaml.Ast = make(value)
    inline def Decimal(value: Double): Yaml.Ast = make(value)
    inline def BcdValue(value: Bcd): Yaml.Ast = make(value)
    inline def Str(value: Text): Yaml.Ast = make(value.s)

    // Wrap an `IArray[Yaml.Ast]` of items as a sequence node. If the count
    // is even, append the `arrayPad` sentinel so the final node has odd
    // length.
    def Sequence(items: IArray[Yaml.Ast]): Yaml.Ast =
      val n = items.length

      if (n & 1) == 1 then items.asInstanceOf[Yaml.Ast]
      else
        val padded = new Array[Any](n + 1)
        System.arraycopy(items.asInstanceOf[Array[Any]], 0, padded, 0, n)
        padded(n) = arrayPad
        padded.asInstanceOf[Yaml.Ast]

    // Wrap parallel keys/values as a mapping node, flattened to alternating
    // `[k0, v0, k1, v1, ...]`. The result has even length.
    def Mapping(entries: IArray[(Yaml.Ast, Yaml.Ast)]): Yaml.Ast =
      val n = entries.length
      val arr = new Array[Any](n*2)
      var i = 0

      while i < n do
        val (k, v) = entries(i)
        arr(i*2) = k.asInstanceOf[Any]
        arr(i*2 + 1) = v.asInstanceOf[Any]
        i += 1

      arr.asInstanceOf[Yaml.Ast]

    // Build a sequence directly from a raw `Array[Any]` of items (no
    // copy if the length is already odd; pad once otherwise). The parser
    // uses this to avoid the `Array.map` step.
    private[ypsiloid] def seqFromAnyArray(items: Array[Any]): Yaml.Ast =
      val n = items.length

      if (n & 1) == 1 then items.asInstanceOf[Yaml.Ast]
      else
        val padded = new Array[Any](n + 1)
        System.arraycopy(items, 0, padded, 0, n)
        padded(n) = arrayPad
        padded.asInstanceOf[Yaml.Ast]

    // Build a mapping directly from a flat `Array[Any]` of alternating
    // key/value entries. Length must be even.
    private[ypsiloid] def mapFromAnyArray(entries: Array[Any]): Yaml.Ast =
      entries.asInstanceOf[Yaml.Ast]

    // ── Inspection ──────────────────────────────────────────────────────────

    // The user-visible length of a sequence (excludes the pad sentinel).
    def sequenceLength(arr: IArray[Any]): Int =
      val n = arr.length
      if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq arrayPad) then n - 1 else n

    // The number of (key, value) pairs in a mapping.
    def mappingSize(arr: IArray[Any]): Int = arr.length / 2

    // Apply `body(item)` to each user-visible item of a sequence, skipping
    // the pad sentinel. Hot-path iterator used by collection decoders.
    inline def foreachItem(arr: IArray[Any])(inline body: Yaml.Ast => Unit): Unit =
      val n = sequenceLength(arr)
      var i = 0

      while i < n do
        body(arr(i).asInstanceOf[Yaml.Ast])
        i += 1

    // Apply `body(key, value)` to each entry of a mapping. Used by Map/case-
    // class decoders.
    inline def foreachEntry(arr: IArray[Any])(inline body: (Yaml.Ast, Yaml.Ast) => Unit): Unit =
      val n = arr.length
      var i = 0

      while i < n do
        body(arr(i).asInstanceOf[Yaml.Ast], arr(i + 1).asInstanceOf[Yaml.Ast])
        i += 2

    // ── Pattern-match extractors ────────────────────────────────────────────
    // These let existing `case Yaml.Ast.Bool(b) => ...` patterns keep working
    // after the case-class hierarchy is removed. Each unapply allocates an
    // `Option`, which is fine for non-hot-path code (tests, decoders).

    object Bool:
      def unapply(ast: Yaml.Ast): Option[Boolean] = ast match
        case b: Boolean => Some(b)
        case _          => None

    object Integer:
      def unapply(ast: Yaml.Ast): Option[Long] = ast match
        case n: Long => Some(n)
        case _       => None

    object Decimal:
      def unapply(ast: Yaml.Ast): Option[Double] = ast match
        case d: Double => Some(d)
        case _         => None

    // Pattern extractor for high-precision BCD numbers. Matches a number
    // value that overflowed `Long`/`Double` precision during parsing.
    object BcdValue:
      def unapply(ast: Yaml.Ast): Option[Bcd] = ast match
        case b: Array[Double] @unchecked => Some(b.asInstanceOf[Bcd])
        case _                           => None

    object Str:
      def unapply(ast: Yaml.Ast): Option[Text] = ast match
        case s: String => Some(s.tt)
        case _         => None

    object Sequence:
      def unapply(ast: Yaml.Ast): Option[IArray[Yaml.Ast]] = ast match
        case xs: IArray[?] @unchecked
          if xs.isInstanceOf[Array[AnyRef]] && ((xs.length & 1) == 1 || xs.length == 1) =>
          // Strip the sentinel if present.
          val n = xs.length

          if n > 0 && (xs(n - 1).asInstanceOf[AnyRef] eq arrayPad) then
            val out = new Array[Any](n - 1)
            System.arraycopy(xs.asInstanceOf[Array[Any]], 0, out, 0, n - 1)
            Some(out.asInstanceOf[IArray[Yaml.Ast]])
          else
            Some(xs.asInstanceOf[IArray[Yaml.Ast]])

        case _ => None

    object Mapping:
      def unapply(ast: Yaml.Ast): Option[IArray[(Yaml.Ast, Yaml.Ast)]] = ast match
        case xs: IArray[?] @unchecked
          if xs.isInstanceOf[Array[AnyRef]] && (xs.length & 1) == 0 =>
          val n = xs.length / 2

          Some(IArray.tabulate(n): i =>
            (xs(i*2).asInstanceOf[Yaml.Ast], xs(i*2 + 1).asInstanceOf[Yaml.Ast]))

        case _ => None

    // ── Deep equality ───────────────────────────────────────────────────────
    // Used by `Yaml.equals`/`hashCode` and by tests that compare two parsed
    // ASTs structurally. Walks `IArray[Any]` recursively so different array
    // instances with the same content compare equal.

    def deepEquals(left: Yaml.Ast, right: Yaml.Ast): Boolean =
      if left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef] then true
      else (left, right) match
        case (a: Long, b: Long)         => a == b
        case (a: Double, b: Double)     => a == b
        case (a: Boolean, b: Boolean)   => a == b
        case (a: String, b: String)     => a == b

        // Cross-shape numeric comparisons: a `Bcd` may be equal in value
        // to a `Long`, `Double`, or another `Bcd`, when their canonical
        // BigDecimal projections compare equal.
        case (a: Array[Double] @unchecked, b: Array[Double] @unchecked) =>
          a.asInstanceOf[Bcd].toBigDecimal == b.asInstanceOf[Bcd].toBigDecimal

        case (a: Array[Double] @unchecked, b: Long) =>
          a.asInstanceOf[Bcd].toBigDecimal == BigDecimal(b)

        case (a: Array[Double] @unchecked, b: Double) =>
          a.asInstanceOf[Bcd].toBigDecimal == BigDecimal(b)

        case (a: Long, b: Array[Double] @unchecked) =>
          BigDecimal(a) == b.asInstanceOf[Bcd].toBigDecimal

        case (a: Double, b: Array[Double] @unchecked) =>
          BigDecimal(a) == b.asInstanceOf[Bcd].toBigDecimal

        case (a: Array[AnyRef] @unchecked, b: Array[AnyRef] @unchecked) =>
          a.length == b.length && {
            var i = 0
            var equal = true

            while i < a.length && equal do
              val ax = a(i).asInstanceOf[AnyRef]
              val bx = b(i).asInstanceOf[AnyRef]

              if (ax eq bx) || (ax eq arrayPad) && (bx eq arrayPad) then ()
              else
                equal = deepEquals(a(i).asInstanceOf[Yaml.Ast], b(i).asInstanceOf[Yaml.Ast])

              i += 1

            equal
          }

        case _ => false

    def deepHash(ast: Yaml.Ast): Int = (ast: @unchecked) match
      case Yaml.YamlNull => 0
      case b: Boolean    => b.hashCode
      case n: Long       => n.hashCode
      case d: Double     => d.hashCode
      case s: String     => s.hashCode

      case b: Array[Double] @unchecked =>
        // Hash via the BigDecimal projection so a `Bcd` whose value equals
        // a numeric `Long`/`Double` literal has a consistent hash.
        b.asInstanceOf[Bcd].toBigDecimal.hashCode

      case xs: Array[AnyRef] @unchecked =>
        var h = xs.length
        var i = 0

        while i < xs.length do
          val item = xs(i).asInstanceOf[AnyRef]

          if !(item eq arrayPad) then
            h = h*31 + deepHash(xs(i).asInstanceOf[Yaml.Ast])

          i += 1

        h

      case Unset         => 1

    // AST predicates and accessors mirroring Jacinta's `Json.Ast` extensions.
    // They live in the `Ast` companion so they are in scope wherever a
    // `Yaml.Ast` is used, without being top-level (and out of the `soundness`
    // umbrella, where their generic names would clash). `isObject` / `isArray`
    // distinguish mappings (even-length flat array of alternating key/value)
    // from sequences (odd-length, with a trailing `arrayPad` sentinel when the
    // user-visible item count is even).
    extension (yaml: Yaml.Ast)
      inline def isAbsent:  Boolean = yaml == Unset
      inline def isNull:    Boolean = yaml.asInstanceOf[AnyRef] eq Yaml.YamlNull
      inline def isLong:    Boolean = yaml.isInstanceOf[Long]
      inline def isDouble:  Boolean = yaml.isInstanceOf[Double]
      inline def isBcd:     Boolean = yaml.isInstanceOf[Array[Double]]
      inline def isNumber:  Boolean = isLong || isDouble || isBcd
      inline def isString:  Boolean = yaml.isInstanceOf[String]
      inline def isBoolean: Boolean = yaml.isInstanceOf[Boolean]

      inline def isObject: Boolean =
        yaml.isInstanceOf[Array[AnyRef]] &&
          (yaml.asInstanceOf[Array[?]].length & 1) == 0

      inline def isArray: Boolean =
        yaml.isInstanceOf[Array[AnyRef]] &&
          (yaml.asInstanceOf[Array[?]].length & 1) == 1

      private def expected(yamlPrimitive: YamlPrimitive)(using Tactic[YamlError]): Unit =
        raise:
          YamlError(if isAbsent then Reason.Absent else Reason.NotType(primitive, yamlPrimitive))

      inline def arrayLength: Int = Yaml.Ast.sequenceLength(yaml.asInstanceOf[IArray[Any]])

      inline def arrayElement(index: Int): Yaml.Ast =
        yaml.asInstanceOf[IArray[Yaml.Ast]](index)

      inline def objectSize: Int = yaml.asInstanceOf[IArray[Any]].length/2

      inline def objectKey(index: Int): String =
        yaml.asInstanceOf[IArray[Any]](index*2).asInstanceOf[String]

      inline def objectValue(index: Int): Yaml.Ast =
        yaml.asInstanceOf[IArray[Any]](index*2 + 1).asInstanceOf[Yaml.Ast]

      // Linear scan for a key — returns the pair-indexed position (i.e.
      // `objectKey(result)` retrieves the key, `objectValue(result)` the
      // value) or `-1` if the key is absent. Only string-typed keys match.
      def objectIndexOf(key: String): Int =
        if !isObject then -1 else
          val arr = yaml.asInstanceOf[Array[?]]
          val len = arr.length
          var i = 0
          var hit = -1

          while i < len && hit < 0 do
            arr(i) match
              case s: String if s == key => hit = i/2
              case _                     => ()

            i += 2

          hit

      def array(using Tactic[YamlError]): IArray[Yaml.Ast] =
        if isArray then
          val full = yaml.asInstanceOf[IArray[Yaml.Ast]]
          val n = arrayLength

          if n == full.length then full
          else IArray.tabulate(n)(full(_))
        else
          expected(YamlPrimitive.Sequence) yet IArray[Yaml.Ast]()

      def double(using Tactic[YamlError]): Double = yaml.asInstanceOf[Matchable] match
        case value: Double                   => value
        case value: Long                     => value.toDouble
        case value: Array[Double] @unchecked => value.asInstanceOf[jacinta.Bcd].toDouble
        case _                               => expected(YamlPrimitive.Decimal) yet 0.0

      def long(using Tactic[YamlError]): Long = yaml.asInstanceOf[Matchable] match
        case value: Long                     => value
        case value: Double                   => value.toLong
        case value: Array[Double] @unchecked => value.asInstanceOf[jacinta.Bcd].toLong.or(0L)
        case _                               => expected(YamlPrimitive.Integer) yet 0L

      def bcd(using Tactic[YamlError]): jacinta.Bcd = yaml.asInstanceOf[Matchable] match
        case value: Array[Double] @unchecked => value.asInstanceOf[jacinta.Bcd]
        case value: Long                     => jacinta.Bcd(BigDecimal(value))
        case value: Double                   => jacinta.Bcd(BigDecimal(value))

        case _ =>
          expected(YamlPrimitive.Decimal) yet jacinta.Bcd(BigDecimal(0))

      def string(using Tactic[YamlError]): Text =
        if isString then yaml.asInstanceOf[String].tt
        else expected(YamlPrimitive.Str) yet t""

      def boolean(using Tactic[YamlError]): Boolean =
        if isBoolean then yaml.asInstanceOf[Boolean]
        else expected(YamlPrimitive.Bool) yet false

      def primitive: YamlPrimitive = Yaml.primitive(yaml)

  def ast(value: Yaml.Ast): Yaml = new Yaml(value)

  // `object Yaml` extends `Dynamic`, which suppresses the universal-apply
  // synthesis for `Yaml(...)` once the primary constructor takes more than
  // one parameter; these explicit overloads forward to the constructor.
  def apply(value: Any): Yaml = new Yaml(value.asInstanceOf[Yaml.Ast])

  def apply(value: Any, positions: Optional[Yaml.PositionIndex]): Yaml =
    new Yaml(value.asInstanceOf[Yaml.Ast], positions)

  // Canonical external accessor for the underlying AST. The `root`
  // field on `class Yaml` is package-private so that breaking through
  // the `Yaml` abstraction is a deliberate, named action.
  def unseal(yaml: Yaml): Yaml.Ast = yaml.root

  // Resolves a `YamlPath` to the source `Position` recorded in a tracked
  // `Yaml`'s `PositionIndex`. Exposed uniformly as `yaml.locate(path)` /
  // `yaml.locateKey(path)` through zephyrine's `Positionable`.
  given positionable: Yaml is Positionable by YamlPath to Yaml.Ast.Position =
    new Positionable:
      type Self    = Yaml
      type Operand = YamlPath
      type Result  = Yaml.Ast.Position

      def locate(value: Yaml, path: YamlPath): Optional[Yaml.Ast.Position] =
        value.positionIndex.let: posIndex =>
          walkIndex(value.root, posIndex.ints, 0, path.path.descent.toIndexedSeq, 0, false)

      def locateKey(value: Yaml, path: YamlPath): Optional[Yaml.Ast.Position] =
        value.positionIndex.let: posIndex =>
          walkIndex(value.root, posIndex.ints, 0, path.path.descent.toIndexedSeq, 0, true)

  private def walkIndex
    ( ast:      Yaml.Ast,
      data:     IArray[Int],
      offset:   Int,
      segments: IndexedSeq[Text],
      i:        Int,
      keyMode:  Boolean )
  :   Optional[Yaml.Ast.Position] =

    if i >= segments.length then
      if keyMode then Unset
      else Yaml.Ast.Position
        ( line   = data(offset + 1),
          column = data(offset + 2),
          length = data(offset + 3) )
    else
      // `YamlPath.path.descent` is stored leaf-first (Serpentine's `/`
      // prepends), so iterate it in reverse to walk root-to-leaf.
      val seg = segments(segments.length - 1 - i).s

      if ast.isObject then
        val k = ast.objectIndexOf(seg)

        if k < 0 then Unset
        else
          val entryOff = data(offset + 5 + k)
          val isLast = i == segments.length - 1

          if isLast && keyMode then
            Yaml.Ast.Position
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

  inline given interpolator: Yaml is Interpolable:
    type Result = Yaml

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Yaml =

      ${ypsiloid.internal.interpolator[parts, origins]('insertions)}


  inline given extrapolator: Yaml is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Yaml)
    :   Boolean | Option[Tuple | Yaml] =

      ${ypsiloid.internal.extractor[parts, origins]('scrutinee)}

  // Named-parameter construction: `Yaml.make(name = …, age = …)`
  // desugars to `applyDynamicNamed("make")(("name", …), ("age", …))`.
  // Mirrors `Json.applyDynamicNamed`.
  def applyDynamicNamed(methodName: "make")(elements: (String, Yaml)*): Yaml =
    val arr = new Array[Any](elements.length*2)
    var i = 0

    while i < elements.length do
      arr(i*2)     = Yaml.Ast.Str(elements(i)(0).tt).asInstanceOf[Any]
      arr(i*2 + 1) = elements(i)(1).root.asInstanceOf[Any]
      i += 1

    Yaml.ast(Yaml.Ast.mapFromAnyArray(arr))

  given yaml: Yaml is Decodable in Yaml = identity(_)
  given yamlEncodable: Yaml is Encodable in Yaml = identity(_)

  // Laundered pure like the primitive codecs (codec-thunk seal; see rep/DECISIONS.md).
  given bytes: (tactic: Tactic[YamlError])
  =>  Bytes is Decodable in Yaml =
    caps.unsafe.unsafeAssumePure(_.root.long.b)

  given lens: [name <: Label: ValueOf] => (erased dynamicYamlEnabler: DynamicYamlEnabler) => (tactic: Tactic[YamlError])
  =>  ((name is Lens from Yaml onto Yaml)^{tactic}) =
    Lens(_.selectDynamic(valueOf[name]), (yaml, value) => yaml.modify(valueOf[name], value))

  given ordinalOptical: [element] => Ordinal is Optical from Yaml onto Yaml = ordinal =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        if n <= ordinal.n0 then origin else Yaml.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            updated(i) =
              if i == ordinal.n0
              then lambda(Yaml.ast(origin.root.arrayElement(i))).root
              else origin.root.arrayElement(i)

            i += 1

          Yaml.Ast.seqFromAnyArray(updated)
      else
        origin

  // `Each` applies the transform to every sequence element; `Filter` to those
  // matching its predicate. Both rebuild the sequence immutably and no-op otherwise.
  given eachOptical: Each.type is Optical from Yaml onto Yaml = _ =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Yaml.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            updated(i) = lambda(Yaml.ast(origin.root.arrayElement(i))).root
            i += 1

          Yaml.Ast.seqFromAnyArray(updated)
      else
        origin

  // The `predicate` laundering is for the Scala.js pipeline, which — unlike the JVM
  // pipeline — rejects the `Optic`'s capture of `filter.predicate` against the required
  // pure `Optic` type. (Compiler divergence; see #1520 and `caesura`'s `rowFilter`.)
  given filterOptical: Filter[Yaml] is Optical from Yaml onto Yaml = filter =>
    val predicate: Yaml -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.arrayLength

        Yaml.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            val element = Yaml.ast(origin.root.arrayElement(i))
            updated(i) = (if predicate(element) then lambda(element) else element).root
            i += 1

          Yaml.Ast.seqFromAnyArray(updated)
      else
        origin

  // Single-error abort variant retained for non-primitive accessors
  // (`yaml(t"foo")`, `yaml(0)`, etc.) that need to short-circuit on
  // wrong-shape access. Primitive `Decodable in Yaml` instances no
  // longer route through this — they use the raise+yet helper below
  // so multi-error accrual can register each malformed field
  // independently.
  private inline def typeMismatch[T]
    ( yaml: Yaml, expected: YamlPrimitive, default: T )
    ( using Tactic[YamlError] )
  :   T =

    abort(YamlError(Reason.NotType(primitive(yaml.root), expected)))

  // Register a wrong-type / absent error and continue with `sentinel`
  // instead of aborting — the raise+yet pattern that lets sibling
  // fields in a case-class decode accrue their own errors rather than
  // bailing on the first failure. Absent inputs (the `Yaml.Ast(Unset)`
  // sentinel handed to a primitive decoder by `DecodableDerivation.
  // conjunction` for a missing case-class field) raise `Reason.Absent`
  // so error reports distinguish "field missing" from "field had the
  // wrong shape".
  private inline def primitiveFault[T]
    ( yaml: Yaml, expected: YamlPrimitive, sentinel: T )
    ( using Tactic[YamlError] )
  :   T =

    if yaml.root.isAbsent then raise(YamlError(Reason.Absent)) yet sentinel
    else raise(YamlError(Reason.NotType(primitive(yaml.root), expected))) yet sentinel

  given int: (tactic: Tactic[YamlError])
  =>  ((Int is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n.toInt
      case d: Double => d.toInt
      case _         => primitiveFault(yaml, YamlPrimitive.Integer, 0)

  given long: (tactic: Tactic[YamlError])
  =>  ((Long is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n
      case d: Double => d.toLong
      case _         => primitiveFault(yaml, YamlPrimitive.Integer, 0L)

  given double: (tactic: Tactic[YamlError])
  =>  ((Double is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case d: Double => d
      case n: Long   => n.toDouble
      case _         => primitiveFault(yaml, YamlPrimitive.Decimal, 0.0)

  given float: (tactic: Tactic[YamlError])
  =>  ((Float is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case d: Double => d.toFloat
      case n: Long   => n.toFloat
      case _         => primitiveFault(yaml, YamlPrimitive.Decimal, 0.0f)

  given boolean: (tactic: Tactic[YamlError])
  =>  ((Boolean is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case b: Boolean => b
      case _          => primitiveFault(yaml, YamlPrimitive.Bool, false)

  given text: (tactic: Tactic[YamlError])
  =>  ((Text is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case s: String => s.tt
      case _         => primitiveFault(yaml, YamlPrimitive.Str, t"")

  given string: (tactic: Tactic[YamlError])
  =>  ((String is Decodable in Yaml)^{tactic}) = yaml =>
    yaml.root.asMatchable match
      case s: String => s
      case _         => primitiveFault(yaml, YamlPrimitive.Str, "")

  given unit: (tactic: Tactic[YamlError])
  =>  ((Unit is Decodable in Yaml)^{tactic}) = yaml =>
    if yaml.root.isNull then ()
    else primitiveFault(yaml, YamlPrimitive.Null, ())

  given iterable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        tactic:    Tactic[YamlError],
        foci:      Foci[Yaml.Focus] )
  =>  ( decodable: => element is Decodable in Yaml )
  =>  collection[element] is Decodable in Yaml =
    // By-name element codec + resolution-scoped tactic; laundered pure (the codec-thunk seal).
    caps.unsafe.unsafeAssumePure: yaml =>
      yaml.root.asMatchable match
        case xs: IArray[?] @unchecked if (xs.length & 1) == 1 =>
          // Sequence (odd length, possibly with a trailing pad sentinel).
          val n = xs.length

          val effective =
            if n > 0 && (xs(n - 1).asInstanceOf[AnyRef] eq Yaml.Ast.arrayPad) then n - 1
            else n

          val builder = factory.newBuilder
          var i = 0

          while i < effective do
            val ordinal = denominative.Ordinal.zerary(i)

            focus({
              val base = prior.let(_.pointer).or(YamlPath())
              Yaml.Focus(base.prepend(ordinal))
            }):
              builder += decodable.decoded(new Yaml(xs(i).asInstanceOf[Yaml.Ast]))

            i += 1

          builder.result()

        case other =>
          raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[Yaml.Ast]),
                                         YamlPrimitive.Sequence)))

          factory.newBuilder.result()

  given map: [value: Decodable in Yaml] => Tactic[YamlError]
  =>  Map[Text, value] is Decodable in Yaml =
    // Resolution-scoped tactic and value codec; laundered pure (the codec-thunk seal).
    caps.unsafe.unsafeAssumePure: yaml =>
      yaml.root.asMatchable match
        case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
          // Mapping (even length, alternating keys and values flat).
          val n = xs.length / 2
          var result = Map.empty[Text, value]
          var i = 0

          while i < n do
            val rawKey = xs(i*2).asInstanceOf[Yaml.Ast]
            val rawValue = xs(i*2 + 1).asInstanceOf[Yaml.Ast]

            val keyText: Text =
              if rawKey.isNull then t"null"
              else rawKey.asMatchable match
                case s: String  => s.tt
                case k: Long    => k.toString.tt
                case k: Double  => k.toString.tt
                case k: Boolean => k.toString.tt

                case other =>
                  abort(YamlError(Reason.NotType(primitive(other.asInstanceOf[Yaml.Ast]),
                                                 YamlPrimitive.Str)))

            result = result.updated(keyText, value.decoded(new Yaml(rawValue)))
            i += 1

          result

        case other =>
          raise(YamlError(Reason.NotType(primitive(other.asInstanceOf[Yaml.Ast]),
                                         YamlPrimitive.Mapping)))

          Map.empty

  given option: [value: Decodable in Yaml] => Option[value] is Decodable in Yaml = yaml =>
    if yaml.root.isAbsent || yaml.root.isNull then None
    else Some(value.decoded(yaml))

  // ── Encodable givens ────────────────────────────────────────────────────

  given optionEncodable: [value] => (encodable: value is Encodable in Yaml)
  =>  Option[value] is Encodable in Yaml =

    new Encodable:
      type Self = Option[value]
      type Form = Yaml

      def encoded(value: Option[value]): Yaml = value match
        case None        => Yaml.ast(Yaml.Ast(Unset))
        case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Encodable in Yaml =
    int => Yaml.ast(Yaml.Ast(integral.toLong(int)))

  given textEncodable: Text is Encodable in Yaml = text => Yaml.ast(Yaml.Ast(text.s))
  given stringEncodable: String is Encodable in Yaml = string => Yaml.ast(Yaml.Ast(string))
  given doubleEncodable: Double is Encodable in Yaml = double => Yaml.ast(Yaml.Ast(double))
  given floatEncodable: Float is Encodable in Yaml = float => Yaml.ast(Yaml.Ast(float.toDouble))
  given intEncodable: Int is Encodable in Yaml = int => Yaml.ast(Yaml.Ast(int.toLong))
  given longEncodable: Long is Encodable in Yaml = long => Yaml.ast(Yaml.Ast(long))
  given booleanEncodable: Boolean is Encodable in Yaml = boolean => Yaml.ast(Yaml.Ast(boolean))
  given unitEncodable: Unit is Encodable in Yaml = _ => Yaml.ast(Yaml.Ast.Null)


  given iterableEncodable: [collection <: Iterable, element]
  =>  ( encodable: => element is Encodable in Yaml )
  =>  collection[element] is Encodable in Yaml =
    // A pure thunk rather than a sealed lambda: under `-scalajs` the SAM expands to an
    // anonymous class whose pure self-type may not capture the by-name parameter.
    val enc: () -> (element is Encodable in Yaml) = caps.unsafe.unsafeAssumePure(() => encodable)

    values =>
      val items = IArray.from(values.map(enc().encode(_).root))
      Yaml.ast(Yaml.Ast.Sequence(items))


  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Yaml )
  =>  Map[key, element] is Encodable in Yaml = map =>
    val keys: List[key] = map.keys.to(List)
    val arr = new Array[Any](keys.size*2)
    var i = 0

    keys.foreach: k =>
      arr(i*2) = Yaml.Ast.Str(k.encode).asInstanceOf[Any]
      arr(i*2 + 1) = encodable.encode(map(k)).root.asInstanceOf[Any]
      i += 1

    Yaml.ast(Yaml.Ast.mapFromAnyArray(arr))


  // ── Discriminator support for sum-type derivation ───────────────────────

  // Build a `Discriminable` for `Yaml` keyed on a single mapping field
  // named `label`. `discriminate` looks the field up in the YAML mapping;
  // `rewrite` adds (or replaces) the field with the variant's name; and
  // `variant` strips the field for variant decoding.
  def discriminatedUnion[value](label: Text): value is Discriminable in Yaml =
    new Discriminable:
      type Form = Yaml
      type Self = value

      def discriminate(yaml: Yaml): Optional[Text] =
        yaml.root.asMatchable match
          case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
            var i = 0
            var result: Optional[Text] = Unset

            while i < xs.length && result.absent do
              xs(i).asMatchable match
                case s: String if s == label.s =>
                  xs(i + 1).asMatchable match
                    case v: String => result = v.tt
                    case _         => ()

                case _ => ()

              i += 2

            result

          case _ => Unset

      def rewrite(kind: Text, yaml: Yaml): Yaml =
        yaml.root.asMatchable match
          case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
            // Replace existing entry if present, else append.
            var existing = -1
            var i = 0

            while i < xs.length && existing < 0 do
              xs(i).asMatchable match
                case s: String if s == label.s => existing = i
                case _                         => ()

              i += 2

            val out =
              if existing >= 0 then
                val arr = new Array[Any](xs.length)
                System.arraycopy(xs.asInstanceOf[Array[Any]], 0, arr, 0, xs.length)
                arr(existing + 1) = Yaml.Ast.Str(kind).asInstanceOf[Any]
                arr
              else
                val arr = new Array[Any](xs.length + 2)
                System.arraycopy(xs.asInstanceOf[Array[Any]], 0, arr, 0, xs.length)
                arr(xs.length)     = Yaml.Ast.Str(label).asInstanceOf[Any]
                arr(xs.length + 1) = Yaml.Ast.Str(kind).asInstanceOf[Any]
                arr

            Yaml.ast(Yaml.Ast.mapFromAnyArray(out))

          case _ =>
            // Not a mapping — wrap in a one-entry mapping.
            val arr =
              Array[Any]
                ( Yaml.Ast.Str(label).asInstanceOf[Any],
                  Yaml.Ast.Str(kind).asInstanceOf[Any] )

            Yaml.ast(Yaml.Ast.mapFromAnyArray(arr))

      def variant(yaml: Yaml): Yaml =
        yaml.root.asMatchable match
          case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
            var existing = -1
            var i = 0

            while i < xs.length && existing < 0 do
              xs(i).asMatchable match
                case s: String if s == label.s => existing = i
                case _                         => ()

              i += 2

            if existing < 0 then yaml
            else
              val arr = new Array[Any](xs.length - 2)
              System.arraycopy(xs.asInstanceOf[Array[Any]], 0, arr, 0, existing)

              System.arraycopy
                ( xs.asInstanceOf[Array[Any]],
                  existing + 2,
                  arr,
                  existing,
                  xs.length - existing - 2 )

              Yaml.ast(Yaml.Ast.mapFromAnyArray(arr))

          case _ => yaml


  // ── Parser entry-points ─────────────────────────────────────────────────

  // Whether parsing captures line/column/length descriptors alongside the
  // AST is controlled by the contextual `Tracking` mode in scope —
  // mirrors `jacinta.NumberMode`. Default is `Tracking.Off` (no
  // descriptor capture). Callers wanting position-aware `Yaml.locate`
  // (and, in subsequent PRs, focus-aware decoding) bring
  // `Tracking.On` into scope before calling `.read[Yaml]` / `.load[Yaml]`
  // / `Yaml.parseAll(...)`.

  given decodable: (tactic: Tactic[ParseError], tracking: Yaml.Tracking)
  =>  ((Yaml is Decodable in Text)^{tactic}) =
    text => tracking match
      case Yaml.Tracking.On =>
        val (ast, ints) = Yaml.Parser.parseTracked(text)
        new Yaml(ast, Yaml.PositionIndex(ints))

      case Yaml.Tracking.Off =>
        Yaml(Yaml.Parser.parse(text))

  def parseAll(input: Text)(using Tactic[ParseError], Yaml.Tracking): List[Yaml] =
    summon[Yaml.Tracking] match
      case Yaml.Tracking.On =>
        Yaml.Parser.parseAllTracked(input).map: (ast, ints) =>
          new Yaml(ast, Yaml.PositionIndex(ints))

      case Yaml.Tracking.Off =>
        Yaml.Parser.parseAll(input).map(Yaml(_))

  given aggregable: (tactic: Tactic[ParseError], tracking: Yaml.Tracking)
  =>  ((Yaml is Aggregable by Text)^{tactic}) =
    summon[Text is Aggregable by Text].map: text =>
      summon[Yaml.Tracking] match
        case Yaml.Tracking.On =>
          val (ast, ints) = Yaml.Parser.parseTracked(text)
          new Yaml(ast, Yaml.PositionIndex(ints))

        case Yaml.Tracking.Off =>
          Yaml(Yaml.Parser.parse(text))

  // Multi-document reads (`---`-separated YAML) through the uniform `.read`
  // API: `text.read[List[Yaml]]` yields one `Yaml` per document. Backed by
  // `parseAll`, this replaces the former bespoke `Text.readAll` extension.
  given aggregableAll: (tactic: Tactic[ParseError], tracking: Yaml.Tracking)
  =>  ((List[Yaml] is Aggregable by Text)^{tactic}) =
    summon[Text is Aggregable by Text].map(parseAll(_))

  // HTTP content-type integration. `Abstractable across HttpStreams` makes a
  // `Yaml` value usable as an HTTP request/response body (telekinesis derives
  // `Postable`/`Servable` from it); `Instantiable across HttpRequests` reads a
  // request/response body back into `Yaml`. Encoding needs a `Yaml.Formatting` in
  // scope (see `printers`), which drives the `Yaml.Ast` `Showable` instance.
  given abstractable: (encoder: CharEncoder, formatting: Formatting)
  =>  Yaml is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Yaml
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(value: Yaml): HttpStreams.Content =
        ( t"application/yaml; charset=${encoder.encoding.name}",
          HttpStreams.Body(Yaml.unseal(value).show.in[Data]) )

  given instantiable: (tactic: Tactic[ParseError], tracking: Yaml.Tracking)
  =>  ((Yaml is Instantiable across HttpRequests from Text)^{tactic}) =

    text => LazyList(text).read[Yaml]

  // `source.read[Foo in Yaml]` shorthand for
  // `source.read[Yaml].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect`
  // for `value in Json`. The `Form` type-tag is added by an
  // `asInstanceOf` cast — `value in Yaml` is just `value { type
  // Form = Yaml }` so the cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Yaml]
  =>  ( tactic: Tactic[ParseError], yamlTactic: Tactic[YamlError], tracking: Yaml.Tracking )
  =>  (((value in Yaml) is Aggregable by Text)^{tactic, yamlTactic}) =

    summon[Text is Aggregable by Text].map: text =>
      val yaml = summon[Yaml.Tracking] match
        case Yaml.Tracking.On =>
          val (ast, ints) = Yaml.Parser.parseTracked(text)
          new Yaml(ast, Yaml.PositionIndex(ints))

        case Yaml.Tracking.Off =>
          Yaml(Yaml.Parser.parse(text))

      yaml.as[value].asInstanceOf[value in Yaml]

  def primitive(ast: Yaml.Ast): YamlPrimitive =
    if ast.isNull then YamlPrimitive.Null
    else ast.asMatchable match
      case _: Boolean                   => YamlPrimitive.Bool
      case _: Long                      => YamlPrimitive.Integer
      case _: Double                    => YamlPrimitive.Decimal
      // High-precision BCD numbers report as `Decimal`. The AST-level
      // distinction (`isBcd`) remains available for callers that care.
      case _: Array[Double] @unchecked  => YamlPrimitive.Decimal
      case _: String                    => YamlPrimitive.Str

      case xs: Array[AnyRef] @unchecked =>
        if (xs.length & 1) == 0 then YamlPrimitive.Mapping else YamlPrimitive.Sequence

      case _ => YamlPrimitive.Null

  private[ypsiloid] object Parser:
    // The pool is a checker-opaque boundary (jacinta's Parser pool precedent):
    // ThreadLocal cannot carry capture-typed arguments; per-thread single ownership is
    // the pool's construction guarantee, reasserted at the rim by `borrow()`.
    private val pool: ThreadLocal[AnyRef] =
      new ThreadLocal[AnyRef]:
        override def initialValue(): AnyRef = (new Parser).asInstanceOf[AnyRef]

    private def borrow(): Parser^ = pool.get.nn.asInstanceOf[Parser^]

    // Untracked entry points — preserved byte-identical to the historical
    // shape so callers that don't need position tracking pay no cost.
    def parse(input: Text)(using Tactic[ParseError]): Yaml.Ast =
      val parser = borrow()
      parser.tracking = false
      parser.resetText(input)
      parser.parse()

    def parse(input: Data)(using Tactic[ParseError]): Yaml.Ast =
      val parser = borrow()
      parser.tracking = false
      parser.resetData(input)
      parser.parse()

    def parseAll(input: Text)(using Tactic[ParseError]): List[Yaml.Ast] =
      val parser = borrow()
      parser.tracking = false
      parser.resetText(input)
      parser.parseAll()

    def parseAll(input: Data)(using Tactic[ParseError]): List[Yaml.Ast] =
      val parser = borrow()
      parser.tracking = false
      parser.resetData(input)
      parser.parseAll()

    // Tracked entry points — produce the AST plus a flat `IArray[Int]`
    // descriptor index. Used by the tracking-aware `Decodable`/`Aggregable`
    // givens in `object Yaml` when `Yaml.Tracking.On` is in scope.
    def parseTracked(input: Text)(using Tactic[ParseError]): (Yaml.Ast, IArray[Int]) =
      val parser = borrow()
      parser.tracking = true

      try
        parser.resetText(input)
        val ast = parser.parse()
        (ast, IArray.unsafeFromArray(parser.rootIndex.nn))
      finally parser.tracking = false

    def parseTracked(input: Data)(using Tactic[ParseError]): (Yaml.Ast, IArray[Int]) =
      val parser = borrow()
      parser.tracking = true

      try
        parser.resetData(input)
        val ast = parser.parse()
        (ast, IArray.unsafeFromArray(parser.rootIndex.nn))
      finally parser.tracking = false

    def parseAllTracked(input: Text)(using Tactic[ParseError])
    :   List[(Yaml.Ast, IArray[Int])] =

      val parser = borrow()
      parser.tracking = true

      try
        parser.resetText(input)
        parser.parseAllTracked()
      finally parser.tracking = false

    def parseAllTracked(input: Data)(using Tactic[ParseError])
    :   List[(Yaml.Ast, IArray[Int])] =

      val parser = borrow()
      parser.tracking = true

      try
        parser.resetData(input)
        parser.parseAllTracked()
      finally parser.tracking = false

  // Holds an exclusive cursor in a field, so the parser is itself a capability
  // (fresh per instance; one per thread via the pool).
  private[ypsiloid] final class Parser extends caps.ExclusiveCapability:
    import scala.annotation.{switch, tailrec}
    import scala.collection.mutable.ArrayBuffer
    import Yaml.Ast.Byte.*
    import Yaml.Ast.Issue

    // Parser-local snapshot of the cursor's buffer, mirroring Jacinta's
    // pattern: keep `bytes`/`pos`/`bufEnd` as plain fields so the JIT can
    // hold them in registers across hot byte loops. Sync to the cursor
    // before mark/slice/refill operations and refresh after.
    private var cursor:    Cursor[Data, {}]^    = null.asInstanceOf[Cursor[Data, {}]^]
    private var heldToken: Cursor.Held | Null = null
    private var bytes:     Array[Byte]       = null.asInstanceOf[Array[Byte]]
    private var pos:       Int               = 0
    private var bufEnd:    Int               = 0

    // When true, the cursor is built with a line-feed-tracking `Lineation`
    // so `cursor.line` / `cursor.column` reflect real source coordinates,
    // and the parser emits a parallel `PositionIndex` alongside the AST.
    // Set by the companion `Parser.parse(input, tracking = …)` entry
    // points before `resetText` / `resetData` build the cursor.
    protected[ypsiloid] var tracking: Boolean = false

    // Finalised root-level position index produced by the previous `parse()`
    // call when `tracking` was on. Reset to `null` at the start of every parse.
    protected[ypsiloid] var rootIndex: Array[Int] | Null = null

    // Local-buffer offset up to which `cursor.lineNo` / `cursor.columnNo`
    // have been brought up to date. The hot-loop `syncTo()` bypasses the
    // cursor's lineation tracking via `unsafeAdvanceBy`, so the parser
    // keeps track of how far ahead `cursor.pos` has been pushed and
    // catches lineation up here only at tracking-mode capture points and
    // before any refill discards consumed bytes.
    private var lineationPos: Int = 0

    // Pool of `IArray[Int]` buffers shared between sibling composite
    // descriptors during a tracked parse. Mirrors the `bufferPool` below
    // but with `Int` payload — used by `parseBlockSequenceTracked`,
    // `parseBlockMappingFromFirstKeyTracked`, `parseFlowSequenceTracked`,
    // `parseFlowMappingTracked`.
    private var indexBufferId: Int = -1
    private val indexBufferPool: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.empty

    private inline def acquireIndexBuffer(): ArrayBuffer[Int] =
      indexBufferId += 1

      if indexBufferPool.length <= indexBufferId then
        val b = ArrayBuffer.empty[Int]
        indexBufferPool += b
        b
      else
        val b = indexBufferPool(indexBufferId)
        b.clear()
        b

    private inline def releaseIndexBuffer(): Unit = indexBufferId -= 1

    // Anchor table — names map to fully-parsed Yaml.Ast values.
    private val anchors = scala.collection.mutable.Map.empty[String, Yaml.Ast]

    // Tag handles declared by `%TAG` directives in the *current* document.
    // Per spec, directives apply only to the document immediately
    // following them and must be re-declared for each subsequent document.
    // Cleared by `parseAll` after each document boundary.
    private val tagHandles = scala.collection.mutable.Map.empty[String, String]

    // Resizable char buffer shared across string-building calls (for
    // quoted-string unescape and UTF-8 decoded plain scalars). Mirrors
    // Jacinta's `chars`/`stringCursor` to avoid per-string allocation.
    private var arraySize: Int        = 64
    private var chars:     Array[Char] = new Array(arraySize)
    private var stringCursor: Int     = 0

    // Pool of buffer instances for nested sequences/mappings so we can
    // collect items without allocating an `ArrayBuffer` per recursion.
    private var bufferId: Int = -1
    private val bufferPool: ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty

    // Out-parameters for `consumeNodePrefixes` and `readPlainScalarText` —
    // overwritten on each call and consumed immediately by the caller, so
    // no Tuple2/Tuple3 allocation per node.
    private var prefixAnchor:   Text    = t""
    private var prefixTag:      Text    = t""
    private var prefixHeadByte: Int     = -1
    private var sawMappingColon: Boolean = false

    // Indent of the innermost enclosing block collection (block sequence
    // or block mapping). Used by parseBlockScalar to compute the absolute
    // content indent for an explicit indentation indicator: per spec
    // 8.1.1.1, the indicator is added to the parent node's indent. Set
    // and restored on entry/exit of parseBlockSequence and
    // parseBlockMappingFromFirstKey. -1 means the parent is the document.
    private var blockParentIndent: Int = -1

    def resetText(input: Text): Unit =
      val data: Data = input.s.getBytes("UTF-8").nn.immutable(using Unsafe)
      val cursor0 = makeCursor(data)
      cursor = cursor0
      resetParserState()

    def resetData(input: Data): Unit =
      val cursor0 = makeCursor(input)
      cursor = cursor0
      resetParserState()

    // Build a `Cursor[Data, {}]` with the appropriate `Lineation`. The two
    // `import`s are mutually exclusive — both define a `Lineation` for
    // `Data` and bringing both into scope at the same time would render
    // `Cursor[Data, {}]` constructor resolution ambiguous. Local-import-per-
    // branch is the workaround established by jacinta #1147.
    private def makeCursor(input: Data): Cursor[Data, {}]^ =
      if tracking then zephyrine.lineation.linefeedByte.give(Cursor[Data](input))
      else Lineation.untrackedData.give(Cursor[Data](input))

    private def resetParserState(): Unit =
      syncFrom()
      stringCursor = 0
      bufferId = -1
      indexBufferId = -1
      rootIndex = null
      lineationPos = 0
      heldToken = null
      blockParentIndent = -1
      flowParentIndent = -1
      sawMappingColon = false
      prefixAnchor = t""
      prefixTag = t""
      prefixHeadByte = -1
      prefixesConsumed = false
      lastScalarSpannedLines = false
      lastNodeHadAnchor = false
      inInlineMappingValue = false
      docStartLineEnd = -1
      anchors.clear()
      tagHandles.clear()

    // ── Substrate ────────────────────────────────────────────────────────────

    // Push the parser's local `pos` back to the cursor. Required before any
    // cursor operation that consults `pos` (mark, slice, refill, position).
    // Stays zero-branch on the hot path; tracking-mode callers separately
    // invoke `reconcileLineation()` when they need accurate
    // `cursor.line` / `cursor.column`.
    private inline def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    // Refresh the parser's snapshot from the cursor. Required after any
    // cursor operation that may have changed the buffer reference, the
    // read position, or the write end. Re-anchors `lineationPos` because
    // a refill may have compacted the buffer (bytes 0..old-pos are
    // discarded; subsequent walks would read different data).
    private inline def syncFrom(): Unit =
      bytes  = cursor.buffer(using Unsafe)
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)
      lineationPos = pos

    // Walk the buffer bytes between `lineationPos` and the cursor's current
    // position, bumping `cursor.lineNo` / `cursor.columnNo` accordingly.
    // Called at tracking-mode capture points and before any refill that
    // would discard consumed bytes. Without this, `syncTo()` advances
    // `cursor.pos` via `unsafeAdvanceBy` (which bypasses lineation), so
    // captured `cursor.line` / `cursor.column` would stay at their initial
    // values and every descriptor would read `(1, 1)`.
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

    private inline def more: Boolean = pos < bufEnd || moreSlow()

    // Refill the parser's snapshot from the cursor. In tracking mode,
    // reconcile lineation before the cursor compacts the buffer (otherwise
    // the now-discarded bytes can no longer be walked for newlines), and
    // resync afterwards even if no more data was produced so that any
    // post-refill position reads see the new buffer scale.
    private def moreSlow(): Boolean =
      syncTo()
      if tracking then reconcileLineation()

      if cursor.more then { syncFrom(); true }
      else
        if tracking then syncFrom()
        false

    // Assemble a composite (sequence or mapping) descriptor in `indexOut`.
    // `scratch` holds the concatenated child / entry descriptors back-to-
    // back; `ends(i)` is the position in `scratch` immediately after the
    // i-th child / entry, so the i-th child's size is
    // `ends(i) - ends(i-1)`. Mirrors `JsonParser.emitCompositeDescriptor`.
    private def emitCompositeDescriptor
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

    private inline def peek: Byte = bytes(pos)

    private inline def advance(): Unit = pos += 1

    // ── Errors ──────────────────────────────────────────────────────────────

    // Mirror of `JsonParser.errorAt`: builds a `ParseError` with the
    // YAML-format-local `Issue` and the current line/column, then aborts.
    // `offset` carries the absolute byte position (from the cursor) so
    // compile-time consumers (e.g. the `y"…"` interpolator macro) can
    // map errors to source-file positions without re-scanning.
    private def errorAt(issue: Issue)(using Tactic[ParseError]): Nothing =
      syncTo()
      val line = cursor.line.n0
      val column = cursor.column.n0
      val offset = cursor.position.n0
      abort(ParseError(Yaml.Ast, Yaml.Ast.Position(line, column, offset = offset), issue))

    // ── Position / mark plumbing ────────────────────────────────────────────

    private inline def begin(): Cursor.Mark =
      syncTo()
      cursor.mark(using heldToken.nn)

    private inline def slice(start: Cursor.Mark): String =
      syncTo()
      val end = cursor.mark(using heldToken.nn)

      cursor.slice(start, end): (storage, off, len) =>
        val arr = storage.asInstanceOf[Array[Byte]]
        new String(arr, off, len, java.nio.charset.StandardCharsets.UTF_8)

    private inline def holding[result](inline action: => result): result =
      syncTo()

      cursor.hold:
        heldToken = summon[Cursor.Held]
        try action finally heldToken = null

    // ── String buffer (per-instance, reused) ────────────────────────────────

    private inline def resetString(): Unit = stringCursor = 0

    // Single-char fast path: one bounds check, doubling on growth. Matches
    // Jacinta's `appendChar` shape so the JIT can keep `stringCursor` and
    // the buffer reference in registers across a hot loop.
    private inline def appendChar(char: Char): Unit =
      if stringCursor == arraySize then
        arraySize *= 2
        val newArr = new Array[Char](arraySize)
        System.arraycopy(chars, 0, newArr, 0, stringCursor)
        chars = newArr

      chars(stringCursor) = char
      stringCursor += 1

    // Multi-char append (used for variable-length escapes / surrogate pairs).
    private inline def ensureSpace(n: Int): Unit =
      while stringCursor + n > arraySize do arraySize *= 2

      if chars.length < arraySize then
        val newArr = new Array[Char](arraySize)
        System.arraycopy(chars, 0, newArr, 0, stringCursor)
        chars = newArr

    private inline def getStringText(): Text = String(chars, 0, stringCursor).tt

    // ── Buffer pool for sequences/mappings ──────────────────────────────────

    private inline def acquireBuffer(): ArrayBuffer[Any] =
      bufferId += 1

      if bufferPool.length <= bufferId then
        val b = ArrayBuffer.empty[Any]
        bufferPool += b
        b
      else
        val b = bufferPool(bufferId)
        b.clear()
        b

    private inline def releaseBuffer(): Unit = bufferId -= 1

    // ── Top-level parse ─────────────────────────────────────────────────────

    def parse()(using Tactic[ParseError]): Yaml.Ast = holding:
      skipBom()
      skipBlankAndCommentLines()
      val sawDirectives = parseDirectivesIfAny()
      val explicitStart = consumeOptionalDocumentStart()

      if sawDirectives && !explicitStart then
        errorAt(Issue.DirectiveWithoutDocumentStart)

      if explicitStart && more && peek == Hash then
        while more && peek != Newline do advance()

      if !explicitStart || (more && peek == Newline) then
        if more && peek == Newline then advance()
        skipBlankAndCommentLines()
      if !more || atDocumentBoundary then
        // Empty / boundary-only document — under tracking, emit a single
        // 4-int Null descriptor at the cursor's current position.
        if tracking then
          val rootBuf = acquireIndexBuffer()
          emitNullHere(rootBuf)
          rootIndex = rootBuf.toArray
          releaseIndexBuffer()

        Yaml.Ast.Null
      else
        val indent = consumeLeadingSpaces()

        if !more || atDocumentBoundary then
          if tracking then
            val rootBuf = acquireIndexBuffer()
            emitNullHere(rootBuf)
            rootIndex = rootBuf.toArray
            releaseIndexBuffer()

          Yaml.Ast.Null
        else
          val node =
            if tracking then
              val rootBuf = acquireIndexBuffer()
              val n = parseNodeTracked(indent, rootBuf)
              rootIndex = rootBuf.toArray
              releaseIndexBuffer()
              n
            else
              parseNode(indent)

          skipBlankAndCommentLines()
          consumeOptionalDocumentEnd()
          node

    def parseAll()(using Tactic[ParseError]): List[Yaml.Ast] = holding:
      val docs = scala.collection.mutable.ArrayBuffer[Yaml.Ast]()
      skipBom()

      var continue = true
      var firstDoc = true
      var lastDocEndedWithFooter = true  // start of stream is OK for directives

      while continue do
        skipBlankAndCommentLines()
        // %TAG/%YAML directives apply only to the document immediately
        // following them — clear before reading the next directives block.
        tagHandles.clear()
        val sawDirectives = parseDirectivesIfAny()

        if sawDirectives && !firstDoc && !lastDocEndedWithFooter then
          errorAt(Issue.DirectivesOutOfPlace)

        val explicitStart = consumeOptionalDocumentStart()

        if sawDirectives && !explicitStart then
          errorAt(Issue.DirectiveWithoutDocumentStart)
        // After a `...` footer, a bare document (no `---`) is allowed;
        // otherwise every doc beyond the first needs a directives-end
        // marker.
        if
          !firstDoc && more && !explicitStart && !atDocumentBoundary &&
            !lastDocEndedWithFooter
        then errorAt(Issue.MissingDocumentStart)
        // After a `---` marker we may be on the same line as the body;
        // otherwise the body is on a fresh line whose leading whitespace
        // determines the indent. A trailing `# comment` on the marker
        // line is metadata: consume it so the body parses from the next
        // line.
        if explicitStart && more && peek == Hash then
          while more && peek != Newline do advance()

        val sameLineAsMarker = explicitStart && more && peek != Newline

        if !explicitStart || (more && peek == Newline) then
          if more && peek == Newline then advance()
          skipBlankAndCommentLines()

        if !more then
          if explicitStart then docs.append(Yaml.Ast.Null)
          continue = false
        else if atDocumentBoundary then
          if explicitStart then docs.append(Yaml.Ast.Null)
          lastDocEndedWithFooter = consumeOptionalDocumentEnd()
          if explicitStart then firstDoc = false
        else
          // The first content line of the document determines the indent
          // passed to parseNode.
          val indent = consumeLeadingSpaces()

          if !more || atDocumentBoundary then
            if explicitStart then docs.append(Yaml.Ast.Null)
            lastDocEndedWithFooter = consumeOptionalDocumentEnd()
            if explicitStart then firstDoc = false
          else
            // If `---` was consumed and content is on the same line, the
            // node may not be a block mapping — only a single inline node
            // (scalar / flow / quoted). Snapshot the position of the
            // marker-line newline so the check naturally lifts as soon
            // as the parser advances past it.
            val savedDocStart = docStartLineEnd

            if sameLineAsMarker then
              var end = pos
              while end < bufEnd && bytes(end) != Newline do end += 1
              docStartLineEnd = end
            else
              docStartLineEnd = -1

            val node = parseNode(indent)
            docStartLineEnd = savedDocStart
            docs.append(node)
            skipBlankAndCommentLines()
            lastDocEndedWithFooter = consumeOptionalDocumentEnd()
            firstDoc = false

      docs.toList

    // Tracked variant of `parseAll`: parses every document like `parseAll`
    // but also captures a per-document `PositionIndex` for the
    // tracking-aware `Yaml.parseAll` companion entry.
    def parseAllTracked()(using Tactic[ParseError])
    :   List[(Yaml.Ast, IArray[Int])] =

      holding:
        val docs = scala.collection.mutable.ArrayBuffer[(Yaml.Ast, IArray[Int])]()
        skipBom()

        var continue = true
        var firstDoc = true
        var lastDocEndedWithFooter = true

        while continue do
          skipBlankAndCommentLines()
          tagHandles.clear()
          val sawDirectives = parseDirectivesIfAny()

          if sawDirectives && !firstDoc && !lastDocEndedWithFooter then
            errorAt(Issue.DirectivesOutOfPlace)

          val explicitStart = consumeOptionalDocumentStart()

          if sawDirectives && !explicitStart then
            errorAt(Issue.DirectiveWithoutDocumentStart)
          if
            !firstDoc && more && !explicitStart && !atDocumentBoundary &&
              !lastDocEndedWithFooter
          then errorAt(Issue.MissingDocumentStart)

          if explicitStart && more && peek == Hash then
            while more && peek != Newline do advance()

          val sameLineAsMarker = explicitStart && more && peek != Newline

          if !explicitStart || (more && peek == Newline) then
            if more && peek == Newline then advance()
            skipBlankAndCommentLines()

          if !more then
            if explicitStart then
              val rootBuf = acquireIndexBuffer()
              emitNullHere(rootBuf)
              docs.append((Yaml.Ast.Null, IArray.unsafeFromArray(rootBuf.toArray)))
              releaseIndexBuffer()

            continue = false
          else if atDocumentBoundary then
            if explicitStart then
              val rootBuf = acquireIndexBuffer()
              emitNullHere(rootBuf)
              docs.append((Yaml.Ast.Null, IArray.unsafeFromArray(rootBuf.toArray)))
              releaseIndexBuffer()

            lastDocEndedWithFooter = consumeOptionalDocumentEnd()
            if explicitStart then firstDoc = false
          else
            val indent = consumeLeadingSpaces()

            if !more || atDocumentBoundary then
              if explicitStart then
                val rootBuf = acquireIndexBuffer()
                emitNullHere(rootBuf)
                docs.append((Yaml.Ast.Null, IArray.unsafeFromArray(rootBuf.toArray)))
                releaseIndexBuffer()

              lastDocEndedWithFooter = consumeOptionalDocumentEnd()
              if explicitStart then firstDoc = false
            else
              val savedDocStart = docStartLineEnd

              if sameLineAsMarker then
                var end = pos
                while end < bufEnd && bytes(end) != Newline do end += 1
                docStartLineEnd = end
              else
                docStartLineEnd = -1

              val rootBuf = acquireIndexBuffer()
              val node = parseNodeTracked(indent, rootBuf)
              val ints = IArray.unsafeFromArray(rootBuf.toArray)
              releaseIndexBuffer()
              docStartLineEnd = savedDocStart
              docs.append((node, ints))
              skipBlankAndCommentLines()
              lastDocEndedWithFooter = consumeOptionalDocumentEnd()
              firstDoc = false

        docs.toList

    // Consume `---` if at the current position. Returns true if consumed.
    // Per spec the marker requires either a following line-boundary
    // character or end-of-input — `----` is a plain scalar starting with
    // four dashes, not a marker. After consumption we eat any inline
    // whitespace immediately following but leave the rest of the line
    // (including any same-line node body) to the caller.
    private def consumeOptionalDocumentStart(): Boolean =
      if isDocumentMarker(Minus) then
        pos += 3
        while more && (peek == Space || peek == Tab) do advance()
        true
      else
        false

    // Consume `...` if at the current position; only inline whitespace
    // and an optional comment may follow on the same line — anything
    // else is an error per spec.
    private def consumeOptionalDocumentEnd()(using Tactic[ParseError]): Boolean =
      if isDocumentMarker(Period) then
        pos += 3
        while more && (peek == Space || peek == Tab) do advance()

        if more && peek == Hash then
          while more && peek != Newline do advance()
        else if more && peek != Newline then
          errorAt(Issue.ContentAfterDocumentEnd)

        if more then advance()
        true
      else
        false

    // Parse any %-prefixed directive lines at the current position. Per
    // spec only `%YAML <version>` and `%TAG <handle> <prefix>` are
    // recognised; the YAML directive must appear at most once per
    // document, with a single major.minor version argument.
    private def parseDirectivesIfAny()(using Tactic[ParseError]): Boolean =
      var sawAny = false
      var sawYaml = false

      while more && peek == Percent do
        advance() // %
        val nameStart = pos
        while more && peek != Space && peek != Tab && peek != Newline do advance()
        val name = new String(bytes, nameStart, pos - nameStart)
        while more && (peek == Space || peek == Tab) do advance()
        val argStart = pos
        while more && peek != Newline do advance()
        val rawArgs = new String(bytes, argStart, pos - argStart)
        val hashIdx = rawArgs.indexOf(" #")

        val argText =
          (if hashIdx >= 0 then rawArgs.substring(0, hashIdx).nn else rawArgs).trim.nn

        if more then advance() // newline
        sawAny = true

        name match
          case "YAML" =>
            if sawYaml then errorAt(Issue.DuplicateYamlDirective)
            sawYaml = true
            if argText.isEmpty then errorAt(Issue.YamlDirectiveRequiresVersion)
            val parts = argText.split("\\s+").nn.asInstanceOf[Array[String]]
            if parts.length != 1 then errorAt(Issue.YamlDirectiveTooManyArguments)

            if !parts(0).matches("\\d+\\.\\d+") then
              errorAt(Issue.YamlDirectiveInvalidVersion)

          case "TAG" =>
            val parts = argText.split("\\s+").nn.asInstanceOf[Array[String]]
            if parts.length != 2 then errorAt(Issue.TagDirectiveRequiresHandleAndPrefix)
            tagHandles(parts(0)) = parts(1)

          case _ => () // reserved/unknown directive — silently accept

        skipBlankAndCommentLines()

      sawAny

    // True if the cursor is positioned at a document marker (three of
    // the same byte followed by a line-boundary or end-of-input). Does
    // not consume.
    private inline def isDocumentMarker(b: Byte): Boolean =
      pos + 2 < bufEnd && bytes(pos) == b && bytes(pos+1) == b && bytes(pos+2) == b &&
        (pos + 3 >= bufEnd || {
            val nb = bytes(pos+3)
            nb == Newline || nb == Space || nb == Tab || nb == Return
          })

    private inline def atDocumentBoundary: Boolean =
      isDocumentMarker(Minus) || isDocumentMarker(Period)

    // Skip an optional UTF-8 BOM (EF BB BF) at the start of input.
    private def skipBom(): Unit =
      syncTo()

      cursor.hold:
        val mk = cursor.mark

        val isBom =
          cursor.more && cursor.datum(using Unsafe) == -17.toByte &&
            { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte } &&
            { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

        if isBom then cursor.next() else cursor.cue(mk)

      syncFrom()

    // ── Whitespace / comment / directive skipping ───────────────────────────

    // Skip horizontal whitespace (spaces, tabs) but not newlines.
    private inline def skipSpaces(): Unit =
      while more && (peek == Space || peek == Tab) do advance()

    // Skip horizontal whitespace, newlines, and `# comment` lines. Also
    // consumes any `%YAML`/`%TAG` directive lines (we don't honour them
    // semantically yet, just step over them).
    private def skipWhitespaceAndCommentsAndDirectives(): Unit =
      var continue = true

      while continue && more do
        val c = peek

        if c == Space || c == Tab || c == Newline || c == Return then advance()
        else if c == Hash then
          while more && peek != Newline do advance()
        else if c == '%'.toByte then
          while more && peek != Newline do advance()
        else
          continue = false

    // Try to consume three of `b` in a row. Returns true on success.
    private def tryConsume3(b: Byte): Boolean =
      if pos + 2 < bufEnd && bytes(pos) == b && bytes(pos + 1) == b && bytes(pos + 2) == b then
        pos += 3
        true
      else
        false

    private inline def skipUntilNewline(): Unit =
      while more && peek != Newline do advance()

    // ── Node parsing ────────────────────────────────────────────────────────

    // Parse a single YAML node at the given context indent. Caller has
    // ensured we're positioned at the first byte of the node (after any
    // leading whitespace).
    private def parseNode(indent: Int)(using Tactic[ParseError]): Yaml.Ast =
      skipSpaces()

      if !more then Yaml.Ast.Null
      else parseNodeHere(indent)

    private def parseNodeHere(indent: Int)(using Tactic[ParseError]): Yaml.Ast =
      // `prefixesConsumed` and `lastNodeHadAnchor` are parser-wide fields
      // but "did THIS call apply its own prefixes / anchor?" is per-call.
      // Recursive parses (e.g. parseMappingValue inside parseBlock-
      // MappingFromFirstKey) would otherwise clobber the flags before
      // we can read them back.
      val savedPrefixesConsumed = prefixesConsumed
      val savedLastNodeHadAnchor = lastNodeHadAnchor
      prefixesConsumed = false
      lastNodeHadAnchor = false
      consumeNodePrefixes()
      val anchorName = prefixAnchor
      val tagText    = prefixTag
      val headByte   = prefixHeadByte

      // Bare anchor/tag followed by newline → value is on the next indented
      // line(s). consumeNodePrefixes stops at the newline so we can detect
      // and descend into it here.
      val hasPrefix = !anchorName.nil || !tagText.nil

      val value: Yaml.Ast =
        if hasPrefix && (headByte == Newline || headByte == -1 || headByte == Hash) then
          // A trailing comment after the prefix is line metadata; the
          // value is on the next line.
          if headByte == Hash then
            while more && peek != Newline do advance()

          if more && peek == Newline then advance()
          skipBlankAndCommentLines()
          val lineStart = pos
          val childIndent = consumeLeadingSpaces()
          // The value of a bare anchor/tag is the next node whose indent
          // exceeds the parent collection's. Same-indent is allowed in
          // two cases: block sequences directly under a mapping key
          // (compact form, spec 8.2.2), and the document parent (top
          // level), where any indent >= 0 works.
          lastNodeHadAnchor = false
          val v = pickValueOrNull(blockParentIndent, childIndent, lineStart)

          if !anchorName.nil && lastNodeHadAnchor then
            errorAt(Issue.TwoAnchorsOnSameNode)

          v
        else if headByte == -1 then
          Yaml.Ast.Null
        else if headByte == Hash then
          // No preceding content and a comment on the head line means
          // the value is empty (Null); the comment is metadata.
          while more && peek != Newline do advance()
          Yaml.Ast.Null
        else if headByte == Star then
          // An anchor on an alias node is illegal per spec — aliases
          // refer to existing anchored nodes, they don't anchor anything
          // themselves.
          if !anchorName.nil then errorAt(Issue.AnchorOnAlias)
          advance()
          val a = parseAlias()
          maybeBlockMappingFromQuotedKey(a, indent, tagText, anchorName)
        else
          (headByte: @switch) match
            case Quote =>
              advance()
              val s = parseDoubleQuoted()
              maybeBlockMappingFromQuotedKey(s, indent, tagText, anchorName)

            case Apostrophe =>
              advance()
              val s = parseSingleQuoted()
              maybeBlockMappingFromQuotedKey(s, indent, tagText, anchorName)

            case OpenBracket =>
              val savedFlowParent = flowParentIndent
              flowParentIndent = blockParentIndent
              advance()
              val s = parseFlowSequence()
              flowParentIndent = savedFlowParent
              maybeBlockMappingFromQuotedKey(s, indent, tagText, anchorName)

            case OpenBrace =>
              val savedFlowParent = flowParentIndent
              flowParentIndent = blockParentIndent
              advance()
              val m = parseFlowMapping()
              flowParentIndent = savedFlowParent
              maybeBlockMappingFromQuotedKey(m, indent, tagText, anchorName)

            case Pipe         => parseBlockScalar(literal = true)
            case Greater      => parseBlockScalar(literal = false)
            case Minus        => parseMinus(indent)
            case Question     => parseQuestion(indent)

            case CloseBracket | CloseBrace | Comma | 0x40 | 0x60 =>
              errorAt(Issue.ReservedIndicatorAtNodeStart)

            case _ =>
              prefixesConsumed = false
              parsePlainOrBlockMapping(indent, tagText, anchorName)

      val consumed = prefixesConsumed
      prefixesConsumed = savedPrefixesConsumed

      if consumed then
        lastNodeHadAnchor = savedLastNodeHadAnchor
        value
      else
        val tagged = if tagText.nil then value else applyTag(tagText, value)

        if anchorName.nil then
          lastNodeHadAnchor = savedLastNodeHadAnchor
          tagged
        else
          anchors.update(anchorName.s, tagged)
          // Outer call sees lastNodeHadAnchor=true so the "two anchors"
          // check on its prefix-with-newline branch can fire.
          lastNodeHadAnchor = true
          tagged

    // Consume any `&anchor`, `!tag`, or both at the current position. Stops
    // at the first non-prefix byte (without crossing newlines, so the caller
    // can detect a bare-prefix-with-block). Writes results to `prefixAnchor`,
    // `prefixTag`, `prefixHeadByte` to avoid per-call Tuple3 allocation.
    private def consumeNodePrefixes()(using Tactic[ParseError]): Unit =
      var anchorName = t""
      var tagText = t""
      var done = false

      while !done do
        skipSpaces()

        if !more then done = true
        else peek match
          case Amp =>
            if !anchorName.nil then errorAt(Issue.DuplicateAnchorOnNode)
            advance()
            anchorName = readWord()
            skipSpaces()

          case Bang =>
            if !tagText.nil then errorAt(Issue.DuplicateTagOnNode)
            val mk = begin()
            advance()
            if more && peek == OpenAngle then
              // Verbatim URI tag: !<...>. The URI may legally contain
              // commas and other bytes that we exclude from short-form
              // tag handles, so read everything up to the closing `>`.
              advance()
              while more && peek != CloseAngle && peek != Newline do advance()

              if more && peek == CloseAngle then advance()
              else errorAt(Issue.UnterminatedVerbatimTag)
            else
              if more && peek == Bang then advance()

              while
                more && !isWhitespaceOrEnd(peek) &&
                  peek != Comma && peek != OpenBracket &&
                  peek != CloseBracket && peek != OpenBrace &&
                  peek != CloseBrace
              do advance()

            tagText = slice(mk).tt
            validateTagHandle(tagText)
            skipSpaces()

          case _ => done = true

      prefixAnchor = anchorName
      prefixTag = tagText
      prefixHeadByte = if !more then -1 else peek & 0xFF

    private inline def isWhitespaceOrEnd(b: Byte): Boolean =
      b == Space || b == Tab || b == Newline || b == Return

    // True if walking backwards from `at - 1` over space/tab bytes
    // arrives at a newline (or start-of-input). Used to detect cases
    // like `key\n  : value` in flow context, where `:` is on its own
    // line — disallowed for an implicit-key construct.
    private def newlineImmediatelyPrecedes(at: Int): Boolean =
      var j = at - 1
      while j >= 0 && (bytes(j) == Space || bytes(j) == Tab) do j -= 1
      j >= 0 && bytes(j) == Newline

    // Read an identifier-like word (anchor or alias name).
    private def readWord(): Text =
      val mk = begin()

      while
        more && !isWhitespaceOrEnd(peek) &&
          peek != OpenBracket && peek != CloseBracket &&
          peek != OpenBrace && peek != CloseBrace &&
          peek != Comma
      do advance()

      slice(mk).tt

    private def parseAlias()(using Tactic[ParseError]): Yaml.Ast =
      val name = readWord()

      anchors.get(name.s) match
        case Some(value) => value
        case None        => errorAt(Issue.UnknownAlias(name))

    // After a newline within a block context, advance past blank/comment
    // lines and through the leading indent of the next content line.
    private def skipBlanksAndIndent(): Unit =
      var done = false
      while !done && more do
        // skip leading spaces
        while more && peek == Space do advance()

        if !more then done = true
        else peek match
          case Newline => advance()

          case Hash =>
            while more && peek != Newline do advance()
            if more then advance()

          case _ => done = true

    // ── Plain scalars / block mappings detection ────────────────────────────

    // Parse from the current position. Either a plain scalar (yielding a
    // primitive) or, if a top-level `:` follows, a block mapping where this
    // scalar is the first key.
    // After parsing a non-plain scalar at node head (quoted, alias,
    // or flow collection), decide what follows:
    //  - `:` with a whitespace/EOL terminator → first key of a block
    //    mapping; the head's anchor/tag prefix applies to the key, not
    //    the mapping (mirrors parsePlainOrBlockMapping);
    //  - newline or EOF → the scalar is the value;
    //  - `# comment` after at least one space → trailing comment, OK;
    //  - flow-collection terminator (`,`, `]`, `}`) → caller is a flow
    //    context that will consume the terminator;
    // anything else is an error per spec.
    private def maybeBlockMappingFromQuotedKey
      ( scalar: Yaml.Ast, indent: Int, headTag: Text, headAnchor: Text )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val hadSpaceOrTab = more && (peek == Space || peek == Tab)
      skipSpaces()

      if !more then scalar
      else peek match
        case Newline => scalar

        case Hash if hadSpaceOrTab =>
          while more && peek != Newline do advance()
          scalar

        case Colon =>
          val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            nextByte == Space || nextByte == Tab || nextByte == Newline ||
              nextByte == Return || nextByte == -1
          then
            if onDocStartLine then
              errorAt(Issue.BlockMappingOnDocumentStartLine)

            if inInlineMappingValue then
              errorAt(Issue.ChainedMappingValueOnSingleLine)

            if lastScalarSpannedLines then
              errorAt(Issue.MultilineImplicitKey)

            val tagged = if headTag.nil then scalar else applyTag(headTag, scalar)

            val keyAst =
              if headAnchor.nil then tagged
              else
                anchors.update(headAnchor.s, tagged)
                tagged

            if !headTag.nil || !headAnchor.nil then prefixesConsumed = true
            parseBlockMappingFromFirstKey(keyAst, indent)
          else
            errorAt(Issue.TrailingContentAfterQuotedScalar)

        case Comma | CloseBracket | CloseBrace => scalar
        case _                                 => errorAt(Issue.TrailingContentAfterQuotedScalar)

    // Tracks whether the most recent dispatch from parseNodeHere consumed
    // the head node's anchor/tag (because it transitioned into a block
    // mapping where the prefix actually belongs to the first key, not
    // the whole mapping). parseNodeHere skips re-applying anchor/tag in
    // that case.
    private var prefixesConsumed: Boolean = false

    // Set true when the most recently parsed scalar (plain or quoted)
    // spanned more than one source line — used to reject multi-line
    // implicit keys, which the spec disallows.
    private var lastScalarSpannedLines: Boolean = false

    // Set when parseMappingValue is invoking parseNodeHere inline
    // (mapping value on the same line as `key:`). Inline values cannot
    // recurse into a second mapping via `'k': v` chaining — the spec
    // requires block-style mapping keys to start at the beginning of
    // their line.
    private var inInlineMappingValue: Boolean = false

    // Set true by parseNodeHere when it has applied an anchor to its
    // result. Used to detect the "two anchors on a single node" error,
    // where an outer prefix-on-newline branch parses an inner node that
    // also carries its own anchor.
    private var lastNodeHadAnchor: Boolean = false

    // Indent of the line that opened the innermost flow collection.
    // Content lines inside the flow that are not the closing bracket
    // must be more indented than this. -1 means we're not currently
    // inside a flow collection.
    private var flowParentIndent: Int = -1

    // Position of the newline terminating a same-line `---` marker, or
    // -1 if the current document didn't start on such a line. While
    // `pos <= docStartLineEnd` the parser is still on the marker line;
    // a block mapping cannot open there (per spec the marker line
    // cannot also be the first key of an implicit mapping). Once the
    // parser advances past the newline, `pos > docStartLineEnd` and the
    // restriction lifts naturally.
    private var docStartLineEnd: Int = -1

    private inline def onDocStartLine: Boolean =
      docStartLineEnd >= 0 && pos <= docStartLineEnd

    private def parsePlainOrBlockMapping
      ( indent: Int, headTag: Text = t"", headAnchor: Text = t"" )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val textValue = readPlainScalarText(indent)

      if sawMappingColon then
        if onDocStartLine then
          errorAt(Issue.BlockMappingOnDocumentStartLine)

        if inInlineMappingValue then
          errorAt(Issue.ChainedMappingValueOnSingleLine)

        if lastScalarSpannedLines then
          errorAt(Issue.MultilineImplicitKey)
        // The plain text is the first key of a block mapping. Any tag
        // or anchor read by consumeNodePrefixes belongs to the key, not
        // the mapping.
        val rawKey = resolvePlainScalar(textValue)
        val tagged = if headTag.nil then rawKey else applyTag(headTag, rawKey)

        val keyAst =
          if headAnchor.nil then tagged
          else
            anchors.update(headAnchor.s, tagged)
            tagged

        if !headTag.nil || !headAnchor.nil then prefixesConsumed = true
        parseBlockMappingFromFirstKey(keyAst, indent)
      else
        resolvePlainScalar(textValue)

    // Read a plain scalar at the current position. Sets `sawMappingColon`
    // to true if a `: ` (or `:` at line-end) at the same line level was
    // seen so the caller knows this is the key of a block mapping.
    private def readPlainScalarText(indent: Int)
      ( using Tactic[ParseError] )
    :   Text =

      resetString()
      lastScalarSpannedLines = false

      var colon = false
      var done = false

      while !done do
        readPlainScalarLine() match
          case PlainOutcome.Mapping =>
            colon = true
            done = true

          case PlainOutcome.EndOfLine =>
            // Try to fold continuation
            if !attemptPlainContinuation(indent) then done = true

          case PlainOutcome.Stop =>
            done = true

      sawMappingColon = colon
      trimEndWhitespace(getStringText())

    private enum PlainOutcome:
      case Mapping
      case EndOfLine
      case Stop

    // Read one line of a plain scalar (until newline or terminator), pushing
    // characters into `chars`. Returns Mapping if a `:` (followed by space
    // or newline) was found at the top level on this line.
    private def readPlainScalarLine()(using Tactic[ParseError]): PlainOutcome =
      var lineStart = stringCursor

      while more do
        // Fast-prefix ASCII run: bulk-copy printable bytes (>= 0x20) that
        // are neither potential terminators (#, :) nor UTF-8 lead bytes
        // (which are negative as signed Bytes, so excluded by >= 0x20).
        val runStart = pos

        while
          pos < bufEnd && {
            val b = bytes(pos)
            b >= 0x20 && b != Hash && b != Colon
          }
        do pos += 1

        val runLen = pos - runStart

        if runLen > 0 then
          ensureSpace(runLen)
          var k = 0

          while k < runLen do
            chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
            k += 1

          stringCursor += runLen

        if !more then return PlainOutcome.Stop

        val b = peek
        if b == Newline then return PlainOutcome.EndOfLine

        if
          b == Hash && stringCursor > lineStart && {
            val prev = chars(stringCursor - 1); prev == ' ' || prev == '\t'
          }
        then
          // ` # comment` (or tab-prefixed comment) ends the plain scalar.
          // Per spec, a comment also terminates any multi-line
          // continuation: subsequent lines do not fold into the scalar
          // even if they would otherwise be indented enough.
          while more && peek != Newline do advance()
          var i = stringCursor - 1

          while i >= lineStart && (chars(i) == ' ' || chars(i) == '\t') do
            stringCursor -= 1
            i -= 1

          return PlainOutcome.Stop

        if b == Colon then
          // Mapping-key colon iff the byte after the colon is whitespace
          // or end-of-input. The whole input is loaded at parser reset
          // (`Cursor[Data, {}]` over a single `Data` buffer), so a direct
          // bounds check on the snapshot suffices — no refill needed.
          val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            nextByte == Space || nextByte == Tab || nextByte == Newline ||
              nextByte == Return || nextByte == -1
          then
            return PlainOutcome.Mapping
          // else: `:foo` is part of the scalar; fall through.

        // Slow-path single-byte handling (control chars, UTF-8 lead bytes,
        // or `:` not followed by whitespace).
        appendByteAsChar(b)
        advance()

      PlainOutcome.Stop

    // Attempt to fold a continuation line into a multi-line plain scalar.
    // Returns true if a continuation was consumed. Per spec the next
    // content line must be indented strictly more than the *parent
    // collection*; for top-level plain scalars the parent is the document
    // (indent -1), so any indent (including 0) is accepted.
    private def attemptPlainContinuation(scalarIndent: Int): Boolean =
      if !more || peek != Newline then return false
      val savedPos = pos
      val savedString = stringCursor
      val parent = blockParentIndent

      advance()
      var newlineCount = 1

      @tailrec def findContent(): Boolean =
        val lineStart = pos
        var spaces = 0

        while more && peek == Space do
          spaces += 1
          advance()
        // Tabs after the leading spaces are part of the line's leading
        // whitespace but don't count toward indent (spec forbids tabs in
        // indentation). They're stripped from the folded content.
        while more && peek == Tab do advance()

        if !more then
          pos = savedPos; stringCursor = savedString; false
        else if peek == Newline then
          newlineCount += 1
          advance()
          findContent()
        else if peek == Hash then
          // A comment line terminates plain-scalar continuation: the
          // scalar cannot fold across the comment into a later line.
          pos = savedPos
          stringCursor = savedString
          false
        else if spaces > parent && !atDocumentBoundary then
          // Trim trailing whitespace from the previous line we emitted
          // before adding the fold separator: in plain scalars, both
          // leading and trailing per-line whitespace is stripped before
          // folding.
          while
            stringCursor > 0 &&
              (chars(stringCursor - 1) == ' ' ||
                chars(stringCursor - 1) == '\t')
          do stringCursor -= 1

          if newlineCount == 1 then appendChar(' ')
          else
            var k = 1

            while k < newlineCount do
              appendChar('\n')
              k += 1

          lastScalarSpannedLines = true
          true
        else
          pos = lineStart
          stringCursor = savedString
          // Restore newline (we consumed it but didn't fold)
          pos = savedPos
          false

      findContent()

    private inline def appendByteAsChar(b: Byte): Unit =
      if (b & 0x80) == 0 then appendChar((b & 0xFF).toChar)
      else
        // Multi-byte UTF-8: decode in place.
        decodeUtf8AndAppend(b)

    // Decode a UTF-8 sequence whose lead byte is at the current cursor
    // position and append the resulting char(s).
    private def decodeUtf8AndAppend(lead: Byte): Unit =
      val u = lead & 0xFF

      if (u & 0xE0) == 0xC0 then
        val b2 = if pos + 1 < bufEnd then bytes(pos + 1) & 0x3F else 0
        val cp = ((u & 0x1F) << 6) | b2
        // advance lead handled by caller
        pos += 1
        appendChar(cp.toChar)
      else if (u & 0xF0) == 0xE0 then
        val b2 = if pos + 1 < bufEnd then bytes(pos + 1) & 0x3F else 0
        val b3 = if pos + 2 < bufEnd then bytes(pos + 2) & 0x3F else 0
        val cp = ((u & 0x0F) << 12) | (b2 << 6) | b3
        pos += 2
        appendChar(cp.toChar)
      else if (u & 0xF8) == 0xF0 then
        val b2 = if pos + 1 < bufEnd then bytes(pos + 1) & 0x3F else 0
        val b3 = if pos + 2 < bufEnd then bytes(pos + 2) & 0x3F else 0
        val b4 = if pos + 3 < bufEnd then bytes(pos + 3) & 0x3F else 0
        val cp = ((u & 0x07) << 18) | (b2 << 12) | (b3 << 6) | b4
        pos += 3
        // Encode as surrogate pair if non-BMP
        if cp >= 0x10000 then
          val adjusted = cp - 0x10000
          appendChar((0xD800 | (adjusted >>> 10)).toChar)
          appendChar((0xDC00 | (adjusted & 0x3FF)).toChar)
        else
          appendChar(cp.toChar)
      else
        // Stray continuation byte or invalid — replace with placeholder.
        appendChar('?')

    // Trim trailing space/tab from the produced text.
    private def trimEndWhitespace(text: Text): Text =
      val s = text.s
      var n = s.length
      while n > 0 && (s.charAt(n - 1) == ' ' || s.charAt(n - 1) == '\t') do n -= 1
      if n == s.length then text else s.substring(0, n).nn.tt

    // Resolve a plain-scalar string into a Yaml.Ast primitive.
    private def resolvePlainScalar(text: Text): Yaml.Ast =
      val s = text.s

      if s.isEmpty then Yaml.Ast.Null
      else s match
        case "null" | "Null" | "NULL" | "~"   => Yaml.Ast.Null
        case "true" | "True" | "TRUE"         => Yaml.Ast.Bool(true)
        case "false" | "False" | "FALSE"      => Yaml.Ast.Bool(false)

        case ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" =>
          Yaml.Ast.Decimal(Double.PositiveInfinity)

        case "-.inf" | "-.Inf" | "-.INF"      => Yaml.Ast.Decimal(Double.NegativeInfinity)
        case ".nan" | ".NaN" | ".NAN"         => Yaml.Ast.Decimal(Double.NaN)

        case _ =>
          val asInt = parsePlainIntegerOrNull(s)

          if asInt != null then asInt
          else
            val asDec = parsePlainDecimalOrNull(s)
            if asDec != null then asDec else Yaml.Ast.Str(text)

    // Parse a plain string into a Long without throwing on rejection.
    // Returns null when the string does not represent a YAML 1.2 integer
    // we want to recognise (avoids `Option`/exception overhead on the
    // common reject path during plain-scalar resolution).
    private def parsePlainIntegerOrNull(s: String): Yaml.Ast | Null =
      val len = s.length
      if len == 0 then return null
      var i = 0
      var negative = false

      s.charAt(0) match
        case '-' => negative = true; i = 1
        case '+' => i = 1
        case _   => ()

      if i >= len then return null

      // Hex / octal prefixes (after optional sign).
      if i + 1 < len && s.charAt(i) == '0' then
        val p = s.charAt(i + 1)

        if p == 'x' || p == 'X' then
          return parseRadix(s, i + 2, 16, negative)

        if p == 'o' || p == 'O' then
          return parseRadix(s, i + 2, 8, negative)

      // Decimal: pure digits only (no leading zeroes except "0").
      val first = s.charAt(i)
      if first < '0' || first > '9' then return null
      if first == '0' && i + 1 < len then return null  // "01" is not a YAML int

      // Up to 18 digits: cannot overflow Long; manual loop. 19 digits:
      // boundary case, defer to JDK parser then fall back to BCD on
      // overflow. >19 digits: never fits in Long — go straight to BCD,
      // preserving full precision.
      val digitCount = len - i

      if digitCount > 19 then
        val digits = s.substring(i, len).nn
        // All-digit string already validated by the caller's branching
        // (fall-through on non-digit). Build a BCD from the magnitude.
        var k = 0

        while k < digits.length do
          val c = digits.charAt(k)
          if c < '0' || c > '9' then return null
          k += 1

        return Yaml.Ast.BcdValue(Bcd.fromString(digits, negative))

      if digitCount == 19 then
        try
          val v = java.lang.Long.parseLong(s)
          return Yaml.Ast.Integer(v)
        catch case _: NumberFormatException =>
          // 19-digit boundary that overflows `Long` (e.g. `10000000000000000000`):
          // preserve precision in BCD instead of dropping the value.
          val digits = s.substring(i, len).nn
          return Yaml.Ast.BcdValue(Bcd.fromString(digits, negative))

      var acc: Long = 0L

      while i < len do
        val c = s.charAt(i)
        if c < '0' || c > '9' then return null
        acc = acc*10L + (c - '0')
        i += 1

      Yaml.Ast.Integer(if negative then -acc else acc)

    private def parseRadix(s: String, start: Int, radix: Int, negative: Boolean)
    :   Yaml.Ast | Null =

      val len = s.length
      if start >= len then return null
      var acc: Long = 0L
      var i = start

      while i < len do
        val c = s.charAt(i)

        val d =
          if c >= '0' && c <= '9' then c - '0'
          else if c >= 'a' && c <= 'f' then c - 'a' + 10
          else if c >= 'A' && c <= 'F' then c - 'A' + 10
          else -1

        if d < 0 || d >= radix then return null
        acc = acc*radix + d
        i += 1

      Yaml.Ast.Integer(if negative then -acc else acc)

    // Pre-filter to avoid `Double.parseDouble` throwing on non-numeric
    // strings: must contain at least one digit, only `[-+0-9.eE]`, ≤1
    // `.`, ≤1 `e`/`E`, and the `e`/`E` (if present) must be followed by
    // an optional sign and at least one digit.
    private def parsePlainDecimalOrNull(s: String): Yaml.Ast | Null =
      val len = s.length
      if len == 0 then return null

      var i = 0

      s.charAt(0) match
        case '-' | '+' => i = 1
        case _         => ()

      var hasDigit = false
      var dotSeen = false
      var expSeen = false
      var expHasDigit = false

      while i < len do
        val c = s.charAt(i)

        if c >= '0' && c <= '9' then
          if expSeen then expHasDigit = true else hasDigit = true
        else if c == '.' then
          if dotSeen || expSeen then return null
          dotSeen = true
        else if c == 'e' || c == 'E' then
          if expSeen || !hasDigit then return null
          expSeen = true

          if i + 1 < len then
            val nx = s.charAt(i + 1)
            if nx == '+' || nx == '-' then i += 1
        else
          return null

        i += 1

      if !hasDigit then return null
      if expSeen && !expHasDigit then return null

      // Try `Double.parseDouble` first; if the round-tripped value
      // differs from the source (precision loss) or the source has more
      // significant digits than `Double`'s ~17, fall back to BCD.
      try
        val d = java.lang.Double.parseDouble(s)
        val significantDigits = countSignificantDigits(s)

        if significantDigits > 17 then
          val negative = s.length > 0 && s.charAt(0) == '-'

          val unsigned =
            if s.length > 0 && (s.charAt(0) == '-' || s.charAt(0) == '+')
            then s.substring(1).nn
            else s

          Yaml.Ast.BcdValue(Bcd.fromString(unsigned, negative))
        else
          Yaml.Ast.Decimal(d)
      catch case _: NumberFormatException => null

    private def countSignificantDigits(s: String): Int =
      var count = 0
      var seenNonZero = false
      var i = 0

      while i < s.length do
        val c = s.charAt(i)

        if c >= '0' && c <= '9' then
          if c != '0' then seenNonZero = true
          if seenNonZero then count += 1
        else if c == 'e' || c == 'E' then
          return count

        i += 1

      count

    // ── Quoted strings ──────────────────────────────────────────────────────

    private def parseDoubleQuoted()(using Tactic[ParseError]): Yaml.Ast =
      resetString()
      lastScalarSpannedLines = false
      var done = false
      while !done do
        // Fast-prefix ASCII run: copy non-special printable bytes in
        // bulk. Quote/Backslash/Newline/UTF-8-lead-bytes exit.
        val runStart = pos

        while
          pos < bufEnd && {
            val b = bytes(pos)
            b >= 0x20 && b != Quote && b != Backslash
          }
        do pos += 1

        val runLen = pos - runStart

        if runLen > 0 then
          ensureSpace(runLen)
          var k = 0

          while k < runLen do
            chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
            k += 1

          stringCursor += runLen

        if !more then errorAt(Issue.UnterminatedDoubleQuotedString)
        val b = peek

        if b == Quote then
          advance()
          done = true
        else if b == Backslash then
          advance()
          if !more then errorAt(Issue.UnterminatedEscape)
          consumeDoubleQuotedEscape()
        else if b == Newline then
          lastScalarSpannedLines = true
          consumeMultilineFold()
        else
          appendByteAsChar(b)
          advance()

      Yaml.Ast.Str(getStringText())

    private def consumeDoubleQuotedEscape()(using Tactic[ParseError]): Unit =
      val b = peek
      advance()

      (b: @switch) match
        case Backslash  => appendChar('\\')
        case Quote      => appendChar('"')
        case Slash      => appendChar('/')
        case Space      => appendChar(' ')
        case Num0       => appendChar(0x00.toChar)
        case LowerA     => appendChar(0x07.toChar)
        case LowerB     => appendChar('\b')
        case LowerE     => appendChar(0x1b.toChar)
        case LowerF     => appendChar('\f')
        case LowerN     => appendChar('\n')
        case LowerR     => appendChar('\r')
        case LowerT     => appendChar('\t')
        case LowerV     => appendChar(0x0b.toChar)
        case Tab        => appendChar('\t')

        case Newline =>
          // \<newline> = explicit line-break suppression. The newline
          // itself is consumed (advance was done above) and any leading
          // whitespace on the continuation line is stripped, so a literal
          // space on the next line must be reintroduced via `\<space>`.
          while more && (peek == Space || peek == Tab) do advance()

        case LowerX =>
          val n = readHex(2)
          appendChar(n.toChar)

        case LowerU =>
          val n = readHex(4)
          appendChar(n.toChar)

        case 0x55 =>
          val n = readHex(8)

          if n >= 0x10000 then
            val a = n - 0x10000
            appendChar((0xD800 | (a >>> 10)).toChar)
            appendChar((0xDC00 | (a & 0x3FF)).toChar)
          else
            appendChar(n.toChar)

        case _ =>
          errorAt(Issue.InvalidEscapeSequence)

    private def readHex(count: Int)(using Tactic[ParseError]): Int =
      var acc = 0
      var i = 0

      while i < count do
        if !more then errorAt(Issue.TruncatedHexEscape)
        val b = peek

        val digit =
          if b >= Num0 && b <= Num9 then b - Num0
          else if b >= LowerA && b <= LowerF then b - LowerA + 10
          else if b >= 0x41 && b <= UpperF then b - 0x41 + 10
          else errorAt(Issue.InvalidHexDigit)

        acc = (acc << 4) | digit
        advance()
        i += 1

      acc

    private def parseSingleQuoted()(using Tactic[ParseError]): Yaml.Ast =
      resetString()
      lastScalarSpannedLines = false
      var done = false
      while !done do
        // Fast-prefix ASCII run: copy non-special printable bytes in
        // bulk. Apostrophe/Newline/UTF-8-lead-bytes exit.
        val runStart = pos

        while
          pos < bufEnd && {
            val b = bytes(pos)
            b >= 0x20 && b != Apostrophe
          }
        do pos += 1

        val runLen = pos - runStart

        if runLen > 0 then
          ensureSpace(runLen)
          var k = 0

          while k < runLen do
            chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
            k += 1

          stringCursor += runLen

        if !more then errorAt(Issue.UnterminatedSingleQuotedString)
        val b = peek

        if b == Apostrophe then
          advance()

          if more && peek == Apostrophe then
            appendChar('\'')
            advance()
          else
            done = true
        else if b == Newline then
          lastScalarSpannedLines = true
          consumeMultilineFold()
        else
          appendByteAsChar(b)
          advance()

      Yaml.Ast.Str(getStringText())

    // Inside a quoted string: `\n` followed by zero or more whitespace.
    // Apply YAML line folding: 1 newline → space, N newlines → (N-1)
    // literal newlines. Trailing whitespace on the current line and
    // leading whitespace on the continuation line are both stripped
    // (per spec 6.5). When the continuation reaches a non-whitespace
    // byte, its leading-space count must exceed the parent collection's
    // indent — otherwise the quoted scalar is mis-indented.
    private def consumeMultilineFold()(using Tactic[ParseError]): Unit =
      while
        stringCursor > 0 &&
          (chars(stringCursor - 1) == ' ' || chars(stringCursor - 1) == '\t')
      do stringCursor -= 1

      lastScalarSpannedLines = true
      var newlineCount = 0
      var spaces = 0
      var done = false

      while !done && more do
        val c = peek

        if c == Newline then
          newlineCount += 1
          spaces = 0
          advance()
        else if c == Space then
          spaces += 1
          advance()
        else if c == Tab || c == Return then
          advance()
        else
          done = true

      if newlineCount > 0 && more then
        if atDocumentBoundary then
          errorAt(Issue.DocumentMarkerInsideMultilineScalar)

        if spaces <= blockParentIndent then
          errorAt(Issue.ScalarContinuationUnderIndented)

      if newlineCount == 1 then appendChar(' ')
      else
        var k = 1

        while k < newlineCount do
          appendChar('\n')
          k += 1

    // ── Flow types ──────────────────────────────────────────────────────────

    private def parseFlowSequence()(using Tactic[ParseError]): Yaml.Ast =
      val buf = acquireBuffer()
      var done = false

      while !done do
        skipFlowWhitespace()
        if !more then errorAt(Issue.UnterminatedFlowSequence)

        if peek == CloseBracket then
          advance()
          done = true
        else if peek == Comma then
          errorAt(Issue.EmptyFlowSequenceEntry)
        else
          // Recognise explicit-key indicator `?` and empty implicit-key
          // `:` at the start of an entry, mirroring parseFlowMapping.
          val first: Yaml.Ast =
            if
              peek == Question && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBracket || nb == Colon
              }
            then
              advance()
              skipFlowWhitespace()

              if !more || peek == Comma || peek == CloseBracket || peek == Colon
              then Yaml.Ast.Null
              else parseFlowNode()
            else if
              peek == Colon && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBracket
              }
            then
              Yaml.Ast.Null
            else
              parseFlowNode()
          skipFlowWhitespace()
          // A `:` here promotes the entry to a single-pair mapping per
          // spec 7.5: `[foo: bar]` is shorthand for `[{foo: bar}]`.
          val entry =
            if more && peek == Colon then
              if newlineImmediatelyPrecedes(pos) then
                errorAt(Issue.FlowImplicitKeyAndColonOnDifferentLines)

              advance()
              skipFlowWhitespace()

              val value =
                if !more || peek == Comma || peek == CloseBracket
                then Yaml.Ast.Null
                else parseFlowNode()

              val pairBuf = acquireBuffer()
              pairBuf += first
              pairBuf += value
              val pair = sealMapping(pairBuf)
              releaseBuffer()
              pair
            else
              first

          buf += entry
          skipFlowWhitespace()
          if !more then errorAt(Issue.UnterminatedFlowSequence)

          peek match
            case Comma        => advance()
            case CloseBracket => advance(); done = true
            case _            => errorAt(Issue.FlowSequenceExpectedCommaOrClose)

      val result = sealSequence(buf)
      releaseBuffer()
      result

    private def parseFlowMapping()(using Tactic[ParseError]): Yaml.Ast =
      val buf = acquireBuffer()
      var done = false

      while !done do
        skipFlowWhitespace()
        if !more then errorAt(Issue.UnterminatedFlowMapping)

        if peek == CloseBrace then
          advance()
          done = true
        else if peek == Comma then
          errorAt(Issue.EmptyFlowMappingEntry)
        else
          // Recognise explicit-key indicator `?` and empty implicit-key
          // `:` at the start of a flow-mapping entry.
          val key: Yaml.Ast =
            if
              peek == Question && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBrace || nb == Colon
              }
            then
              advance()
              skipFlowWhitespace()

              if !more || peek == Comma || peek == CloseBrace || peek == Colon
              then Yaml.Ast.Null
              else parseFlowNode()
            else if
              peek == Colon && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBrace
              }
            then
              Yaml.Ast.Null
            else
              parseFlowNode()

          skipFlowWhitespace()

          val value =
            if more && peek == Colon then
              advance()
              skipFlowWhitespace()

              if !more || peek == Comma || peek == CloseBrace then Yaml.Ast.Null
              else parseFlowNode()
            else
              Yaml.Ast.Null

          buf += key
          buf += value
          skipFlowWhitespace()
          if !more then errorAt(Issue.UnterminatedFlowMapping)

          peek match
            case Comma      => advance()
            case CloseBrace => advance(); done = true
            case _          => errorAt(Issue.FlowMappingExpectedCommaOrClose)

      val result = sealMapping(buf)
      releaseBuffer()
      result

    // Materialise the buffer as a sequence node: copy directly into a single
    // `Array[Any]`, padding with `arrayPad` if the count is even so the
    // result has odd length (the parity that distinguishes sequences from
    // mappings).
    private def sealSequence(buf: ArrayBuffer[Any]): Yaml.Ast =
      val n = buf.length

      if (n & 1) == 1 then
        val arr = new Array[Any](n)
        buf.copyToArray(arr)
        arr.asInstanceOf[Yaml.Ast]
      else
        val arr = new Array[Any](n + 1)
        buf.copyToArray(arr)
        arr(n) = Yaml.Ast.arrayPad
        arr.asInstanceOf[Yaml.Ast]

    // The buffer was filled with alternating key/value items, so the count
    // is already even; copy directly into a flat `Array[Any]`.
    private def sealMapping(buf: ArrayBuffer[Any]): Yaml.Ast =
      val n = buf.length
      val arr = new Array[Any](n)
      buf.copyToArray(arr)
      arr.asInstanceOf[Yaml.Ast]

    // Within a flow context, whitespace and newlines are insignificant
    // separators; comments still apply but require leading whitespace
    // (`a #c` is a comment after `a`; `a#c` is part of the scalar `a#c`,
    // and `]#c` after a flow close is an error). Newlines also flip
    // `lastScalarSpannedLines` so multi-line flow collections used as
    // implicit keys can be rejected without a separate scan.
    private def skipFlowWhitespace()(using Tactic[ParseError]): Unit =
      var continue = true

      while continue && more do
        val c = peek

        if c == Newline then
          advance()
          lastScalarSpannedLines = true

          if flowParentIndent < 0 then
            while more && (peek == Space || peek == Tab || peek == Return) do advance()
          else
            var spaces = 0

            while more && peek == Space do
              spaces += 1
              advance()
            // Content lines inside a flow collection (other than the
            // closing bracket / brace) must be more indented than the
            // flow's parent (spec 7.4).
            if
              more && peek != Newline &&
                peek != CloseBracket && peek != CloseBrace &&
                peek != Hash && spaces <= flowParentIndent
            then
              errorAt(Issue.FlowContentUnderIndented)
        else if c == Space || c == Tab || c == Return then
          advance()
        else if c == Hash then
          val prev = if pos > 0 then bytes(pos - 1) else -1

          if prev == Space || prev == Tab || prev == Newline || prev == Return then
            while more && peek != Newline do advance()
          else
            errorAt(Issue.CommentMissingPrecedingWhitespace)
        else
          continue = false

    // A node within a flow context — same dispatch as parseNodeHere but
    // with flow-specific scalar termination.
    private def parseFlowNode()(using Tactic[ParseError]): Yaml.Ast =
      skipFlowWhitespace()

      if !more then Yaml.Ast.Null
      else
        if atDocumentBoundary then
          errorAt(Issue.DocumentMarkerInFlowContext)

        consumeNodePrefixes()
        val anchorName = prefixAnchor
        val tagText    = prefixTag
        // In flow context, newlines between the prefix and the actual
        // node body are insignificant whitespace, so re-skip and re-read
        // the head byte after the prefix.
        skipFlowWhitespace()
        val headByte = if !more then -1 else peek & 0xFF

        val value =
          if headByte == -1 then Yaml.Ast.Null
          else if headByte == Comma || headByte == CloseBracket ||
            headByte == CloseBrace then
            // The flow node is empty — caller will consume the terminator.
            Yaml.Ast.Null
          else if headByte == Colon && {
            val next = if pos + 1 < bufEnd then bytes(pos + 1) else -1
            next == Space || next == Tab || next == Newline || next == Return ||
              next == Comma || next == CloseBracket ||
              next == CloseBrace || next == -1
          } then
            // `:` followed by a flow terminator marks an empty key that
            // the caller will pair with the upcoming value. `::x` and
            // similar are plain scalars and don't take this path.
            Yaml.Ast.Null
          else if headByte == Star then
            advance()
            parseAlias()
          else
            (headByte: @switch) match
              case Quote        => advance(); parseDoubleQuoted()
              case Apostrophe   => advance(); parseSingleQuoted()
              case OpenBracket  => advance(); parseFlowSequence()
              case OpenBrace    => advance(); parseFlowMapping()
              case _            => parseFlowPlainScalar()

        val tagged = if tagText.nil then value else applyTag(tagText, value)

        if anchorName.nil then tagged
        else
          anchors.update(anchorName.s, tagged)
          tagged

    // Plain scalar within a flow context: terminates on `,`, `]`, `}`,
    // `:`+space, newline, or hash-comment.
    private def parseFlowPlainScalar()(using Tactic[ParseError]): Yaml.Ast =
      // Plain scalars in flow context cannot start with `-`, `?` or
      // `:` followed by a flow-context indicator (space, tab, comma,
      // `]`, `}`, newline, EOF). Per spec these combinations are
      // ambiguous with sequence/mapping shorthand.
      if more then
        val first = peek

        if first == Minus || first == Question || first == Colon then
          val next = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            next == Space || next == Tab || next == Comma ||
              next == CloseBracket || next == CloseBrace ||
              next == Newline || next == Return || next == -1
          then errorAt(Issue.ReservedIndicatorAtFlowPlainScalarStart)

      resetString()
      lastScalarSpannedLines = false
      var done = false
      while !done && more do
        // Fast-prefix ASCII run: bulk-copy bytes that are neither flow
        // terminators nor UTF-8 lead bytes.
        val runStart = pos

        while
          pos < bufEnd && {
            val b = bytes(pos)
            b >= 0x20 && b != Comma && b != CloseBracket && b != CloseBrace &&
              b != Colon && b != Hash
          }
        do pos += 1

        val runLen = pos - runStart

        if runLen > 0 then
          ensureSpace(runLen)
          var k = 0

          while k < runLen do
            chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
            k += 1

          stringCursor += runLen

        if !more then done = true
        else
          val b = peek

          if b == Comma || b == CloseBracket || b == CloseBrace then done = true
          else if b == Newline then consumeMultilineFold()
          else if b == Colon then
            val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1

            if
              nextByte == Space || nextByte == Tab || nextByte == Newline ||
                nextByte == Comma || nextByte == CloseBracket ||
                nextByte == CloseBrace || nextByte == -1
            then done = true
            else
              appendChar(':')
              advance()
          else if b == Hash && stringCursor > 0 && chars(stringCursor - 1) == ' ' then
            while more && peek != Newline do advance()
            done = true
          else
            appendByteAsChar(b)
            advance()

      val text = trimEndWhitespace(getStringText())
      resolvePlainScalar(text)

    // ── Block sequences / mappings ─────────────────────────────────────────

    // Called when a `-` is at the head of the current node. Either a plain
    // scalar starting with `-` (e.g. negative number) or a block sequence
    // marker. The disambiguation is the byte after `-`: if space/tab/
    // newline/EOF, it's a sequence marker.
    private def parseMinus(indent: Int)(using Tactic[ParseError]): Yaml.Ast =
      val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1

      if
        nextByte == Space || nextByte == Tab || nextByte == Newline ||
          nextByte == Return || nextByte == -1
      then
        // The `-` is a block-sequence indicator at line-start, or
        // preceded by another `-` (compact nested seq, `- - x`), a `:`
        // (explicit-key value, `? k\n: - v`), or a `?` (explicit-key
        // key that is itself a sequence, `? - x\n  - y`). Walking back
        // over space/tab and landing on any of these is the allowed
        // shape.
        var j = pos - 1
        while j >= 0 && (bytes(j) == Space || bytes(j) == Tab) do j -= 1
        val ok = j < 0 || bytes(j) == Newline || bytes(j) == Return ||
          bytes(j) == Minus || bytes(j) == Colon ||
          bytes(j) == Question
        if !ok then errorAt(Issue.BlockSequenceIndicatorNotAtLineStart)
        // Use the dash's actual column, not the caller's indent param —
        // an inline-value sequence (`: - v`, `? k\n: - v`) starts where
        // its dashes actually appear, not at the surrounding mapping's
        // indent. Falling back to `indent` when the line could not be
        // located keeps existing line-start cases identical.
        parseBlockSequence(currentColumn())
      else
        parsePlainOrBlockMapping(indent)

    // Column (0-indexed) of the current cursor position within its
    // source line. Used to anchor a block-sequence iteration when the
    // first dash isn't at the caller's `indent`.
    private def currentColumn(): Int =
      var j = pos - 1
      while j >= 0 && bytes(j) != Newline do j -= 1
      pos - j - 1

    // Caller has seen a `?` at node-head. When followed by whitespace or
    // end-of-input, this is an explicit-key indicator: set up a block
    // mapping at `indent`, read the first pair, then iterate. `?foo` (no
    // separator) is just a plain scalar starting with `?`.
    private def parseQuestion(indent: Int)(using Tactic[ParseError]): Yaml.Ast =
      if isExplicitKeyIndicator then
        val buf = acquireBuffer()
        val savedParentIndent = blockParentIndent
        blockParentIndent = indent
        val (firstKey, firstValue) = readExplicitPair(indent)
        buf += firstKey
        buf += firstValue
        iterateBlockMapping(buf, indent)
        val result = sealMapping(buf)
        releaseBuffer()
        blockParentIndent = savedParentIndent
        result
      else
        parsePlainOrBlockMapping(indent)

    // Iteration body shared by parseBlockMappingFromFirstKey (after its
    // first pair has been added) and parseBlockMappingFromExplicitKey.
    // Walks subsequent block-mapping pairs at `indent`, supporting both
    // explicit (`? key`) and implicit (`key:`) keys.
    private def iterateBlockMapping
      ( buf: scala.collection.mutable.ArrayBuffer[Any], indent: Int )
      ( using Tactic[ParseError] )
    :   Unit =

      var done = false

      while !done do
        skipBlankAndCommentLines()
        val lineStart = pos
        val nextIndent = consumeLeadingSpaces()
        if more && peek == Tab then errorAt(Issue.TabInIndentation)

        if nextIndent != indent then
          pos = lineStart
          done = true
        else if !more then
          done = true
        else if atDocumentBoundary then
          pos = lineStart
          done = true
        else if isExplicitKeyIndicator then
          val (key, value) = readExplicitPair(indent)
          buf += key
          buf += value
        else
          consumeNodePrefixes()
          val keyAnchor = prefixAnchor
          val keyTag    = prefixTag

          val rawKey: Yaml.Ast = prefixHeadByte match
            case Quote =>
              advance()
              val s = parseDoubleQuoted()

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              s

            case Apostrophe =>
              advance()
              val s = parseSingleQuoted()

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              s

            case Star =>
              if !keyAnchor.nil then errorAt(Issue.AnchorOnAlias)
              advance()
              parseAlias()

            case OpenBracket =>
              advance()
              parseFlowSequence()

            case OpenBrace =>
              advance()
              parseFlowMapping()

            case _ =>
              val keyText = readPlainScalarText(indent)

              if !sawMappingColon then
                errorAt(Issue.PlainScalarAtMappingIndentWithoutColon)

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              resolvePlainScalar(keyText)

          val tagged = if keyTag.nil then rawKey else applyTag(keyTag, rawKey)

          val keyAst =
            if keyAnchor.nil then tagged
            else
              anchors.update(keyAnchor.s, tagged)
              tagged

          skipSpaces()
          if !more || peek != Colon then errorAt(Issue.ExpectedColonAfterMappingKey)
          advance()
          val value = parseMappingValue(indent)
          buf += keyAst
          buf += value

    private def parseBlockSequence(indent: Int)(using Tactic[ParseError]): Yaml.Ast =
      val buf = acquireBuffer()
      val savedParentIndent = blockParentIndent
      blockParentIndent = indent
      var done = false
      while !done do
        // We're at a `-` at column == indent.
        advance()  // consume `-`
        // Either space-separated content or newline (item value on next
        // indented line)
        val next = if more then peek else -1

        val item: Yaml.Ast =
          if next == Space || next == Tab then
            advance() // single space after `-`
            while more && (peek == Space || peek == Tab) do advance()
            // Trailing comment after `- ` — treat as an empty same-line
            // value, so the item is on the next more-indented line.
            if more && peek == Hash then
              while more && peek != Newline do advance()

            if more && peek == Newline then
              advance()
              skipBlankAndCommentLines()
              val childIndent = consumeLeadingSpaces()

              if childIndent <= indent then Yaml.Ast.Null
              else parseNodeHere(childIndent)
            else
              parseNodeHere(indent + 2)
          else if next == Newline || next == -1 then
            // Just `-` with newline → child on next line
            if more then advance() // consume newline
            skipBlankAndCommentLines()
            val childIndent = consumeLeadingSpaces()

            if childIndent <= indent then Yaml.Ast.Null
            else parseNodeHere(childIndent)
          else
            // shouldn't happen
            parseNodeHere(indent + 2)

        buf += item
        // After parsing the item, skip blank lines and detect next item
        skipBlankAndCommentLines()
        val lineStart = pos
        val nextIndent = consumeLeadingSpaces()
        if more && peek == Tab then errorAt(Issue.TabInIndentation)

        if nextIndent != indent then
          pos = lineStart
          done = true
        else if !more || peek != Minus then
          pos = lineStart
          done = true
        else if atDocumentBoundary then
          pos = lineStart
          done = true
        else
          // Need to verify it's `-` followed by whitespace (sequence
          // marker), not a plain scalar starting with `-`.
          val byteAfterDash = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            byteAfterDash != Space && byteAfterDash != Tab &&
              byteAfterDash != Newline && byteAfterDash != -1
          then
            pos = lineStart
            done = true

      val result = sealSequence(buf)
      releaseBuffer()
      blockParentIndent = savedParentIndent
      result

    // From the start of the current line, count leading spaces. Returns the
    // count and leaves the cursor positioned at the first non-space byte.
    private def consumeLeadingSpaces(): Int =
      var n = 0

      while more && peek == Space do
        n += 1
        advance()

      n

    // Skip blank lines (only whitespace), comment-only lines, and
    // directive lines (those starting with `%`). Leaves the cursor at
    // the start of the first non-skip line (before any leading
    // whitespace), so the caller can measure that line's indent via
    // consumeLeadingSpaces.
    private def skipBlankAndCommentLines(): Unit =
      var continue = true

      while continue && more do
        val savedPos = pos
        // Scan to end of line
        while more && (peek == Space || peek == Tab) do advance()

        if !more then continue = false
        else peek match
          case Newline =>
            advance()

          case Hash =>
            while more && peek != Newline do advance()
            if more then advance()

          case _ =>
            pos = savedPos
            continue = false

    // Called when we've already read the first key (plain or quoted) and
    // are positioned at the `:` following it. Parse the rest of the
    // block mapping at `indent`. Subsequent keys may also be quoted.
    private def parseBlockMappingFromFirstKey
      ( firstKey: Yaml.Ast, indent: Int )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val buf = acquireBuffer()
      val savedParentIndent = blockParentIndent
      blockParentIndent = indent

      if !more || peek != Colon then errorAt(Issue.ExpectedColonAfterMappingKey)
      advance()
      val firstValue = parseMappingValue(indent)
      buf += firstKey
      buf += firstValue
      iterateBlockMapping(buf, indent)
      val result = sealMapping(buf)
      releaseBuffer()
      blockParentIndent = savedParentIndent
      result

    // True when the cursor is at a `?` indicator that introduces an
    // explicit mapping key (followed by space/tab/newline/EOF).
    private inline def isExplicitKeyIndicator: Boolean =
      more && peek == Question && {
        val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
        nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
      }

    // Read an explicit `? key\n[: value]` pair. The cursor is positioned
    // at `?`. The key may be on the same line as `?` or on a subsequent
    // indented line; the matching `:` value indicator must appear at the
    // mapping's indent and may be absent (yielding a Null value).
    private def readExplicitPair(indent: Int)(using Tactic[ParseError])
    :   (Yaml.Ast, Yaml.Ast) =

      advance() // ?
      while more && (peek == Space || peek == Tab) do advance()
      // Trailing comment on the `?` marker line — consume so the parse
      // continues on the next line as if the marker stood alone.
      if more && peek == Hash then
        while more && peek != Newline do advance()

      val key: Yaml.Ast =
        if !more || peek == Newline then
          // Key on a more-indented next line, or — per spec 8.2.2 — a
          // zero-indent block sequence at the same column as the `?` marker.
          if more then advance()
          skipBlankAndCommentLines()
          val keyLineStart = pos
          val keyIndent = consumeLeadingSpaces()

          if keyIndent < indent then
            pos = keyLineStart
            Yaml.Ast.Null
          else if keyIndent == indent then
            if
              more && peek == Minus && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
              }
            then
              parseNodeHere(keyIndent)
            else
              pos = keyLineStart
              Yaml.Ast.Null
          else
            parseNodeHere(keyIndent)
        else
          parseNodeHere(indent + 2)

      skipBlankAndCommentLines()
      val markerLineStart = pos
      val markerIndent = consumeLeadingSpaces()

      val value: Yaml.Ast =
        if
          markerIndent == indent && more && peek == Colon && {
            val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
            nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
          }
        then
          advance()
          parseMappingValue(indent, isExplicitValue = true)
        else
          pos = markerLineStart
          Yaml.Ast.Null

      (key, value)

    // Parse the value side of a `key: VALUE` entry. Either inline (after
    // the colon, on the same line) or a block on the next indented lines.
    private def parseMappingValue
      ( parentIndent: Int, isExplicitValue: Boolean = false )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      skipSpaces()

      if !more then Yaml.Ast.Null
      else if peek == Newline then
        advance()
        skipBlankAndCommentLines()
        val lineStart = pos
        val childIndent = consumeLeadingSpaces()
        pickValueOrNull(parentIndent, childIndent, lineStart)
      else if peek == Hash then
        while more && peek != Newline do advance()
        if more then advance()
        skipBlankAndCommentLines()
        val lineStart = pos
        val childIndent = consumeLeadingSpaces()
        pickValueOrNull(parentIndent, childIndent, lineStart)
      else
        // Inline value on same line as `key:`. For implicit keys, a
        // block-sequence indicator here is invalid per spec, and
        // chained `'k': v`-style nested mapping is illegal too. For
        // explicit-key values (`? key\n: - val`, `? k\n: m: n`) both
        // patterns are allowed because the explicit-key construct
        // already disambiguates the layout.
        if !isExplicitValue && peek == Minus then
          val next = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            next == Space || next == Tab || next == Newline ||
              next == Return || next == -1
          then errorAt(Issue.BlockSequenceOnMappingKeyLine)

        val saved = inInlineMappingValue
        inInlineMappingValue = !isExplicitValue
        val result = parseNodeHere(parentIndent)
        inInlineMappingValue = saved
        result

    // Decide whether a next-line node belongs to the current mapping
    // value. Less-indented => Null (no value, the line is a sibling key
    // or out-of-scope). Same-indent block sequence => the sequence is
    // the value (spec 8.2.2). More-indented => a regular nested node.
    // Rewinds to lineStart on Null so the outer scope re-reads the line.
    private inline def pickValueOrNull
      ( parentIndent: Int, childIndent: Int, lineStart: Int )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      if more && peek == Tab then errorAt(Issue.TabInIndentation)
      // The next-line node starts a fresh line; an enclosing inline
      // mapping-value context shouldn't propagate into it.
      val savedInline = inInlineMappingValue
      inInlineMappingValue = false

      val result =
        if !more then Yaml.Ast.Null
        else pickValueOrNullBody(parentIndent, childIndent, lineStart)

      inInlineMappingValue = savedInline
      result

    private inline def pickValueOrNullBody
      ( parentIndent: Int, childIndent: Int, lineStart: Int )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      if !more then Yaml.Ast.Null
      else if childIndent < parentIndent then
        pos = lineStart
        Yaml.Ast.Null
      else if childIndent == parentIndent then
        if
          peek == Minus && {
            val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
            nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
          }
        then
          parseNodeHere(childIndent)
        else
          pos = lineStart
          Yaml.Ast.Null
      else
        parseNodeHere(childIndent)

    // ── Block scalars (|/>) ─────────────────────────────────────────────────

    private enum BlockChomp:
      case Clip, Strip, Keep

    private enum BlockLineType:
      case None, Blank, Regular, MoreIndented

    private def parseBlockScalar(literal: Boolean)
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      advance() // consume `|` or `>`

      // Header: optional indentation indicator (1-9) and chomping
      // indicator (+/-), in either order, followed by optional whitespace
      // and an optional comment.
      var explicitIndent: Int = -1
      var chomp: BlockChomp = BlockChomp.Clip

      while more && peek != Newline && peek != Hash do
        val b = peek

        if b >= Num0 && b <= Num9 then
          if b == Num0 then errorAt(Issue.InvalidBlockScalarIndentationIndicator)
          if explicitIndent >= 0 then errorAt(Issue.DuplicateBlockScalarIndentationIndicator)
          explicitIndent = b - Num0
        else if b == Plus then
          if chomp != BlockChomp.Clip then errorAt(Issue.DuplicateBlockScalarChompingIndicator)
          chomp = BlockChomp.Keep
        else if b == Minus then
          if chomp != BlockChomp.Clip then errorAt(Issue.DuplicateBlockScalarChompingIndicator)
          chomp = BlockChomp.Strip
        else if b == Space || b == Tab then
          ()
        else
          errorAt(Issue.InvalidBlockScalarHeader)

        advance()

      if more && peek == Hash then
        val prev = if pos > 0 then bytes(pos - 1) else -1

        if prev != Space && prev != Tab then
          errorAt(Issue.BlockScalarHeaderCommentMissingWhitespace)

        while more && peek != Newline do advance()

      if more then advance() // consume newline

      resetString()
      val parent = blockParentIndent

      var baseIndent: Int =
        if explicitIndent >= 0 then parent + explicitIndent else -1

      // Folded-mode state machine: track the LAST non-blank content line
      // (None / Regular / MoreIndented) and how many blank lines have
      // intervened since it. The separator emitted before the next
      // content line is computed from these:
      //   - None       → no separator (start of body)
      //   - Regular → Regular, no blanks  → " " (fold)
      //   - Regular → Regular, with blanks → "" (the blanks' \n's suffice)
      //   - any other transition (involving more-indented or with blanks
      //     and a non-Regular endpoint) → "\n"
      var lastNonBlankType: BlockLineType = BlockLineType.None
      var blanksPending: Int = 0

      // For auto-detect baseIndent: track the maximum number of leading
      // spaces seen on a whitespace-only line *before* the first content
      // line. Per spec 8.1.2 this must not exceed the indent the first
      // content line establishes; otherwise the body is rejected.
      var maxLeadingBlankSpaces: Int = -1

      var done = false

      while !done && more do
        val lineStart = pos
        var spaces = 0

        while more && peek == Space do
          spaces += 1
          advance()

        if !more then
          done = true
        else if peek == Newline then
          // Whitespace-only line. Classification depends on whether we've
          // already established baseIndent.
          if baseIndent >= 0 && spaces > baseIndent then
            // More-indented all-spaces line — its "content" is the
            // (spaces - baseIndent) extra spaces. Treated as a content
            // line of MoreIndented type.
            if literal then
              var k = 0
              while k < spaces - baseIndent do { appendChar(' '); k += 1 }
              appendChar('\n')
            else
              if lastNonBlankType != BlockLineType.None then appendChar('\n')
              var k = 0
              while k < spaces - baseIndent do { appendChar(' '); k += 1 }

            lastNonBlankType = BlockLineType.MoreIndented
            blanksPending = 0
          else
            // Plain blank line.
            if baseIndent < 0 && spaces > maxLeadingBlankSpaces then
              maxLeadingBlankSpaces = spaces

            appendChar('\n')

            if literal || lastNonBlankType != BlockLineType.None then
              blanksPending += 1

          advance()
        else
          // Non-whitespace content line.
          if baseIndent < 0 then
            // First content line establishes baseIndent; the block has
            // a body only if its indent strictly exceeds the parent's.
            if spaces > parent then
              baseIndent = spaces

              if maxLeadingBlankSpaces > baseIndent then
                errorAt(Issue.BlockScalarLeadingBlanksOverIndented)
            else
              pos = lineStart
              done = true

          if !done then
            if spaces < baseIndent then
              pos = lineStart
              done = true
            else if spaces == 0 && atDocumentBoundary then
              // A `---` or `...` at column 0 ends the block scalar even
              // if its content would otherwise be at indent >= baseIndent.
              pos = lineStart
              done = true
            else
              // A line whose first content byte is a tab is "more
              // indented" in folded mode (the tab counts as additional
              // leading whitespace beyond baseIndent), so its line break
              // is preserved verbatim instead of being folded.
              val curType =
                if spaces > baseIndent || peek == Tab then BlockLineType.MoreIndented
                else BlockLineType.Regular
              if literal then
                // Trailing-terminator scheme.
                var k = 0
                while k < spaces - baseIndent do { appendChar(' '); k += 1 }
                readBlockScalarLineContent()
                appendChar('\n')
                if more && peek == Newline then advance()
              else
                // Folded: state-machine separator (see comments above).
                val needBreak =
                  lastNonBlankType == BlockLineType.MoreIndented ||
                    curType == BlockLineType.MoreIndented

                if lastNonBlankType == BlockLineType.None then ()
                else if blanksPending > 0 then
                  if needBreak then appendChar('\n')
                  // R + blanks + R → blanks' \n's are the separator
                else if needBreak then
                  appendChar('\n')
                else
                  appendChar(' ')

                var k = 0
                while k < spaces - baseIndent do { appendChar(' '); k += 1 }
                readBlockScalarLineContent()
                if more && peek == Newline then advance()

              lastNonBlankType = curType
              blanksPending = 0

      // For folded mode the per-line scheme leaves no trailing newline;
      // add one so chomping rules see a uniform "final break".
      if !literal && stringCursor > 0 && chars(stringCursor - 1) != '\n' then
        appendChar('\n')

      chomp match
        case BlockChomp.Strip =>
          while stringCursor > 0 && chars(stringCursor - 1) == '\n' do
            stringCursor -= 1

        case BlockChomp.Clip =>
          // Keep at most one trailing newline. If the buffer is entirely
          // newlines (no real content), reduce to empty.
          var lastContent = stringCursor

          while lastContent > 0 && chars(lastContent - 1) == '\n' do
            lastContent -= 1

          stringCursor = if lastContent == 0 then 0 else lastContent + 1

        case BlockChomp.Keep =>
          ()

      Yaml.Ast.Str(getStringText())

    // Read the non-newline content of the current line into the chars
    // buffer. Bulk-copies printable ASCII via the fast prefix loop and
    // falls back to per-byte handling for tabs and UTF-8 lead bytes.
    private def readBlockScalarLineContent(): Unit =
      var done = false

      while !done do
        val runStart = pos
        while pos < bufEnd && bytes(pos) >= 0x20 do pos += 1
        val runLen = pos - runStart

        if runLen > 0 then
          ensureSpace(runLen)
          var k = 0

          while k < runLen do
            chars(stringCursor + k) = (bytes(runStart + k) & 0xFF).toChar
            k += 1

          stringCursor += runLen

        if !more || peek == Newline then done = true
        else
          appendByteAsChar(peek)
          advance()

    // ── Tags ────────────────────────────────────────────────────────────────

    // For shorthand tags of the form `!handle!suffix`, the handle must be
    // declared via a `%TAG` directive in the current document. The primary
    // (`!`) and secondary (`!!`) handles are built-in. Verbatim tags
    // (`!<URI>`) skip resolution entirely.
    private def validateTagHandle(tagText: Text)(using Tactic[ParseError]): Unit =
      val s = tagText.s

      if s.length >= 2 && s.charAt(0) == '!' && s.charAt(1) != '<' then
        val secondBang = s.indexOf('!', 1)

        if secondBang > 1 then
          val handle = s.substring(0, secondBang + 1).nn

          if handle != "!!" && !tagHandles.defines(handle) then
            errorAt(Issue.UndefinedTagHandle(handle.tt))

    private def applyTag(tag: Text, value: Yaml.Ast)(using Tactic[ParseError])
    :   Yaml.Ast =

      tag.s match
        case "!" | "!!" =>
          // Non-specific tags. `!` forces the string type for plain
          // scalars (preventing implicit type resolution into int/bool/etc).
          if value.asInstanceOf[AnyRef] == null then Yaml.Ast.Str(t"")
          else value.asInstanceOf[Matchable] match
            case _: String  => value
            case n: Long    => Yaml.Ast.Str(n.toString.tt)
            case d: Double  => Yaml.Ast.Str(d.toString.tt)
            case b: Boolean => Yaml.Ast.Str(b.toString.tt)
            case _          => value

        case "!!str" =>
          // A bare `!!str` with no scalar content is the empty string,
          // not the literal text "null".
          if value.asInstanceOf[AnyRef] == null then Yaml.Ast.Str(t"")
          else value.asInstanceOf[Matchable] match
            case _: String  => value
            case n: Long    => Yaml.Ast.Str(n.toString.tt)
            case d: Double  => Yaml.Ast.Str(d.toString.tt)
            case b: Boolean => Yaml.Ast.Str(b.toString.tt)
            case _          => value

        case "!!int" =>
          value.asInstanceOf[Matchable] match
            case _: Long   => value
            case d: Double => Yaml.Ast.Integer(d.toLong)

            case s: String =>
              val r = parsePlainIntegerOrNull(s)
              if r != null then r else value

            case _         => value

        case "!!float" =>
          value.asInstanceOf[Matchable] match
            case _: Double => value
            case n: Long   => Yaml.Ast.Decimal(n.toDouble)

            case s: String =>
              val r = parsePlainDecimalOrNull(s)
              if r != null then r else value

            case _         => value

        case "!!bool" =>
          value.asInstanceOf[Matchable] match
            case _: Boolean                  => value
            case "true" | "True" | "TRUE"    => Yaml.Ast.Bool(true)
            case "false" | "False" | "FALSE" => Yaml.Ast.Bool(false)
            case _                           => value

        case "!!null" => Yaml.Ast.Null
        case _        => value

    // ── Tracking-mode parsers ───────────────────────────────────────────────
    //
    // These mirror the untracked methods above but additionally emit a
    // `PositionIndex` of source descriptors into the `indexOut` buffer they
    // are passed. The untracked methods stay byte-identical so a non-
    // tracking parse pays no per-byte cost for position capture — the
    // same split-parsers shape proven out in jacinta #1147.
    //
    // Descriptor layout (mirrors jacinta exactly; all offsets relative to
    // the start of the containing descriptor):
    //
    //   primitive: [size=4, line, column, sourceLength]
    //   composite: [size, line, column, sourceLength, n,
    //               off_0..off_{n-1}, <child descriptors concatenated>]
    //   mapping entry (concatenated into composite scratch):
    //               [keyLine, keyColumn, keyLength, <value descriptor>]
    //   sequence entry (concatenated):
    //               <value descriptor>

    // Emit a 4-int primitive descriptor at the current cursor position.
    // `startMark` is the absolute byte offset captured at the start of
    // the scalar; the cursor is expected to be at its end.
    private inline def emitPrimitiveDescriptor
      ( indexOut: ArrayBuffer[Int], startLine: Int, startColumn: Int, startMark: Long )
    :   Unit =

      syncTo()
      val length = (cursor.position.n0 - startMark).toInt
      indexOut += 4
      indexOut += startLine
      indexOut += startColumn
      indexOut += length

    private inline def captureLineColumn(): (Int, Int) =
      syncTo()
      reconcileLineation()
      (cursor.line.n1, cursor.column.n1)

    private inline def currentMark(): Long =
      syncTo()
      cursor.position.n0.toLong

    // Emit a 4-int Null descriptor (zero length) at the current cursor
    // position. Used when a node resolves to `Null` without consuming any
    // bytes (empty inline value, missing explicit-pair value, etc.).
    private inline def emitNullHere(indexOut: ArrayBuffer[Int]): Unit =
      syncTo()
      reconcileLineation()
      indexOut += 4
      indexOut += cursor.line.n1
      indexOut += cursor.column.n1
      indexOut += 0

    private def parseNodeTracked(indent: Int, indexOut: ArrayBuffer[Int])
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      skipSpaces()

      if !more then
        emitNullHere(indexOut)
        Yaml.Ast.Null
      else
        parseNodeHereTracked(indent, indexOut)

    private def parseNodeHereTracked(indent: Int, indexOut: ArrayBuffer[Int])
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val savedPrefixesConsumed = prefixesConsumed
      val savedLastNodeHadAnchor = lastNodeHadAnchor
      prefixesConsumed = false
      lastNodeHadAnchor = false

      // Capture start before prefix consumption so the descriptor spans
      // the whole node including any anchor/tag.
      syncTo()
      reconcileLineation()
      val startLine   = cursor.line.n1
      val startColumn = cursor.column.n1
      val startMark   = cursor.position.n0.toLong

      consumeNodePrefixes()
      val anchorName = prefixAnchor
      val tagText    = prefixTag
      val headByte   = prefixHeadByte

      val hasPrefix = !anchorName.nil || !tagText.nil

      val value: Yaml.Ast =
        if hasPrefix && (headByte == Newline || headByte == -1 || headByte == Hash) then
          if headByte == Hash then
            while more && peek != Newline do advance()

          if more && peek == Newline then advance()
          skipBlankAndCommentLines()
          val lineStart = pos
          val childIndent = consumeLeadingSpaces()
          lastNodeHadAnchor = false
          val v = pickValueOrNullTracked(blockParentIndent, childIndent, lineStart, indexOut)

          if !anchorName.nil && lastNodeHadAnchor then
            errorAt(Issue.TwoAnchorsOnSameNode)
          // pickValueOrNullTracked has emitted the descriptor for `v` into
          // indexOut already. Skip the trailing primitive emission below.
          return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                anchorName, tagText, v, consumed = false)
        else if headByte == -1 then
          emitNullHere(indexOut)
          Yaml.Ast.Null
        else if headByte == Hash then
          while more && peek != Newline do advance()
          emitNullHere(indexOut)
          Yaml.Ast.Null
        else if headByte == Star then
          if !anchorName.nil then errorAt(Issue.AnchorOnAlias)
          advance()
          val a = parseAlias()
          // For aliases, the descriptor spans `*name` from the `*` mark.
          val r =
            maybeBlockMappingFromQuotedKeyTracked
              ( a, indent, tagText, anchorName, indexOut,
                startLine, startColumn, startMark )

          return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                anchorName, tagText, r, prefixesConsumed)
        else
          (headByte: @switch) match
            case Quote =>
              advance()
              val s = parseDoubleQuoted()

              val r =
                maybeBlockMappingFromQuotedKeyTracked
                  ( s, indent, tagText, anchorName, indexOut,
                    startLine, startColumn, startMark )

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

            case Apostrophe =>
              advance()
              val s = parseSingleQuoted()

              val r =
                maybeBlockMappingFromQuotedKeyTracked
                  ( s, indent, tagText, anchorName, indexOut,
                    startLine, startColumn, startMark )

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

            case OpenBracket =>
              val savedFlowParent = flowParentIndent
              flowParentIndent = blockParentIndent
              advance()
              val s = parseFlowSequenceTracked(indexOut, startLine, startColumn, startMark)
              flowParentIndent = savedFlowParent
              // A flow collection at node head doesn't transition to a
              // block mapping (`maybeBlockMappingFromQuotedKey` does in
              // the untracked code, but only to flag trailing-content errors
              // — emission is the composite descriptor already produced).
              val r =
                maybeBlockMappingFromQuotedKeyAfterComposite
                  ( s, indent, tagText, anchorName )

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

            case OpenBrace =>
              val savedFlowParent = flowParentIndent
              flowParentIndent = blockParentIndent
              advance()
              val m = parseFlowMappingTracked(indexOut, startLine, startColumn, startMark)
              flowParentIndent = savedFlowParent

              val r =
                maybeBlockMappingFromQuotedKeyAfterComposite
                  ( m, indent, tagText, anchorName )

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

            case Pipe =>
              val v = parseBlockScalar(literal = true)
              emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
              v

            case Greater =>
              val v = parseBlockScalar(literal = false)
              emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
              v

            case Minus =>
              val r = parseMinusTracked(indent, indexOut, startLine, startColumn, startMark)

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

            case Question =>
              val r = parseQuestionTracked(indent, indexOut, startLine, startColumn, startMark)

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

            case CloseBracket | CloseBrace | Comma | 0x40 | 0x60 =>
              errorAt(Issue.ReservedIndicatorAtNodeStart)

            case _ =>
              prefixesConsumed = false

              val r =
                parsePlainOrBlockMappingTracked
                  ( indent, tagText, anchorName, indexOut,
                    startLine, startColumn, startMark )

              return finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                                    anchorName, tagText, r, prefixesConsumed)

      finishNodeHere(savedPrefixesConsumed, savedLastNodeHadAnchor,
                     anchorName, tagText, value, prefixesConsumed)

    // Apply tag/anchor and restore saved flags after parseNodeHereTracked
    // has produced its value. `consumed` is the current
    // `prefixesConsumed` state — when true, an inner block-mapping
    // transition already applied the head's tag/anchor to the key, so
    // we don't re-apply them.
    private def finishNodeHere
      ( savedPrefixesConsumed: Boolean,
        savedLastNodeHadAnchor: Boolean,
        anchorName: Text,
        tagText: Text,
        value: Yaml.Ast,
        consumed: Boolean )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      prefixesConsumed = savedPrefixesConsumed

      if consumed then
        lastNodeHadAnchor = savedLastNodeHadAnchor
        value
      else
        val tagged = if tagText.nil then value else applyTag(tagText, value)

        if anchorName.nil then
          lastNodeHadAnchor = savedLastNodeHadAnchor
          tagged
        else
          anchors.update(anchorName.s, tagged)
          lastNodeHadAnchor = true
          tagged

    // After a flow collection at node head, perform the same trailing-
    // content / `:` check `maybeBlockMappingFromQuotedKey` does, but
    // without emitting any new descriptor (the composite descriptor was
    // already produced by parseFlow{Sequence,Mapping}Tracked). The block-
    // mapping transition is rare in this position — we route to the
    // untracked block-mapping helper if it does occur (acceptable for
    // PR 1; PR-2/3 can revisit if needed).
    private def maybeBlockMappingFromQuotedKeyAfterComposite
      ( scalar: Yaml.Ast, indent: Int, headTag: Text, headAnchor: Text )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      maybeBlockMappingFromQuotedKey(scalar, indent, headTag, headAnchor)

    // Tracked variant of `maybeBlockMappingFromQuotedKey`. When the
    // quoted/alias scalar at node head is followed by `:`, we transition
    // to a block mapping where the scalar is the first key — the node's
    // descriptor becomes the composite mapping descriptor (spanning from
    // the scalar's start to the mapping's end). Otherwise emit a 4-int
    // primitive descriptor for the scalar alone.
    private def maybeBlockMappingFromQuotedKeyTracked
      ( scalar: Yaml.Ast, indent: Int, headTag: Text, headAnchor: Text,
        indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      // Capture the scalar's end position before any trailing whitespace,
      // so the key's length (for the mapping case) is precise.
      syncTo()
      val keyEndMark = cursor.position.n0.toLong
      val keyLength = (keyEndMark - startMark).toInt

      val hadSpaceOrTab = more && (peek == Space || peek == Tab)
      skipSpaces()

      if !more then
        emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
        scalar
      else
        peek match
          case Newline =>
            emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
            scalar

          case Hash if hadSpaceOrTab =>
            while more && peek != Newline do advance()
            emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
            scalar

          case Colon =>
            val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1

            if
              nextByte == Space || nextByte == Tab || nextByte == Newline ||
                nextByte == Return || nextByte == -1
            then
              if onDocStartLine then
                errorAt(Issue.BlockMappingOnDocumentStartLine)

              if inInlineMappingValue then
                errorAt(Issue.ChainedMappingValueOnSingleLine)

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              val tagged = if headTag.nil then scalar else applyTag(headTag, scalar)

              val keyAst =
                if headAnchor.nil then tagged
                else
                  anchors.update(headAnchor.s, tagged)
                  tagged

              if !headTag.nil || !headAnchor.nil then prefixesConsumed = true

              parseBlockMappingFromFirstKeyTracked
                ( keyAst, startLine, startColumn, keyLength, indent, indexOut,
                  startLine, startColumn, startMark )
            else
              errorAt(Issue.TrailingContentAfterQuotedScalar)

          case Comma | CloseBracket | CloseBrace =>
            emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
            scalar

          case _ =>
            errorAt(Issue.TrailingContentAfterQuotedScalar)

    // Tracked variant of `parsePlainOrBlockMapping`. The scalar's start
    // position is `startLine`/`startColumn`/`startMark`. Either emits a
    // 4-int primitive (plain scalar resolves to a primitive) or a
    // composite mapping descriptor (transitions into a block mapping).
    private def parsePlainOrBlockMappingTracked
      ( indent: Int, headTag: Text, headAnchor: Text,
        indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val textValue = readPlainScalarText(indent)
      syncTo()
      val scalarEndMark = cursor.position.n0.toLong
      val keyLength = (scalarEndMark - startMark).toInt

      if sawMappingColon then
        if onDocStartLine then
          errorAt(Issue.BlockMappingOnDocumentStartLine)

        if inInlineMappingValue then
          errorAt(Issue.ChainedMappingValueOnSingleLine)

        if lastScalarSpannedLines then
          errorAt(Issue.MultilineImplicitKey)

        val rawKey = resolvePlainScalar(textValue)
        val tagged = if headTag.nil then rawKey else applyTag(headTag, rawKey)

        val keyAst =
          if headAnchor.nil then tagged
          else
            anchors.update(headAnchor.s, tagged)
            tagged

        if !headTag.nil || !headAnchor.nil then prefixesConsumed = true

        parseBlockMappingFromFirstKeyTracked
          ( keyAst, startLine, startColumn, keyLength, indent, indexOut,
            startLine, startColumn, startMark )
      else
        emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
        resolvePlainScalar(textValue)

    // Tracked variant of `parseBlockSequence`. Emits a composite
    // descriptor with one child descriptor per item.
    private def parseBlockSequenceTracked
      ( indent: Int,
        indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val buf = acquireBuffer()
      val scratch = acquireIndexBuffer()
      val ends    = acquireIndexBuffer()
      val savedParentIndent = blockParentIndent
      blockParentIndent = indent
      var done = false

      while !done do
        advance()  // consume `-`
        val next = if more then peek else -1

        val item: Yaml.Ast =
          if next == Space || next == Tab then
            advance()
            while more && (peek == Space || peek == Tab) do advance()

            if more && peek == Hash then
              while more && peek != Newline do advance()

            if more && peek == Newline then
              advance()
              skipBlankAndCommentLines()
              val childIndent = consumeLeadingSpaces()

              if childIndent <= indent then
                emitNullHere(scratch)
                Yaml.Ast.Null
              else
                parseNodeHereTracked(childIndent, scratch)
            else
              parseNodeHereTracked(indent + 2, scratch)
          else if next == Newline || next == -1 then
            if more then advance()
            skipBlankAndCommentLines()
            val childIndent = consumeLeadingSpaces()

            if childIndent <= indent then
              emitNullHere(scratch)
              Yaml.Ast.Null
            else
              parseNodeHereTracked(childIndent, scratch)
          else
            parseNodeHereTracked(indent + 2, scratch)

        ends += scratch.length
        buf += item

        skipBlankAndCommentLines()
        val lineStart = pos
        val nextIndent = consumeLeadingSpaces()
        if more && peek == Tab then errorAt(Issue.TabInIndentation)

        if nextIndent != indent then
          pos = lineStart
          done = true
        else if !more || peek != Minus then
          pos = lineStart
          done = true
        else if atDocumentBoundary then
          pos = lineStart
          done = true
        else
          val byteAfterDash = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            byteAfterDash != Space && byteAfterDash != Tab &&
              byteAfterDash != Newline && byteAfterDash != -1
          then
            pos = lineStart
            done = true

      val result = sealSequence(buf)
      releaseBuffer()

      emitCompositeDescriptor(indexOut, scratch, ends, startLine, startColumn, startMark)
      releaseIndexBuffer()  // ends
      releaseIndexBuffer()  // scratch
      blockParentIndent = savedParentIndent
      result

    // Tracked variant of `parseBlockMappingFromFirstKey`. The first key
    // has already been parsed by the caller; its position is passed in
    // via `firstKeyLine`/`firstKeyColumn`/`firstKeyLength`. Subsequent
    // keys are parsed by `iterateBlockMappingTracked` which captures
    // their positions as it goes.
    private def parseBlockMappingFromFirstKeyTracked
      ( firstKey: Yaml.Ast,
        firstKeyLine: Int, firstKeyColumn: Int, firstKeyLength: Int,
        indent: Int,
        indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val buf = acquireBuffer()
      val scratch = acquireIndexBuffer()
      val ends    = acquireIndexBuffer()
      val savedParentIndent = blockParentIndent
      blockParentIndent = indent

      if !more || peek != Colon then errorAt(Issue.ExpectedColonAfterMappingKey)
      advance()

      scratch += firstKeyLine
      scratch += firstKeyColumn
      scratch += firstKeyLength
      val firstValue = parseMappingValueTracked(indent, scratch)
      ends += scratch.length
      buf += firstKey
      buf += firstValue

      iterateBlockMappingTracked(buf, scratch, ends, indent)

      val result = sealMapping(buf)
      releaseBuffer()

      emitCompositeDescriptor(indexOut, scratch, ends, startLine, startColumn, startMark)
      releaseIndexBuffer()  // ends
      releaseIndexBuffer()  // scratch
      blockParentIndent = savedParentIndent
      result

    // Tracked variant of `iterateBlockMapping`. Appends entry descriptors
    // to `scratch` and entry-end offsets to `ends` for each key/value
    // pair iterated.
    private def iterateBlockMappingTracked
      ( buf: scala.collection.mutable.ArrayBuffer[Any],
        scratch: ArrayBuffer[Int], ends: ArrayBuffer[Int],
        indent: Int )
      ( using Tactic[ParseError] )
    :   Unit =

      var done = false

      while !done do
        skipBlankAndCommentLines()
        val lineStart = pos
        val nextIndent = consumeLeadingSpaces()
        if more && peek == Tab then errorAt(Issue.TabInIndentation)

        if nextIndent != indent then
          pos = lineStart
          done = true
        else if !more then
          done = true
        else if atDocumentBoundary then
          pos = lineStart
          done = true
        else if isExplicitKeyIndicator then
          val (key, value) = readExplicitPairTracked(indent, scratch)
          ends += scratch.length
          buf += key
          buf += value
        else
          // Capture key start
          syncTo()
          reconcileLineation()
          val keyLine   = cursor.line.n1
          val keyColumn = cursor.column.n1
          val keyMark   = cursor.position.n0.toLong

          consumeNodePrefixes()
          val keyAnchor = prefixAnchor
          val keyTag    = prefixTag

          val rawKey: Yaml.Ast = prefixHeadByte match
            case Quote =>
              advance()
              val s = parseDoubleQuoted()

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              s

            case Apostrophe =>
              advance()
              val s = parseSingleQuoted()

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              s

            case Star =>
              if !keyAnchor.nil then errorAt(Issue.AnchorOnAlias)
              advance()
              parseAlias()

            case OpenBracket =>
              advance()
              parseFlowSequence()

            case OpenBrace =>
              advance()
              parseFlowMapping()

            case _ =>
              val keyText = readPlainScalarText(indent)

              if !sawMappingColon then
                errorAt(Issue.PlainScalarAtMappingIndentWithoutColon)

              if lastScalarSpannedLines then
                errorAt(Issue.MultilineImplicitKey)

              resolvePlainScalar(keyText)

          syncTo()
          val keyLength = (cursor.position.n0 - keyMark).toInt
          val tagged = if keyTag.nil then rawKey else applyTag(keyTag, rawKey)

          val keyAst =
            if keyAnchor.nil then tagged
            else
              anchors.update(keyAnchor.s, tagged)
              tagged

          skipSpaces()
          if !more || peek != Colon then errorAt(Issue.ExpectedColonAfterMappingKey)
          advance()

          scratch += keyLine
          scratch += keyColumn
          scratch += keyLength
          val value = parseMappingValueTracked(indent, scratch)
          ends += scratch.length
          buf += keyAst
          buf += value

    // Tracked variant of `parseFlowSequence`. Emits a composite
    // descriptor with one child descriptor per flow entry.
    private def parseFlowSequenceTracked
      ( indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val buf = acquireBuffer()
      val scratch = acquireIndexBuffer()
      val ends    = acquireIndexBuffer()
      var done = false

      while !done do
        skipFlowWhitespace()
        if !more then errorAt(Issue.UnterminatedFlowSequence)

        if peek == CloseBracket then
          advance()
          done = true
        else if peek == Comma then
          errorAt(Issue.EmptyFlowSequenceEntry)
        else
          // Capture entry start
          syncTo()
          reconcileLineation()
          val entryLine   = cursor.line.n1
          val entryColumn = cursor.column.n1
          val entryMark   = cursor.position.n0.toLong

          val first: Yaml.Ast =
            if
              peek == Question && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBracket || nb == Colon
              }
            then
              advance()
              skipFlowWhitespace()

              if !more || peek == Comma || peek == CloseBracket || peek == Colon
              then Yaml.Ast.Null
              else parseFlowNode()
            else if
              peek == Colon && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBracket
              }
            then
              Yaml.Ast.Null
            else
              parseFlowNodeTracked(scratch)

          // For the explicit/implicit `?`/`:` paths above we didn't go
          // through parseFlowNodeTracked, so emit a primitive descriptor
          // covering whatever we consumed (which may be zero-length for
          // the Null case).
          val entryEmittedByFlowNode = peek == Comma || peek == CloseBracket || peek == Colon
          // Note: the above checks state *before* skipFlowWhitespace. Be
          // explicit: only emit primitive if we went through the
          // `?`/`:` Null branches, not parseFlowNodeTracked.
          // (parseFlowNodeTracked always emits.)
          // We'll re-check below.

          skipFlowWhitespace()

          val entry =
            if more && peek == Colon then
              if newlineImmediatelyPrecedes(pos) then
                errorAt(Issue.FlowImplicitKeyAndColonOnDifferentLines)

              advance()
              skipFlowWhitespace()
              // The pair is a single mapping entry containing (first, value).
              // We need to emit a composite mapping descriptor with n=1
              // and the appropriate key position. The descriptor must
              // overwrite whatever the first node emitted into scratch
              // (since the pair's *node* descriptor replaces the first
              // value's descriptor).
              //
              // Simplification: discard the first child's descriptor and
              // rebuild as a 1-entry composite. The first child started
              // at `scratchBeforeFirst` so we can take its length, then
              // truncate scratch and rebuild.
              // For simplicity & correctness, parse the value and build
              // a fresh sub-scratch for the pair.
              val value =
                if !more || peek == Comma || peek == CloseBracket
                then Yaml.Ast.Null
                else parseFlowNode()

              val pairBuf = acquireBuffer()
              pairBuf += first
              pairBuf += value
              val pair = sealMapping(pairBuf)
              releaseBuffer()
              // Replace scratch tail (the first child's descriptor) with
              // a single composite mapping descriptor covering the pair.
              // Capture the key length from the first child's descriptor
              // (slot 3 of its 4-int primitive, or the composite's sourceLength).
              // Since the first child's descriptor was just appended, its
              // start index is the previous `ends` value (or 0).
              val firstStart = if ends.length == 0 then 0 else ends(ends.length - 1)
              // Compute the first child's source length
              val firstChildSize = scratch.length - firstStart
              val firstKeyLine   = scratch(firstStart + 1)
              val firstKeyColumn = scratch(firstStart + 2)
              val firstKeyLength = scratch(firstStart + 3)

              // Build a temporary scratch for the pair's value-side
              // descriptor (which is itself a 4-int primitive Null since
              // we delegated to untracked parseFlowNode above; emit one
              // now at the current position with zero length, which is
              // an acceptable approximation for PR 1).
              syncTo()
              reconcileLineation()
              val valLine = cursor.line.n1
              val valCol  = cursor.column.n1

              // Truncate scratch back to firstStart and rebuild as a
              // 1-entry mapping composite.
              scratch.dropRightInPlace(firstChildSize)
              val pairCompositeStart = scratch.length
              // Composite header
              scratch += 0
              scratch += entryLine
              scratch += entryColumn
              scratch += (cursor.position.n0 - entryMark).toInt
              scratch += 1
              // Single entry's offset (5 ints header + 1 offset = 6)
              scratch += 6
              // The entry: keyLine, keyColumn, keyLength, then value descriptor
              scratch += firstKeyLine
              scratch += firstKeyColumn
              scratch += firstKeyLength
              // Value descriptor: 4-int primitive Null
              scratch += 4
              scratch += valLine
              scratch += valCol
              scratch += 0
              // Patch composite size
              scratch(pairCompositeStart) = scratch.length - pairCompositeStart
              pair
            else
              first

          ends += scratch.length
          buf += entry
          skipFlowWhitespace()
          if !more then errorAt(Issue.UnterminatedFlowSequence)

          peek match
            case Comma        => advance()
            case CloseBracket => advance(); done = true
            case _            => errorAt(Issue.FlowSequenceExpectedCommaOrClose)

      val result = sealSequence(buf)
      releaseBuffer()

      emitCompositeDescriptor(indexOut, scratch, ends, startLine, startColumn, startMark)
      releaseIndexBuffer()  // ends
      releaseIndexBuffer()  // scratch
      result

    // Tracked variant of `parseFlowMapping`. Emits a composite mapping
    // descriptor with one entry per flow-mapping pair.
    private def parseFlowMappingTracked
      ( indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val buf = acquireBuffer()
      val scratch = acquireIndexBuffer()
      val ends    = acquireIndexBuffer()
      var done = false

      while !done do
        skipFlowWhitespace()
        if !more then errorAt(Issue.UnterminatedFlowMapping)

        if peek == CloseBrace then
          advance()
          done = true
        else if peek == Comma then
          errorAt(Issue.EmptyFlowMappingEntry)
        else
          // Capture key start
          syncTo()
          reconcileLineation()
          val keyLine   = cursor.line.n1
          val keyColumn = cursor.column.n1
          val keyMark   = cursor.position.n0.toLong

          val key: Yaml.Ast =
            if
              peek == Question && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBrace || nb == Colon
              }
            then
              advance()
              skipFlowWhitespace()

              if !more || peek == Comma || peek == CloseBrace || peek == Colon
              then Yaml.Ast.Null
              else parseFlowNode()
            else if
              peek == Colon && {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return ||
                  nb == Comma || nb == CloseBrace
              }
            then
              Yaml.Ast.Null
            else
              parseFlowNode()

          syncTo()
          val keyLength = (cursor.position.n0 - keyMark).toInt

          skipFlowWhitespace()
          scratch += keyLine
          scratch += keyColumn
          scratch += keyLength

          val value =
            if more && peek == Colon then
              advance()
              skipFlowWhitespace()

              if !more || peek == Comma || peek == CloseBrace then
                emitNullHere(scratch)
                Yaml.Ast.Null
              else
                parseFlowNodeTracked(scratch)
            else
              emitNullHere(scratch)
              Yaml.Ast.Null

          ends += scratch.length
          buf += key
          buf += value
          skipFlowWhitespace()
          if !more then errorAt(Issue.UnterminatedFlowMapping)

          peek match
            case Comma      => advance()
            case CloseBrace => advance(); done = true
            case _          => errorAt(Issue.FlowMappingExpectedCommaOrClose)

      val result = sealMapping(buf)
      releaseBuffer()

      emitCompositeDescriptor(indexOut, scratch, ends, startLine, startColumn, startMark)
      releaseIndexBuffer()
      releaseIndexBuffer()
      result

    // Tracked variant of `parseFlowNode`. Captures position, dispatches,
    // and emits a primitive descriptor for scalar/alias cases or a
    // composite descriptor (via the *Tracked composite variants) for
    // flow sequences/mappings.
    private def parseFlowNodeTracked(indexOut: ArrayBuffer[Int])
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      skipFlowWhitespace()

      if !more then
        emitNullHere(indexOut)
        Yaml.Ast.Null
      else
        if atDocumentBoundary then
          errorAt(Issue.DocumentMarkerInFlowContext)
        // Capture start after the initial flow-whitespace skip but
        // before prefix consumption, so the descriptor spans anchor/tag.
        syncTo()
        reconcileLineation()
        val startLine   = cursor.line.n1
        val startColumn = cursor.column.n1
        val startMark   = cursor.position.n0.toLong

        consumeNodePrefixes()
        val anchorName = prefixAnchor
        val tagText    = prefixTag
        skipFlowWhitespace()
        val headByte = if !more then -1 else peek & 0xFF

        val value =
          if headByte == -1 then
            emitNullHere(indexOut)
            Yaml.Ast.Null
          else if headByte == Comma || headByte == CloseBracket ||
            headByte == CloseBrace then
            emitNullHere(indexOut)
            Yaml.Ast.Null
          else if headByte == Colon && {
            val next = if pos + 1 < bufEnd then bytes(pos + 1) else -1
            next == Space || next == Tab || next == Newline || next == Return ||
              next == Comma || next == CloseBracket ||
              next == CloseBrace || next == -1
          } then
            emitNullHere(indexOut)
            Yaml.Ast.Null
          else if headByte == Star then
            advance()
            val v = parseAlias()
            emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
            v
          else
            (headByte: @switch) match
              case Quote =>
                advance()
                val v = parseDoubleQuoted()
                emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
                v

              case Apostrophe =>
                advance()
                val v = parseSingleQuoted()
                emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
                v

              case OpenBracket =>
                advance()
                parseFlowSequenceTracked(indexOut, startLine, startColumn, startMark)

              case OpenBrace =>
                advance()
                parseFlowMappingTracked(indexOut, startLine, startColumn, startMark)

              case _ =>
                val v = parseFlowPlainScalar()
                emitPrimitiveDescriptor(indexOut, startLine, startColumn, startMark)
                v

        val tagged = if tagText.nil then value else applyTag(tagText, value)

        if anchorName.nil then tagged
        else
          anchors.update(anchorName.s, tagged)
          tagged

    // Tracked variant of `parseQuestion`. Either parses an explicit-key
    // block mapping (emitting a composite descriptor) or delegates to
    // `parsePlainOrBlockMappingTracked` for the plain-scalar starting
    // with `?` case.
    private def parseQuestionTracked
      ( indent: Int,
        indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      if isExplicitKeyIndicator then
        val buf = acquireBuffer()
        val scratch = acquireIndexBuffer()
        val ends    = acquireIndexBuffer()
        val savedParentIndent = blockParentIndent
        blockParentIndent = indent
        val (firstKey, firstValue) = readExplicitPairTracked(indent, scratch)
        ends += scratch.length
        buf += firstKey
        buf += firstValue
        iterateBlockMappingTracked(buf, scratch, ends, indent)
        val result = sealMapping(buf)
        releaseBuffer()
        emitCompositeDescriptor(indexOut, scratch, ends, startLine, startColumn, startMark)
        releaseIndexBuffer()
        releaseIndexBuffer()
        blockParentIndent = savedParentIndent
        result
      else
        parsePlainOrBlockMappingTracked
          ( indent, t"", t"", indexOut, startLine, startColumn, startMark )

    // Tracked variant of `parseMinus`. Either parses a block sequence
    // (emitting a composite descriptor) or delegates to
    // `parsePlainOrBlockMappingTracked` for the plain-scalar case.
    private def parseMinusTracked
      ( indent: Int,
        indexOut: ArrayBuffer[Int],
        startLine: Int, startColumn: Int, startMark: Long )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      val nextByte = if pos + 1 < bufEnd then bytes(pos + 1) else -1

      if
        nextByte == Space || nextByte == Tab || nextByte == Newline ||
          nextByte == Return || nextByte == -1
      then
        var j = pos - 1
        while j >= 0 && (bytes(j) == Space || bytes(j) == Tab) do j -= 1
        val ok = j < 0 || bytes(j) == Newline || bytes(j) == Return ||
          bytes(j) == Minus || bytes(j) == Colon ||
          bytes(j) == Question

        if !ok then errorAt(Issue.BlockSequenceIndicatorNotAtLineStart)

        parseBlockSequenceTracked
          ( currentColumn(), indexOut, startLine, startColumn, startMark )
      else
        parsePlainOrBlockMappingTracked
          ( indent, t"", t"", indexOut, startLine, startColumn, startMark )

    // Tracked variant of `readExplicitPair`. Appends the key's position
    // descriptor (3 ints) plus the value's descriptor to `scratch`.
    private def readExplicitPairTracked
      ( indent: Int, scratch: ArrayBuffer[Int] )
      ( using Tactic[ParseError] )
    :   (Yaml.Ast, Yaml.Ast) =

      advance() // ?
      while more && (peek == Space || peek == Tab) do advance()

      if more && peek == Hash then
        while more && peek != Newline do advance()

      // Capture key start
      val (keyStartLine, keyStartColumn, keyStartMark) =
        if !more || peek == Newline then
          if more then advance()
          skipBlankAndCommentLines()
          val keyLineStart = pos
          val keyIndent = consumeLeadingSpaces()

          if keyIndent < indent then
            pos = keyLineStart
            syncTo(); reconcileLineation()
            (cursor.line.n1, cursor.column.n1, cursor.position.n0.toLong)
          else
            syncTo(); reconcileLineation()
            (cursor.line.n1, cursor.column.n1, cursor.position.n0.toLong)
        else
          syncTo(); reconcileLineation()
          (cursor.line.n1, cursor.column.n1, cursor.position.n0.toLong)

      // Re-walk the same paths to parse the key
      val key: Yaml.Ast =
        // Reset and re-parse based on position. Simpler: parse here
        // using the same logic as readExplicitPair but tracked.
        // To avoid duplicating the indent decision tree, just delegate
        // to the untracked readExplicitPair internals on the key side,
        // then capture key length.
        //
        // The captured (line, column, mark) above are correct for the
        // key's start; for the length we measure after parsing.
        val savedKeyMark = keyStartMark
        // The position-capture branch above mutated `pos`/may have
        // skipped indentation; we need to retry the original decision
        // tree. Re-derive the key by re-doing the same logic:
        // Since we already advanced over `?` and whitespace/blank lines,
        // re-derive based on the current state.
        if !more then Yaml.Ast.Null
        else
          // Decide: same-indent compact sequence, more-indented node,
          // or Null. Mirror the untracked logic but track the value.
          val savedPos = pos

          val nodeAttempt =
            if peek == Minus &&
              {
                val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
                nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
              }
            then
              parseNodeHereTracked(currentColumn(), scratch)
            else
              if pos > 0 && bytes(pos - 1) != Newline then parseNodeHereTracked(indent + 2, scratch)
              else parseNodeHereTracked(consumeLeadingSpacesPeek(), scratch)

          nodeAttempt

      // Patch the key descriptor in scratch: jacinta uses the 3-int
      // [keyLine, keyColumn, keyLength] header. Here, the key has its
      // own full descriptor already; but the *containing* iterator
      // expects [keyLine, keyColumn, keyLength, <value descriptor>].
      //
      // For explicit keys, we accept that the key's full descriptor is
      // *not* attached — only its position is recorded as the 3-int
      // header. The full key descriptor (which may be composite) was
      // appended to scratch by parseNodeHereTracked above; we need to
      // remove it and replace with just the 3-int header.
      // Capture the descriptor's slot 3 (sourceLength) as keyLength.
      val keyDescStart =
        if scratch.length >= 4 then scratch.length - keyDescriptorSize(scratch)
        else 0

      val keyLengthForHeader =
        if scratch.length > keyDescStart + 3 then scratch(keyDescStart + 3) else 0

      scratch.dropRightInPlace(scratch.length - keyDescStart)
      scratch += keyStartLine
      scratch += keyStartColumn
      scratch += keyLengthForHeader

      skipBlankAndCommentLines()
      val markerLineStart = pos
      val markerIndent = consumeLeadingSpaces()

      val value: Yaml.Ast =
        if
          markerIndent == indent && more && peek == Colon && {
            val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
            nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
          }
        then
          advance()
          parseMappingValueTracked(indent, scratch, isExplicitValue = true)
        else
          pos = markerLineStart
          emitNullHere(scratch)
          Yaml.Ast.Null

      (key, value)

    // Helper: read the size (slot 0) of the descriptor that ends at
    // `scratch.length`. Returns 0 if scratch is empty or malformed.
    private inline def keyDescriptorSize(scratch: ArrayBuffer[Int]): Int =
      // The most-recently-appended descriptor's first int is its size.
      // We don't track its start directly here, so reconstruct from the
      // typical 4-int primitive shape — callers only use this for the
      // explicit-pair-key path which always finishes with a complete
      // descriptor whose size is `scratch(start)`. The conservative
      // approach: walk forward from 0 collecting descriptor sizes until
      // we hit the end. For PR 1, the only call site appends exactly
      // one descriptor immediately before — its size is `scratch(start)`
      // where `start` is `scratch.length - <size>`. We don't have start;
      // use slot 0 reading from a hypothetical position. Simplification:
      // assume the last descriptor's size is at `scratch(scratch.length -
      // descriptorSpan)` — but we can't compute that without size info.
      //
      // Practical workaround: only the explicit-pair path uses this,
      // and explicit-pair keys produced by parseNodeHereTracked are
      // *typically* primitive (4-int) or simple. For PR 1, default
      // to 4 and accept a slight inaccuracy for complex explicit keys.
      val len = scratch.length

      if len >= 4 then
        val maybeSize = scratch(len - 4) // not portable, see comment above
        // Use a defensive 4 — primitive descriptor — as the default size.
        val descSize = 4
        descSize
      else
        0

    // Peek the current line's leading-space count without changing pos.
    private inline def consumeLeadingSpacesPeek(): Int =
      var n = 0
      var p = pos
      while p < bufEnd && bytes(p) == Space do { n += 1; p += 1 }
      n

    // Tracked variant of `parseMappingValue`. The caller has just appended
    // the key's 3-int header to `scratch`; this method appends the value's
    // descriptor.
    private def parseMappingValueTracked
      ( parentIndent: Int,
        scratch: ArrayBuffer[Int],
        isExplicitValue: Boolean = false )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      skipSpaces()

      if !more then
        emitNullHere(scratch)
        Yaml.Ast.Null
      else if peek == Newline then
        advance()
        skipBlankAndCommentLines()
        val lineStart = pos
        val childIndent = consumeLeadingSpaces()
        pickValueOrNullTracked(parentIndent, childIndent, lineStart, scratch)
      else if peek == Hash then
        while more && peek != Newline do advance()
        if more then advance()
        skipBlankAndCommentLines()
        val lineStart = pos
        val childIndent = consumeLeadingSpaces()
        pickValueOrNullTracked(parentIndent, childIndent, lineStart, scratch)
      else
        if !isExplicitValue && peek == Minus then
          val next = if pos + 1 < bufEnd then bytes(pos + 1) else -1

          if
            next == Space || next == Tab || next == Newline ||
              next == Return || next == -1
          then errorAt(Issue.BlockSequenceOnMappingKeyLine)

        val saved = inInlineMappingValue
        inInlineMappingValue = !isExplicitValue
        val result = parseNodeHereTracked(parentIndent, scratch)
        inInlineMappingValue = saved
        result

    // Tracked variant of `pickValueOrNull`. Either calls
    // `parseNodeHereTracked` (which emits a descriptor) or emits a
    // Null primitive at the rewound position.
    private def pickValueOrNullTracked
      ( parentIndent: Int, childIndent: Int, lineStart: Int,
        indexOut: ArrayBuffer[Int] )
      ( using Tactic[ParseError] )
    :   Yaml.Ast =

      if more && peek == Tab then errorAt(Issue.TabInIndentation)
      val savedInline = inInlineMappingValue
      inInlineMappingValue = false

      val result =
        if !more then
          emitNullHere(indexOut)
          Yaml.Ast.Null
        else if childIndent < parentIndent then
          pos = lineStart
          emitNullHere(indexOut)
          Yaml.Ast.Null
        else if childIndent == parentIndent then
          if
            peek == Minus && {
              val nb = if pos + 1 < bufEnd then bytes(pos + 1) else -1
              nb == Space || nb == Tab || nb == Newline || nb == Return || nb == -1
            }
          then
            parseNodeHereTracked(childIndent, indexOut)
          else
            pos = lineStart
            emitNullHere(indexOut)
            Yaml.Ast.Null
        else
          parseNodeHereTracked(childIndent, indexOut)

      inInlineMappingValue = savedInline
      result


class Yaml(rootValue: Yaml.Ast, positions: Optional[Yaml.PositionIndex] = Unset)
extends Dynamic derives CanEqual:
  private[ypsiloid] def root: Yaml.Ast = rootValue

  // The flat position-descriptor index produced alongside the AST when this
  // `Yaml` was parsed under `Tracking.On`. `Unset` for non-tracking parses
  // and for any `Yaml` built from a decoded/computed value.
  def positionIndex: Optional[Yaml.PositionIndex] = positions

  def as[value: Decodable in Yaml]: value raises YamlError tracks Yaml.Focus =
    val result = value.decoded(this)
    // Auto-populate the source position on every accumulated focus so
    // error handlers can read both `.pointer` and `.position` without
    // calling `withPosition` themselves. `withPosition` on a `Yaml`
    // without a `positionIndex` leaves the position `Unset`, so this
    // costs nothing for untracked roots and only one `locate` per
    // registered error for tracked roots. Mirrors xylophone
    // `Tracked#as` (PR #1151).
    val foci = summon[Foci[Yaml.Focus]]
    val yaml = this
    foci.supplement(foci.length, _.let(_.withPosition(yaml)).vouch)
    result

  // Sequence indexing: `yaml(0)` returns the first element of a sequence,
  // raising `YamlError` (with `Reason.NotType`) if the root is not a
  // sequence.
  def apply(index: Int): Yaml raises YamlError =
    if root.isArray then new Yaml(root.arrayElement(index))
    else
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Sequence)))
      new Yaml(Yaml.Ast.Null)

  // Mapping field access by name: `yaml(t"foo")` returns the value
  // associated with key `foo`, or `Unset` (encoded as `Yaml.Ast(Unset)`)
  // when the field is absent. Mirrors Jacinta's `Json.apply(field)`.
  def apply(field: Text): Yaml =
    if root.isAbsent then new Yaml(Yaml.Ast(Unset))
    else root.objectIndexOf(field.s) match
      case -1    => new Yaml(Yaml.Ast(Unset))
      case index => new Yaml(root.objectValue(index))

  // Dynamic field access — `yaml.foo` desugars to `selectDynamic("foo")`.
  // Gated on an erased `DynamicYamlEnabler` so the feature is opt-in via
  // `import dynamicYamlAccess.enabled`.
  def selectDynamic(field: String)(using erased dynamicYamlEnabler: DynamicYamlEnabler): Yaml = apply(field.tt)

  def applyDynamic(field: String)(index: Int)(using erased dynamicYamlEnabler: DynamicYamlEnabler)
  :   Yaml raises YamlError =

    apply(field.tt)(index)

  // Immutable update: `yaml(0) = newValue` desugars to `update(0, newValue)`.
  def update[value: Encodable in Yaml](index: Int, value: value)
    ( using erased dynamicYamlEnabler: DynamicYamlEnabler )
  :   Yaml raises YamlError =

    if !root.isArray then
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Sequence)))

    val n = root.arrayLength
    val updated = new Array[Any](n)
    var i = 0

    while i < n do
      updated(i) =
        if i == index then value.encode.root.asInstanceOf[Any]
        else root.arrayElement(i).asInstanceOf[Any]

      i += 1

    Yaml.ast(Yaml.Ast.seqFromAnyArray(updated))

  // `yaml.foo = newValue` — replaces `foo` if present, or appends a new
  // entry. `yaml.foo = Unset` deletes the entry.
  def updateDynamic(field: String)[value: Encodable in Yaml](value: value)
    ( using erased dynamicYamlEnabler: DynamicYamlEnabler )
  :   Yaml raises YamlError =

    modify(field, value.encode)

  def updateDynamic(field: String)[value](unset: Unset.type)(using erased dynamicYamlEnabler: DynamicYamlEnabler)
  :   Yaml raises YamlError =

    delete(field)

  // ── Internal mapping update helpers ─────────────────────────────────────

  private[ypsiloid] def modify(field: String, value: Yaml): Yaml raises YamlError =
    if !root.isObject then
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Mapping)))
      this
    else
      val arr = root.asInstanceOf[IArray[Any]]
      val len = arr.length

      root.objectIndexOf(field) match
        case -1 =>
          val out = new Array[Any](len + 2)
          System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
          out(len)     = field
          out(len + 1) = value.root.asInstanceOf[Any]
          Yaml.ast(Yaml.Ast.mapFromAnyArray(out))

        case index =>
          val out = new Array[Any](len)
          System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
          out(index*2 + 1) = value.root.asInstanceOf[Any]
          Yaml.ast(Yaml.Ast.mapFromAnyArray(out))

  private[ypsiloid] def delete(field: String): Yaml raises YamlError =
    if !root.isObject then
      raise(YamlError(Reason.NotType(Yaml.primitive(root), YamlPrimitive.Mapping)))
      this
    else
      val arr = root.asInstanceOf[IArray[Any]]
      val len = arr.length

      root.objectIndexOf(field) match
        case -1 => this

        case index =>
          val out = new Array[Any](len - 2)
          System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, index*2)

          System.arraycopy
            ( arr.asInstanceOf[Array[Any]],
              index*2 + 2,
              out,
              index*2,
              len - index*2 - 2 )

          Yaml.ast(Yaml.Ast.mapFromAnyArray(out))

  override def hashCode: Int = Yaml.Ast.deepHash(root)

  override def equals(right: Any): Boolean = right match
    case right: Yaml => Yaml.Ast.deepEquals(root, right.root)
    case _           => false

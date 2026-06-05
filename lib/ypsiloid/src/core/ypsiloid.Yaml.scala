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
import symbolism.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import YamlError.Reason

trait Yaml2:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Yaml )
  =>  value is Encodable in Yaml =

    new Encodable:
      type Self = Optional[value]
      type Form = Yaml

      def encoded(value: Optional[value]): Yaml =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_))
        . or(Yaml.ast(Yaml.Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[YamlError]
  =>  ( decodable: => inner is Decodable in Yaml )
  =>  value is Decodable in Yaml = yaml =>
    if yaml.root.asInstanceOf[AnyRef] eq Unset then Unset else decodable.decoded(yaml)


  inline given decodable: [value] => value is Decodable in Yaml = summonFrom:
    case given (`value` is Decodable in Text) =>
      yaml =>
        provide[Tactic[YamlError]]:
          yaml.root.asMatchable match
            case s: String => s.tt.decode[value]

            case _ =>
              abort(YamlError(Reason.NotType(Yaml.primitive(yaml.root), YamlPrimitive.Str)))

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Yaml = summonFrom:
    case given (`value` is Encodable in Text) => value => Yaml.ast(Yaml.Ast(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Yaml]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Yaml = yaml =>
      provide[Foci[Yaml.Focus]]:
        provide[Tactic[YamlError]]:
          val arr: IArray[Any] | Null = yaml.root.asMatchable match
            case xs: IArray[?] @unchecked if (xs.length & 1) == 0 =>
              xs.asInstanceOf[IArray[Any]]

            case _ => null

          if arr != null then buildWith(arr)
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
                  else Reason.NotType(primitive(yaml.root), YamlPrimitive.Mapping)
                raise(YamlError(reason)) yet derivationDefault()
              case _ =>
                buildWith(null)

    private inline def buildWith[derivation <: Product: ProductReflection]
      ( arr: IArray[Any] | Null )
      ( using Foci[Yaml.Focus], Tactic[YamlError] )
    :   derivation =

      // `@name[Yaml]` / bare `@name` renames: field name -> mapping key, read
      // back the same way they are written.
      val renames: Map[Text, Text] = relabelling[derivation, Yaml]

      build: [field] =>
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
            val labels: List[Text] = variantLabels[derivation]

            // `@name[Yaml]` / bare `@name` variant renames: map the serialized
            // discriminator back to the variant name before delegating.
            val variantNames: Map[Text, Text] =
              variantRelabelling[derivation, Yaml].map((variant, wire) => wire -> variant)

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
    :   derivation is Encodable in Yaml = value =>
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
              if !(encoded.asInstanceOf[AnyRef] eq Unset) then
                entries += Yaml.Ast.Str(key).asInstanceOf[Any]
                entries += encoded.asInstanceOf[Any]
        Yaml.ast(Yaml.Ast.mapFromAnyArray(entries.toArray))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Yaml = value =>
      val discriminable = infer[derivation is Discriminable in Yaml]

      // `@name[Yaml]` / bare `@name` variant renames: variant name -> wire
      // discriminator, read back the same way by the decoder.
      val variantNames: Map[Text, Text] = variantRelabelling[derivation, Yaml]

      variant(value): [variant <: derivation] =>
        value => discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

object Yaml extends Yaml2, Dynamic:
  type YamlString    = String
  type YamlInteger   = Long | Bcd
  type YamlDecimal   = Double | Bcd
  type YamlBoolean   = Boolean
  type YamlNull      = Null
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
  opaque type Ast =
    YamlString | YamlInteger | YamlDecimal | YamlBoolean | YamlNull | YamlSequence | YamlMapping
    | Unset.type

  // Whether `YamlParser` captures line/column/length descriptors
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
  
    inline def apply
      ( value:
        Long | Double | Bcd | Boolean | String | IArray[Any] | Null | Unset.type )
    :   Yaml.Ast =

      value
  
    val Null: Yaml.Ast = null
  
    inline def Bool(value: Boolean): Yaml.Ast = value
    inline def Integer(value: Long): Yaml.Ast = value
    inline def Decimal(value: Double): Yaml.Ast = value
    inline def BcdValue(value: Bcd): Yaml.Ast = value
    inline def Str(value: Text): Yaml.Ast = value.s
  
    // Wrap an `IArray[Yaml.Ast]` of items as a sequence node. If the count
    // is even, append the `arrayPad` sentinel so the final node has odd
    // length.
    def Sequence(items: IArray[Yaml.Ast]): Yaml.Ast =
      val n = items.length
      if (n & 1) == 1 then items.asInstanceOf[IArray[Any]]
      else
        val padded = new Array[Any](n + 1)
        System.arraycopy(items.asInstanceOf[Array[Any]], 0, padded, 0, n)
        padded(n) = arrayPad
        padded.asInstanceOf[IArray[Any]]
  
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
      arr.asInstanceOf[IArray[Any]]
  
    // Build a sequence directly from a raw `Array[Any]` of items (no
    // copy if the length is already odd; pad once otherwise). The parser
    // uses this to avoid the `Array.map` step.
    private[ypsiloid] def seqFromAnyArray(items: Array[Any]): Yaml.Ast =
      val n = items.length
      if (n & 1) == 1 then items.asInstanceOf[IArray[Any]]
      else
        val padded = new Array[Any](n + 1)
        System.arraycopy(items, 0, padded, 0, n)
        padded(n) = arrayPad
        padded.asInstanceOf[IArray[Any]]
  
    // Build a mapping directly from a flat `Array[Any]` of alternating
    // key/value entries. Length must be even.
    private[ypsiloid] def mapFromAnyArray(entries: Array[Any]): Yaml.Ast =
      entries.asInstanceOf[IArray[Any]]
  
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
        case _                         => None
  
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
  
    def deepHash(ast: Yaml.Ast): Int = ast match
      case null         => 0
      case b: Boolean   => b.hashCode
      case n: Long      => n.hashCode
      case d: Double    => d.hashCode
      case s: String    => s.hashCode
  
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

      case _: Unset.type => 1

  def ast(value: Yaml.Ast): Yaml = new Yaml(value)

  // `object Yaml` extends `Dynamic`, which suppresses the universal-apply
  // synthesis for `Yaml(...)` once the primary constructor takes more than
  // one parameter; these explicit overloads forward to the constructor.
  def apply(value: Any): Yaml = new Yaml(value)
  def apply(value: Any, positions: Optional[Yaml.PositionIndex]): Yaml =
    new Yaml(value, positions)

  // Canonical external accessor for the underlying AST. The `root`
  // field on `class Yaml` is package-private so that breaking through
  // the `Yaml` abstraction is a deliberate, named action.
  def unseal(yaml: Yaml): Yaml.Ast = yaml.root

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

  given bytes: Tactic[YamlError] => Bytes is Decodable in Yaml = _.root.long.b

  given lens: [name <: Label: ValueOf] => (erased DynamicYamlEnabler) => Tactic[YamlError]
  =>  name is Lens from Yaml onto Yaml =
    Lens(_.selectDynamic(valueOf[name]), _.modify(valueOf[name], _))

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

  given int: Tactic[YamlError] => Int is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n.toInt
      case d: Double => d.toInt
      case _         => primitiveFault(yaml, YamlPrimitive.Integer, 0)

  given long: Tactic[YamlError] => Long is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case n: Long   => n
      case d: Double => d.toLong
      case _         => primitiveFault(yaml, YamlPrimitive.Integer, 0L)

  given double: Tactic[YamlError] => Double is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case d: Double => d
      case n: Long   => n.toDouble
      case _         => primitiveFault(yaml, YamlPrimitive.Decimal, 0.0)

  given float: Tactic[YamlError] => Float is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case d: Double => d.toFloat
      case n: Long   => n.toFloat
      case _         => primitiveFault(yaml, YamlPrimitive.Decimal, 0.0f)

  given boolean: Tactic[YamlError] => Boolean is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case b: Boolean => b
      case _          => primitiveFault(yaml, YamlPrimitive.Bool, false)

  given text: Tactic[YamlError] => Text is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case s: String => s.tt
      case _         => primitiveFault(yaml, YamlPrimitive.Str, t"")

  given string: Tactic[YamlError] => String is Decodable in Yaml = yaml =>
    yaml.root.asMatchable match
      case s: String => s
      case _         => primitiveFault(yaml, YamlPrimitive.Str, "")

  given unit: Tactic[YamlError] => Unit is Decodable in Yaml = yaml =>
    if yaml.root.asInstanceOf[AnyRef] == null then ()
    else primitiveFault(yaml, YamlPrimitive.Null, ())

  given iterable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        tactic:    Tactic[YamlError],
        foci:      Foci[Yaml.Focus] )
  =>  ( decodable: => element is Decodable in Yaml )
  =>  collection[element] is Decodable in Yaml = yaml =>
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
  =>  Map[Text, value] is Decodable in Yaml = yaml =>
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
            if rawKey.asInstanceOf[AnyRef] == null then t"null"
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
    if yaml.root.isAbsent || yaml.root.asInstanceOf[AnyRef] == null then None
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
  =>  collection[element] is Encodable in Yaml = values =>
    val items = IArray.from(values.map(encodable.encode(_).root))
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

  given decodable: (Tactic[ParseError], Yaml.Tracking) => Yaml is Decodable in Text =
    text => summon[Yaml.Tracking] match
      case Yaml.Tracking.On =>
        val (ast, ints) = YamlParser.parseTracked(text)
        new Yaml(ast, Yaml.PositionIndex(ints))
      case Yaml.Tracking.Off =>
        Yaml(YamlParser.parse(text))

  def parseAll(input: Text)(using Tactic[ParseError], Yaml.Tracking): List[Yaml] =
    summon[Yaml.Tracking] match
      case Yaml.Tracking.On =>
        YamlParser.parseAllTracked(input).map: (ast, ints) =>
          new Yaml(ast, Yaml.PositionIndex(ints))
      case Yaml.Tracking.Off =>
        YamlParser.parseAll(input).map(Yaml(_))

  given aggregable: (Tactic[ParseError], Yaml.Tracking) => Yaml is Aggregable by Text =
    summon[Text is Aggregable by Text].map: text =>
      summon[Yaml.Tracking] match
        case Yaml.Tracking.On =>
          val (ast, ints) = YamlParser.parseTracked(text)
          new Yaml(ast, Yaml.PositionIndex(ints))
        case Yaml.Tracking.Off =>
          Yaml(YamlParser.parse(text))

  // `source.read[List[Yaml]]` / `read[Vector[Yaml]]` / `read[Stream[Yaml]]` etc. for
  // a multi-document `---`-separated stream. The collection bound `<: Iterable[Yaml]`
  // keeps this from overlapping the single-document `Yaml` instance, and the
  // fully-ground `streamAggregable` wins for `Stream`/`LazyList`.
  given collectionAggregable: [collection <: Iterable[Yaml]]
  =>  (factory: Factory[Yaml, collection], tactic: Tactic[ParseError], tracking: Yaml.Tracking)
  =>  collection is Aggregable by Text =
    summon[Text is Aggregable by Text].map(parseAll(_).to(factory))

  // Genuinely lazy: each `---`-separated document is parsed on demand by
  // `YamlParser.parseStream`, so a malformed later document doesn't prevent reading
  // the earlier ones and consumption can stop early.
  given streamAggregable: (Tactic[ParseError], Yaml.Tracking)
  =>  Stream[Yaml] is Aggregable by Text =
    summon[Text is Aggregable by Text].map: text =>
      summon[Yaml.Tracking] match
        case Yaml.Tracking.On =>
          YamlParser.parseStreamTracked(text).map: (ast, ints) =>
            new Yaml(ast, Yaml.PositionIndex(ints))
        case Yaml.Tracking.Off =>
          YamlParser.parseStream(text).map(Yaml(_))

  // HTTP content-type integration. `Abstractable across HttpStreams` makes a
  // `Yaml` value usable as an HTTP request/response body (telekinesis derives
  // `Postable`/`Servable` from it); `Instantiable across HttpRequests` reads a
  // request/response body back into `Yaml`. Encoding needs a `YamlPrinter` (see
  // `yamlPrinters`).
  given abstractable: (encoder: CharEncoder, printer: YamlPrinter)
  =>  Yaml is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Yaml
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(value: Yaml): HttpStreams.Content =
        ( t"application/yaml; charset=${encoder.encoding.name}",
          Stream(printer.print(Yaml.unseal(value)).data) )

  given instantiable: (Tactic[ParseError], Yaml.Tracking)
  =>  Yaml is Instantiable across HttpRequests from Text =

    text => Stream(text).read[Yaml]

  // `source.read[Foo over Yaml]` shorthand for
  // `source.read[Yaml].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect`
  // for `value over Json`. The `Transport` type-tag is added by an
  // `asInstanceOf` cast — `value over Yaml` is just `value { type
  // Transport = Yaml }` so the cast is a no-op at runtime.
  given aggregableOver: [value: Decodable in Yaml]
  =>  (Tactic[ParseError], Tactic[YamlError], Yaml.Tracking)
  =>  (value over Yaml) is Aggregable by Text =

    summon[Text is Aggregable by Text].map: text =>
      val yaml = summon[Yaml.Tracking] match
        case Yaml.Tracking.On =>
          val (ast, ints) = YamlParser.parseTracked(text)
          new Yaml(ast, Yaml.PositionIndex(ints))
        case Yaml.Tracking.Off =>
          Yaml(YamlParser.parse(text))
      yaml.as[value].asInstanceOf[value over Yaml]

  def primitive(ast: Yaml.Ast): YamlPrimitive =
    if ast.asInstanceOf[AnyRef] == null then YamlPrimitive.Null
    else ast.asMatchable match
      case _: Boolean                 => YamlPrimitive.Bool
      case _: Long                    => YamlPrimitive.Integer
      case _: Double                  => YamlPrimitive.Decimal
      // High-precision BCD numbers report as `Decimal`. The AST-level
      // distinction (`isBcd`) remains available for callers that care.
      case _: Array[Double] @unchecked  => YamlPrimitive.Decimal
      case _: String                  => YamlPrimitive.Str

      case xs: Array[AnyRef] @unchecked =>
        if (xs.length & 1) == 0 then YamlPrimitive.Mapping else YamlPrimitive.Sequence

      case _ => YamlPrimitive.Null

class Yaml(rootValue: Any, positions: Optional[Yaml.PositionIndex] = Unset)
extends Dynamic derives CanEqual:
  private[ypsiloid] def root: Yaml.Ast = rootValue.asInstanceOf[Yaml.Ast]

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
  def selectDynamic(field: String)(using erased DynamicYamlEnabler): Yaml = apply(field.tt)

  def applyDynamic(field: String)(index: Int)(using erased DynamicYamlEnabler)
  :   Yaml raises YamlError =

    apply(field.tt)(index)

  // Immutable update: `yaml(0) = newValue` desugars to `update(0, newValue)`.
  def update[value: Encodable in Yaml](index: Int, value: value)
    ( using erased DynamicYamlEnabler )
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
    ( using erased DynamicYamlEnabler )
  :   Yaml raises YamlError =

    modify(field, value.encode)

  def updateDynamic(field: String)[value](unset: Unset.type)(using erased DynamicYamlEnabler)
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


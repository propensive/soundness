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

import anticipation.*
import denominative.*
import fulminate.*
import vacuous.*

object TelError:

  // 1-indexed source position attached to a `TelError` raised by the
  // TEL parser, so a caller capturing the error can point at the
  // offending line in the source. `column = 1` refers to the first
  // character of the line (including any leading spaces). Parse errors
  // that pre-date the document body (e.g. `BomPresent` at offset 0)
  // report `(1, 1)`. Post-parse / validation errors leave `position`
  // `Unset` because they apply to AST nodes rather than source bytes.
  case class Position(line: Int, column: Int) derives CanEqual

  object Position:
    given communicable: Position is Communicable =
      position => m"line ${position.line}, column ${position.column}"

  // Recovery strategies prescribed by §19.5 of the TEL specification. Each
  // reason carries the recovery strategy declared for its E-code.
  enum Recovery:
    case SkipBom
    case RestartFromPragma
    case AllowOversize
    case IgnoreVersion
    case UseDefaultSigil
    case AdjustMargin
    case ShallowerIndent
    case StripTrailing
    case AttachComment
    case SkipOverIndented
    case IgnoreDuplicateAtom
    case PayloadToEof
    case SuppressColumnAlignment
    case CollapseLineEndings
    case IgnoreSchemaId
    case IgnoreExtraPragmaAtoms
    case IgnoreErroneousNode

  object Reason:
    given communicable: Reason is Communicable =
      case BomPresent     => m"the document begins with a byte-order mark"
      case PragmaNotFirst => m"the pragma is not the first non-blank line"
      case PragmaTooLong  => m"the pragma extends beyond the first 4096 bytes"
      case BadVersion     => m"the pragma version is not of the form x.y"
      case BadSigil       => m"the pragma sigil is not a permitted symbolic character"
      case LessThanMargin => m"the line begins with fewer spaces than the document margin"
      case OddIndentation => m"the relative indentation after the margin is odd"
      case TrailingSpaces => m"the non-blank line has trailing spaces"

      case CommentNotPreceded =>
        m"the comment is not preceded by a blank line, comment, start, or lesser indent"

      case UnmatchedDedent =>
        m"the line indent does not match any open compound"

      case OverIndentation =>
        m"the line is indented more than one level deeper than the previous compound"

      case ChildOfNonCompound =>
        m"the line is a child of a comment, tabulation, or tabulated row"

      case DuplicateSource =>
        m"the compound already has a source or literal atom"

      case DuplicateLiteral =>
        m"the compound already has a source or literal atom"

      case UnclosedLiteral =>
        m"the literal atom reaches end of file before its closing delimiter"

      case RowWrongIndent =>
        m"the tabulated row has a different indent from its tabulation line"

      case HardSpaceWrongPosition =>
        m"a hard space on the tabulated row does not end at a column boundary"

      case ConsecutiveSpacesInValue =>
        m"consecutive spaces appear within a keyword or column value"

      case ColumnValueTooWide =>
        m"the column value exceeds the maximum width for its column"

      case BadTabulationHeading =>
        m"the tabulation line heading is malformed"

      case BadLineEnding =>
        m"line endings are inconsistent or a CR is not followed by an LF"

      case BadSchemaIdentifier =>
        m"the schema identifier is neither a valid URL nor a BASE-256 schema signature"

      case ExtraPragmaContent =>
        m"the pragma line has extra atoms or a remark"

      case DuplicateKeywordInStruct => m"the same keyword appears more than once in a struct"
      case EmptySelectVariants      => m"the SelectDefinition has an empty variants list"
      case RootRequiredAtom         => m"the root struct has a required atom-assignable member"
      case DefaultOnOptional        => m"a non-required member must not specify a default"
      case DuplicateLayerName       => m"two or more layers share the same name"
      case LayerKeywordCollision    => m"the layer introduces a keyword colliding with the base"
      case LayerFieldTypeMismatch   => m"a layer Field's declared type conflicts with the base"
      case BadSchemaSigil           => m"the schema sigil is not a permitted symbolic character"
      case TelKeywordReserved       => m"the keyword `tel` is reserved in user schemas"
      case UnresolvedReference      => m"a Reference or SelectRef names an undeclared TypeName"
      case DuplicateDefinition      => m"two or more Definitions share the same name"
      case ExcludeMissingVariant    => m"Exclude names a variant absent from the base"
      case ExcludeEmptiesRequired   => m"Exclude would empty a SelectDefinition that a "+
                                       m"required SelectRef references"

      case LayerVariantAddition     =>
        m"a layer SelectDefinition introduces a variant absent from the base"

      case LayerLoosenRequired      => m"a layer cannot loosen the `required` axis"
      case LayerLoosenRepeatable    => m"a layer cannot loosen the `repeatable` axis"
      case ExcludeOutsideSelect     => m"Exclude appears outside a SelectDefinition body"

      case ReferenceKindMismatch    =>
        m"a Reference / SelectRef resolves to a Definition of the wrong kind"

      case NonStructCompound        => m"the compound's type is not a Struct"
      case TooManyAtoms             => m"more atoms than assignable member positions"
      case AtomAtNonAssignablePos   => m"the atom is at a non-atom-assignable member position"
      case AtomVariantUnmatched     => m"the atom text matches no variant keyword of the SelectRef"
      case AtomFlagKeywordMismatch  => m"the atom text does not match the Flag member's keyword"
      case UnknownKeyword           => m"the compound keyword is not recognised for the parent"
      case RequiredMemberAbsent     => m"a required member is absent and has no default"
      case NonRepeatableTooMany     => m"a non-repeatable member is filled more than once"
      case MembersNonContiguous     => m"compound children of the same member are not contiguous"
      case ValidatorRejected        => m"a scalar value or struct failed a named validator"
      case FlagWithContent          => m"the Flag-typed compound has atoms or compound children"

    def recoveryOf(reason: Reason): Recovery = reason match
      case BomPresent               => Recovery.SkipBom
      case PragmaNotFirst           => Recovery.RestartFromPragma
      case PragmaTooLong            => Recovery.AllowOversize
      case BadVersion               => Recovery.IgnoreVersion
      case BadSigil                 => Recovery.UseDefaultSigil
      case LessThanMargin           => Recovery.AdjustMargin
      case OddIndentation           => Recovery.ShallowerIndent
      case TrailingSpaces           => Recovery.StripTrailing
      case CommentNotPreceded       => Recovery.AttachComment
      case UnmatchedDedent          => Recovery.ShallowerIndent
      case OverIndentation          => Recovery.SkipOverIndented
      case ChildOfNonCompound       => Recovery.ShallowerIndent
      case DuplicateSource          => Recovery.IgnoreDuplicateAtom
      case DuplicateLiteral         => Recovery.IgnoreDuplicateAtom
      case UnclosedLiteral          => Recovery.PayloadToEof
      case RowWrongIndent           => Recovery.SuppressColumnAlignment
      case HardSpaceWrongPosition   => Recovery.SuppressColumnAlignment
      case ConsecutiveSpacesInValue => Recovery.SuppressColumnAlignment
      case ColumnValueTooWide       => Recovery.SuppressColumnAlignment
      case BadTabulationHeading     => Recovery.SuppressColumnAlignment
      case BadLineEnding            => Recovery.CollapseLineEndings
      case BadSchemaIdentifier      => Recovery.IgnoreSchemaId
      case ExtraPragmaContent       => Recovery.IgnoreExtraPragmaAtoms

      // E2xx and E3xx recoveries: discard the offending node and
      // continue validation; the document is reported as invalid but
      // remaining nodes are still inspected.
      case DuplicateKeywordInStruct | EmptySelectVariants | RootRequiredAtom
        | DefaultOnOptional | DuplicateLayerName | LayerKeywordCollision
        | LayerFieldTypeMismatch | BadSchemaSigil | TelKeywordReserved
        | UnresolvedReference | DuplicateDefinition | ExcludeMissingVariant
        | ExcludeEmptiesRequired | LayerVariantAddition | LayerLoosenRequired
        | LayerLoosenRepeatable | ExcludeOutsideSelect | ReferenceKindMismatch
        | NonStructCompound | TooManyAtoms | AtomAtNonAssignablePos
        | AtomVariantUnmatched | AtomFlagKeywordMismatch | UnknownKeyword
        | RequiredMemberAbsent | NonRepeatableTooMany | MembersNonContiguous
        | ValidatorRejected | FlagWithContent =>
        Recovery.IgnoreErroneousNode

  enum Reason(val number: Int) extends Clarification:
    case BomPresent              extends Reason(101)
    case PragmaNotFirst          extends Reason(102)
    case PragmaTooLong           extends Reason(103)
    case BadVersion              extends Reason(104)
    case BadSigil                extends Reason(105)
    case LessThanMargin          extends Reason(106)
    case OddIndentation          extends Reason(107)
    case TrailingSpaces          extends Reason(108)
    case CommentNotPreceded      extends Reason(109)
    case UnmatchedDedent         extends Reason(110)
    case OverIndentation         extends Reason(111)
    case ChildOfNonCompound      extends Reason(112)
    case DuplicateSource         extends Reason(113)
    case DuplicateLiteral        extends Reason(114)
    case UnclosedLiteral         extends Reason(115)
    case RowWrongIndent          extends Reason(116)
    case HardSpaceWrongPosition  extends Reason(117)
    case ConsecutiveSpacesInValue extends Reason(118)
    case ColumnValueTooWide      extends Reason(119)
    case BadTabulationHeading    extends Reason(120)
    case BadLineEnding           extends Reason(121)
    case BadSchemaIdentifier     extends Reason(122)
    case ExtraPragmaContent      extends Reason(123)

    // E2xx — schema validity errors per §20.1.
    case DuplicateKeywordInStruct extends Reason(201)
    case EmptySelectVariants     extends Reason(202)
    case RootRequiredAtom        extends Reason(203)
    case DefaultOnOptional       extends Reason(204)
    case DuplicateLayerName      extends Reason(205)
    case LayerKeywordCollision   extends Reason(206)
    case LayerFieldTypeMismatch  extends Reason(207)
    case BadSchemaSigil          extends Reason(208)
    case TelKeywordReserved      extends Reason(209)
    case UnresolvedReference     extends Reason(210)
    case DuplicateDefinition     extends Reason(211)
    case ExcludeMissingVariant   extends Reason(212)
    case ExcludeEmptiesRequired  extends Reason(213)
    case LayerVariantAddition    extends Reason(214)
    case LayerLoosenRequired     extends Reason(215)
    case LayerLoosenRepeatable   extends Reason(216)
    case ExcludeOutsideSelect    extends Reason(217)
    case ReferenceKindMismatch   extends Reason(218)

    // E3xx — validation errors per §19.3 / §21.
    case NonStructCompound       extends Reason(301)
    case TooManyAtoms            extends Reason(302)
    case AtomAtNonAssignablePos  extends Reason(303)
    case AtomVariantUnmatched    extends Reason(304)
    case AtomFlagKeywordMismatch extends Reason(305)
    case UnknownKeyword          extends Reason(306)
    case RequiredMemberAbsent    extends Reason(307)
    case NonRepeatableTooMany    extends Reason(308)
    case MembersNonContiguous    extends Reason(309)
    case ValidatorRejected       extends Reason(310)
    case FlagWithContent         extends Reason(311)

case class TelError(reason: TelError.Reason, position: Optional[TelError.Position] = Unset)
                  ( using Diagnostics )
extends Error
  ( 605, reason.number )
  ( position.let: p =>
      m"the TEL document is invalid at $p because $reason"

    . or(m"the TEL document is invalid because $reason") ):

  // The internal 1-indexed position rendered as a uniform 0-based `Span`.
  def span: Span = position.lay(Span.empty): p =>
    Span.line((p.line - 1).max(0).z, (p.column - 1).max(0).z, 0)

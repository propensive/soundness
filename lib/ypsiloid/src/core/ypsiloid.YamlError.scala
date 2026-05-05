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

import anticipation.*
import fulminate.*

object YamlError:
  object Reason:
    given communicable: Reason is Communicable =
      case NotType(found, expected) => m"the YAML value had type $found instead of $expected"
      case Absent                   => m"the YAML value was not present"
      case Unparseable(input)       => m"the input could not be parsed as YAML: $input"
      case UnknownAlias(name)       => m"the alias *$name does not refer to a known anchor"

      case DirectiveWithoutDocumentStart(line, column) =>
        m"directive must be followed by a `---` document-start marker (line $line, column $column)"

      case DirectivesOutOfPlace(line, column) =>
        m"directives can only appear at the start of a stream or after `...` (line $line, column $column)"

      case MissingDocumentStart(line, column) =>
        m"missing `---` between documents (line $line, column $column)"

      case ContentAfterDocumentEnd(line, column) =>
        m"unexpected content after `...` document-end marker (line $line, column $column)"

      case DuplicateYamlDirective(line, column) =>
        m"duplicate %YAML directive (line $line, column $column)"

      case YamlDirectiveRequiresVersion(line, column) =>
        m"%YAML directive requires a version argument (line $line, column $column)"

      case YamlDirectiveTooManyArguments(line, column) =>
        m"%YAML directive takes a single version argument (line $line, column $column)"

      case YamlDirectiveInvalidVersion(line, column) =>
        m"%YAML directive version must be `major.minor` (line $line, column $column)"

      case TagDirectiveRequiresHandleAndPrefix(line, column) =>
        m"%TAG directive requires a `handle prefix` argument pair (line $line, column $column)"

      case TwoAnchorsOnSameNode(line, column) =>
        m"two anchors on the same node (line $line, column $column)"

      case AnchorOnAlias(line, column) =>
        m"anchor on alias node (line $line, column $column)"

      case ReservedIndicatorAtNodeStart(line, column) =>
        m"reserved indicator at start of node (line $line, column $column)"

      case DuplicateAnchorOnNode(line, column) =>
        m"duplicate anchor on a single node (line $line, column $column)"

      case DuplicateTagOnNode(line, column) =>
        m"duplicate tag on a single node (line $line, column $column)"

      case UnterminatedVerbatimTag(line, column) =>
        m"unterminated verbatim tag (line $line, column $column)"

      case BlockMappingOnDocumentStartLine(line, column) =>
        m"block mapping cannot start on the document-start line (line $line, column $column)"

      case ChainedMappingValueOnSingleLine(line, column) =>
        m"chained mapping value not allowed on a single line (line $line, column $column)"

      case MultilineImplicitKey(line, column) =>
        m"implicit mapping key cannot span multiple lines (line $line, column $column)"

      case TrailingContentAfterQuotedScalar(line, column) =>
        m"trailing content after quoted scalar (line $line, column $column)"

      case UnterminatedDoubleQuotedString(line, column) =>
        m"unterminated double-quoted string (line $line, column $column)"

      case UnterminatedEscape(line, column) =>
        m"unterminated escape sequence (line $line, column $column)"

      case InvalidEscapeSequence(line, column) =>
        m"invalid escape sequence (line $line, column $column)"

      case TruncatedHexEscape(line, column) =>
        m"truncated hex escape (line $line, column $column)"

      case InvalidHexDigit(line, column) =>
        m"invalid hex digit (line $line, column $column)"

      case UnterminatedSingleQuotedString(line, column) =>
        m"unterminated single-quoted string (line $line, column $column)"

      case DocumentMarkerInsideMultilineScalar(line, column) =>
        m"document marker inside multi-line scalar (line $line, column $column)"

      case ScalarContinuationUnderIndented(line, column) =>
        m"multi-line scalar continuation insufficiently indented (line $line, column $column)"

      case UnterminatedFlowSequence(line, column) =>
        m"unterminated flow sequence (line $line, column $column)"

      case EmptyFlowSequenceEntry(line, column) =>
        m"empty flow-sequence entry (line $line, column $column)"

      case FlowImplicitKeyAndColonOnDifferentLines(line, column) =>
        m"implicit mapping key and `:` must be on the same line (line $line, column $column)"

      case FlowSequenceExpectedCommaOrClose(line, column) =>
        m"expected `,` or `]` in flow sequence (line $line, column $column)"

      case UnterminatedFlowMapping(line, column) =>
        m"unterminated flow mapping (line $line, column $column)"

      case EmptyFlowMappingEntry(line, column) =>
        m"empty flow-mapping entry (line $line, column $column)"

      case FlowMappingExpectedCommaOrClose(line, column) =>
        m"expected `,` or `}` in flow mapping (line $line, column $column)"

      case FlowContentUnderIndented(line, column) =>
        m"flow content insufficiently indented (line $line, column $column)"

      case CommentMissingPrecedingWhitespace(line, column) =>
        m"comment must be preceded by whitespace (line $line, column $column)"

      case DocumentMarkerInFlowContext(line, column) =>
        m"document markers not allowed in flow style (line $line, column $column)"

      case ReservedIndicatorAtFlowPlainScalarStart(line, column) =>
        m"reserved indicator at start of flow plain scalar (line $line, column $column)"

      case BlockSequenceIndicatorNotAtLineStart(line, column) =>
        m"block-sequence indicator must start its line (line $line, column $column)"

      case TabInIndentation(line, column) =>
        m"tab character used in indentation (line $line, column $column)"

      case PlainScalarAtMappingIndentWithoutColon(line, column) =>
        m"plain scalar at mapping indent without `:` (line $line, column $column)"

      case ExpectedColonAfterMappingKey(line, column) =>
        m"expected `:` after mapping key (line $line, column $column)"

      case BlockSequenceOnMappingKeyLine(line, column) =>
        m"sequence cannot start on the same line as a mapping key (line $line, column $column)"

      case InvalidBlockScalarIndentationIndicator(line, column) =>
        m"block-scalar indentation indicator must be 1-9 (line $line, column $column)"

      case DuplicateBlockScalarIndentationIndicator(line, column) =>
        m"duplicate block-scalar indentation indicator (line $line, column $column)"

      case DuplicateBlockScalarChompingIndicator(line, column) =>
        m"duplicate block-scalar chomping indicator (line $line, column $column)"

      case InvalidBlockScalarHeader(line, column) =>
        m"invalid block-scalar header (line $line, column $column)"

      case BlockScalarHeaderCommentMissingWhitespace(line, column) =>
        m"comment in block-scalar header requires preceding whitespace (line $line, column $column)"

      case BlockScalarLeadingBlanksOverIndented(line, column) =>
        m"leading empty lines have more indentation than the body (line $line, column $column)"

      case UndefinedTagHandle(handle, line, column) =>
        m"undefined tag handle: $handle (line $line, column $column)"

  enum Reason(val number: Int) extends Clarification:
    case NotType(found: YamlPrimitive, expected: YamlPrimitive) extends Reason(1)
    case Absent extends Reason(2)
    case Unparseable(input: Text) extends Reason(3)
    case UnknownAlias(name: Text) extends Reason(4)
    case DirectiveWithoutDocumentStart(line: Int, column: Int) extends Reason(5)
    case DirectivesOutOfPlace(line: Int, column: Int) extends Reason(6)
    case MissingDocumentStart(line: Int, column: Int) extends Reason(7)
    case ContentAfterDocumentEnd(line: Int, column: Int) extends Reason(8)
    case DuplicateYamlDirective(line: Int, column: Int) extends Reason(9)
    case YamlDirectiveRequiresVersion(line: Int, column: Int) extends Reason(10)
    case YamlDirectiveTooManyArguments(line: Int, column: Int) extends Reason(11)
    case YamlDirectiveInvalidVersion(line: Int, column: Int) extends Reason(12)
    case TagDirectiveRequiresHandleAndPrefix(line: Int, column: Int) extends Reason(13)
    case TwoAnchorsOnSameNode(line: Int, column: Int) extends Reason(14)
    case AnchorOnAlias(line: Int, column: Int) extends Reason(15)
    case ReservedIndicatorAtNodeStart(line: Int, column: Int) extends Reason(16)
    case DuplicateAnchorOnNode(line: Int, column: Int) extends Reason(17)
    case DuplicateTagOnNode(line: Int, column: Int) extends Reason(18)
    case UnterminatedVerbatimTag(line: Int, column: Int) extends Reason(19)
    case BlockMappingOnDocumentStartLine(line: Int, column: Int) extends Reason(20)
    case ChainedMappingValueOnSingleLine(line: Int, column: Int) extends Reason(21)
    case MultilineImplicitKey(line: Int, column: Int) extends Reason(22)
    case TrailingContentAfterQuotedScalar(line: Int, column: Int) extends Reason(23)
    case UnterminatedDoubleQuotedString(line: Int, column: Int) extends Reason(24)
    case UnterminatedEscape(line: Int, column: Int) extends Reason(25)
    case InvalidEscapeSequence(line: Int, column: Int) extends Reason(26)
    case TruncatedHexEscape(line: Int, column: Int) extends Reason(27)
    case InvalidHexDigit(line: Int, column: Int) extends Reason(28)
    case UnterminatedSingleQuotedString(line: Int, column: Int) extends Reason(29)
    case DocumentMarkerInsideMultilineScalar(line: Int, column: Int) extends Reason(30)
    case ScalarContinuationUnderIndented(line: Int, column: Int) extends Reason(31)
    case UnterminatedFlowSequence(line: Int, column: Int) extends Reason(32)
    case EmptyFlowSequenceEntry(line: Int, column: Int) extends Reason(33)
    case FlowImplicitKeyAndColonOnDifferentLines(line: Int, column: Int) extends Reason(34)
    case FlowSequenceExpectedCommaOrClose(line: Int, column: Int) extends Reason(35)
    case UnterminatedFlowMapping(line: Int, column: Int) extends Reason(36)
    case EmptyFlowMappingEntry(line: Int, column: Int) extends Reason(37)
    case FlowMappingExpectedCommaOrClose(line: Int, column: Int) extends Reason(38)
    case FlowContentUnderIndented(line: Int, column: Int) extends Reason(39)
    case CommentMissingPrecedingWhitespace(line: Int, column: Int) extends Reason(40)
    case DocumentMarkerInFlowContext(line: Int, column: Int) extends Reason(41)
    case ReservedIndicatorAtFlowPlainScalarStart(line: Int, column: Int) extends Reason(42)
    case BlockSequenceIndicatorNotAtLineStart(line: Int, column: Int) extends Reason(43)
    case TabInIndentation(line: Int, column: Int) extends Reason(44)
    case PlainScalarAtMappingIndentWithoutColon(line: Int, column: Int) extends Reason(45)
    case ExpectedColonAfterMappingKey(line: Int, column: Int) extends Reason(46)
    case BlockSequenceOnMappingKeyLine(line: Int, column: Int) extends Reason(47)
    case InvalidBlockScalarIndentationIndicator(line: Int, column: Int) extends Reason(48)
    case DuplicateBlockScalarIndentationIndicator(line: Int, column: Int) extends Reason(49)
    case DuplicateBlockScalarChompingIndicator(line: Int, column: Int) extends Reason(50)
    case InvalidBlockScalarHeader(line: Int, column: Int) extends Reason(51)
    case BlockScalarHeaderCommentMissingWhitespace(line: Int, column: Int) extends Reason(52)
    case BlockScalarLeadingBlanksOverIndented(line: Int, column: Int) extends Reason(53)
    case UndefinedTagHandle(handle: Text, line: Int, column: Int) extends Reason(54)

case class YamlError(reason: YamlError.Reason)(using Diagnostics)
extends Error(realm"yp", 1, reason.number)(m"could not access the YAML value because $reason")

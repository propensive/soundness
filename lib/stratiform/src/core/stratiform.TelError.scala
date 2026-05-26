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
import fulminate.*

object TelError:

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

case class TelError(reason: TelError.Reason)(using Diagnostics)
extends Error(605, reason.number)(m"the TEL document is invalid because $reason")

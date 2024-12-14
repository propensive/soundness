/*
    Merino
jawn, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package merino

import scala.annotation.*

import anticipation.*
import fulminate.*

object JsonParseError:
  enum Reason:
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

  given Reason is Communicable =
    case Reason.EmptyInput =>
      m"the input was empty"

    case Reason.UnexpectedChar(found) =>
      m"the character $found was not expected"

    case Reason.ExpectedTrue =>
      m"true was expected"

    case Reason.ExpectedFalse =>
      m"false was expected"

    case Reason.ExpectedNull =>
      m"null was expected"

    case Reason.ExpectedSomeValue(char) =>
      m"a value was expected but instead found $char"

    case Reason.ExpectedColon(found) =>
      m"a colon was expected but instead found $found"

    case Reason.InvalidWhitespace =>
      m"invalid whitespace was found"

    case Reason.ExpectedString(found) =>
      m"expected a string but instead found $found"

    case Reason.ExpectedHexDigit(found) =>
      m"expected a hexadecimal digit"

    case Reason.PrematureEnd =>
      m"the stream was ended prematurely"

    case Reason.NumberHasLeadingZero =>
      m"a number cannot start with a zero except when followed by a decimal point"

    case Reason.SpuriousContent(found) =>
      m"$found was found after the full JSON value was read"

    case Reason.LeadingDecimalPoint =>
      m"a number cannot start with a decimal point"

    case Reason.NotEscaped(char) =>
      m"the character $char must be escaped with a backslash"

    case Reason.IncorrectEscape(char) =>
      m"the character $char was escaped with a backslash unnecessarily"

    case Reason.MultipleDecimalPoints =>
      m"the number cannot contain more than one decimal point"

    case Reason.ExpectedDigit(found) =>
      m"expected a digit but instead found $found"


import JsonParseError.Reason

case class JsonParseError(line: Int, col: Int, reason: Reason)(using Diagnostics)
extends Error(m"Could not parse JSON because $reason at ${line + 1}:${col + 1}")

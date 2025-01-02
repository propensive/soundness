/*
    Cellulose, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import anticipation.*
import fulminate.*

export CodlError.Reason.*

import language.experimental.captureChecking

object CodlError:
  given Reason is Communicable =
    case UnexpectedCarriageReturn =>
      m"a carriage return character ('\\r') was followed by a character other than a newline ('\\n')"

    case CarriageReturnMismatch(true) =>
      m"""a newline character ('\\n') was found without a preceding carriage return ('\\r'), which does not
            match the document's prior newline convention"""

    case CarriageReturnMismatch(false) =>
      m"a carriage return ('\\r') was encountered, which does not match the document's prior newline convention"

    case UnevenIndent(initial, indent) =>
      m"the indentation level of ${indent - initial} (with a margin of $initial) is not an exact multiple of 2"

    case IndentAfterComment =>
      m"indentation was given after a comment; the comment should be aligned with its next key"

    case BadSubstitution =>
      m"a substitution cannot be made at this point"

    case BadTermination =>
      m"two # symbols terminates the document and must appear alone on a line"

    case SurplusIndent =>
      m"too much indentation was given"

    case InsufficientIndent =>
      m"insufficient indentation was specified"

    case MissingKey(point, key) =>
      m"the value $key was missing at $point"

    case DuplicateKey(point, key) =>
      m"the unique key $key has already been used at $point"

    case SurplusParams(point, key) =>
      m"too many parameters were given to the key $key at $point"

    case InvalidKey(point, key) =>
      m"the key $key was invalid at $point"

    case DuplicateId(point, line, col) =>
      m"the unique ID has been used before at $line:$col, $point"

  enum Reason:
    case UnexpectedCarriageReturn
    case BadSubstitution
    case BadTermination
    case CarriageReturnMismatch(required: Boolean)
    case UnevenIndent(initial: Int, indent: Int)
    case IndentAfterComment, SurplusIndent, InsufficientIndent

    case MissingKey(point: Text, key: Text)
    case DuplicateKey(point: Text, key: Text)
    case SurplusParams(point: Text, cmd: Text)
    case InvalidKey(point: Text, key: Text)
    case DuplicateId(point: Text, line: Int, col: Int)

case class CodlError(line: Int, col: Int, length: Int, reason: CodlError.Reason)(using Diagnostics)
extends Error(m"could not read the CoDL document at $line:$col: $reason")

case class BcodlError(expectation: Text, pos: Int)(using Diagnostics)
extends Error(m"expected $expectation at position $pos")

case class MultipleIdentifiersError(key: Text)(using Diagnostics)
extends Error(m"multiple parameters of $key have been marked as identifiers")

case class MissingValueError(key: Text)(using Diagnostics)
extends Error(m"the key $key does not exist in the CoDL document")

case class MissingIndexValueError(index: Int)(using Diagnostics)
extends Error(m"the index $index does not exist in the CoDL document")

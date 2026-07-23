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
package facsimile

import proscenium.compat.*
import rudiments.*

import anticipation.*
import fulminate.*

object PdfError:
  enum Reason(val number: Int) extends Clarification:
    case NotPdf                                       extends Reason(1)
    case Truncated                                    extends Reason(2)
    case MissingStartxref                             extends Reason(3)
    case MalformedXref(offset: Long)                  extends Reason(4)
    case Unparseable(offset: Long, expected: Text)    extends Reason(5)
    case MissingObject(objectNumber: Int, generation: Int)  extends Reason(6)
    case CircularReference(objectNumber: Int)                extends Reason(7)
    case MissingEntry(key: Text)                      extends Reason(8)
    case TypeMismatch(key: Text, expected: Text)      extends Reason(9)
    case UnknownFilter(name: Text)                    extends Reason(10)
    case CorruptStream(filter: Text)                  extends Reason(11)
    case MalformedOperator(operator: Text)            extends Reason(12)
    case UnsupportedEncryption(version: Int)          extends Reason(13)
    case BadPassword                                  extends Reason(14)
    case CircularPageTree                             extends Reason(15)
    case Io(detail: Text)                             extends Reason(16)
    case WriteUnsupported                             extends Reason(17)

  given communicable: Reason is Communicable =
    case Reason.NotPdf =>
      m"the file does not begin with a PDF header"

    case Reason.Truncated =>
      m"the PDF file ended unexpectedly"

    case Reason.MissingStartxref =>
      m"no startxref keyword could be found at the end of the file"

    case Reason.MalformedXref(offset) =>
      m"the cross-reference section at offset $offset could not be interpreted"

    case Reason.Unparseable(offset, expected) =>
      m"$expected was expected at offset $offset"

    case Reason.MissingObject(objectNumber, generation) =>
      m"the object $objectNumber $generation was missing or invalid"

    case Reason.CircularReference(objectNumber) =>
      m"resolving the object $objectNumber returned to itself"

    case Reason.MissingEntry(key) =>
      m"the required dictionary entry $key was absent"

    case Reason.TypeMismatch(key, expected) =>
      m"the dictionary entry $key was not $expected"

    case Reason.UnknownFilter(name) =>
      m"the stream filter $name is not recognized"

    case Reason.CorruptStream(filter) =>
      m"a stream could not be decoded with the $filter filter"

    case Reason.MalformedOperator(operator) =>
      m"the content operator $operator had malformed operands"

    case Reason.UnsupportedEncryption(version) =>
      m"the encryption scheme (version $version) is not supported"

    case Reason.BadPassword =>
      m"the password was incorrect"

    case Reason.CircularPageTree =>
      m"the page tree contains a cycle"

    case Reason.Io(detail) =>
      m"an I/O operation failed: $detail"

    case Reason.WriteUnsupported =>
      m"this document cannot be written (only an unencrypted, on-disk file with a valid "+
          m"cross-reference table can be edited in place)"

case class PdfError(reason: PdfError.Reason)(using Diagnostics)
extends Error(728, reason.number)(m"the PDF could not be read because $reason")

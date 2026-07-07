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
package stratiform

import anticipation.*
import fulminate.*

object BintelError:

  object Reason:
    given communicable: Reason is Communicable =
      case BadSignature        => m"the schema signature does not decode against the library"
      case BadKeywordIndex     => m"a keyword index exceeds the parent's flat-keyword count"
      case ValueTruncated      => m"a Scalar value's byte length extends beyond end of input"
      case BadUtf8             => m"a Scalar value's bytes are not valid UTF-8"
      case TrailingBytes       => m"the document root completed with input bytes remaining"
      case UnexpectedEoi       => m"the decoder requested bytes beyond end of input"
      case ReferenceUnresolved => m"a Reference type in the schema does not resolve"
      case VarintError         => m"a variable-length integer in the stream is invalid"

      case BadMagic =>
        m"""
          the magic number is missing or matches neither B2 C4 B5 BB (external mode) nor B2 C4 B5 BC
          (self-contained mode)
        """

      case BadSignatureLength =>
        m"the schema signature length is not a valid palimpsest length under any (H, k_i, k_r)"


      case EmbeddedSignatureMismatch =>
        m"""
          the signature recomputed from the embedded schema body does not equal the carried
          signature
        """

      case EmbeddedSchemaUndecodable =>
        m"the embedded schema body does not decode as a valid TEL document under tel-schema"

  enum Reason(val number: Int) extends Clarification:
    case BadMagic            extends Reason(1)
    case VarintError         extends Reason(2)
    case BadSignatureLength  extends Reason(3)
    case BadSignature        extends Reason(4)
    case BadKeywordIndex     extends Reason(5)
    case ValueTruncated      extends Reason(6)
    case BadUtf8             extends Reason(7)
    case TrailingBytes       extends Reason(8)
    case UnexpectedEoi       extends Reason(9)
    case ReferenceUnresolved extends Reason(10)
    case EmbeddedSignatureMismatch extends Reason(11)
    case EmbeddedSchemaUndecodable extends Reason(12)

case class BintelError(reason: BintelError.Reason)(using Diagnostics)
extends Error(609, reason.number)(m"the BinTEL stream is invalid because $reason")

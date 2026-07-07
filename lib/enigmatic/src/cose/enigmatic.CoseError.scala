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
package enigmatic

import anticipation.*
import fulminate.*

object CoseError:
  object Reason:
    given communicable: Reason is Communicable =
      case MalformedStructure              => m"the COSE structure was not well-formed"
      case VariantMismatch(want, got)      => m"expected a $want COSE message but found a $got"
      case VerificationFailed              => m"the COSE signature or MAC did not verify"
      case CborParseError                  => m"the COSE message contained malformed CBOR"
      case DetachedPayloadRequired         => m"the COSE message has a detached payload"

      case UnknownTag(tag) =>
        m"the CBOR tag ${tag.toString} is not a COSE message tag"

      case UnsupportedAlgorithm(id) =>
        m"the COSE algorithm identifier ${id.toString} is not supported"

      case AlgorithmMismatch(want, got) =>
        m"expected COSE algorithm ${want.toString} but found ${got.toString}"

  enum Reason(val number: Int) extends Clarification:
    case MalformedStructure                              extends Reason(1)
    case UnknownTag(tag: Long)                           extends Reason(2)
    case UnsupportedAlgorithm(id: Long)                  extends Reason(3)
    case AlgorithmMismatch(expected: Long, actual: Long) extends Reason(4)
    case VariantMismatch(expected: Text, actual: Text)   extends Reason(5)
    case VerificationFailed                              extends Reason(6)
    case CborParseError                                  extends Reason(7)
    case DetachedPayloadRequired                         extends Reason(8)

case class CoseError(reason: CoseError.Reason)(using Diagnostics)
extends Error(596, reason.number)(m"could not process the COSE message because $reason")

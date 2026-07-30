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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import enigmatic.*
import prepositional.*
import rudiments.*
import vacuous.*

// Selects the COSE message variant from the key type:
//   asymmetric (PrivateKey)        -> COSE_Sign1
//   symmetric  (SymmetricKey)      -> COSE_Mac0
// `SymmetricKey <: PrivateKey`, so resolution picks the more-specific
// symmetric given for symmetric keys and falls back to asymmetric otherwise.
object CoseAuthenticator:
  given asymmetric: [cipher <: Cipher & Signing]
  =>  ( algorithm: cipher & Signing, coseAlg: cipher is CoseAlgorithm )
  =>  PrivateKey[cipher] is CoseAuthenticator in Sign1 by cipher =
    new CoseAuthenticator:
      type Self    = PrivateKey[cipher]
      type Form    = Sign1
      type Operand = cipher
      def algId:         Long   = coseAlg.algId
      def contextString: String = CoseContext.Signature1
      def cborTag:       Long   = CoseTag.Sign1

      def authenticate(toBeSigned: Data, key: PrivateKey[cipher]): Data =
        key.secret.uncloak: bytes => algorithm.sign(toBeSigned, bytes.immutable(using Unsafe))

  given symmetric: [cipher <: Cipher & Symmetric & Signing]
  =>  ( algorithm: cipher & Signing, coseAlg: cipher is CoseAlgorithm )
  =>  SymmetricKey[cipher] is CoseAuthenticator in Mac0 by cipher =
    new CoseAuthenticator:
      type Self    = SymmetricKey[cipher]
      type Form    = Mac0
      type Operand = cipher
      def algId:         Long   = coseAlg.algId
      def contextString: String = CoseContext.Mac0
      def cborTag:       Long   = CoseTag.Mac0

      def authenticate(toBeSigned: Data, key: SymmetricKey[cipher]): Data =
        key.secret.uncloak: bytes => algorithm.sign(toBeSigned, bytes.immutable(using Unsafe))

trait CoseAuthenticator:
  type Self
  type Form    <: CoseStructure
  type Operand <: Cipher
  def algId:         Long
  def contextString: String
  def cborTag:       Long
  def authenticate(toBeSigned: Data, key: Self): Data

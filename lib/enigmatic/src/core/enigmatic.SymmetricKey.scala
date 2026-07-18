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
import gastronomy.ProcessingPermit
import prepositional.*
import rudiments.*
import vacuous.*

object SymmetricKey:
  def generate[cipher <: Cipher & Symmetric]()(using cipher: cipher, cloak: Cloak^)
  :   SymmetricKey[cipher]^{cloak} =

    new SymmetricKey(cloak.cloak(cipher.genKey().mutable(using Unsafe)))

  // Adopt externally-supplied key material — for example a key produced by a key-
  // derivation function — as a symmetric key, in contrast to `generate`'s fresh random
  // key. The bytes must be a valid key length for the cipher; an invalid length surfaces
  // when the key is used (as a `CryptoError` on decryption). The input is immutable, so
  // the caller's copy cannot be zeroed: the cloak stores its own copy, and the caller
  // should prefer the `Array[Byte]` overload where the material is mutable. Defining
  // `apply` suppresses the synthetic constructor proxy, so these are the sole
  // `SymmetricKey(bytes)` constructors.
  def apply[cipher <: Cipher](bytes: Data)(using cloak: Cloak^): SymmetricKey[cipher]^{cloak} =
    new SymmetricKey(cloak.cloak(bytes.to(Array)))

  // Adopt externally-supplied key material from a mutable array, which is zeroed as it is
  // cloaked, leaving the cloaked copy as the only key material. (The `@targetName`
  // disambiguates from the `Data` overload, with which it clashes after erasure.)
  @annotation.targetName("applyMutable")
  def apply[cipher <: Cipher](bytes: Array[Byte])(using cloak: Cloak^)
  :   SymmetricKey[cipher]^{cloak} =

    new SymmetricKey(cloak.cloak(bytes))

class SymmetricKey[cipher <: Cipher](handle: Secret^)
extends PrivateKey[cipher](handle):
  def verify[value: Encodable in Data](value: value, signature: Signature[cipher])
    ( using signing: cipher & Signing, erased weakness: ProcessingPermit[Weakness[cipher]] )
  :   Boolean =

    public.verify(value, signature)

  // The immutable `Data` in the result outlives the cloak's zeroing, which is why obtaining
  // it demands the explicit `Divulgence` token. This replaces the former `Encodable` given,
  // which serialized the key material silently.
  def data(reveal: Divulgence.type): Data = secret.uncloak: bytes =>
    bytes.clone.immutable(using Unsafe)

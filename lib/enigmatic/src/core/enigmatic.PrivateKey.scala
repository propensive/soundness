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
import gastronomy.*, providers.javaStdlibProvider
import gossamer.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object PrivateKey:
  def generate[cipher <: Cipher]()(using cipher: cipher, cloak: Cloak^)
  :   PrivateKey[cipher]^{cloak} =

    PrivateKey(cipher.genKey())

  // Adopts freshly-generated key material: the `Data` is ours, so it is zeroed in place
  // (through a mutable view) as it is cloaked.
  private[enigmatic] def apply[cipher <: Cipher](data: Data)(using cloak: Cloak^)
  :   PrivateKey[cipher]^{cloak} =

    new PrivateKey(cloak.cloak(data.mutable(using Unsafe)))

  given showable: [key <: Cipher] => PrivateKey[key] is Showable = key =>
    import alphabets.base64Standard

    key.secret.uncloak: bytes =>
      t"PrivateKey(${bytes.immutable(using Unsafe).digest[Sha2[256]].serialize[Base64]})"

// A private key held opaquely by whichever `Cloak` was in scope at construction, capturing
// that cloak. Operations that need the key material — `public`, `sign`, `pem` — materialize
// it transiently through the cloak, and the transient copy is zeroed as soon as the
// operation completes; only `pem`, gated on `Divulgence`, lets the material escape.
class PrivateKey[cipher <: Cipher](private[enigmatic] val secret: Secret^):
  def public(using cipher: cipher): PublicKey[cipher] =
    secret.uncloak: bytes =>
      PublicKey(cipher.privateToPublic(bytes.immutable(using Unsafe)))


  def sign[encodable: Encodable in Data](value: encodable)
    ( using cipher: cipher & Signing, erased weakness: Permit[Weakness[cipher]] )
  :   Signature[cipher] =

    secret.uncloak: bytes =>
      Signature(cipher.sign(encodable.encode(value), bytes.immutable(using Unsafe)))


  // The immutable `Data` in the result outlives the cloak's zeroing, which is exactly why
  // revealing it demands the explicit `Divulgence` token.
  def pem(reveal: Divulgence.type): Pem = secret.uncloak: bytes =>
    Pem(PemLabel.PrivateKey, bytes.clone.immutable(using Unsafe))

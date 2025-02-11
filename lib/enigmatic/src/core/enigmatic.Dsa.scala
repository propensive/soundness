/*
    Enigmatic, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package enigmatic

import java.security as js, js.spec as jss, js.interfaces as jsi

import anticipation.*
import fulminate.*
import rudiments.*
import vacuous.*

object Dsa:
  given [BitsType <: 512 | 1024 | 2048 | 3072: ValueOf] => Dsa[BitsType] = Dsa()

class Dsa[BitsType <: 512 | 1024 | 2048 | 3072: ValueOf]() extends Cipher, Signing:
  type Size = BitsType
  def keySize: BitsType = valueOf[BitsType]

  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("DSA").nn
    val random = js.SecureRandom()
    generator.initialize(keySize, random)
    val keyPair = generator.generateKeyPair().nn

    val pubKey = keyPair.getPublic.nn match
      case key: jsi.DSAPublicKey => key
      case key: js.PublicKey     => panic(m"unexpected public key type")

    keyPair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

  def sign(data: Bytes, keyBytes: Bytes): Bytes =
    val sig = init()
    val key = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(keyBytes.to(Array)))
    sig.initSign(key)
    sig.update(data.to(Array))
    sig.sign().nn.immutable(using Unsafe)

  def verify(data: Bytes, signature: Bytes, keyBytes: Bytes): Boolean =
    val sig = init()
    val key = keyFactory().generatePublic(jss.X509EncodedKeySpec(keyBytes.to(Array)))
    sig.initVerify(key)
    sig.update(data.to(Array))
    sig.verify(signature.to(Array))

  def privateToPublic(keyBytes: Bytes): Bytes =
    val key = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(keyBytes.to(Array))).nn match
      case key: jsi.DSAPrivateKey => key
      case key: js.PrivateKey     => panic(m"unexpected private key type")

    val params = key.getParams.nn
    val y = params.getG.nn.modPow(key.getX, params.getP.nn)
    val spec = jss.DSAPublicKeySpec(y, params.getP, params.getQ, params.getG)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): js.Signature = js.Signature.getInstance("DSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("DSA").nn

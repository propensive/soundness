/*
    Gastronomy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gastronomy

import java.util as ju

import scala.compiletime.*

import anticipation.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object Digest:
  def apply[HashType <: Algorithm](bytes: Bytes): Digest in HashType = new Digest(bytes):
    type Format = HashType

  given [DigestType <: Algorithm](using Alphabet[Base64]) => Digest in DigestType is Showable =
    _.bytes.serialize[Base64]

  given [DigestType <: Algorithm] => Digest in DigestType is Encodable in Bytes = _.bytes

class Digest(val bytes: Bytes):
  type Format <: Algorithm

  override def equals(that: Any) = that.asMatchable match
    case digest: Digest => bytes.sameElements(digest.bytes)
    case _              => false

  override def hashCode: Int = ju.Arrays.hashCode(bytes.mutable(using Unsafe): Array[Byte])

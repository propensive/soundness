/*
    Gastronomy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import java.util as ju, ju.zip as juz
import javax.crypto as jc

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Crc32:
  given HashFunction in Crc32 as hashFunction:
    def init(): Digestion = new Digestion:
      private val state: juz.CRC32 = juz.CRC32()
      def append(bytes: Bytes): Unit = state.update(bytes.mutable(using Unsafe))

      def digest(): Bytes =
        val int = state.getValue()
        IArray[Byte]((int >> 24).toByte, (int >> 16).toByte, (int >> 8).toByte, int.toByte)

    def name: Text = t"CRC32"
    def hmacName: Text = t"HMAC-CRC32"
    def hmac0: jc.Mac = panic(m"this has not been implemented")

sealed trait Crc32 extends Algorithm:
  type Bits = 32

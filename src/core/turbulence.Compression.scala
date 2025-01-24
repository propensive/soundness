/*
    Turbulence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import language.experimental.captureChecking

import java.io as ji
import java.util.zip as juz

import anticipation.*
import contingency.*
import rudiments.*
import vacuous.*

trait Compression:
  type Self <: CompressionAlgorithm
  def compress(stream: Stream[Bytes]): Stream[Bytes]
  def decompress(stream: Stream[Bytes]): Stream[Bytes]

object Compression:
  given Gzip is Compression:
    def compress(stream: Stream[Bytes]): Stream[Bytes] =
      val out = ji.ByteArrayOutputStream()
      val out2 = juz.GZIPOutputStream(out)

      def recur(stream: Stream[Bytes]): Stream[Bytes] = stream match
        case head #:: tail =>
          out2.write(head.mutable(using Unsafe))
          if out.size == 0 then recur(tail) else
            val data = out.toByteArray().nn.immutable(using Unsafe)
            out.reset()
            data #:: recur(tail)

        case _ =>
          out2.close()
          if out.size == 0 then Stream() else Stream(out.toByteArray().nn.immutable(using Unsafe))

      recur(stream)

    def decompress(stream: Stream[Bytes]): Stream[Bytes] =
      unsafely(juz.GZIPInputStream(stream.inputStream).stream[Bytes])

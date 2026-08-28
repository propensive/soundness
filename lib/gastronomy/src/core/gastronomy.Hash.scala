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
package gastronomy

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import beneficence.*
import corpuscular.*
import gossamer.*
import prepositional.*

object Hash:
  // The checksum algorithms live in corpuscular (which cannot see `Hashing`, being the lower
  // layer), so their `Hash` givens are placed in this companion rather than in each algorithm's.
  // Resolution is unaffected — the implicit scope of `Hash in Crc32` spans both companions — and
  // it keeps the provider machinery, and the single `import providers.…` that selects it, here.
  given crc32: (hashing: Hashing { def crc32: Hashing.Function }) => Hash in Crc32 =
    Hash(t"CRC32", t"HMAC-CRC32", hashing.crc32)

  given crc64: (hashing: Hashing { def crc64: Hashing.Function }) => Hash in Crc64 =
    Hash(t"CRC64", t"HMAC-CRC64", hashing.crc64)

  given adler32: (hashing: Hashing { def adler32: Hashing.Function }) => Hash in Adler32 =
    Hash(t"ADLER32", t"HMAC-ADLER32", hashing.adler32)

  // Builds a `Hash` for `algorithm` from the `Hashing.Function` an in-scope
  // provider supplies. `name`/`hmacName` are the algorithm's JCE-style descriptors
  // (the latter used by enigmatic's HMAC); the byte-level digesting is delegated to
  // the provider's `Function`.
  def apply[algorithm <: Algorithm](name0: Text, hmacName0: Text, function: Hashing.Function)
  :   Hash in algorithm =

    new Hash:
      type Form = algorithm
      val name: Text = name0
      val hmacName: Text = hmacName0
      def initialize(): Digestion^ = function.digestion()

trait Hash extends Findable:
  type Form <: Algorithm

  def name: Text
  def hmacName: Text
  def initialize(): Digestion^

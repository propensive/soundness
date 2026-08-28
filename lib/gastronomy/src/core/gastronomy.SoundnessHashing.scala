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

import corpuscular.*

// A complete pure-Scala hashing provider: BLAKE3 (which the JDK does not implement) plus MD5,
// SHA-1 and the SHA-2 family (implemented in `PureHashes`), plus the corpuscular checksums, so
// hashing is available on
// every platform. On the JVM the JDK-backed `JavaStdlibHashing` remains the default, delegating
// to native `MessageDigest`; off the JVM (`JavaStdlibHashing`'s native variant) it forwards here.
// Select it explicitly with `import providers.soundnessProvider`.
object SoundnessHashing extends Hashing:
  def blake3: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = Blake3.digestion()

  def md5: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = PureHashes.md5

  def sha1: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = PureHashes.sha1

  def sha2(bits: Int): Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = PureHashes.sha2(bits)

  // The checksums come from corpuscular, whose accumulators implement `Digestion` natively —
  // there is no second table here, and no adapter on the windowed path.
  def crc32: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = corpuscular.Crc32()

  def crc64: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = corpuscular.Crc64()

  def adler32: Hashing.Function = new Hashing.Function:
    def digestion(): Digestion^ = corpuscular.Adler32()

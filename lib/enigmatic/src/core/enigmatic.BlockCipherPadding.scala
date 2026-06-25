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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import beneficence.*
import contingency.*
import gossamer.*

object BlockCipherPadding:
  def apply[padding](name0: Text): padding is BlockCipherPadding = new BlockCipherPadding:
    type Self = padding
    val name: Text = name0

// `verify` is a no-op for paddings that accept any input length. `NoPadding`
// overrides it to reject misaligned input (for block-structured modes), raising a
// `CryptoError` through the `Tactic` its `given` captures — which is why
// constructing a `NoPadding` cipher, and hence encrypting with one, demands a
// `Tactic[CryptoError]` in scope while all other paddings remain total.

trait BlockCipherPadding extends Findable:
  type Self
  def name: Text
  def verify(length: Int, blockSize: Int, blockAligned: Boolean): Unit = ()

object Pkcs7:
  // The JDK calls PKCS#7 padding "PKCS5Padding" for historical reasons.
  given padding: Pkcs7 is BlockCipherPadding = BlockCipherPadding(t"PKCS5Padding")

sealed trait Pkcs7

object Iso10126:
  given padding: Iso10126 is BlockCipherPadding = BlockCipherPadding(t"ISO10126Padding")

sealed trait Iso10126

object NoPadding:
  given padding: Tactic[CryptoError] => ((NoPadding is BlockCipherPadding)^) =
    new BlockCipherPadding:
      type Self = NoPadding
      val name: Text = t"NoPadding"

      override def verify(length: Int, blockSize: Int, blockAligned: Boolean): Unit =
        if blockAligned && length%blockSize != 0
        then abort(CryptoError(CryptoError.Reason.IllegalBlockSize))

sealed trait NoPadding

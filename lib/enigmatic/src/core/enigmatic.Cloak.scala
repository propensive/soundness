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

import java.lang.ref as jlr
import java.util as ju

// Where secret material — passwords, private and symmetric keys — is stored. A `Cloak` must
// be in scope to construct any secret value, and the secret captures the cloak that stores
// it, so a secret can never outlive its storage. There is deliberately no default given:
// end-users import a cloak by name (e.g. `import enigmatic.cloaks.cloakHeap`) so the storage
// decision is always explicit in user code.
//
// No cloak offers complete protection: a debugger or same-process attacker sees everything,
// JCE makes transient heap copies of key material, and construction from `Text` pins an
// unzeroable `String` on the heap. What the non-heap cloaks reduce is the *window* during
// which cleartext is reachable from a heap dump or core file.
object Cloak:
  // Shared cleaner for cloaks whose storage must be zeroed and released when a secret
  // becomes unreachable, following the idiom of `parasite.Destruction`.
  private[enigmatic] lazy val cleaner: jlr.Cleaner = jlr.Cleaner.create().nn

trait Cloak:
  // Copies `bytes` into strategy-specific storage and ZEROES the input array before
  // returning, so the caller's copy of the cleartext does not linger.
  def cloak(bytes: Array[Byte]): Secret^{this}

// Cleartext on the heap, as a private byte array. The weakest strategy — the material is
// visible in a heap dump for the secret's lifetime — but portable and pure: heap-cloaked
// secrets have empty capture sets, so they can be stored and returned without tracking.
// Selected as `cloaks.cloakHeap`.
private[enigmatic] object HeapCloak extends Cloak:
  def cloak(bytes: Array[Byte]): Secret =
    val copy = bytes.clone
    ju.Arrays.fill(bytes, 0.toByte)

    new Secret:
      def uncloak[result](block: Array[Byte] => result): result =
        val cleartext = copy.clone
        try block(cleartext) finally ju.Arrays.fill(cleartext, 0.toByte)

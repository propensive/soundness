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
package reliquary

import anticipation.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import stratiform.*

// Domain-separated hashing per §7.1 of the LIRA specification: every hash the format defines is
// `BLAKE3-256(utf8(domain) ++ 0x00 ++ content)`, with the domain string carrying the `lira/1`
// format epoch. Atom domains additionally carry the full discipline identifier, so atoms from
// different disciplines — or different versions of one discipline — can never collide.
object LiraHash:
  val epoch: Text = t"lira/1"
  val size: Int = 32

  // The `0x00` byte separating the domain from the content; a fresh byte array is
  // zero-initialized, so freezing a unit array yields it directly.
  private val separator: Data = Array.freeze(Array[Byte](1))

  enum Domain:
    case Blob, Snapshot, Manifest, Key, Derivative
    case Atom(discipline: Text)

    def text: Text = this match
      case Blob             => t"$epoch:blob"
      case Snapshot         => t"$epoch:snapshot"
      case Manifest         => t"$epoch:manifest"
      case Key              => t"$epoch:key"
      case Derivative       => t"$epoch:derivative"
      case Atom(discipline) => t"$epoch:atom:$discipline"

  def apply(domain: Domain, content: Data): Data =
    val prefix: Data = charEncoders.utf8Encoder.encoded(domain.text)
    val buffer = Array[Byte](prefix.length + 1 + content.length)
    System.arraycopy(Array.unsafeJvm(prefix), 0, buffer.raw, 0, prefix.length)
    System.arraycopy(Array.unsafeJvm(content), 0, buffer.raw, prefix.length + 1, content.length)

    Blake3.hashOf(Array.freeze(buffer))

  // The textual form of any LIRA hash: 32 BASE-256 characters (§7).
  def text(hash: Data): Text = Base256.encode(hash)

  // The hash of the empty byte string in the blob domain, pinned as a golden value guarding the
  // stability of the domain-separation construction itself.
  val emptyBlob: Text = t"ǢjЪ6ДľIẈḟžЭŠГȕJЂĘґƟḁsЬțДǶṛḠẄήϋƧЪ"

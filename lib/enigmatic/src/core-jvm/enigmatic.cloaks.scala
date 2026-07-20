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

import java.lang.foreign as jlf
import java.security as js
import java.util as ju
import javax.crypto as jc, jc.spec as jcs

// The available cloaks, selected by named import (e.g. `import enigmatic.cloaks.cloakOffHeap`).
// All but `heap` keep secret material out of (or encrypted on) the Java heap, so that a
// heap dump or core file taken while a secret is at rest reveals nothing directly. Off-heap
// segments are allocated from `java.lang.foreign` arenas, zeroed and released by
// `Cloak.cleaner` when the secret becomes unreachable; encrypting strategies use
// AES-256-GCM with a fresh random nonce per secret. See the `Cloak` doc for what none of
// these can protect against.
package cloaks:
  // Cleartext on the heap: portable and pure, so heap-cloaked secrets are untracked.
  given cloakHeap: Cloak = HeapCloak

  // The three JVM strategies are capabilities, and a capability cannot sit in a top-level
  // given field, so their givens are `transparent inline`: each summon site instantiates a
  // fresh cloak — with its own arena and, for the encrypting strategies, its own ephemeral
  // key — captured by the secrets built with it.

  // Cleartext in off-heap memory: nothing at rest on the heap at all.
  transparent inline given cloakOffHeap: (OffHeapCloak^) = OffHeapCloak()

  // Ciphertext on the heap, encrypted with an ephemeral key held in off-heap memory: at
  // rest, the heap holds only ciphertext, and the key needed to read it is off-heap.
  transparent inline given cloakVeiledHeap: (VeiledHeapCloak^) = VeiledHeapCloak()

  // Ciphertext in off-heap memory, encrypted with an ephemeral key held on the heap: the
  // mirror image, for when bulk secret material should stay out of the heap but a 32-byte
  // heap-resident key is acceptable.
  transparent inline given cloakVeiledOffHeap: (VeiledOffHeapCloak^) = VeiledOffHeapCloak()

// Copies `bytes` into fresh off-heap memory in `arena`, zeroing the input.
private def offload(bytes: Array[Byte], arena: jlf.Arena): jlf.MemorySegment =
  val segment = arena.allocate(bytes.length.toLong).nn
  jlf.MemorySegment.copy(bytes, 0, segment, jlf.ValueLayout.JAVA_BYTE, 0L, bytes.length)
  ju.Arrays.fill(bytes, 0.toByte)
  segment

// Reads a segment back into a fresh mutable heap array.
private def reload(segment: jlf.MemorySegment, length: Int): Array[Byte] =
  val bytes = new Array[Byte](length)
  jlf.MemorySegment.copy(segment, jlf.ValueLayout.JAVA_BYTE, 0L, bytes, 0, length)
  bytes

// A per-secret shared arena, zeroed and closed by the cleaner when `secret` becomes
// unreachable. The action closes over the segment and arena but never the secret itself,
// which would otherwise be permanently reachable and never cleaned.
private def cleanable(secret: Secret^, arena: jlf.Arena, segment: jlf.MemorySegment): Unit =
  Cloak.cleaner.register(secret, { () => segment.fill(0.toByte); arena.close() })

private[enigmatic] class OffHeapCloak extends Cloak, caps.SharedCapability:
  def cloak(bytes: Array[Byte]): Secret^{this} =
    val arena = jlf.Arena.ofShared().nn
    val length = bytes.length
    val segment = offload(bytes, arena)

    val secret: Secret^{this} = new Secret:
      def uncloak[result](block: Array[Byte] => result): result =
        val cleartext = reload(segment, length)
        try block(cleartext) finally ju.Arrays.fill(cleartext, 0.toByte)

    cleanable(secret, arena, segment)
    secret

// The AES-256-GCM machinery shared by the two encrypting cloaks: an ephemeral key generated
// at cloak construction, a fresh 12-byte nonce per secret, and the 16-byte tag left appended
// to the ciphertext, which authenticates it against tampering as a side benefit.
private def freshKey(): Array[Byte] =
  val generator = jc.KeyGenerator.getInstance("AES").nn
  generator.init(256)
  generator.generateKey().nn.getEncoded.nn

// A transient `SecretKeySpec` view of the key, lent to `block`. The spec itself makes a
// heap copy of the key internally (`javax.crypto` offers no way to avoid this); it lives
// only for the duration of the operation.
private def aes[result](keyBytes: Array[Byte], mode: Int, nonce: Array[Byte])
  ( block: jc.Cipher => result )
:   result =

  val cipher = jc.Cipher.getInstance("AES/GCM/NoPadding").nn
  cipher.init(mode, jcs.SecretKeySpec(keyBytes, "AES"), jcs.GCMParameterSpec(128, nonce))
  block(cipher)

private[enigmatic] class VeiledHeapCloak extends Cloak, caps.SharedCapability:
  private val random = js.SecureRandom()
  private val arena = jlf.Arena.ofShared().nn
  private val keySegment = offload(freshKey(), arena)

  locally:
    val segment = keySegment
    val owned = arena
    Cloak.cleaner.register(this, { () => segment.fill(0.toByte); owned.close() })

  // Lends the key as a transient heap array, zeroed as soon as the operation completes.
  private def withKey[result](block: Array[Byte] => result): result =
    val keyBytes = reload(keySegment, 32)
    try block(keyBytes) finally ju.Arrays.fill(keyBytes, 0.toByte)

  def cloak(bytes: Array[Byte]): Secret^{this} =
    val nonce = new Array[Byte](12)
    random.nextBytes(nonce)

    val ciphertext = withKey: keyBytes =>
      aes(keyBytes, jc.Cipher.ENCRYPT_MODE, nonce)(_.doFinal(bytes).nn)

    ju.Arrays.fill(bytes, 0.toByte)

    new Secret:
      def uncloak[result](block: Array[Byte] => result): result =
        val cleartext = withKey: keyBytes =>
          aes(keyBytes, jc.Cipher.DECRYPT_MODE, nonce)(_.doFinal(ciphertext).nn)

        try block(cleartext) finally ju.Arrays.fill(cleartext, 0.toByte)

private[enigmatic] class VeiledOffHeapCloak extends Cloak, caps.SharedCapability:
  private val random = js.SecureRandom()
  private val keyBytes = freshKey()

  def cloak(bytes: Array[Byte]): Secret^{this} =
    val nonce = new Array[Byte](12)
    random.nextBytes(nonce)
    val ciphertext = aes(keyBytes, jc.Cipher.ENCRYPT_MODE, nonce)(_.doFinal(bytes).nn)
    ju.Arrays.fill(bytes, 0.toByte)

    val arena = jlf.Arena.ofShared().nn
    val length = ciphertext.length
    val segment = offload(ciphertext, arena)

    val secret: Secret^{this} = new Secret:
      def uncloak[result](block: Array[Byte] => result): result =
        val recovered = reload(segment, length)

        val cleartext =
          try aes(keyBytes, jc.Cipher.DECRYPT_MODE, nonce)(_.doFinal(recovered).nn)
          finally ju.Arrays.fill(recovered, 0.toByte)

        try block(cleartext) finally ju.Arrays.fill(cleartext, 0.toByte)

    cleanable(secret, arena, segment)
    secret

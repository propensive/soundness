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
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import stratiform.*
import turbulence.*
import vacuous.*

import LiraError.Reason

object Lira:
  // The interpreter directive's payload, as the parser stores it (without the `#!`). The full
  // first line of every `.lira` file is byte-fixed (§5.1, L115).
  val directive: Text = t"/usr/bin/env lira"

  private val directiveBytes: Data =
    charEncoders.utf8Encoder.encoded(t"#!/usr/bin/env lira\n")

  private val separatorBytes: Data = charEncoders.utf8Encoder.encoded(t"\n##\n")

  // Locates the document separator: the first line that is exactly `##`. §5.2 fixes the byte
  // layout so this split needs no TEL parsing — which is essential, since everything after the
  // separator is binary.
  private def separatorIndex(data: Data): Optional[Int] =
    var index = 0

    while index + separatorBytes.length <= data.length do
      var offset = 0

      while offset < separatorBytes.length && data(index + offset) == separatorBytes(offset) do
        offset += 1

      if offset == separatorBytes.length then return index
      index += 1

    Unset

  private def slice(data: Data, from: Int, until: Int): Data =
    val buffer = Array[Byte](until - from)
    System.arraycopy(Array.unsafeJvm(data), from, buffer.raw, 0, until - from)
    Array.freeze(buffer)

  def read(data: Data): Lira raises LiraError =
    // Step 0 (§16): the directive is checked byte-for-byte before anything is parsed (L115).
    if data.length < directiveBytes.length then abort(LiraError(Reason.BadDirective))

    var index = 0

    while index < directiveBytes.length do
      if data(index) != directiveBytes(index) then abort(LiraError(Reason.BadDirective))
      index += 1

    val separator = separatorIndex(data) match
      case position: Int => position

      case _ =>
        abort(LiraError(Reason.InvalidManifest(t"the document separator is missing")))

    val manifestData = slice(data, 0, separator + 1)
    val compressed = slice(data, separator + separatorBytes.length, data.length)

    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case TelError(reason, _) => LiraError(Reason.InvalidManifest(t"$reason"))

      . protect(manifestData.utf8.load[Tel])

    // L116: the pragma must not specify a sigil; the separator is therefore always `##`.
    document.metadata.pragma.let: pragma =>
      if pragma.sigil.present then abort(LiraError(Reason.SigilSpecified))

    val tel = document.root

    import Tels.Decoder.validate

    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case TelError(reason, _) => LiraError(Reason.InvalidManifest(t"$reason"))

    . protect(tel.validate(using LiraSchemas.lira, LiraValidators.registry))

    Lira(LiraManifest.decode(tel), tel, compressed)

  // Assembles a complete `.lira` file: the blob stream is built from `blobs` (deduplicated and
  // sorted), compressed, and described by a payload record that replaces whatever `manifest`
  // carried. Byte-deterministic for fixed inputs and toolchain (§17).
  def assemble(manifest: LiraManifest, blobs: List[Data]): Data =
    val stream = BlobStream.write(blobs)
    val compressed = LiraPayload.compress(stream)

    val payload =
      LiraManifest.Payload(t"brotli", stream.length.toLong, LiraPayload.hash(stream))

    val text = manifest.copy(payload = payload).render
    val manifestData = charEncoders.utf8Encoder.encoded(text)
    val buffer = Array[Byte](manifestData.length + 3 + compressed.length)
    System.arraycopy(Array.unsafeJvm(manifestData), 0, buffer.raw, 0, manifestData.length)
    buffer(manifestData.length) = '#'.toByte
    buffer(manifestData.length + 1) = '#'.toByte
    buffer(manifestData.length + 2) = '\n'.toByte

    System.arraycopy
      ( Array.unsafeJvm(compressed), 0, buffer.raw, manifestData.length + 3, compressed.length )

    Array.freeze(buffer)

// A read `.lira` file: the typed manifest, the parsed TEL document it projects (the semantic
// model that signing and reserialization operate on), and the still-compressed payload.
case class Lira(manifest: LiraManifest, tel: Tel, compressed: Data)

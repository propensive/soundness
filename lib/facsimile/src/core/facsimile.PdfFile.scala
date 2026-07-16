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
package facsimile

import java.io as ji
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object PdfFile:
  def apply[path: Abstractable across Paths to Text](path: path): PdfFile =
    new PdfFile(Origin.OnDisk(path.generic))

  def apply(data: Data): PdfFile = new PdfFile(Origin.InMemory(data))

  private enum Origin:
    case OnDisk(filename: Text)
    case InMemory(data: Data)

// An unopened PDF: a locator, analogous to `Zipfile`, holding either a path or in-memory
// bytes. Nothing is read until `open` is called; a future write mode becomes a sibling scope
// (`edit`) rather than a change to this one.
class PdfFile private (origin: PdfFile.Origin):
  import PdfFile.Origin

  // Runs `block` with a contextual `Pdf` — reached through the package-level `pdf` accessor —
  // over a source held open exactly for the block's duration. The header, cross-reference
  // chain and (once supported) encryption keys are read eagerly, so a malformed file fails
  // here and not at first access. Capture checking confines the `Pdf`, and anything that
  // still resolves through it, to the block.
  def open[result](password: Optional[Text] = Unset)(block: Pdf ?=> result)
  :   result raises PdfError =

    origin match
      case Origin.InMemory(data) =>
        read(DataSource(data), password)(block)

      case Origin.OnDisk(filename) =>
        val channel =
          try jnc.FileChannel.open(jnf.Path.of(filename.s), jnf.StandardOpenOption.READ).nn
          catch case error: ji.IOException =>
            abort(PdfError(PdfError.Reason.Io(error.getMessage.nn.tt)))

        try read(ChannelSource(channel), password)(block) finally channel.close()

  // The capability must be minted where the block is applied: a `Pdf` returned from another
  // method is a distinct fresh capability which could not flow into the block's own.
  private def read[result](source: ByteSource, password: Optional[Text])(block: Pdf ?=> result)
  :   result raises PdfError =

    val version = Pdf.readVersion(source) // check the header before anything else is trusted
    val pdf = Pdf(source, Xref.load(source), version)
    Pdf.validate(pdf, password)
    block(using pdf)

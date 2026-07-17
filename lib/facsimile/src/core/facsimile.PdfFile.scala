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
import aperture.*
import contingency.*
import enigmatic.*
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

  // A named class rather than an anonymous given instance, for the reasons documented on
  // galilei's `FileOpenable`. Documents open read-only: a future write mode is a staged
  // sibling scope (PDF incremental updates), refused until it lands. A `Password` is passed
  // as a flag: `PdfFile(path).open(Password(t"..."))`.
  class PdfOpenable(using pdfError: Tactic[PdfError]) extends Openable:
    type Self = PdfFile
    type Form = Pdf
    type Operand = Password
    type Result = Pdf

    def open[grants <: Grant, result]
      ( value: PdfFile, mode: Mode granting grants, flags: List[Password] )
      ( block: ((Pdf & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.contains(Write) then abort(PdfError(PdfError.Reason.WriteUnsupported))
      value.openAs[grants, result](flags.prim)(block)

  // Opening a path or in-memory data directly as `Pdf`, without naming the `PdfFile` locator:
  // `path.open[Pdf]()`. (The form must be explicit for these targets, which are openable in
  // several forms.)
  class PdfPathOpenable[path: Abstractable across Paths to Text]
    ( using pdfError: Tactic[PdfError] )
  extends Openable:

    type Self = path
    type Form = Pdf
    type Operand = Password
    type Result = Pdf

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Password] )
      ( block: ((Pdf & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.contains(Write) then abort(PdfError(PdfError.Reason.WriteUnsupported))
      PdfFile(value).openAs[grants, result](flags.prim)(block)

  class PdfDataOpenable(using pdfError: Tactic[PdfError]) extends Openable:
    type Self = Data
    type Form = Pdf
    type Operand = Password
    type Result = Pdf

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Password] )
      ( block: ((Pdf & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.contains(Write) then abort(PdfError(PdfError.Reason.WriteUnsupported))
      PdfFile(value).openAs[grants, result](flags.prim)(block)

  // Anchored here so `pdfFile.open(...)` resolves — and, `PdfFile` having a unique instance,
  // infers the `Pdf` form — with no import.
  given openable: Tactic[PdfError] => ( PdfOpenable^ ) = PdfOpenable()

// An unopened PDF: a locator, analogous to `Zipfile`, holding either a path or in-memory
// bytes. Nothing is read until `open` is called; a future write mode becomes a sibling scope
// (`edit`) rather than a change to this one.
class PdfFile private (origin: PdfFile.Origin):
  import PdfFile.Origin

  // Runs `block` with a contextual `Pdf` — reached through the package-level `pdf` accessor —
  // over a source held open exactly for the block's duration. The header, cross-reference
  // chain and, for an encrypted document, the encryption keys are read eagerly, so a
  // malformed file — or a wrong password — fails here and not at first access. Capture
  // checking confines the `Pdf`, and anything that still resolves through it, to the block.
  // Reached through aperture's `open` (the `Openable` instances in the companion), which
  // supplies the grants to reflect into the capability's type.
  private[facsimile] def openAs[grants <: Grant, result](password: Optional[Password])
    ( block: ((Pdf & Granting[grants])^) ?=> result )
  :   result raises PdfError =

    origin match
      case Origin.InMemory(data) =>
        read[grants, result](DataSource(data), password)(block)

      case Origin.OnDisk(filename) =>
        val channel =
          try jnc.FileChannel.open(jnf.Path.of(filename.s), jnf.StandardOpenOption.READ).nn
          catch case error: ji.IOException =>
            abort(PdfError(PdfError.Reason.Io(error.getMessage.nn.tt)))

        try read[grants, result](ChannelSource(channel), password)(block)
        finally channel.close()

  // The capability must be minted where the block is applied: a `Pdf` returned from another
  // method is a distinct fresh capability which could not flow into the block's own. The
  // `Granting` cast only refines the static type with the grants aperture has already vetted.
  private def read[grants <: Grant, result]
    ( source: ByteSource, password: Optional[Password] )
    ( block: ((Pdf & Granting[grants])^) ?=> result )
  :   result raises PdfError =

    val version = Pdf.readVersion(source) // check the header before anything else is trusted
    val pdf = Pdf(source, Xref.load(source), version)
    Pdf.unlock(pdf, password)
    block(using pdf.asInstanceOf[Pdf & Granting[grants]])

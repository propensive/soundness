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
import java.nio as jn
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import enigmatic.*
import galilei.CreateFlag
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

  // Writes the authored bytes to a hidden temporary sibling, then moves it atomically onto
  // the target — so an exception escaping the creation scope leaves nothing behind.
  private[facsimile] def commit(filename: Text, flags: List[CreateFlag], bytes: Data)
    ( using Tactic[PdfError] )
  :   Unit =

    val target = jnf.Path.of(filename.s).nn

    if !flags.contains(CreateFlag.Replace) && jnf.Files.exists(target)
    then abort(PdfError(PdfError.Reason.Io(t"the file already exists")))

    try
      if flags.contains(CreateFlag.Parents) then
        Optional(target.toAbsolutePath.nn.getParent).let(jnf.Files.createDirectories(_))

      val last = filename.s.split('/').nn.last.nn
      val temporary = target.resolveSibling(t".$last.part".s).nn

      try
        jnf.Files.write(temporary, bytes.mutable(using Unsafe))
        jnf.Files.move(temporary, target, jnf.StandardCopyOption.ATOMIC_MOVE,
            jnf.StandardCopyOption.REPLACE_EXISTING)
      catch case throwable: Throwable =>
        try jnf.Files.deleteIfExists(temporary) catch case _: Exception => ()
        throw throwable
    catch case error: ji.IOException =>
      abort(PdfError(PdfError.Reason.Io(error.getMessage.nn.tt)))

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

      value.openAs[grants, result](flags.prim, mode.atoms.contains(Write))(block)

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

      PdfFile(value).openAs[grants, result](flags.prim, mode.atoms.contains(Write))(block)

  class PdfDataOpenable(using pdfError: Tactic[PdfError]) extends Openable:
    type Self = Data
    type Form = Pdf
    type Operand = Password
    type Result = Pdf

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Password] )
      ( block: ((Pdf & Granting[grants])^) ?=> result )
    :   result =

      PdfFile(value).openAs[grants, result](flags.prim, mode.atoms.contains(Write))(block)

  // Anchored here so `pdfFile.open(...)` resolves — and, `PdfFile` having a unique instance,
  // infers the `Pdf` form — with no import.
  given openable: Tactic[PdfError] => ( PdfOpenable^ ) = PdfOpenable()

  // Authoring a new document: `path.create[Pdf](): doc ?=> doc.appendPage(...)`. The block
  // edits a fresh, empty document — the same write surface as editing an existing one — and
  // the result is written in full, to a temporary sibling moved atomically onto the target,
  // so a failure leaves nothing behind. `Replace`/`Parents` govern a pre-existing target.
  class PdfCreatable[path: Abstractable across Paths to Text](using pdfError: Tactic[PdfError])
  extends Creatable:

    type Self = path
    type Form = Pdf
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = Pdf

    def create[result](value: path, flags: List[CreateFlag])
      ( block: ((Pdf & Granting[Grant.Read & Grant.Write])^) ?=> result )
    :   result =

      val pdf = Pdf.blank()
      val outcome = block(using pdf.asInstanceOf[Pdf & Granting[Grant.Read & Grant.Write]])
      PdfFile.commit(value.generic, flags, PdfWriter.full(pdf))
      outcome

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
  private[facsimile] def openAs[grants <: Grant, result]
    ( password: Optional[Password], writable: Boolean )
    ( block: ((Pdf & Granting[grants])^) ?=> result )
  :   result raises PdfError =

    origin match
      case Origin.InMemory(data) =>
        // In-memory bytes have nowhere to be written back; opening them writably is refused.
        if writable then abort(PdfError(PdfError.Reason.WriteUnsupported))
        read[grants, result](DataSource(data), password, Unset)(block)

      case Origin.OnDisk(filename) =>
        val options =
          if writable
          then List(jnf.StandardOpenOption.READ, jnf.StandardOpenOption.WRITE)
          else List(jnf.StandardOpenOption.READ)

        val channel =
          try jnc.FileChannel.open(jnf.Path.of(filename.s), options*).nn
          catch case error: ji.IOException =>
            abort(PdfError(PdfError.Reason.Io(error.getMessage.nn.tt)))

        try read[grants, result](ChannelSource(channel), password, if writable then channel else Unset)(block)
        finally channel.close()

  // The capability must be minted where the block is applied: a `Pdf` returned from another
  // method is a distinct fresh capability which could not flow into the block's own. The
  // `Granting` cast only refines the static type with the grants aperture has already vetted.
  // When a write channel is supplied, the overlay accumulated during the block is serialised
  // as an incremental update and appended to the file on the way out.
  private def read[grants <: Grant, result]
    ( source: ByteSource, password: Optional[Password], sink: Optional[jnc.FileChannel] )
    ( block: ((Pdf & Granting[grants])^) ?=> result )
  :   result raises PdfError =

    val version = Pdf.readVersion(source) // check the header before anything else is trusted
    val pdf = Pdf(source, Xref.load(source), version)
    Pdf.unlock(pdf, password)
    val outcome = block(using pdf.asInstanceOf[Pdf & Granting[grants]])

    sink.let: channel =>
      if pdf.dirty then
        // An incremental update chains to the previous cross-reference section; a file whose
        // table was only recovered by scanning has none to chain to.
        if pdf.xref.startxref.absent then abort(PdfError(PdfError.Reason.WriteUnsupported))

        val bytes = PdfWriter.increment(pdf, source.size)
        val buffer = jn.ByteBuffer.wrap(bytes.mutable(using Unsafe)).nn
        var position = source.size
        while buffer.hasRemaining do position += channel.write(buffer, position)

    outcome

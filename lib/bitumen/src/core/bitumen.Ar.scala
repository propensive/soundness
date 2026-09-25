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
package bitumen

import scala.caps

import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.asciiEncoder, textMetrics.uniformMetric
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import zephyrine.*

// `ar` is the container format of every `.deb` and every `.a` static library: an eight-byte magic,
// then a flat run of sixty-byte headers, each followed inline by its payload and padded to an even
// offset. It carries no index, no checksum, no entry type and — unlike TAR — no end-of-archive
// marker, so EOF ends the archive and a member whose recorded size overruns the file is the only
// corruption signal the format offers.
object Ar:
  val magic: Text = t"!<arch>\n"
  val magicSize: Int = 8
  val headerSize: Int = 60

  // Anchored here so `data.open[Ar]()` resolves with no import. Opening a filesystem *path* as
  // `Ar` (`path.open[Ar]()`) lives in `bitumen.jvm`, alongside the disk backend.
  given dataOpenable: (tactic: Tactic[Ar.Error]) => (Ar.DataOpenable^{tactic}) = Ar.DataOpenable()

  // The kinds of member a GNU archive holds, told apart only by name — `ar` has no type field.
  // Debian's `.deb` uses `Regular` exclusively; the other two appear in `.a` static libraries,
  // which nest inside the `data.tar` of every `-dev` package.
  enum Role:
    case Regular, SymbolTable, NameTable

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case BadMagic(actual: Data) extends Reason(1)
      case BadTerminator(actual: Data) extends Reason(2)
      case BadDecimal(field: Text, data: Data) extends Reason(3)
      case BadOctal(field: Text, data: Data) extends Reason(4)
      case TruncatedStream(needed: Int, got: Int) extends Reason(5)
      case BadLongNameRef(reference: Text) extends Reason(6)
      case UnsupportedNameFormat(name: Text) extends Reason(7)
      case WriteUnsupported extends Reason(8)

    given communicable: Reason is Communicable =
      case Reason.BadMagic(actual) =>
        m"the archive does not begin with the ar magic bytes (got ${actual.length} bytes)"

      case Reason.BadTerminator(actual) =>
        m"the header is not terminated by the two magic bytes (got ${actual.length} bytes)"

      case Reason.BadDecimal(field, _) =>
        m"the $field field did not contain a valid decimal value"

      case Reason.BadOctal(field, _) =>
        m"the $field field did not contain a valid octal value"

      case Reason.TruncatedStream(needed, got) =>
        m"the archive stream ended unexpectedly (needed $needed bytes, got $got)"

      case Reason.BadLongNameRef(reference) =>
        m"the long-name reference $reference does not resolve in the archive's name table"

      case Reason.UnsupportedNameFormat(name) =>
        m"the member name $name uses the BSD length-prefixed form, which is not supported"

      case Reason.WriteUnsupported =>
        m"ar archives cannot yet be opened for writing"

  case class Error(reason: Ar.Error.Reason)(using Diagnostics)
  extends fulminate.Error(286, reason.number)
    ( m"the ar archive could not be read or written because $reason" )

  object Entry:
    // The padding rule: a member's payload is followed by a single newline when its size is odd,
    // so every header begins at an even offset.
    def padded(size: Long): Long = ((size + 1)/2)*2

    // A member built for writing rather than parsed. The stored name takes the SysV terminating
    // `/`, which is what lets a name with trailing spaces survive a round trip, and no verbatim
    // header is retained, so the header is formatted from these fields.
    def apply
      ( name: Text, data: Archive.Body, mtime: Long, user: UnixUser, group: UnixGroup, mode: Int )
    :   Ar.Entry =

      Ar.Entry(name, t"$name/", mtime, user, group, mode, data)

  // A single archive member. `name` is resolved — a GNU long name has been looked up in the
  // archive's `//` table and a SysV trailing `/` stripped — while `rawName` keeps the literal
  // sixteen-byte field, so an archive can be rewritten byte for byte. `mode` is the raw octal
  // field rather than a `UnixMode`: `ar` stores the file-type bits (`100644`), which `UnixMode`
  // does not represent, and dropping them would change the formatted header.
  case class Entry
    ( name:    Text,
      rawName: Text,
      mtime:   Long,
      user:    UnixUser,
      group:   UnixGroup,
      mode:    Int,
      data:    Archive.Body,
      origin:  Optional[Data] = Unset ):

    def role: Ar.Role =
      // `/SYM64/` is the 64-bit symbol table GNU writes when member offsets outgrow 32 bits; it
      // occurs in real `.a` files (one in sixty of libxml2's) and, beginning with a slash, would
      // otherwise be mistaken for a long-name reference.
      if rawName == t"/" || rawName == t"/SYM64/" then Ar.Role.SymbolTable
      else if rawName == t"//" then Ar.Role.NameTable
      else Ar.Role.Regular

    def size: Long = data.size

    // The verbatim header when this entry was parsed from an archive, and a formatted one when it
    // was constructed. Re-emitting the original matters for byte-exact reconstruction: GNU writes
    // the `//` name table with its mtime, uid, gid and mode fields left *blank*, and no decoded
    // numeric form can tell blank from zero. The formatted fallback reproduces the original
    // `rawName` rather than the resolved name, so a long-name archive still round-trips against
    // its own `//` table.
    def header: Data =
      origin.or(Ar.Header.format(rawName, mtime, user.value, group.value, mode, size))

    def serialize: Iterator[Data] =
      val body = data.chunks

      if size%2 == 0 then Iterator(header) ++ body
      else Iterator(header) ++ body ++ Iterator(Ar.Header.padByte)

  object Header:
    val headerSize: Int = 60
    val terminator: Text = t"`\n"
    val padByte: Data = t"\n".in[Data]

    def parse(block: Data): Header raises Ar.Error =
      // Unlike a TAR header, a short `ar` header cannot be parsed best-effort: there is no checksum
      // and no end marker, so the recorded size is the only thing locating the next header. Stop.
      if block.length < headerSize
      then abort(Ar.Error(Ar.Error.Reason.TruncatedStream(headerSize, block.length)))

      Header
        ( name       = block.segment((0).z till (16).z),
          mtime      = block.segment((16).z till (28).z),
          uid        = block.segment((28).z till (34).z),
          gid        = block.segment((34).z till (40).z),
          mode       = block.segment((40).z till (48).z),
          size       = block.segment((48).z till (58).z),
          terminator = block.segment((58).z till (60).z) )

    // `ar` has no checksum; the two-byte terminator is the only structural check a header offers,
    // and a mismatch means the cursor is no longer on a header boundary.
    def verifyTerminator(header: Header): Unit raises Ar.Error =
      if header.terminator.utf8 != terminator
      then abort(Ar.Error(Ar.Error.Reason.BadTerminator(header.terminator)))

    def decodeDecimal(data: Data, field: Text): Long raises Ar.Error = decodeRadix(data, field, 10)

    def decodeOctal(data: Data, field: Text): Long raises Ar.Error = decodeRadix(data, field, 8)

    // The digit run starts at the field's first byte and is followed by padding. An entirely
    // blank field decodes as zero: some archivers leave `uid` and `gid` empty rather than
    // writing `0`, and rejecting that would fail on real archives for no benefit.
    private def decodeRadix(data: Data, field: Text, radix: Int): Long raises Ar.Error =
      val top: Byte = ('0'.toInt + radix).toByte

      val digits = data.prefix: index =>
        val byte: Byte = data.at(index)
        byte >= '0'.toByte && byte < top

      // Cumulative: `padded` spans the digits and then the padding, so a limit short of the
      // field's end means a stray byte — a non-digit, or a digit resuming after padding.
      val padded = data.prefix(digits): index =>
        val byte: Byte = data.at(index)
        byte == ' '.toByte || byte == 0.toByte

      if (padded: Interval).size != data.length then
        val reason =
          if radix == 8 then Ar.Error.Reason.BadOctal(field, data)
          else Ar.Error.Reason.BadDecimal(field, data)

        raise(Ar.Error(reason))

      var n: Long = 0L

      data.iterate(digits): index => n = n*radix + (data.at(index) - '0'.toByte).toLong

      n

    // The literal name field with its trailing space padding removed. The SysV terminating `/` is
    // *not* stripped here: `/` and `//` name the symbol table and the name table, and telling those
    // apart from a terminated name is the reader's job, which needs the raw form.
    def decodeName(data: Data): Text =
      data.segment(data.pare(0) { index => data.at(index) == ' '.toByte }).utf8

    // Every field is left-aligned and space-padded, and a value too wide for its field is clipped.
    def format(name: Text, mtime: Long, user: Int, group: Int, mode: Int, size: Long): Data =
      val fields: Text =
        t"${name.fit(16)}${mtime.show.fit(12)}${user.show.fit(6)}${group.show.fit(6)}"

      t"$fields${mode.octal.fit(8)}${size.show.fit(10)}$terminator".in[Data]

  // The sixty-byte `ar` member header, held as raw slices: nothing is decoded in the case class, as
  // in `TarHeader`. Every field is left-aligned ASCII padded with trailing spaces — the opposite of
  // TAR's right-aligned, space-led form — and every numeric field is decimal except `mode`, which
  // is octal.
  case class Header
    ( name:       Data,
      mtime:      Data,
      uid:        Data,
      gid:        Data,
      mode:       Data,
      size:       Data,
      terminator: Data )

  // Opens in-memory `Data` as an `ar` archive: `data.open[Ar]()`. Archives open read-only: a
  // `Write` mode is refused with `Ar.Error.Reason.WriteUnsupported`. There are no flags — a `.deb`
  // is plain `ar`; it is the members inside that are compressed — so the operand is uninhabited.
  class DataOpenable(using Tactic[Ar.Error]) extends Openable:
    type Self = Data
    type Form = Ar
    type Operand = Nothing
    type Result = Ar.Handle

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
      ( block: ((Ar.Handle & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.has(Write) then abort(Ar.Error(Ar.Error.Reason.WriteUnsupported))
      val entries = Ar.Handle.entries(value.stream)
      block(using new Ar.Handle(entries) with Granting[grants] {})

  object Handle:
    private[bitumen] def entries(consume stream: (Stream[Data] over Credit)^)
      ( using tactic: Tactic[Ar.Error] )
    :   Iterator[Ar.Entry]^{tactic} =

      Arfile.read(stream)

  // The scoped capability provided by opening an archive as `Ar`: `path.open[Ar]()`. `ar` is a
  // sequential format, so `entries` parses lazily from the underlying source, one entry per
  // step; payloads must be consumed within the scope, while the source remains open. The
  // iterator is single-pass: an entry passed over remains readable (its body memoizes when the
  // iterator advances), but the sequence itself is not replayable within the scope.
  class Handle private[bitumen] (entries0: Iterator[Ar.Entry]^)
  extends caps.ExclusiveCapability:

    // Reached only through this exclusive handle, which scopes it; its capture of the
    // underlying source is erased here, as `Tar.Handle` erases its own.
    @caps.unsafe.untrackedCaptures
    val entries: Iterator[Ar.Entry] = caps.unsafe.unsafeAssumePure(entries0)

// An uninhabited marker naming the format, in the manner of `Tar`.
sealed trait Ar

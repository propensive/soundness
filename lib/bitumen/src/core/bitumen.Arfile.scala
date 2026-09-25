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
import contingency.*
import denominative.*
import gossamer.*
import hieroglyph.*, charEncoders.asciiEncoder
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Arfile:
  given streamable: Arfile is Streamable by Data over Credit = arfile => Stream(arfile.blocks)

  // The endpoint form, as `Tarfile.read`: entries parse lazily straight off a pull endpoint,
  // absorbing arbitrary chunk boundaries, and are single-owner — consume them in order, on one
  // thread. An explicit `Tactic` rather than `raises` sugar: a fresh capability in a
  // context-function result cannot flow to a forwarding caller.
  def read(consume stream: (Stream[Data] over Credit)^)(using tactic: Tactic[Ar.Error])
  :   Iterator[Ar.Entry]^{tactic} =

    // The stream's single ownership passes to the cursor inside the iterator, whose fresh
    // capability is laundered (nothing else can reach it).
    scala.caps.unsafe.unsafeAssumePure:
      scala.caps.unsafe.unsafeAssumeSeparate(entryIterator(Cursor[Data](stream)))

  def from(consume stream: (Stream[Data] over Credit)^)(using Tactic[Ar.Error]): Arfile =
    // The stream's single ownership passes with this call; the checker cannot see through the
    // consumed parameter's re-use in the nested call.
    scala.caps.unsafe.unsafeAssumeSeparate:
      Arfile(read(stream).to(List).asInstanceOf[List[Ar.Entry]])

  private def entryIterator(cursor: Cursor[Data, {}]^)(using tactic: Tactic[Ar.Error])
  :   Iterator[Ar.Entry]^{cursor, tactic} =

    def truncated(needed: Int, got: Int): Nothing =
      abort(Ar.Error(Ar.Error.Reason.TruncatedStream(needed, got)))

    // A member read in full, for the one member the reader cannot lend to its consumer: the
    // `//` name table, which every later long name resolves against.
    def takeWhole(size: Long): Data =
      val data = Archive.takeExactly(cursor, Ar.Entry.padded(size).toInt)(truncated)
      data.segment((0).z till (size.toInt).z)

    new Archive.Lookahead[Ar.Entry]:
      @caps.unsafe.untrackedCaptures
      private var nameTable: Optional[Data] = Unset

      @caps.unsafe.untrackedCaptures
      private var started: Boolean = false

      protected def parse(): Unit =
        if !started then
          started = true
          val head = Archive.takeExactly(cursor, Ar.magicSize)(truncated)
          if head.utf8 != Ar.magic then abort(Ar.Error(Ar.Error.Reason.BadMagic(head)))

        // No end-of-archive marker exists: the archive is over exactly when the stream is.
        if cursor.finished then finish() else
          val block = Archive.takeExactly(cursor, Ar.Header.headerSize)(truncated)
          val header = Ar.Header.parse(block)
          Ar.Header.verifyTerminator(header)

          val rawName = Ar.Header.decodeName(header.name)
          val size = Ar.Header.decodeDecimal(header.size, t"size")
          val mtime = Ar.Header.decodeDecimal(header.mtime, t"mtime")
          val user = UnixUser(Ar.Header.decodeDecimal(header.uid, t"uid").toInt)
          val group = UnixGroup(Ar.Header.decodeDecimal(header.gid, t"gid").toInt)
          val mode = Ar.Header.decodeOctal(header.mode, t"mode").toInt

          if rawName == t"//" then
            // Every later long name resolves against this member, so it is read here rather than
            // lent to the consumer, who might never force it.
            val table = takeWhole(size)
            nameTable = table

            emit(Ar.Entry(rawName, rawName, mtime, user, group, mode, Archive.Body(table), block))
          else
            val name = resolveName(rawName)

            val body =
              Archive.Body.deferred:
                // Erases the two independently-freshened `any.rd`s on the frozen chunk type.
                Archive.bodyPull(cursor, size, Ar.Entry.padded(size))(truncated)
                . asInstanceOf[() => Optional[Data]]

            emit(Ar.Entry(name, rawName, mtime, user, group, mode, body, block), body)

      // GNU stores a name longer than fifteen characters in the `//` member and refers to it by
      // byte offset; SysV terminates a shorter name with `/`, which disambiguates a name with
      // trailing spaces. BSD's `#1/<length>` form, which puts the name in the payload, does not
      // occur in Debian archives.
      private def resolveName(raw: Text): Text =
        val text: String = raw.s

        // `/`, `//` and the 64-bit symbol table `/SYM64/` name themselves; everything else
        // beginning with a slash is an offset into the name table.
        if text == "/" || text == "//" || text == "/SYM64/" then raw
        else if text.startsWith("#1/")
        then abort(Ar.Error(Ar.Error.Reason.UnsupportedNameFormat(raw)))
        else if text.startsWith("/") then longName(raw, text.substring(1).nn)
        else if text.endsWith("/") then text.substring(0, text.length - 1).nn.tt
        else raw

      private def longName(raw: Text, reference: String): Text =
        val table: Data =
          nameTable.lay(abort(Ar.Error(Ar.Error.Reason.BadLongNameRef(raw))))(identity)

        val offset: Int =
          try Integer.parseInt(reference)
          catch case _: NumberFormatException =>
            abort(Ar.Error(Ar.Error.Reason.BadLongNameRef(raw)))

        if offset < 0 || offset >= table.length
        then abort(Ar.Error(Ar.Error.Reason.BadLongNameRef(raw)))

        // Names in the table are terminated by `/` (GNU) or by the newline that separates them;
        // the scan resumes from `offset`, the limit of the capped extent.
        val run = table.prefix(table.extent.capped(offset)): index =>
          val byte: Byte = table.at(index)
          byte != '/'.toByte && byte != '\n'.toByte

        table.segment((offset).z till (run: Interval).limit).utf8

case class Arfile(entries: List[Ar.Entry]):
  // The raw bytes of the archive: the magic, then each member's header and padded payload. There
  // is no trailing marker to emit — `ar` archives simply end. Reach this externally through the
  // `Streamable` given, i.e. `arfile.source[Data]`.
  private[bitumen] def blocks: Iterator[Data] =
    // The blocks are emitted through a stdlib `Iterator`, which the opaque `List` cannot yield.
    Iterator(Ar.magic.in[Data]) ++ entries.stdlib.iterator.flatMap(_.serialize)

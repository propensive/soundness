                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╭─────────╮│   │╭───╮╭─────────╮╭─────────╮╭─────────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ││   ╭─╮   ││   ││   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   │   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   ││   │ │   ││   ││   ││   ╰─╯   ││   ╰─╯   ││   ╰─╯   │   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   ││   │ │   ││   ││   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   ││   ╰─╯   ││   ││   ││   │ │   ││   │ │   ││   │ │   │   ┃
┃   ╰───────╯╰─────────╯╰────╮   ╭╯╰───╯╰─────────╯╰───╯╰───╯╰───╯ ╰───╯╰───╯ ╰───╯╰───╯ ╰───╯   ┃
┃                       ╭─╮  │   │                                                                 ┃
┃                       │ ╰──╯   │                                                                 ┃
┃                       ╰────────╯                                                                 ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃      https://soundness.dev/                                                                      ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃      http://www.apache.org/licenses/LICENSE-2.0                                                  ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ethereal

import java.io as ji
import java.lang as jl

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*, codepages.utf8Codepage
import prepositional.*
import rudiments.*
import spectacular.*
import stratiform.*
import turbulence.*
import vacuous.*

// The contract between an Ethereal launcher (the Rust runner published by `propensive/xek`,
// in its `src/runner`) and its daemon. An invocation is one connection: it opens with an
// `init` document and then carries, in both directions, the documents of a session — `data`
// chunks of every stream, `end`, `credit`, `open`, `signal`/`signal-ack`, `mode` and
// `closed` — until the daemon's `exit-status` ends it. `verify` and `shutdown` are asked on
// short connections of their own. The session's rules are in xek's `spec/launcher.md`, and
// `Session` is their daemon half.
//
// The schema is the specification: the runner's `bintel.rs` encodes and decodes exactly its
// keyword order, and carries the schema's 33-byte signature as a constant, so a launcher and
// a daemon built against different schemas refuse each other at the first document rather
// than misreading fields. The TEL text is the source of truth — byte for byte xek's
// `spec/ethereal-launcher.tel` — and the enum mirrors it member for member; the tests pin
// the signature and the wire bytes of a sample of messages against the values the runner's
// unit tests pin.
object Launcher:
  val schemaText: Text = Text("""|name ethereal-launcher
                            |
                            |document
                            |  select Message required
                            |
                            |select Message
                            |  variant init Init
                            |  variant data Data
                            |  variant end End
                            |  variant credit Credit
                            |  variant open Open
                            |  variant signal Signal
                            |  variant signal-ack SignalAck
                            |  variant mode Mode
                            |  variant closed Closed
                            |  variant exit-status ExitStatus
                            |  variant verify Verify
                            |  variant verdict Verdict
                            |  variant shutdown Shutdown
                            |
                            |scalar Bytes
                            |  description
                            |      Raw bytes: one chunk of a stream.
                            |  encoding base-256
                            |
                            |record Init
                            |  description
                            |      A new invocation, opening the session that carries it: the
                            |      connection then carries data, end, credit, open, signal,
                            |      signal-ack, mode and closed documents in either direction,
                            |      until the daemon ends it with exit-status. The three tty
                            |      flags say which of the client's streams are attached to a
                            |      terminal; the daemon sees only sockets and cannot determine
                            |      this for itself. The uid is the platform's identifier for the
                            |      user: numeric on Unix, a SID on Windows. The invoked-as field
                            |      is argv[0] as the caller supplied it, for a multi-call binary
                            |      to dispatch on; script is the canonical path. The umask is
                            |      octal; columns and rows are the terminal's size when stdout is
                            |      a terminal; the code pages are the Windows console's input and
                            |      output code pages. Each descriptor is a file descriptor the
                            |      client holds, which the daemon may open as a stream.
                            |  field pid String required
                            |  field uid String required
                            |  field username String required
                            |  field script String required
                            |  field pwd String required
                            |  field stdin-tty Flag optional
                            |  field stdout-tty Flag optional
                            |  field stderr-tty Flag optional
                            |  field argument String optional repeatable
                            |  field environment String optional repeatable
                            |  field invoked-as String optional
                            |  field umask String optional
                            |  field columns String optional
                            |  field rows String optional
                            |  field input-codepage String optional
                            |  field output-codepage String optional
                            |  field descriptor Descriptor optional repeatable
                            |
                            |record Descriptor
                            |  description
                            |      A file descriptor open in the client when it connected,
                            |      numbered as the client sees it; 0, 1 and 2 are among them.
                            |      The direction is r, w or rw, as the descriptor was opened.
                            |      The kind is file, pipe, tty, socket or other; for a file the
                            |      path is its real path, which the daemon may open directly.
                            |      Anything else is reached by opening the descriptor as a
                            |      stream named by its number.
                            |  field fd String required
                            |  field direction String required
                            |  field kind String required
                            |  field path String optional
                            |
                            |record Data
                            |  description
                            |      One chunk of a stream, of at most 65536 bytes. The stream is
                            |      stdin, stdout, stderr or the number of an open descriptor.
                            |      A sender never has more bytes outstanding on a stream than
                            |      the credit it holds for it.
                            |  field stream String required
                            |  field bytes Bytes required
                            |
                            |record End
                            |  description
                            |      The named stream has ended, after every chunk sent before
                            |      this: end-of-file for the daemon's reader of stdin or of a
                            |      descriptor opened to read, or the daemon's close of a
                            |      descriptor opened to write. Nothing more is sent on it.
                            |  field stream String required
                            |
                            |record Credit
                            |  description
                            |      The receiver of a stream can take this many more bytes of it,
                            |      in addition to any credit it granted before. The session opens
                            |      with 65536 bytes of credit on every stream in each direction.
                            |  field stream String required
                            |  field bytes String required
                            |
                            |record Open
                            |  description
                            |      Asks the launcher to start carrying the named descriptor: as
                            |      data documents from the client if it was opened to read, or
                            |      by writing data documents the daemon sends to it if it was
                            |      opened to write. Not answered; the stream simply begins. A
                            |      descriptor the daemon did not advertise, or opens again
                            |      while it is open, is ignored.
                            |  field stream String required
                            |
                            |record Signal
                            |  description
                            |      A signal the client received, named without its SIG prefix,
                            |      or a Windows console control event. WINCH and CONT carry the
                            |      terminal's current size; a Windows close, logoff or shutdown
                            |      carries the milliseconds the system allows before it ends
                            |      the client regardless. Answered with signal-ack; the launcher
                            |      sends no further signal until it has the answer.
                            |  field name String required
                            |  field columns String optional
                            |  field rows String optional
                            |  field deadline String optional
                            |
                            |record SignalAck
                            |  description
                            |      Whether the invocation accepted the signal last sent.
                            |  field accept Flag optional
                            |
                            |record Mode
                            |  description
                            |      Asks the launcher to put the client's terminal into canonical
                            |      (cooked) mode, or back into raw mode when the flag is absent.
                            |  field canonical Flag optional
                            |
                            |record Closed
                            |  description
                            |      The named stream has lost its reader at the client: stdout or
                            |      stderr could not be written, or a descriptor opened to write
                            |      could not be. The daemon should fail the invocation's further
                            |      writes to that stream, as a broken pipe would. Sent by the
                            |      daemon, it says the invocation has closed a descriptor it
                            |      opened to read before reading it to its end, and the launcher
                            |      stops carrying it.
                            |  field stream String required
                            |
                            |record ExitStatus
                            |  description
                            |      The invocation's exit status, ending the session: the last
                            |      document the daemon writes, after every chunk of every stream.
                            |  field code String required
                            |
                            |record Verify
                            |  description
                            |      Asks, on a connection of its own, whether the launcher file
                            |      the daemon started from still has the content it remembers;
                            |      answered with a verdict.
                            |  field launcher String optional
                            |
                            |record Verdict
                            |  field fresh Flag optional
                            |
                            |record Shutdown
                            |  description
                            |      Asks the daemon to exit: to accept no further invocations, to
                            |      let those in flight finish, and then to end. Not answered; the
                            |      connection is closed. A launcher whose daemon is gone starts a
                            |      fresh one, so this reclaims a warm JVM without leaving anything
                            |      broken.
                            |""".stripMargin)

  // A file descriptor the client holds, as `init` advertises it: its number, `r`/`w`/`rw`,
  // its kind (`file`, `pipe`, `tty`, `socket`, `other`) and, for a regular file, its real path.
  case class Descriptor(fd: Int, direction: Text, kind: Text, path: Optional[Text] = Unset)

  enum Message:
    case Init
      ( pid:            Int,
        uid:            Text,
        username:       Text,
        script:         Text,
        pwd:            Text,
        stdinTty:       Boolean,
        stdoutTty:      Boolean,
        stderrTty:      Boolean,
        arguments:      List[Text],
        environment:    List[Text],
        invokedAs:      Optional[Text] = Unset,
        umask:          Optional[Text] = Unset,
        columns:        Optional[Int]  = Unset,
        rows:           Optional[Int]  = Unset,
        inputCodepage:  Optional[Int]  = Unset,
        outputCodepage: Optional[Int]  = Unset,
        descriptors:    List[Descriptor] = Nil )

    case Data(stream: Text, bytes: anticipation.Data)
    case End(stream: Text)
    case Credit(stream: Text, bytes: Long)
    case Open(stream: Text)

    case Signal
      ( name:     Text,
        columns:  Optional[Int]  = Unset,
        rows:     Optional[Int]  = Unset,
        deadline: Optional[Long] = Unset )

    case SignalAck(accept: Boolean)
    case Mode(canonical: Boolean)
    case Closed(stream: Text)
    case ExitStatus(code: Int)
    case Verify
    case Verdict(fresh: Boolean)
    case Shutdown

  // Parsed once; a malformed schema text is a programming error, not a runtime condition.
  lazy val schema: Tels =
    import strategies.throwUnsafely
    Tels.Validation.validate(Tels.Reconstructor.fromTel(schemaText.read[Tel]))

  // The §8 palimpsest signature of the schema, carried by every document on the wire and
  // compared byte-for-byte on receipt.
  lazy val signature: Data =
    import strategies.throwUnsafely
    SchemaSignature.fromDocument(schemaText.read[Tel], Tels.Axiom.tels)

  // The variant indices of `Message` in the document root's keyword order — a single
  // `SelectRef`, so its variants occupy indices 0 to 12 in declaration order.
  private object Variant:
    val init = 0; val data = 1; val end = 2; val credit = 3; val open = 4; val signal = 5
    val signalAck = 6; val mode = 7; val closed = 8; val exitStatus = 9; val verify = 10
    val verdict = 11; val shutdown = 12

  private val scalar: Tels.Scalar = Tels.Scalar(Array.empty)

  // The `Bytes` scalar carries raw bytes under the `base-256` codec (§21.7): on the wire the
  // bytes themselves, framed as any scalar is; in the element tree, a BASE-256 text.
  private val bytes: Tels.Scalar = Tels.Scalar(Array.empty, t"base-256")

  private def record(name: Text): Tels.Struct =
    val definition = schema.records.seek(_.name == name).or(panic(m"the schema declares $name"))
    Tels.Struct(definition.members, definition.validators)

  private def value(index: Int, text: Text): Tel.Element = Tel.Element.Value(index, scalar, text)
  private def raw(index: Int, text: Text): Tel.Element = Tel.Element.Value(index, bytes, text)
  private def flag(index: Int): Tel.Element = Tel.Element.Node(index, Tels.Flag, Array.empty)

  private def node(variant: Int, name: Text, children: Array[Tel.Element]^{}): Tel.Element =
    Tel.Element.Node
      ( Unset, schema.document, Array(Tel.Element.Node(variant, record(name), children)) )

  private def stream(variant: Int, name: Text, stream: Text): Tel.Element =
    node(variant, name, Array(value(0, stream)))

  private def element(message: Message): Tel.Element = message match
    case Message.Init(pid, uid, username, script, pwd, stdinTty, stdoutTty, stderrTty,
                      arguments, environment, invokedAs, umask, columns, rows, inputCodepage,
                      outputCodepage, descriptors) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, pid.show)
      children += value(1, uid)
      children += value(2, username)
      children += value(3, script)
      children += value(4, pwd)
      if stdinTty then children += flag(5)
      if stdoutTty then children += flag(6)
      if stderrTty then children += flag(7)
      arguments.each { argument => children += value(8, argument) }
      environment.each { variable => children += value(9, variable) }
      invokedAs.let { name => children += value(10, name) }
      umask.let { mask => children += value(11, mask) }
      columns.let { count => children += value(12, count.show) }
      rows.let { count => children += value(13, count.show) }
      inputCodepage.let { page => children += value(14, page.show) }
      outputCodepage.let { page => children += value(15, page.show) }

      descriptors.each: descriptor =>
        val fields = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
        fields += value(0, descriptor.fd.show)
        fields += value(1, descriptor.direction)
        fields += value(2, descriptor.kind)

        descriptor.path match
          case path: Text => fields += value(3, path)
          case _          => ()

        children += Tel.Element.Node(16, record(t"Descriptor"), Array.from(fields))

      node(Variant.init, t"Init", Array.from(children))

    case Message.Data(name, data) =>
      node(Variant.data, t"Data", Array(value(0, name), raw(1, Base256.encode(data))))

    case Message.End(name)         => stream(Variant.end, t"End", name)
    case Message.Open(name)        => stream(Variant.open, t"Open", name)
    case Message.Closed(name)      => stream(Variant.closed, t"Closed", name)
    case Message.Verify            => node(Variant.verify, t"Verify", Array.empty)
    case Message.ExitStatus(code)  => node(Variant.exitStatus, t"ExitStatus", Array(value(0, code.show)))
    case Message.Shutdown          => node(Variant.shutdown, t"Shutdown", Array.empty)

    case Message.Credit(name, count) =>
      node(Variant.credit, t"Credit", Array(value(0, name), value(1, count.show)))

    case Message.Signal(name, columns, rows, deadline) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, name)
      columns.let { count => children += value(1, count.show) }
      rows.let { count => children += value(2, count.show) }
      deadline.let { millis => children += value(3, millis.show) }
      node(Variant.signal, t"Signal", Array.from(children))

    case Message.SignalAck(accept) =>
      node(Variant.signalAck, t"SignalAck", if accept then Array(flag(0)) else Array.empty)

    case Message.Verdict(fresh) =>
      node(Variant.verdict, t"Verdict", if fresh then Array(flag(0)) else Array.empty)

    case Message.Mode(canonical) =>
      node(Variant.mode, t"Mode", if canonical then Array(flag(0)) else Array.empty)

  // A message as one framed BinTEL document (§6.1): magic, length, signature, body. A `data`
  // document is framed by hand: its bytes go straight into the frame, with no BASE-256 text
  // in between, since every chunk of every stream passes this way.
  def encode(message: Message): Data =
    import strategies.throwUnsafely
    message match
      case Message.Data(stream, chunk) => Bintel.frame(dataBody(stream, chunk), signature)
      case other => Bintel.frame(Bintel.encode(element(other), schema, Tel.Codec.Bindings.builtins), signature)

  // The body of a `data` document: the root's one child, the `data` variant, its two fields.
  private def dataBody(stream: Text, chunk: Data): Data =
    val name: scala.Array[Byte] = Array.unsafeJvm(stream.in[Data])
    val out = ji.ByteArrayOutputStream(chunk.length + name.length + 16)
    def varint(value: Long): Unit =
      var n = value
      while n >= 0x80 do
        out.write(((n & 0x7f) | 0x80).toInt)
        n >>= 7
      out.write(n.toInt)

    varint(1); varint(Variant.data); varint(2)
    varint(0); varint(name.length); out.write(name)
    varint(1); varint(chunk.length); out.write(Array.unsafeJvm(chunk))
    Array.unsafeFrozen(out.toByteArray.nn)

  private def sameBytes(left: Data, right: Data): Boolean =
    left.length == right.length && {
      var i = 0
      var same = true

      while same && i < left.length do
        if left.readable(i) != right.readable(i) then same = false
        i += 1

      same
    }

  // The message a framed document carries, or `Unset` if the document is malformed, carries
  // another schema's signature, or does not fit the enum. A `data` document is read by hand,
  // as it is written.
  def decode(data: Data): Optional[Message] = safely:
    val document = Bintel.decodeDocument(data, schema, Tel.Codec.Bindings.builtins)
    if !sameBytes(document.signature, signature) then abort(Launcher.Mismatch())

    document.root match
      case Tel.Element.Node(_, _, Array(Tel.Element.Node(index, _, children))) =>
        def optional(field: Int): Optional[Text] = children.readable.collectFirst:
          case Tel.Element.Value(`field`, _, text) => text
        . getOrElse(Unset)

        def text(field: Int): Text = optional(field).or(abort(Launcher.Mismatch()))

        def texts(field: Int): List[Text] =
          children.readable.toList.collect { case Tel.Element.Value(`field`, _, text) => text }
          . to(List)

        def flag(field: Int): Boolean = children.readable.exists:
          case Tel.Element.Node(`field`, Tels.Flag, _) => true
          case _                                       => false

        def int(field: Int): Int = text(field).as[Int]
        def long(field: Int): Long = text(field).as[Long]
        def optionalInt(field: Int): Optional[Int] = optional(field).let(_.as[Int])
        def optionalLong(field: Int): Optional[Long] = optional(field).let(_.as[Long])

        def descriptors: List[Descriptor] =
          children.readable.toList.collect:
            case Tel.Element.Node(16, _, fields) =>
              def field(index: Int): Optional[Text] = fields.readable.collectFirst:
                case Tel.Element.Value(`index`, _, text) => text
              . getOrElse(Unset)

              Descriptor
                ( field(0).or(abort(Launcher.Mismatch())).as[Int],
                  field(1).or(abort(Launcher.Mismatch())),
                  field(2).or(abort(Launcher.Mismatch())),
                  field(3) )
          . to(List)

        index.or(-1) match
          case Variant.init =>
            Message.Init
              ( int(0), text(1), text(2), text(3), text(4), flag(5), flag(6), flag(7),
                texts(8), texts(9), optional(10), optional(11), optionalInt(12), optionalInt(13),
                optionalInt(14), optionalInt(15), descriptors )

          case Variant.data       => Message.Data(text(0), Base256.decodeStrict(text(1)))
          case Variant.end        => Message.End(text(0))
          case Variant.credit     => Message.Credit(text(0), long(1))
          case Variant.open       => Message.Open(text(0))
          case Variant.signalAck  => Message.SignalAck(flag(0))
          case Variant.mode       => Message.Mode(flag(0))
          case Variant.closed     => Message.Closed(text(0))
          case Variant.exitStatus => Message.ExitStatus(int(0))
          case Variant.verify     => Message.Verify
          case Variant.verdict    => Message.Verdict(flag(0))
          case Variant.shutdown   => Message.Shutdown

          case Variant.signal =>
            Message.Signal(text(0), optionalInt(1), optionalInt(2), optionalLong(3))

          case _                  => abort(Launcher.Mismatch())

      case _ => abort(Launcher.Mismatch())

  // §11: the daemon reads documents from an untrusted peer, so a declared length is bounded
  // before it is acted on. One mebibyte, as the launcher bounds it: a chunk is at most 64 KiB,
  // and an invocation's environment and arguments fit comfortably.
  val maximumLength: Int = 1024*1024

  // The most bytes one `data` document carries.
  val maximumChunk: Int = 65536

  // Reads exactly one framed document from `in` — the magic number, the length varint and
  // then the declared number of bytes — leaving whatever follows unread. `Unset` at end of
  // input or on a malformed header.
  def readDocument(in: ji.InputStream): Optional[Data] =
    val header = ji.ByteArrayOutputStream(16)

    def readByte(): Int =
      val byte = in.read()
      if byte >= 0 then header.write(byte)
      byte

    var magic = 0
    while magic < 4 && readByte() >= 0 do magic += 1

    if magic < 4 then Unset else
      var declared = 0L
      var shift = 0
      var done = false
      var ok = true

      while ok && !done && shift <= 63 do
        val byte = readByte()
        if byte < 0 then ok = false
        else
          declared |= (byte & 0x7fL) << shift
          shift += 7
          if (byte & 0x80) == 0 then done = true

      if !ok || !done || declared > maximumLength.toLong then Unset
      else
        in.readNBytes(declared.toInt) match
          case null => Unset
          case body: scala.Array[Byte] =>
            if body.length < declared.toInt then Unset else
              val prefix: scala.Array[Byte] = header.toByteArray.nn
              val whole: scala.Array[Byte] = new scala.Array[Byte](prefix.length + body.length)
              jl.System.arraycopy(prefix, 0, whole, 0, prefix.length)
              jl.System.arraycopy(body, 0, whole, prefix.length, body.length)
              Array.unsafeFrozen(whole)

  // Raised internally to turn any structural surprise into `Unset`.
  private case class Mismatch()(using Diagnostics)
  extends fulminate.Error(m"the document is not a launcher message")

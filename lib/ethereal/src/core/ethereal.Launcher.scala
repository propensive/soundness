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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import prepositional.*
import rudiments.*
import spectacular.*
import stratiform.*
import turbulence.*
import vacuous.*

// The contract between an Ethereal launcher (the Rust runner published by `propensive/xeq`,
// in its `src/runner`) and its daemon.
// Every connection the launcher opens begins with exactly one BinTEL document — a `Message`
// typed by the `schema` below — and the daemon answers, where the message calls for an
// answer, with one or more BinTEL documents of the same schema. After the `init` message the
// connection becomes a raw byte pipe (stdin one way, stdout the other), and after `stderr`
// and `control` it carries raw stderr bytes and a stream of `mode` documents respectively.
//
// The schema is the specification: the runner's `bintel.rs` encodes and decodes exactly its
// keyword order, and carries the schema's 33-byte signature as a constant, so a launcher and
// a daemon built against different schemas refuse each other at the first document rather
// than misreading fields. The TEL text is the source of truth; the enum mirrors it member for
// member, and the tests pin the signature and the wire bytes of a sample of messages against
// the values the runner's unit tests pin.
object Launcher:
  val schemaText: Text = Text("""|name ethereal-launcher
                            |
                            |document
                            |  select Message required
                            |
                            |select Message
                            |  variant init Init
                            |  variant stderr Stderr
                            |  variant control Control
                            |  variant signal Signal
                            |  variant exit Exit
                            |  variant verify Verify
                            |  variant signal-ack SignalAck
                            |  variant verdict Verdict
                            |  variant mode Mode
                            |  variant exit-status ExitStatus
                            |  variant closed Closed
                            |  variant shutdown Shutdown
                            |
                            |record Init
                            |  description
                            |      A new invocation: the connection then carries the client's
                            |      stdin to the daemon and the daemon's stdout to the client.
                            |      The three tty flags say which of the client's streams are
                            |      attached to a terminal; the daemon sees only sockets and
                            |      cannot determine this for itself. The uid is the platform's
                            |      identifier for the user: numeric on Unix, a SID on Windows.
                            |      The invoked-as field is argv[0] as the caller supplied it,
                            |      for a multi-call binary to dispatch on; script is the
                            |      canonical path. The umask is octal; columns and rows are the
                            |      terminal's size when stdout is a terminal; the code pages are
                            |      the Windows console's input and output code pages.
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
                            |
                            |record Stderr
                            |  description
                            |      The connection on which the invocation's stderr is delivered.
                            |  field pid String required
                            |
                            |record Control
                            |  description
                            |      The connection on which the daemon sends mode documents.
                            |  field pid String required
                            |
                            |record Signal
                            |  description
                            |      A signal the client received, named without its SIG prefix,
                            |      or a Windows console control event. WINCH and CONT carry the
                            |      terminal's current size; a Windows close, logoff or shutdown
                            |      carries the milliseconds the system allows before it ends
                            |      the client regardless.
                            |  field pid String required
                            |  field name String required
                            |  field columns String optional
                            |  field rows String optional
                            |  field deadline String optional
                            |
                            |record Exit
                            |  description
                            |      A request for the invocation's exit status, sent after its
                            |      streams have drained; answered with an exit-status document.
                            |  field pid String required
                            |
                            |record Verify
                            |  description
                            |      Asks whether the launcher file the daemon started from still
                            |      has the content it remembers; answered with a verdict.
                            |  field launcher String optional
                            |
                            |record SignalAck
                            |  description
                            |      Whether the invocation accepted a forwarded signal.
                            |  field accept Flag optional
                            |
                            |record Verdict
                            |  field fresh Flag optional
                            |
                            |record Mode
                            |  description
                            |      Asks the launcher to put the client's terminal into canonical
                            |      (cooked) mode, or back into raw mode when the flag is absent.
                            |  field canonical Flag optional
                            |
                            |record ExitStatus
                            |  field code String required
                            |
                            |record Closed
                            |  description
                            |      The named output stream of the invocation, stdout or stderr,
                            |      has lost its reader: the client could not write to it. Sent
                            |      once, on its own connection, and not answered. The daemon
                            |      should fail the invocation's further writes to that stream,
                            |      as a broken pipe would.
                            |  field pid String required
                            |  field stream String required
                            |
                            |record Shutdown
                            |  description
                            |      Asks the daemon to exit: to accept no further invocations, to
                            |      let those in flight finish, and then to end. Not answered; the
                            |      connection is closed. A launcher whose daemon is gone starts a
                            |      fresh one, so this reclaims a warm JVM without leaving anything
                            |      broken.
                            |""".stripMargin)

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
        outputCodepage: Optional[Int]  = Unset )

    case Stderr(pid: Int)
    case Control(pid: Int)

    case Signal
      ( pid:      Int,
        name:     Text,
        columns:  Optional[Int]  = Unset,
        rows:     Optional[Int]  = Unset,
        deadline: Optional[Long] = Unset )

    case Exit(pid: Int)
    case Verify
    case SignalAck(accept: Boolean)
    case Verdict(fresh: Boolean)
    case Mode(canonical: Boolean)
    case ExitStatus(code: Int)
    case Closed(pid: Int, stream: Text)
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
  // `SelectRef`, so its variants occupy indices 0 to 11 in declaration order.
  private object Variant:
    val init = 0; val stderr = 1; val control = 2; val signal = 3; val exit = 4
    val verify = 5; val signalAck = 6; val verdict = 7; val mode = 8; val exitStatus = 9
    val closed = 10; val shutdown = 11

  private val scalar: Tels.Scalar = Tels.Scalar(Array.empty)

  private def record(name: Text): Tels.Struct =
    val definition = schema.records.seek(_.name == name).or(panic(m"the schema declares $name"))
    Tels.Struct(definition.members, definition.validators)

  private def value(index: Int, text: Text): Tel.Element = Tel.Element.Value(index, scalar, text)
  private def flag(index: Int): Tel.Element = Tel.Element.Node(index, Tels.Flag, Array.empty)

  private def node(variant: Int, name: Text, children: Array[Tel.Element]^{}): Tel.Element =
    Tel.Element.Node
      ( Unset, schema.document, Array(Tel.Element.Node(variant, record(name), children)) )

  private def element(message: Message): Tel.Element = message match
    case Message.Init(pid, uid, username, script, pwd, stdinTty, stdoutTty, stderrTty,
                      arguments, environment, invokedAs, umask, columns, rows, inputCodepage,
                      outputCodepage) =>
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
      node(Variant.init, t"Init", Array.from(children))

    case Message.Stderr(pid)       => node(Variant.stderr, t"Stderr", Array(value(0, pid.show)))
    case Message.Control(pid)      => node(Variant.control, t"Control", Array(value(0, pid.show)))
    case Message.Exit(pid)         => node(Variant.exit, t"Exit", Array(value(0, pid.show)))
    case Message.Verify            => node(Variant.verify, t"Verify", Array.empty)
    case Message.ExitStatus(code)  => node(Variant.exitStatus, t"ExitStatus", Array(value(0, code.show)))
    case Message.Shutdown          => node(Variant.shutdown, t"Shutdown", Array.empty)

    case Message.Closed(pid, stream) =>
      node(Variant.closed, t"Closed", Array(value(0, pid.show), value(1, stream)))

    case Message.Signal(pid, name, columns, rows, deadline) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, pid.show)
      children += value(1, name)
      columns.let { count => children += value(2, count.show) }
      rows.let { count => children += value(3, count.show) }
      deadline.let { millis => children += value(4, millis.show) }
      node(Variant.signal, t"Signal", Array.from(children))

    case Message.SignalAck(accept) =>
      node(Variant.signalAck, t"SignalAck", if accept then Array(flag(0)) else Array.empty)

    case Message.Verdict(fresh) =>
      node(Variant.verdict, t"Verdict", if fresh then Array(flag(0)) else Array.empty)

    case Message.Mode(canonical) =>
      node(Variant.mode, t"Mode", if canonical then Array(flag(0)) else Array.empty)

  // A message as one framed BinTEL document (§6.1): magic, length, signature, body.
  def encode(message: Message): Data =
    import strategies.throwUnsafely
    Bintel.frame(Bintel.encode(element(message), schema), signature)

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
  // another schema's signature, or does not fit the enum.
  def decode(data: Data): Optional[Message] = safely:
    val document = Bintel.decodeDocument(data, schema)
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
        def optionalInt(field: Int): Optional[Int] = optional(field).let(_.as[Int])
        def optionalLong(field: Int): Optional[Long] = optional(field).let(_.as[Long])

        index.or(-1) match
          case Variant.init =>
            Message.Init
              ( int(0), text(1), text(2), text(3), text(4), flag(5), flag(6), flag(7),
                texts(8), texts(9), optional(10), optional(11), optionalInt(12), optionalInt(13),
                optionalInt(14), optionalInt(15) )

          case Variant.stderr     => Message.Stderr(int(0))
          case Variant.control    => Message.Control(int(0))
          case Variant.exit       => Message.Exit(int(0))
          case Variant.verify     => Message.Verify
          case Variant.signalAck  => Message.SignalAck(flag(0))
          case Variant.verdict    => Message.Verdict(flag(0))
          case Variant.mode       => Message.Mode(flag(0))
          case Variant.exitStatus => Message.ExitStatus(int(0))
          case Variant.closed     => Message.Closed(int(0), text(1))
          case Variant.shutdown   => Message.Shutdown

          case Variant.signal =>
            Message.Signal(int(0), text(1), optionalInt(2), optionalInt(3), optionalLong(4))

          case _                  => abort(Launcher.Mismatch())

      case _ => abort(Launcher.Mismatch())

  // §11: the daemon reads documents from an untrusted peer, so a declared length is bounded
  // before it is acted on. An invocation's environment and arguments fit comfortably.
  val maximumLength: Int = 16*1024*1024

  // Reads exactly one framed document from `in` — the magic number, the length varint and
  // then the declared number of bytes — leaving whatever follows unread, since after an
  // `init` document the same stream carries the client's stdin. `Unset` at end of input or
  // on a malformed header.
  def readDocument(in: ji.InputStream): Optional[Data] =
    val buffer = scala.collection.mutable.ArrayBuffer.empty[Byte]

    def readByte(): Int =
      val byte = in.read()
      if byte >= 0 then buffer += byte.toByte
      byte

    def fully(count: Int): Boolean =
      var remaining = count
      var ok = true

      while ok && remaining > 0 do
        val byte = readByte()
        if byte < 0 then ok = false else remaining -= 1

      ok

    if !fully(4) then Unset else
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
      else if !fully(declared.toInt) then Unset
      else Array.from(buffer)

  // Raised internally to turn any structural surprise into `Unset`.
  private case class Mismatch()(using Diagnostics)
  extends fulminate.Error(m"the document is not a launcher message")

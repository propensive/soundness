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

// The contract between an Ethereal launcher (the Rust runner in `src/runner`) and its daemon.
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
                            |
                            |record Init
                            |  description
                            |      A new invocation: the connection then carries the client's
                            |      stdin to the daemon and the daemon's stdout to the client.
                            |  field pid String required
                            |  field uid String required
                            |  field username String required
                            |  field script String required
                            |  field pwd String required
                            |  field tty Flag optional
                            |  field argument String optional repeatable
                            |  field environment String optional repeatable
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
                            |      A signal the client received, named without its SIG prefix.
                            |  field pid String required
                            |  field name String required
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
                            |""".stripMargin)

  enum Message:
    case Init
      ( pid:         Int,
        uid:         Int,
        username:    Text,
        script:      Text,
        pwd:         Text,
        tty:         Boolean,
        arguments:   List[Text],
        environment: List[Text] )

    case Stderr(pid: Int)
    case Control(pid: Int)
    case Signal(pid: Int, name: Text)
    case Exit(pid: Int)
    case Verify
    case SignalAck(accept: Boolean)
    case Verdict(fresh: Boolean)
    case Mode(canonical: Boolean)
    case ExitStatus(code: Int)

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
  // `SelectRef`, so its variants occupy indices 0 to 9 in declaration order.
  private object Variant:
    val init = 0; val stderr = 1; val control = 2; val signal = 3; val exit = 4
    val verify = 5; val signalAck = 6; val verdict = 7; val mode = 8; val exitStatus = 9

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
    case Message.Init(pid, uid, username, script, pwd, tty, arguments, environment) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, pid.show)
      children += value(1, uid.show)
      children += value(2, username)
      children += value(3, script)
      children += value(4, pwd)
      if tty then children += flag(5)
      arguments.each { argument => children += value(6, argument) }
      environment.each { variable => children += value(7, variable) }
      node(Variant.init, t"Init", Array.from(children))

    case Message.Stderr(pid)       => node(Variant.stderr, t"Stderr", Array(value(0, pid.show)))
    case Message.Control(pid)      => node(Variant.control, t"Control", Array(value(0, pid.show)))
    case Message.Exit(pid)         => node(Variant.exit, t"Exit", Array(value(0, pid.show)))
    case Message.Verify            => node(Variant.verify, t"Verify", Array.empty)
    case Message.ExitStatus(code)  => node(Variant.exitStatus, t"ExitStatus", Array(value(0, code.show)))

    case Message.Signal(pid, name) =>
      node(Variant.signal, t"Signal", Array(value(0, pid.show), value(1, name)))

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
        def text(field: Int): Text = children.readable.collectFirst:
          case Tel.Element.Value(`field`, _, text) => text
        . getOrElse(abort(Launcher.Mismatch()))

        def texts(field: Int): List[Text] =
          children.readable.toList.collect { case Tel.Element.Value(`field`, _, text) => text }
          . to(List)

        def flag(field: Int): Boolean = children.readable.exists:
          case Tel.Element.Node(`field`, Tels.Flag, _) => true
          case _                                       => false

        def int(field: Int): Int = text(field).as[Int]

        index.or(-1) match
          case Variant.init =>
            Message.Init
              ( int(0), int(1), text(2), text(3), text(4), flag(5), texts(6), texts(7) )

          case Variant.stderr     => Message.Stderr(int(0))
          case Variant.control    => Message.Control(int(0))
          case Variant.signal     => Message.Signal(int(0), text(1))
          case Variant.exit       => Message.Exit(int(0))
          case Variant.verify     => Message.Verify
          case Variant.signalAck  => Message.SignalAck(flag(0))
          case Variant.verdict    => Message.Verdict(flag(0))
          case Variant.mode       => Message.Mode(flag(0))
          case Variant.exitStatus => Message.ExitStatus(int(0))
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

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
import hellenism.*
import hieroglyph.*, codepages.utf8Codepage
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
// than misreading fields. The TEL text is the source of truth — the resource
// `ethereal/ethereal-launcher.tel`, byte for byte xek's `spec/ethereal-launcher.tel` — and the
// enum mirrors it member for member; the tests pin the signature and the wire bytes of a sample
// of messages against the values the runner's unit tests pin.
object Launcher:
  // Read through ethereal's own classloader, since the resource is in its jar and the first
  // thread to need the schema may carry any context classloader.
  lazy val schemaText: Text =
    import strategies.throwUnsafely
    import charsets.utf8Charset
    import textSanitizers.strictSanitizer
    given classloader: Classloader = Classloader[Launcher.type]
    cp"/ethereal/ethereal-launcher.tel".read[Text]

  // A file descriptor the client holds, as `init` advertises it: its number, `r`/`w`/`rw`,
  // its kind (`file`, `pipe`, `tty`, `socket`, `other`) and, for a regular file, its real path.
  case class Descriptor(fd: Int, direction: Text, kind: Text, path: Optional[Text] = Unset)

  // A value of `init` as the operating system gave it, where its text form lost something:
  // `argument`, `environment` or `pwd`; the position among the arguments or entries, from 0,
  // or none for the working directory; and the platform's own bytes.
  case class Raw(kind: Text, index: Optional[Int], bytes: anticipation.Data)

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
        descriptors:    List[Descriptor] = Nil,
        raws:           List[Raw] = Nil )

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
    case Mode(canonical: Boolean, echo: Boolean = false)
    case Closed(stream: Text)
    case ExitStatus(code: Int)
    case Verify
    case Verdict(fresh: Boolean)
    case Shutdown
    case Run(command: Text, arguments: List[Text] = Nil, pwd: Optional[Text] = Unset)
    case Exited(code: Int)

  // Parsed once; a malformed schema text is a programming error, not a runtime condition.
  lazy val schema: Tels =
    import strategies.throwUnsafely
    Tels.Validation.validate(schemaText.read[Tel].as[Tels])

  // The §8 palimpsest signature of the schema, carried by every document on the wire and
  // compared byte-for-byte on receipt.
  lazy val signature: Data =
    import strategies.throwUnsafely
    SchemaSignature.fromDocument(schemaText.read[Tel], Tels.Axiom.tels)

  // The variant indices of `Message` in the document root's keyword order — a single
  // `SelectRef`, so its variants occupy indices 0 to 14 in declaration order.
  private object Variant:
    val init = 0; val data = 1; val end = 2; val credit = 3; val open = 4; val signal = 5
    val signalAck = 6; val mode = 7; val closed = 8; val exitStatus = 9; val verify = 10
    val verdict = 11; val shutdown = 12; val run = 13; val exited = 14

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
                      outputCodepage, descriptors, raws) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, pid.show)
      children += value(1, uid)
      children += value(2, username)
      children += value(3, script)
      children += value(4, pwd)
      if stdinTty then children += flag(5)
      if stdoutTty then children += flag(6)
      if stderrTty then children += flag(7)
      arguments.each: argument => children += value(8, argument)
      environment.each: variable => children += value(9, variable)
      invokedAs.let: name => children += value(10, name)
      umask.let: mask => children += value(11, mask)
      columns.let: count => children += value(12, count.show)
      rows.let: count => children += value(13, count.show)
      inputCodepage.let: page => children += value(14, page.show)
      outputCodepage.let: page => children += value(15, page.show)

      descriptors.each: descriptor =>
        val fields = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
        fields += value(0, descriptor.fd.show)
        fields += value(1, descriptor.direction)
        fields += value(2, descriptor.kind)

        descriptor.path match
          case path: Text => fields += value(3, path)
          case _          => ()

        children += Tel.Element.Node(16, record(t"Descriptor"), Array.from(fields))

      raws.each: raw =>
        val fields = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
        fields += value(0, raw.kind)

        raw.index match
          case index: Int => fields += value(1, index.show)
          case _          => ()

        fields += this.raw(2, Base256.encode(raw.bytes))
        children += Tel.Element.Node(17, record(t"Raw"), Array.from(fields))

      node(Variant.init, t"Init", Array.from(children))

    case Message.Data(name, data) =>
      node(Variant.data, t"Data", Array(value(0, name), raw(1, Base256.encode(data))))

    case Message.End(name)         => stream(Variant.end, t"End", name)
    case Message.Open(name)        => stream(Variant.open, t"Open", name)
    case Message.Closed(name)      => stream(Variant.closed, t"Closed", name)
    case Message.Verify            => node(Variant.verify, t"Verify", Array.empty)

    case Message.ExitStatus(code) =>
      node(Variant.exitStatus, t"ExitStatus", Array(value(0, code.show)))

    case Message.Shutdown          => node(Variant.shutdown, t"Shutdown", Array.empty)

    case Message.Credit(name, count) =>
      node(Variant.credit, t"Credit", Array(value(0, name), value(1, count.show)))

    case Message.Signal(name, columns, rows, deadline) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, name)
      columns.let: count => children += value(1, count.show)
      rows.let: count => children += value(2, count.show)
      deadline.let: millis => children += value(3, millis.show)
      node(Variant.signal, t"Signal", Array.from(children))

    case Message.SignalAck(accept) =>
      node(Variant.signalAck, t"SignalAck", if accept then Array(flag(0)) else Array.empty)

    case Message.Verdict(fresh) =>
      node(Variant.verdict, t"Verdict", if fresh then Array(flag(0)) else Array.empty)

    case Message.Mode(canonical, echo) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      if canonical then children += flag(0)
      if echo then children += flag(1)
      node(Variant.mode, t"Mode", Array.from(children))

    case Message.Exited(code) => node(Variant.exited, t"Exited", Array(value(0, code.show)))

    case Message.Run(command, arguments, pwd) =>
      val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      children += value(0, command)
      arguments.each: argument => children += value(1, argument)

      pwd match
        case pwd: Text => children += value(2, pwd)
        case _         => ()

      node(Variant.run, t"Run", Array.from(children))

  // A message as one framed BinTEL document (§6.1): magic, length, signature, body. A `data`
  // document is framed by hand: its bytes go straight into the frame, with no BASE-256 text
  // in between, since every chunk of every stream passes this way.
  def encode(message: Message): Data =
    import strategies.throwUnsafely

    message match
      case Message.Data(stream, chunk) => Bintel.frame(dataBody(stream, chunk), signature)

      case other =>
        Bintel.frame(Bintel.encode(element(other), schema, Tel.Codec.Bindings.builtins), signature)

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

        . optional

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

              . optional

              Descriptor
                ( field(0).or(abort(Launcher.Mismatch())).as[Int],
                  field(1).or(abort(Launcher.Mismatch())),
                  field(2).or(abort(Launcher.Mismatch())),
                  field(3) )

          . to(List)

        def raws: List[Raw] =
          children.readable.toList.collect:
            case Tel.Element.Node(17, _, fields) =>
              def field(index: Int): Optional[Text] = fields.readable.collectFirst:
                case Tel.Element.Value(`index`, _, text) => text

              . optional

              Raw
                ( field(0).or(abort(Launcher.Mismatch())),
                  field(1).let(_.as[Int]),
                  Base256.decodeStrict(field(2).or(abort(Launcher.Mismatch()))) )

          . to(List)

        index.or(-1) match
          case Variant.init =>
            Message.Init
              ( int(0), text(1), text(2), text(3), text(4), flag(5), flag(6), flag(7),
                texts(8), texts(9), optional(10), optional(11), optionalInt(12), optionalInt(13),
                optionalInt(14), optionalInt(15), descriptors, raws )

          case Variant.data       => Message.Data(text(0), Base256.decodeStrict(text(1)))
          case Variant.end        => Message.End(text(0))
          case Variant.credit     => Message.Credit(text(0), long(1))
          case Variant.open       => Message.Open(text(0))
          case Variant.signalAck  => Message.SignalAck(flag(0))
          case Variant.mode       => Message.Mode(flag(0), flag(1))
          case Variant.run        => Message.Run(text(0), texts(1), optional(2))
          case Variant.exited     => Message.Exited(int(0))
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

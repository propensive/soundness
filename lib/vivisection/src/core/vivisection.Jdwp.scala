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
package vivisection

import java.io as ji
import java.lang as jl
import java.nio.charset as jnc
import java.util.concurrent.atomic as juca

import scala.caps
import scala.collection.concurrent as scc

import anticipation.*
import coaxial.*
import contingency.*
import fulminate.*
import gossamer.*
import parasite.*
import proscenium.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

// A Scala-native model of the Java Debug Wire Protocol (JDWP). The names mirror the specification's
// so that this file can be read against it: command sets and commands keep their published numbers,
// event and error kinds keep their published names, and the identifier types keep the sizes the VM
// negotiates. Nothing here touches a socket — `Jdwp.Reader` and `Jdwp.Writer` marshal to and from a
// materialised `Data` payload, and `Jdwp.Connection` (elsewhere) frames and correlates packets.
object Jdwp:
  // The 14 bytes each end sends, and expects back, before any packet flows.
  val Handshake: Data =
    Array.unsafeFrozen("JDWP-Handshake".getBytes(jnc.StandardCharsets.US_ASCII).nn)

  // Every JDWP identifier is an integer that fits a `Long`, but the wire width of each *kind* of
  // identifier is negotiated per connection (`VirtualMachine.IDSizes`). `Ref` is one opaque
  // integer tagged by a phantom kind, so a `ThreadId` can never be passed where a `FieldId` is
  // wanted, while a `ThreadId` still widens to an `ObjectId` (a thread *is* an object in JDWP).
  opaque type Ref[+kind <: Ref.Kind] = Long

  object Ref:
    sealed trait Kind
    sealed trait Object extends Kind
    sealed trait Thread extends Object
    sealed trait Group extends Object
    sealed trait Str extends Object
    sealed trait Loader extends Object
    sealed trait Type extends Kind
    sealed trait Method extends Kind
    sealed trait Field extends Kind
    sealed trait Frame extends Kind

    def apply[kind <: Kind](long: Long): Ref[kind] = long

    // The JDWP null object: identifier zero. `absent` tests for it.
    def empty[kind <: Kind]: Ref[kind] = 0L

    given showable: [kind <: Kind] => Ref[kind] is Showable = ref => ref.long.show

    given equality: [kind <: Kind] => CanEqual[Ref[kind], Ref[kind]] = CanEqual.derived

  extension [kind <: Ref.Kind](ref: Ref[kind])
    def long: Long = ref
    def absent: Boolean = ref == 0L

  // The five negotiated identifier widths, in bytes (each 1..8). `objectId` also sizes strings,
  // threads, thread groups, class loaders, class objects and arrays; `referenceTypeId` sizes
  // class, interface and array-type identifiers.
  object IdSizes:
    // Used only to decode the fixed-layout IDSizes reply itself, which carries no identifiers.
    val bootstrap: IdSizes = IdSizes(8, 8, 8, 8, 8)

  case class IdSizes(field: Int, method: Int, objectId: Int, referenceType: Int, frame: Int)

  // A byte tag classifying a reference type as a class, interface or array.
  object TypeTag:
    def apply(id: Byte): TypeTag = id match
      case 1 => Class
      case 2 => Interface
      case 3 => Array
      case _ => Class

  enum TypeTag(val id: Byte):
    case Class     extends TypeTag(1)
    case Interface extends TypeTag(2)
    case Array     extends TypeTag(3)

  // How much of the VM to suspend when an event fires.
  object SuspendPolicy:
    def apply(id: Byte): SuspendPolicy = id match
      case 0 => None
      case 1 => EventThread
      case 2 => All
      case _ => None

  enum SuspendPolicy(val id: Byte):
    case None        extends SuspendPolicy(0)
    case EventThread extends SuspendPolicy(1)
    case All         extends SuspendPolicy(2)

  // The kinds of event a request may ask for, and that a composite event reports.
  object EventKind:
    def apply(id: Int): EventKind = id match
      case 1   => SingleStep
      case 2   => Breakpoint
      case 3   => FramePop
      case 4   => Exception
      case 5   => UserDefined
      case 6   => ThreadStart
      case 7   => ThreadDeath
      case 8   => ClassPrepare
      case 9   => ClassUnload
      case 10  => ClassLoad
      case 20  => FieldAccess
      case 21  => FieldModified
      case 40  => MethodEntry
      case 41  => MethodExit
      case 90  => VmStart
      case 99  => VmDeath
      case id0 => Other(id0)

  enum EventKind(val id: Int):
    case SingleStep    extends EventKind(1)
    case Breakpoint    extends EventKind(2)
    case FramePop      extends EventKind(3)
    case Exception     extends EventKind(4)
    case UserDefined   extends EventKind(5)
    case ThreadStart   extends EventKind(6)
    case ThreadDeath   extends EventKind(7)
    case ClassPrepare  extends EventKind(8)
    case ClassUnload   extends EventKind(9)
    case ClassLoad     extends EventKind(10)
    case FieldAccess   extends EventKind(20)
    case FieldModified extends EventKind(21)
    case MethodEntry   extends EventKind(40)
    case MethodExit    extends EventKind(41)
    case VmStart       extends EventKind(90)
    case VmDeath       extends EventKind(99)
    case Other(id0: Int) extends EventKind(id0)

  // The direction and grain of a single-step request.
  enum StepDepth(val id: Int):
    case Into extends StepDepth(0)
    case Over extends StepDepth(1)
    case Out  extends StepDepth(2)

  enum StepSize(val id: Int):
    case Min  extends StepSize(0)
    case Line extends StepSize(1)

  // A tag byte identifying the runtime type of a value on the wire.
  object Tag:
    def apply(id: Char): Tag = id match
      case '[' => ArrayTag
      case 'B' => ByteTag
      case 'C' => CharTag
      case 'L' => ObjectTag
      case 'F' => FloatTag
      case 'D' => DoubleTag
      case 'I' => IntTag
      case 'J' => LongTag
      case 'S' => ShortTag
      case 'V' => VoidTag
      case 'Z' => BooleanTag
      case 's' => StringTag
      case 't' => ThreadTag
      case 'g' => GroupTag
      case 'l' => LoaderTag
      case 'c' => ClassTag0
      case _   => ObjectTag

    // The tags whose wire payload is an objectID rather than an inline primitive.
    def isObject(tag: Tag): Boolean = tag match
      case ArrayTag | ObjectTag | StringTag | ThreadTag | GroupTag | LoaderTag | ClassTag0 => true
      case _                                                                               => false

  enum Tag(val id: Char):
    case ArrayTag   extends Tag('[')
    case ByteTag    extends Tag('B')
    case CharTag    extends Tag('C')
    case ObjectTag  extends Tag('L')
    case FloatTag   extends Tag('F')
    case DoubleTag  extends Tag('D')
    case IntTag     extends Tag('I')
    case LongTag    extends Tag('J')
    case ShortTag   extends Tag('S')
    case VoidTag    extends Tag('V')
    case BooleanTag extends Tag('Z')
    case StringTag  extends Tag('s')
    case ThreadTag  extends Tag('t')
    case GroupTag   extends Tag('g')
    case LoaderTag  extends Tag('l')
    case ClassTag0  extends Tag('c')

  // An executable location: a reference type, one of its methods, and a code index within it.
  case class Location(tag: TypeTag, cls: ReferenceTypeId, method: MethodId, index: Long)

  // A tagged value read from or written to a frame slot, field, or array. Primitives are decoded
  // eagerly; references stay as identifiers for the semantics layer to interpret.
  enum Value:
    case OfByte(byte: Byte)
    case OfChar(char: Char)
    case OfDouble(double: Double)
    case OfFloat(float: Float)
    case OfInt(int: Int)
    case OfLong(long: Long)
    case OfShort(short: Short)
    case OfBoolean(boolean: Boolean)
    case Reference(tag: Tag, id: ObjectId)
    case Void

  // The modifiers that constrain an event request. The trailing number is the JDWP modkind.
  enum Modifier(val kind: Byte):
    case Count(count: Int) extends Modifier(1)
    case ThreadOnly(thread: ThreadId) extends Modifier(3)
    case ClassOnly(cls: ReferenceTypeId) extends Modifier(4)
    case ClassMatch(pattern: Text) extends Modifier(5)
    case ClassExclude(pattern: Text) extends Modifier(6)
    case LocationOnly(location: Location) extends Modifier(7)
    case ExceptionOnly(cls: ReferenceTypeId, caught: Boolean, uncaught: Boolean) extends Modifier(8)
    case Step(thread: ThreadId, size: StepSize, depth: StepDepth) extends Modifier(10)
    case SourceNameMatch(pattern: Text) extends Modifier(12)

  // Selected reply structures, decoded by the command methods on `Jdwp.Connection`.
  case class Version(description: Text, major: Int, minor: Int, vmVersion: Text, vmName: Text)
  case class ClassInfo(tag: TypeTag, cls: ReferenceTypeId, signature: Text, status: Int)
  case class MethodInfo(method: MethodId, name: Text, signature: Text, modifiers: Int)
  case class LineEntry(index: Long, line: Int)
  case class LineTable(start: Long, end: Long, lines: List[LineEntry])
  case class SlotInfo(index: Long, name: Text, signature: Text, length: Int, slot: Int)
  case class VariableTable(argCount: Int, slots: List[SlotInfo])

  // The capabilities a VM advertises (`VirtualMachine.CapabilitiesNew`); only the flags this
  // library consults are named, the rest are carried positionally as needed.
  case class Capabilities(canGetSourceDebugExtension: Boolean, canUseSourceNameFilters: Boolean)

  // JDWP strings are JNI *modified* UTF-8: the null character is `C0 80`, and characters outside
  // the basic multilingual plane arrive as two three-byte sequences (a surrogate pair), which we
  // pass through as the two chars they encode. A strict UTF-8 decoder would reject both forms.
  private[vivisection] def decodeModifiedUtf8(bytes: Data, offset: Int, length: Int): Text =
    val builder = jl.StringBuilder()
    var index = offset
    val end = offset + length

    while index < end do
      val first = bytes.readUnchecked(index) & 0xff

      if first < 0x80 then
        builder.append(first.toChar)
        index += 1
      else if (first & 0xe0) == 0xc0 then
        val second = bytes.readUnchecked(index + 1) & 0xff
        builder.append((((first & 0x1f) << 6) | (second & 0x3f)).toChar)
        index += 2
      else
        val second = bytes.readUnchecked(index + 1) & 0xff
        val third = bytes.readUnchecked(index + 2) & 0xff
        builder.append((((first & 0x0f) << 12) | ((second & 0x3f) << 6) | (third & 0x3f)).toChar)
        index += 3

    builder.toString.nn.tt

  private[vivisection] def encodeModifiedUtf8(text: Text): scala.Array[Byte] =
    val string = text.s
    val buffer = ji.ByteArrayOutputStream()
    var index = 0

    while index < string.length do
      val char = string.charAt(index).toInt

      if char >= 0x01 && char <= 0x7f then buffer.write(char)
      else if char <= 0x7ff then
        buffer.write(0xc0 | (char >> 6))
        buffer.write(0x80 | (char & 0x3f))
      else
        buffer.write(0xe0 | (char >> 12))
        buffer.write(0x80 | ((char >> 6) & 0x3f))
        buffer.write(0x80 | (char & 0x3f))

      index += 1

    buffer.toByteArray.nn

  // Decodes a JDWP packet payload. Big-endian throughout; identifier reads consult the negotiated
  // `sizes`. Stateful and single-threaded — one reader decodes one reply or one event, in order.
  class Reader(data: Data, sizes: IdSizes):
    @scala.caps.unsafe.untrackedCaptures
    private var position: Int = 0

    def remaining: Int = data.length - position

    private def next(): Int =
      val byte = data.readUnchecked(position) & 0xff
      position += 1
      byte

    def byte(): Byte = next().toByte
    def boolean(): Boolean = next() != 0
    def short(): Short = ((next() << 8) | next()).toShort
    def char(): Char = ((next() << 8) | next()).toChar
    def int(): Int = (next() << 24) | (next() << 16) | (next() << 8) | next()
    def long(): Long = (int().toLong << 32) | (int().toLong & 0xffffffffL)
    def float(): Float = jl.Float.intBitsToFloat(int())
    def double(): Double = jl.Double.longBitsToDouble(long())

    def id(width: Int): Long =
      var accumulator = 0L
      var count = 0

      while count < width do
        accumulator = (accumulator << 8) | next().toLong
        count += 1

      accumulator

    def objectId(): ObjectId = Ref(id(sizes.objectId))
    def threadId(): ThreadId = Ref(id(sizes.objectId))
    def threadGroupId(): ThreadGroupId = Ref(id(sizes.objectId))
    def stringId(): StringId = Ref(id(sizes.objectId))
    def classLoaderId(): ClassLoaderId = Ref(id(sizes.objectId))
    def referenceTypeId(): ReferenceTypeId = Ref(id(sizes.referenceType))
    def methodId(): MethodId = Ref(id(sizes.method))
    def fieldId(): FieldId = Ref(id(sizes.field))
    def frameId(): FrameId = Ref(id(sizes.frame))

    def string(): Text =
      val length = int()
      val text = Jdwp.decodeModifiedUtf8(data, position, length)
      position += length
      text

    def location(): Location =
      Location(TypeTag(byte()), referenceTypeId(), methodId(), long())

    def value(): Value = untaggedValue(Tag(next().toChar))

    def untaggedValue(tag: Tag): Value = tag match
      case Tag.ByteTag    => Value.OfByte(byte())
      case Tag.CharTag    => Value.OfChar(char())
      case Tag.DoubleTag  => Value.OfDouble(double())
      case Tag.FloatTag   => Value.OfFloat(float())
      case Tag.IntTag     => Value.OfInt(int())
      case Tag.LongTag    => Value.OfLong(long())
      case Tag.ShortTag   => Value.OfShort(short())
      case Tag.BooleanTag => Value.OfBoolean(boolean())
      case Tag.VoidTag    => Value.Void
      case reference      => Value.Reference(reference, objectId())

  // Marshals a JDWP packet payload. Fluent: each write returns the writer. `data` snapshots the
  // accumulated bytes.
  class Writer(sizes: IdSizes):
    private val out = ji.ByteArrayOutputStream()

    def byte(value: Byte): Writer = { out.write(value.toInt & 0xff); this }
    def boolean(value: Boolean): Writer = byte(if value then 1 else 0)

    def short(value: Short): Writer =
      out.write((value >> 8) & 0xff)
      out.write(value & 0xff)
      this

    def char(value: Char): Writer =
      out.write((value >> 8) & 0xff)
      out.write(value & 0xff)
      this

    def int(value: Int): Writer =
      out.write((value >> 24) & 0xff)
      out.write((value >> 16) & 0xff)
      out.write((value >> 8) & 0xff)
      out.write(value & 0xff)
      this

    def long(value: Long): Writer =
      int((value >>> 32).toInt)
      int(value.toInt)

    def float(value: Float): Writer = int(jl.Float.floatToIntBits(value))
    def double(value: Double): Writer = long(jl.Double.doubleToLongBits(value))

    def id(width: Int, value: Long): Writer =
      var shift = (width - 1)*8

      while shift >= 0 do
        out.write(((value >>> shift) & 0xff).toInt)
        shift -= 8

      this

    def objectId(ref: ObjectId): Writer = id(sizes.objectId, ref.long)
    def threadId(ref: ThreadId): Writer = id(sizes.objectId, ref.long)
    def threadGroupId(ref: ThreadGroupId): Writer = id(sizes.objectId, ref.long)
    def stringId(ref: StringId): Writer = id(sizes.objectId, ref.long)
    def classLoaderId(ref: ClassLoaderId): Writer = id(sizes.objectId, ref.long)
    def referenceTypeId(ref: ReferenceTypeId): Writer = id(sizes.referenceType, ref.long)
    def methodId(ref: MethodId): Writer = id(sizes.method, ref.long)
    def fieldId(ref: FieldId): Writer = id(sizes.field, ref.long)
    def frameId(ref: FrameId): Writer = id(sizes.frame, ref.long)

    def string(text: Text): Writer =
      val encoded = Jdwp.encodeModifiedUtf8(text)
      int(encoded.length)
      out.write(encoded)
      this

    def location(location: Location): Writer =
      byte(location.tag.id)
      referenceTypeId(location.cls)
      methodId(location.method)
      long(location.index)

    def value(value: Value): Writer = value match
      case Value.OfByte(byte0)       => byte(Tag.ByteTag.id.toByte).byte(byte0)
      case Value.OfChar(char0)       => byte(Tag.CharTag.id.toByte).char(char0)
      case Value.OfDouble(double0)   => byte(Tag.DoubleTag.id.toByte).double(double0)
      case Value.OfFloat(float0)     => byte(Tag.FloatTag.id.toByte).float(float0)
      case Value.OfInt(int0)         => byte(Tag.IntTag.id.toByte).int(int0)
      case Value.OfLong(long0)       => byte(Tag.LongTag.id.toByte).long(long0)
      case Value.OfShort(short0)     => byte(Tag.ShortTag.id.toByte).short(short0)
      case Value.OfBoolean(boolean0) => byte(Tag.BooleanTag.id.toByte).boolean(boolean0)
      case Value.Void                => byte(Tag.VoidTag.id.toByte)
      case Value.Reference(tag, id0) => byte(tag.id.toByte).objectId(id0)

    def modifier(modifier: Modifier): Writer =
      byte(modifier.kind)

      modifier match
        case Modifier.Count(count)          => int(count)
        case Modifier.ThreadOnly(thread)    => threadId(thread)
        case Modifier.ClassOnly(cls)        => referenceTypeId(cls)
        case Modifier.ClassMatch(pattern)   => string(pattern)
        case Modifier.ClassExclude(pattern) => string(pattern)
        case Modifier.LocationOnly(loc)     => location(loc)
        case Modifier.SourceNameMatch(pat)  => string(pat)

        case Modifier.ExceptionOnly(cls, caught, uncaught) =>
          referenceTypeId(cls).boolean(caught).boolean(uncaught)

        case Modifier.Step(thread, size, depth) =>
          threadId(thread).int(size.id).int(depth.id)

    def bytes(data: Data): Writer =
      out.write(Array.unsafeJvm(data))
      this

    def data: Data = Array.unsafeFrozen(out.toByteArray.nn)

  // The events a suspended (or running) VM sends back, decoded from a Composite command. Only the
  // kinds this debugger requests are modelled in full; an unrecognized kind terminates decoding of
  // the enclosing composite, since its payload length is unknown and the stream cannot be resynced
  // within the packet (the next packet is unaffected, being length-framed).
  object Event:
    case class Composite(policy: SuspendPolicy, events: List[Event])

    def composite(reader: Reader): Composite =
      val policy = SuspendPolicy(reader.byte())
      val count = reader.int()

      // Builds the list in order; an unrecognized event kind ends the list, since its unknown
      // payload length prevents locating the next event within this composite.
      def decode(remaining: Int): List[Event] =
        if remaining <= 0 then Nil else
          val event = one(reader)

          event match
            case Unknown(_, _) => event :: Nil
            case _             => event :: decode(remaining - 1)

      Composite(policy, decode(count))

    private def one(reader: Reader): Event =
      val kind = EventKind(reader.byte() & 0xff)
      val request = reader.int()

      kind match
        case EventKind.VmStart      => VmStart(request, reader.threadId())
        case EventKind.VmDeath      => VmDeath(request)
        case EventKind.ThreadStart  => ThreadStart(request, reader.threadId())
        case EventKind.ThreadDeath  => ThreadDeath(request, reader.threadId())
        case EventKind.Breakpoint   => Breakpoint(request, reader.threadId(), reader.location())
        case EventKind.SingleStep   => SingleStep(request, reader.threadId(), reader.location())
        case EventKind.MethodEntry  => MethodEntry(request, reader.threadId(), reader.location())
        case EventKind.MethodExit   => MethodExit(request, reader.threadId(), reader.location())

        case EventKind.Exception =>
          val thread = reader.threadId()
          val location = reader.location()
          val exception = reader.objectId()
          val catchLocation = reader.location()
          Thrown(request, thread, location, exception, catchLocation)

        case EventKind.ClassPrepare =>
          val thread = reader.threadId()
          val tag = TypeTag(reader.byte())
          val cls = reader.referenceTypeId()
          val signature = reader.string()
          val status = reader.int()
          ClassPrepared(request, thread, tag, cls, signature, status)

        case other => Unknown(other.id, request)

  enum Event:
    case VmStart(request: Int, thread: ThreadId)
    case VmDeath(request: Int)
    case ThreadStart(request: Int, thread: ThreadId)
    case ThreadDeath(request: Int, thread: ThreadId)
    case Breakpoint(request: Int, thread: ThreadId, location: Location)
    case SingleStep(request: Int, thread: ThreadId, location: Location)
    case MethodEntry(request: Int, thread: ThreadId, location: Location)
    case MethodExit(request: Int, thread: ThreadId, location: Location)

    case Thrown(request: Int, thread: ThreadId, location: Location, exception: ObjectId,
        catchLocation: Location)

    case ClassPrepared(request: Int, thread: ThreadId, tag: TypeTag, cls: ReferenceTypeId,
        signature: Text, status: Int)

    case Unknown(kind: Int, request: Int)

  // A complete JDWP packet. The 4-byte length prefix counts the whole packet, header included; the
  // `flags` high bit marks a reply, which carries a 2-byte error code where a command carries a
  // 1-byte command set and a 1-byte command.
  object Packet:
    val headerLength: Int = 11

    private def payload(bytes: Data): Data =
      val length = bytes.length - headerLength
      val array = Array.scratch[Byte](length)
      var index = 0

      while index < length do
        array(index) = bytes.readUnchecked(headerLength + index)
        index += 1

      Array.unsafeFrozen(array)

    // Frames an outgoing command packet.
    def command(id: Int, set: Int, command: Int, body: Data): Data =
      val writer = Writer(IdSizes.bootstrap)
      writer.int(headerLength + body.length)
      writer.int(id)
      writer.byte(0)
      writer.byte(set.toByte)
      writer.byte(command.toByte)
      writer.bytes(body)
      writer.data

    // Frames a reply packet (used by the test harness's fake VM). A non-zero `code` is an error.
    def reply(id: Int, code: Int, body: Data): Data =
      val writer = Writer(IdSizes.bootstrap)
      writer.int(headerLength + body.length)
      writer.int(id)
      writer.byte(0x80.toByte)
      writer.short(code.toShort)
      writer.bytes(body)
      writer.data

    // Decodes one complete packet (exactly its own `length` bytes).
    def decode(bytes: Data): Packet =
      val reader = Reader(bytes, IdSizes.bootstrap)
      reader.int()
      val id = reader.int()
      val flags = reader.byte() & 0xff
      val body = payload(bytes)

      if (flags & 0x80) != 0 then Packet(id, flags, reader.short() & 0xffff, 0, 0, body)
      else Packet(id, flags, 0, reader.byte() & 0xff, reader.byte() & 0xff, body)

  case class Packet(id: Int, flags: Int, code: Int, commandSet: Int, command: Int, body: Data):
    def reply: Boolean = (flags & 0x80) != 0

  // A live JDWP connection: the wire-level client that frames and correlates packets over a duplex
  // byte channel. Not itself the debug session capability — `Debug` owns one of these — but sealed
  // (`ExclusiveCapability`) so it cannot be shared past the session it serves. The reader task only
  // fulfils promises and enqueues composites; command handlers run elsewhere, so a handler issuing
  // a further command never blocks the reader that must deliver its reply.
  object Connection:
    // The outcome of a command, as a dedicated type rather than an `Either[Int, Data]`: a `Left`
    // with `Nothing` in the payload slot flows into the mutable-classified `Data` position and
    // crashes the fork's read-only capture adaptation, so the reply channel avoids it entirely.
    enum Reply:
      case Ok(data: Data)
      case Failed(code: Int)

    // Reads a big-endian 32-bit integer from a raw byte array at the given offset.
    private def int32(bytes: scala.Array[Byte], offset: Int): Int =
      ((bytes(offset) & 0xff) << 24) | ((bytes(offset + 1) & 0xff) << 16) |
        ((bytes(offset + 2) & 0xff) << 8) | (bytes(offset + 3) & 0xff)

    // Opens a connection over an already-connected duplex: exchanges the handshake, starts the
    // writer and reader pumps, negotiates identifier sizes, lends the connection, and tears
    // everything down afterwards. Modelled on `exegesis.LspSessional.exchange`.
    private[vivisection] def exchange[result](duplex: Duplex)(lambda: Connection => result)
      ( using monitor: Monitor, probate: Probate, diagnostics: Diagnostics )
      ( using Tactic[Debugger.Error] )
    :   result =

      duplex.send(Stream(Jdwp.Handshake))

      // Sealed: the connection captures this session's monitor and diagnostics, which an honest
      // `Connection^` would hide from the pumps that serve it. It is a local of this method, lent
      // to `lambda` and dead once `lambda` returns.
      val connection: Connection = caps.unsafe.unsafeAssumePure(Connection(monitor, diagnostics))

      // A single writer drains outgoing packets so writes never interleave.
      val writer: Task[Unit] = async:
        connection.outgoing.lazyList.stdlib.foreach: packet => duplex.send(Stream(packet))

      // The reader owns the read side: it is minted and consumed here, which also keeps the caller
      // thread off the channel's first (blocking) refill before the writer has started.
      val reader: Task[Unit] = async:
        val source = duplex.source
        val accumulator = ji.ByteArrayOutputStream()
        var handshaken = false

        source.toProgression.stdlib.iterator.foreach: chunk =>
          accumulator.write(Array.unsafeJvm(chunk))
          val bytes = accumulator.toByteArray.nn
          var offset = 0
          var advancing = true

          while advancing do
            advancing = false

            if !handshaken then
              if bytes.length - offset >= Jdwp.Handshake.length then
                offset += Jdwp.Handshake.length
                handshaken = true
                advancing = true
            else if bytes.length - offset >= Packet.headerLength then
              val length = int32(bytes, offset)

              if length >= Packet.headerLength && bytes.length - offset >= length then
                val packet = Array.scratch[Byte](length)
                java.lang.System.arraycopy(bytes, offset, packet, 0, length)
                connection.dispatch(Packet.decode(Array.unsafeFrozen(packet)))
                offset += length
                advancing = true

          accumulator.reset()
          accumulator.write(bytes, offset, bytes.length - offset)

        connection.disconnect()

      try
        connection.negotiate()
        lambda(connection)
      finally
        reader.cancel()
        writer.cancel()

  class Connection private[vivisection] (monitor: Monitor, note: Diagnostics)
  extends caps.ExclusiveCapability:
    private val counter: juca.AtomicInteger = juca.AtomicInteger(0)
    private val pending: scc.TrieMap[Int, Promise[Connection.Reply]] = scc.TrieMap()
    private[vivisection] val outgoing: Relay[Data] = Relay()
    private[vivisection] val composites: Relay[Event.Composite] = Relay()

    @scala.caps.unsafe.untrackedCaptures
    private var sizes0: IdSizes = IdSizes.bootstrap

    def sizes: IdSizes = sizes0

    // Routes a decoded packet: a reply fulfils its pending promise (with the error code, or the
    // payload); a Composite command (command set 64, command 100) is enqueued for the dispatcher.
    private[vivisection] def dispatch(packet: Packet): Unit =
      if packet.reply then pending.remove(packet.id).foreach: promise =>
        promise.offer:
          if packet.code != 0 then Connection.Reply.Failed(packet.code)
          else Connection.Reply.Ok(packet.body)
      else if packet.commandSet == 64 && packet.command == 100 then
        composites.put(Event.composite(Reader(packet.body, sizes0)))

    // Fails every in-flight request and ends the event stream when the channel closes.
    private[vivisection] def disconnect(): Unit =
      pending.values.foreach(_.offer(Connection.Reply.Failed(-1)))
      pending.clear()
      composites.stop()

    // The one seam every command goes through: allocate an id, frame the request, await the reply,
    // and hand back a reader over its payload (raising the VM's error code on failure).
    def request(set: Int, command: Int)(write: Writer => Unit)
      ( using Tactic[Debugger.Error] )
    :   Reader =

      val id = counter.getAndIncrement()
      val writer = Writer(sizes0)
      write(writer)
      val promise = Promise[Connection.Reply]()
      pending(id) = promise
      outgoing.put(Packet.command(id, set, command, writer.data))

      given Diagnostics = note

      // The session's monitor is passed to `await` explicitly rather than as a `given`, which would
      // hide `this`. A cancelled or interrupted await is reported as a lost connection (code −1); an
      // error is raised recoverably, then an empty reader returned, so no `Nothing`-typed expression
      // reaches the reply position.
      safely(promise.await()(using monitor)).or(Connection.Reply.Failed(-1)) match
        case Connection.Reply.Ok(data) =>
          Reader(data, sizes0)

        case Connection.Reply.Failed(code) =>
          raise(Debugger.Error(Debugger.Error.Reason(code), t"command ($set, $command)"))
          Reader(Array.empty[Byte], sizes0)

    // VirtualMachine (command set 1): negotiate the identifier sizes the rest of the session needs.
    private[vivisection] def negotiate()(using Tactic[Debugger.Error]): Unit =
      val reader = request(1, 7)(_ => ())
      sizes0 = IdSizes(reader.int(), reader.int(), reader.int(), reader.int(), reader.int())

    // Reads `count` items in sequence. A helper rather than `map` over a range, because the reads
    // are ordered side effects and must run strictly left to right.
    private def list[element](count: Int)(read: () => element): List[element] =
      def recur(remaining: Int): List[element] =
        if remaining <= 0 then Nil else
          val head = read()
          head :: recur(remaining - 1)

      recur(count)

    // Issues a command whose reply carries no data of interest, discarding the reader.
    private def command(set: Int, cmd: Int)(write: Writer => Unit)
      ( using Tactic[Debugger.Error] )
    :   Unit =

      request(set, cmd)(write)
      ()

    // VirtualMachine (command set 1).
    def version()(using Tactic[Debugger.Error]): Version =
      val reader = request(1, 1)(_ => ())
      Version(reader.string(), reader.int(), reader.int(), reader.string(), reader.string())

    def allThreads()(using Tactic[Debugger.Error]): List[ThreadId] =
      val reader = request(1, 4)(_ => ())
      list(reader.int()): () => reader.threadId()

    def suspendAll()(using Tactic[Debugger.Error]): Unit = command(1, 8)(_ => ())
    def resumeAll()(using Tactic[Debugger.Error]): Unit = command(1, 9)(_ => ())
    def dispose()(using Tactic[Debugger.Error]): Unit = command(1, 6)(_ => ())

    // ReferenceType (command set 2).
    def signature(cls: ReferenceTypeId)(using Tactic[Debugger.Error]): Text =
      request(2, 1)(_.referenceTypeId(cls)).string()

    def sourceFile(cls: ReferenceTypeId)(using Tactic[Debugger.Error]): Text =
      request(2, 7)(_.referenceTypeId(cls)).string()

    def sourceDebugExtension(cls: ReferenceTypeId)
      ( using Tactic[Debugger.Error] )
    :   Text =

      request(2, 12)(_.referenceTypeId(cls)).string()

    def methods(cls: ReferenceTypeId)(using Tactic[Debugger.Error]): List[MethodInfo] =
      val reader = request(2, 5)(_.referenceTypeId(cls))

      list(reader.int()): () =>
        MethodInfo(reader.methodId(), reader.string(), reader.string(), reader.int())

    // Method (command set 6).
    def lineTable(cls: ReferenceTypeId, method: MethodId)
      ( using Tactic[Debugger.Error] )
    :   LineTable =

      val reader = request(6, 1)(_.referenceTypeId(cls).methodId(method))
      val start = reader.long()
      val end = reader.long()
      val lines = list(reader.int()): () => LineEntry(reader.long(), reader.int())

      LineTable(start, end, lines)

    // ThreadReference (command set 11).
    def threadName(thread: ThreadId)(using Tactic[Debugger.Error]): Text =
      request(11, 1)(_.threadId(thread)).string()

    def suspendThread(thread: ThreadId)(using Tactic[Debugger.Error]): Unit =
      command(11, 2)(_.threadId(thread))

    def resumeThread(thread: ThreadId)(using Tactic[Debugger.Error]): Unit =
      command(11, 3)(_.threadId(thread))

    def frameCount(thread: ThreadId)(using Tactic[Debugger.Error]): Int =
      request(11, 7)(_.threadId(thread)).int()

    def frames(thread: ThreadId, start: Int, length: Int)
      ( using Tactic[Debugger.Error] )
    :   List[(FrameId, Location)] =

      val reader = request(11, 6): writer => writer.threadId(thread).int(start).int(length)
      list(reader.int()): () => (reader.frameId(), reader.location())

    // EventRequest (command set 15). `set` returns the request id used to `clear` it later.
    def eventRequestSet(kind: EventKind, policy: SuspendPolicy, modifiers: List[Modifier])
      ( using Tactic[Debugger.Error] )
    :   Int =

      request(15, 1): writer =>
        writer.byte(kind.id.toByte).byte(policy.id).int(modifiers.stdlib.length)
        modifiers.stdlib.foreach(writer.modifier)

      . int()

    def eventRequestClear(kind: EventKind, request0: Int)
      ( using Tactic[Debugger.Error] )
    :   Unit =

      command(15, 2)(_.byte(kind.id.toByte).int(request0))

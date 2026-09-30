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
package zephyrine

import scala.caps

import java.util.concurrent as juc

import anticipation.Data
import anticipation.*
import denominative.*
import prepositional.*
import vacuous.*

// A sink into which a value of one medium is written in pieces. The same producing code can be
// driven two ways without duplicating it: `Producer.collect` runs it synchronously and returns the
// whole result, while `Producer.apply` (streaming) writes chunks into a bounded buffer drained
// lazily through `iterator` — the producing code then runs on a separate fiber. Neither path
// allocates per `put` along a contiguous run. `Operand` is the element type — `Char` for
// `Producer[Text]`, `Byte` for `Producer[Data]` — and types the element-at-a-time `push`.
object Producer:
  // A byte producer: `Producer[Data]` with its element type pinned to `Byte`, the shape binary
  // encoders (CBOR, Protobuf, …) write into. `Producer[Data](…)` / `Producer.collect[Data]` already
  // surface this refinement; this alias names it for helper signatures.
  type Bytes = Producer[Data] { type Operand = Byte }

  // Streaming: chunks are queued (with backpressure) and drained through `iterator`. The producing
  // code must run on a separate fiber, since `put` blocks once the queue is full. The staging
  // block defaults to the buffering policy's capacity for the medium; `depth` keeps its own
  // shallow default rather than adopting `Buffering.depth`, since this queue holds materialized
  // chunks rather than recycled transfer blocks, so a conduit-deep queue would multiply retention
  // for no measured benefit.
  def apply[medium](block: Optional[Int] = Unset, depth: Int = 2)
    ( using addr: medium is Addressable, buffering: Buffering )
  :   (Channel[medium, addr.Operand])^ =

    Channel[medium, addr.Operand](block.or(buffering.capacity(addr.substrate)), depth)(using addr)

  // Synchronous: run `body`, accumulating directly into a builder, and return the whole value. No
  // concurrency, no chunk buffer, and none of the streaming path's single-thread deadlock risk.
  // The hint sizes the initial builder: small by default, since the builder doubles as it fills
  // (the copying that costs for a large output is the same from any start), whereas a large
  // start is paid in full by every small output — a 4096 default was the biggest single
  // allocation of rendering a one-line JSON response.
  def collect[medium](using addressable: medium is Addressable)(hint: Int = 256)
    ( body: ((Producer[medium] { type Operand = addressable.Operand })^) => Unit )
  :   medium =

    val target = addressable.blank(hint)

    val producer = new Producer[medium]:
      type Operand = addressable.Operand

      def put(source: medium): Unit =
        val size = addressable.length(source)
        if size > 0 then addressable.clone(source, Prim, (size - 1).z)(target)

      def put(source: medium, offset: Ordinal, size: Int): Unit =
        if size > 0 then addressable.clone(source, offset, (offset.n0 + size - 1).z)(target)

      def push(operand: Operand): Unit = addressable.append(target, operand)

    body(producer)
    addressable.build(target)

  // Synchronous streaming: each staging block is handed to `consume` on the caller's thread as
  // it fills, so the producing code runs inline — no fiber, no queue — and the consumer still
  // sees the output in pieces before it is complete. This is the model of Jackson's generator
  // writing to an `OutputStream`, and the right shape for a push sink (a file, a socket, an
  // `OutputStream`); a pull consumer (an `Iterator`, an HTTP body) needs `apply`'s channel.
  // `finish()` flushes the partial last block.
  // Like `collect`, the producing `body` runs inline, so the sink never escapes and `deliver`
  // need not be consumed; the partial last block is flushed once `body` returns.
  def sink[medium](deliver: medium => Unit, block: Optional[Int] = Unset)
    ( using addr: medium is Addressable, buffering: Buffering )
    ( body: ((Producer[medium] { type Operand = addr.Operand })^) => Unit )
  :   Unit =

    val producer =
      Sink[medium, addr.Operand](deliver, block.or(buffering.capacity(addr.substrate)))(using addr)

    body(producer)
    producer.finish()

  // The staging shared by the streaming producers: writes fill a block of `block` elements,
  // and each full block (and, at `finish`, the partial last one) is materialized and published
  // — to a queue by `Channel`, to a consumer by `Sink`.
  abstract class Staged[medium, operand](block: Int)
    ( using val addressable: medium is Addressable { type Operand = operand } )
  extends Producer[medium]:

    type Operand = operand

    // Untracked, cast-erased: reached only through this producer.
    @caps.unsafe.untrackedCaptures
    private val current: addressable.Storage =
      addressable.allocate(block).asInstanceOf[addressable.Storage]
    private var index: Ordinal = Prim

    private inline def free: Int = block - index.n0

    protected update def publishChunk(chunk: medium): Unit

    private inline update def publish(): Unit =
      if index != Prim then
        publishChunk(addressable.materialize(current, 0, index.n0))
        index = Prim

    update def put(source: medium): Unit = put(source, Prim, addressable.length(source))

    update def put(source: medium, offset: Ordinal, size: Int): Unit =
      var done = 0

      while size - done > free do
        addressable.copyChunk(source, offset.n0 + done, current, index.n0, free)
        done += free
        index = (index.n0 + free).z
        publish()

      addressable.copyChunk(source, offset.n0 + done, current, index.n0, size - done)
      index = (index.n0 + size - done).z

      if free == 0 then publish()

    update def push(operand: Operand): Unit =
      addressable.storageUpdate(current, index.n0, operand)
      index = (index.n0 + 1).z
      if free == 0 then publish()

    update def finish(): Unit = publish()

  // Streaming to a queue: full blocks wait (with backpressure) for the consuming thread to drain
  // them through `iterator`, so the producing code must run on its own fiber.
  final class Channel[medium, operand](block: Int, depth: Int)
    ( using medium is Addressable { type Operand = operand } )
  extends Staged[medium, operand](block):

    private object Done

    private val queue: juc.ArrayBlockingQueue[medium | Done.type] =
      juc.ArrayBlockingQueue(depth)

    protected update def publishChunk(chunk: medium): Unit = queue.put(chunk)

    override update def finish(): Unit =
      super.finish()
      queue.put(Done)

    // The reader-side view: like a conduit's stream endpoint, it is owned by the
    // consuming thread and mediated by the queue's happens-before; the seal is that rim.
    lazy val iterator: Iterator[medium] = caps.unsafe.unsafeAssumePure(new Iterator[medium]:
      // The iterator is the reader-side view of the channel (non-Stateful by design:
      // scala.Iterator's methods cannot be update methods); its staging slot is untracked.
      @caps.unsafe.untrackedCaptures
      private var ready: medium | Done.type = Done

      def hasNext: Boolean =
        ready = queue.take().nn
        ready != Done

      def next(): medium = ready.asInstanceOf[medium])

  // Streaming to a consumer: each full block is handed to `consume` inline, on the producing
  // thread, so no fiber or queue is involved; see `Producer.sink`.
  final class Sink[medium, operand](deliver: medium => Unit, block: Int)
    ( using medium is Addressable { type Operand = operand } )
  extends Staged[medium, operand](block):

    protected update def publishChunk(chunk: medium): Unit = deliver(chunk)

  // A consumer lent a filled block: the block's storage as a `Region` with its branded
  // extent, valid only for the duration of the call — the discipline of `Stream.lend` — so
  // nothing is copied, and a consumer that must keep the bytes materializes them itself.
  type Lending[medium] = (region: Region[medium]) => (Interval in region.type) => Unit

  // Streaming text as UTF-8: characters are encoded straight into a byte block, and each full
  // block is lent to `lending` inline, so a text serializer writes bytes to a socket or an
  // `OutputStream` without materializing a `Text` per block and encoding it again, and without
  // copying the block. A lone surrogate encodes as U+FFFD, as `String.getBytes` would.
  def lendUtf8(lending: Lending[Data], block: Optional[Int] = Unset)(using buffering: Buffering)
    ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
  :   Unit =

    val producer = Utf8Sink(lending, block.or(buffering.capacity(Substrate.Bytes)))
    body(producer)
    producer.finish()

  // The owning form of `lendUtf8`: each block is delivered as a fresh `Data` the consumer may
  // keep.
  def utf8(deliver: Data => Unit, block: Optional[Int] = Unset)(using buffering: Buffering)
    ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
  :   Unit =

    lendUtf8(region => interval => deliver(region.materialize(interval)), block)(body)

  // The block classes keying the shared `Blockpool`: the cast erases the allocation's fresh
  // capture before `getClass`, which needs no capability (as `Conduit` does).
  private val bytesClass: Class[?] = (new scala.Array[Byte](0)).asInstanceOf[AnyRef].getClass.nn
  private val charsClass: Class[?] = (new scala.Array[Char](0)).asInstanceOf[AnyRef].getClass.nn

  private val namesClass: Class[?] =
    (new scala.Array[AnyRef | Null](0)).asInstanceOf[AnyRef].getClass.nn

  final class Utf8Sink(lending: Lending[Data], block: Int) extends Producer[Text]:
    type Operand = Char

    // Encoding goes through the JDK's UTF-8 `CharsetEncoder`, whose ASCII path is vectorized:
    // characters are inflated into a reused scratch `CharBuffer` and encoded into the byte block,
    // which is a heap `ByteBuffer` (a Java object the checker does not track, reached only through
    // this producer), materialized by copying its filled prefix. The encoder keeps a trailing high
    // surrogate between calls, and a malformed pair encodes as U+FFFD, as `String.getBytes` would.
    private val encoder: java.nio.charset.CharsetEncoder =
      java.nio.charset.StandardCharsets.UTF_8.nn.newEncoder().nn
      . onMalformedInput(java.nio.charset.CodingErrorAction.REPLACE).nn
      . onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPLACE).nn

    // The byte block and the char scratch are leased from the shared `Blockpool` and offered
    // back at `finish`, so a sink allocates nothing of its own once the pool is warm.
    private val current: java.nio.ByteBuffer =
      Blockpool.poll(bytesClass, block) match
        case null   => java.nio.ByteBuffer.allocate(block).nn
        case pooled => java.nio.ByteBuffer.wrap(pooled.asInstanceOf[scala.Array[Byte]]).nn

    // The scratch is a raw array with its own fill count — a `CharBuffer`'s position bookkeeping
    // per `put` measured as most of the sink's cost — wrapped only when a whole block is encoded.
    private val scratch: scala.Array[Char] =
      Blockpool.poll(charsClass, block) match
        case null   => new scala.Array[Char](block)
        case pooled => pooled.asInstanceOf[scala.Array[Char]]
    private val scratchView: java.nio.CharBuffer = java.nio.CharBuffer.wrap(scratch).nn
    private var filled: Int = 0

    private update def publish(): Unit =
      if current.position() > 0 then
        Region.over[Data, Unit](current.array().nn, 0, current.position())(lending)
        current.clear()

    // Encodes the scratch, publishing the block each time it fills; a trailing high surrogate is
    // moved to the front of the scratch for the next call to complete.
    private update def drain(endOfInput: Boolean): Unit =
      val input = java.nio.CharBuffer.wrap(scratch, 0, filled).nn
      var result = encoder.encode(input, current, endOfInput).nn

      while result.isOverflow do
        publish()
        result = encoder.encode(input, current, endOfInput).nn

      val remaining = input.remaining()
      if remaining > 0 then System.arraycopy(scratch, filled - remaining, scratch, 0, remaining)
      filled = remaining

    update def put(source: Text): Unit = put(source, Prim, source.s.length)

    update def put(source: Text, offset: Ordinal, size: Int): Unit =
      val string = source.s
      val end = offset.n0 + size
      var from = offset.n0

      // Shape 3 (a zephyrine internal): `from` advances through `[offset, offset + size)` of
      // `string` by the number of characters that fit the scratch, which is drained only when
      // full — so the encoder runs over whole blocks — and is never full after a `drain` (at
      // most one pending surrogate remains), so every step makes progress.
      while from < end do
        if filled == scratch.length then drain(false)
        val count = (end - from).min(scratch.length - filled)
        string.getChars(from, from + count, scratch, filled)
        filled += count
        from += count

    update def push(operand: Char): Unit =
      if filled == scratch.length then drain(false)
      scratchView.put(filled, operand)
      filled += 1

    update def finish(): Unit =
      drain(true)
      var result = encoder.flush(current).nn

      while result.isOverflow do
        publish()
        result = encoder.flush(current).nn

      filled = 0
      encoder.reset()
      publish()
      Blockpool.offer(bytesClass, block, current.array().nn.asInstanceOf[AnyRef])
      Blockpool.offer(charsClass, block, scratch.asInstanceOf[AnyRef])

  object Utf8Writer:
    // The number of slots in the writer's name cache: a power of two.
    val NameSlots: Int = 256

    // An escape table: `codes` holds, for each ASCII character, zero when it is written as it
    // is, or the one-based number of the entity that replaces it, whose bytes are
    // `data(offsets(code - 1) until offsets(code))`. Pure and flat, so the encoding loop tests
    // one byte per character.
    final class Escapes private[Utf8Writer]
      ( val codes:   scala.IArray[Byte],
        val offsets: scala.IArray[Int],
        val data:    scala.IArray[Byte] )

    // The table replacing each character in `entities`, which must be ASCII, with its entity.
    def escapes(entities: (Char, String)*): Escapes =
      val codes = new scala.Array[Byte](128)
      val offsets = new scala.Array[Int](entities.length + 1)
      val data = java.io.ByteArrayOutputStream()

      entities.zipWithIndex.foreach: (entity, index) =>
        codes(entity(0)) = (index + 1).toByte
        data.write(entity(1).getBytes(java.nio.charset.StandardCharsets.US_ASCII).nn)
        offsets(index + 1) = data.size

      Escapes
        ( scala.IArray.unsafeFromArray(codes),
          scala.IArray.unsafeFromArray(offsets),
          scala.IArray.unsafeFromArray(data.toByteArray.nn) )

    // Writes with `body` through a writer lending each block to `lending`, then flushes it.
    def lend(lending: Lending[Data], block: Optional[Int] = Unset)(using buffering: Buffering)
      ( body: (Utf8Writer^) => Unit )
    :   Unit =

      val writer = Utf8Writer(lending, block.or(buffering.capacity(Substrate.Bytes)))
      body(writer)
      writer.finish()

  // A byte-level writer for a text format's push form: it encodes UTF-8 straight into a block
  // leased from the `Blockpool`, escaping each string through the format's table in the same
  // pass, and lends each filled block to `lending` as `lendUtf8` does. A serializer writing
  // through it never builds a `Text` for its output, so nothing is encoded twice; this is the
  // model of JSON's byte writer, shared so that each format supplies only its escapes. A lone
  // surrogate encodes as U+FFFD, except in a name (see `name`).
  final class Utf8Writer(lending: Lending[Data], block: Int)
  extends caps.ExclusiveCapability, caps.Stateful:
    // The byte block and the char scratch are leased from the shared `Blockpool` and offered
    // back at `finish`, so a writer allocates nothing of its own once the pool is warm. Each is
    // reached only through this writer, and `untrackedCaptures` keeps the block's exclusivity
    // out of the class's own type.
    @caps.unsafe.untrackedCaptures
    private val current: scala.Array[Byte]^ =
      Blockpool.poll(bytesClass, block) match
        case null   => new scala.Array[Byte](block)
        case pooled => pooled.asInstanceOf[scala.Array[Byte]]

    private var index: Int = 0

    // Characters are inflated into this scratch a block at a time, so the encoding loop reads a
    // raw array rather than paying `charAt`'s coder check per character.
    @caps.unsafe.untrackedCaptures
    private val scratch: scala.Array[Char] =
      Blockpool.poll(charsClass, block) match
        case null   => new scala.Array[Char](block)
        case pooled => pooled.asInstanceOf[scala.Array[Char]]

    // A high surrogate awaiting its low half across a scratch boundary, or zero.
    private var pending: Char = 0

    // The filled block is lent, not copied: the consumer sees the writer's own array through a
    // `Region` for the duration of the call, after which the block is reused.
    private update def publish(): Unit =
      if index > 0 then
        Region.over[Data, Unit](current, 0, index)(lending)
        index = 0

    update def byte(value: Int): Unit =
      if index == block then publish()
      current(index) = value.toByte
      index += 1

    // `String#getBytes(int, int, byte[], int)` is deprecated because it drops each character's
    // high byte, which is exactly right for ASCII, and it copies a Latin-1 string's storage
    // directly.
    @scala.annotation.nowarn("cat=deprecation")
    private update def copyAscii(text: String, from: Int, end: Int): Unit =
      text.getBytes(from, end, current, index)

    // Text known to be ASCII, such as markup: each character is one byte.
    update def ascii(text: String): Unit =
      val length = text.length

      if index + length <= block then
        copyAscii(text, 0, length)
        index += length
      else
        var from = 0

        // Shape 2: `from` advances through `text` by the room left in the block, which is
        // published whenever it fills, so every step makes progress.
        while from < length do
          if index == block then publish()
          val count = (length - from).min(block - index)
          copyAscii(text, from, from + count)
          index += count
          from += count

    // Bytes already encoded as UTF-8, copied verbatim: `source(from until end)`. A copy that
    // fits the block is the common case, and is one `arraycopy`, which measured faster than a
    // loop even for a name of a few bytes.
    update def bytes(source: scala.Array[Byte], from: Int, end: Int): Unit =
      val length = end - from

      if index + length <= block then
        System.arraycopy(source, from, current, index, length)
        index += length
      else
        spill(source, from, end)

    private update def spill(source: scala.Array[Byte], from: Int, end: Int): Unit =
      var start = from

      // Shape 2: `start` advances through `[from, end)` by the room left in the block, which is
      // published whenever it fills, so every step makes progress.
      while start < end do
        if index == block then publish()
        val count = (end - start).min(block - index)
        System.arraycopy(source, start, current, index, count)
        index += count
        start += count

    // Text encoded as UTF-8 with nothing escaped.
    update def text(text: String): Unit = chars(text, null)

    // The names a document repeats (its element and attribute names) and their encodings, in a
    // direct-mapped cache keyed by the string's own cached hash and matched by identity, or
    // failing that by equality: a parser shares one instance per distinct name, and literals
    // are interned, so a repeated name is usually found with one comparison, and a name built
    // afresh for each use (as a derived encoder's labels are) with a comparison of its
    // characters, and in both cases written with one copy. Any other string is encoded by
    // `String#getBytes`, which writes a lone surrogate as `?` where the other
    // methods write U+FFFD, and no well-formed name contains one; it then replaces the entry
    // in its slot. The cache holds each slot's name and
    // encoding side by side, and is leased from the `Blockpool` like the block, so it stays
    // warm from one document to the next.
    @caps.unsafe.untrackedCaptures
    private val names: scala.Array[AnyRef | Null]^ =
      Blockpool.poll(namesClass, Utf8Writer.NameSlots*2) match
        case null   => new scala.Array[AnyRef | Null](Utf8Writer.NameSlots*2)
        case pooled => pooled.asInstanceOf[scala.Array[AnyRef | Null]]

    // A name, encoded as UTF-8 with nothing escaped, through the cache.
    update def name(text: String): Unit =
      val hash = text.hashCode
      val slot = ((hash ^ (hash >>> 16)) & (Utf8Writer.NameSlots - 1))*2
      val cached = names(slot + 1)

      val name = names(slot)

      val encoded: scala.Array[Byte] =
        if cached != null && name != null && ((name eq text.asInstanceOf[AnyRef]) || name == text)
        then cached.asInstanceOf[scala.Array[Byte]]
        else
          val fresh = text.getBytes(java.nio.charset.StandardCharsets.UTF_8).nn
          names(slot) = text
          names(slot + 1) = fresh.asInstanceOf[AnyRef]
          fresh

      bytes(encoded, 0, encoded.length)

    // Text encoded as UTF-8 with each ASCII character that `escapes` names replaced by its
    // entity.
    update def escaped(text: String, escapes: Utf8Writer.Escapes): Unit = chars(text, escapes)

    // A decimal integer.
    update def long(value: Long): Unit =
      if value == Long.MinValue then ascii("-9223372036854775808")
      else
        if index + 20 > block then publish()
        val out = current
        var n = if value < 0 then -value else value
        var k = index + 20

        // Digits are written backwards from a fixed end; shape 1, terminating when `n` is 0.
        while n != 0 do
          k -= 1
          out(k) = ('0' + n % 10).toByte
          n /= 10

        if k == index + 20 then
          k -= 1
          out(k) = '0'

        if value < 0 then
          k -= 1
          out(k) = '-'

        val length = index + 20 - k
        System.arraycopy(out, k, out, index, length)
        index += length

    // The bytes of the entity numbered `code` in `escapes`.
    private update def entity(escapes: Utf8Writer.Escapes, code: Int): Unit =
      val offsets = escapes.offsets.asInstanceOf[scala.Array[Int]]
      val start = offsets(code - 1)
      val end = offsets(code)
      if index + end - start > block then publish()
      System.arraycopy(escapes.data.asInstanceOf[AnyRef], start, current, index, end - start)
      index += end - start

    private update def replacement(): Unit =
      byte(0xef)
      byte(0xbf)
      byte(0xbd)

    // The slow path: a non-ASCII character, or any character while a surrogate is pending.
    private update def encode(char: Char): Unit =
      if pending != 0 then
        val high = pending
        pending = 0

        if Character.isLowSurrogate(char) then
          val codepoint = Character.toCodePoint(high, char)
          byte(0xf0 | (codepoint >> 18))
          byte(0x80 | ((codepoint >> 12) & 0x3f))
          byte(0x80 | ((codepoint >> 6) & 0x3f))
          byte(0x80 | (codepoint & 0x3f))
        else
          replacement()
          encode(char)
      else if Character.isHighSurrogate(char) then
        pending = char
      else if Character.isLowSurrogate(char) then
        replacement()
      else if char < 0x80 then
        byte(char)
      else if char < 0x800 then
        byte(0xc0 | (char >> 6))
        byte(0x80 | (char & 0x3f))
      else
        byte(0xe0 | (char >> 12))
        byte(0x80 | ((char >> 6) & 0x3f))
        byte(0x80 | (char & 0x3f))

    // Encodes `text(from until end)` when `chars` is null, else `chars(from until end)`,
    // escaping through `escapes` unless it is null. A short string is read through `charAt`,
    // whose per-character cost is below the fixed cost of inflating it into the scratch; a
    // long one is inflated a scratch-full at a time by `chars`. Shape 2 (index arithmetic
    // derived from the data): `k` runs over `[from, end)`, and `at` shadows `index` within
    // `[0, block]`, written back around every slow-path call and at exit, so the block's fill
    // count is always exact. The hot loop reads only locals.
    private update def encodeRange
      ( text:    String,
        chars:   scala.Array[Char] | Null,
        from:    Int,
        end:     Int,
        escapes: Utf8Writer.Escapes | Null )
    :   Unit =

      val out = current
      val limit = block

      // The table's codes, read as the plain array they are: `IArray`'s generic `apply` would box
      // each byte.
      val codes: scala.Array[Byte] | Null =
        if escapes == null then null else escapes.codes.asInstanceOf[scala.Array[Byte]]

      var k = from
      var at = index

      while k < end do
        val char = if chars == null then text.charAt(k) else chars(k)

        if char < 0x80 && pending == 0 then
          val code = if codes == null then 0 else codes(char)

          if code == 0 then
            if at == limit then
              index = at
              publish()
              at = 0

            out(at) = char.toByte
            at += 1
          else
            index = at
            entity(escapes.nn, code)
            at = index
        else
          index = at
          encode(char)
          at = index

        k += 1

      index = at

    private update def chars(text: String, escapes: Utf8Writer.Escapes | Null): Unit =
      val end = text.length

      if end <= 64 then encodeRange(text, null, 0, end, escapes)
      else
        var from = 0

        // Shape 2: `from` advances through `text` by at most a scratch-full of characters at a
        // time, so every step makes progress.
        while from < end do
          val count = (end - from).min(scratch.length)
          text.getChars(from, from + count, scratch, 0)
          encodeRange(text, scratch, 0, count, escapes)
          from += count

      if pending != 0 then
        pending = 0
        replacement()

    // Flushes the last block and returns both buffers to the pool; the writer is spent.
    update def finish(): Unit =
      publish()
      Blockpool.offer(bytesClass, block, current.asInstanceOf[AnyRef])
      Blockpool.offer(charsClass, block, scratch.asInstanceOf[AnyRef])
      Blockpool.offer(namesClass, Utf8Writer.NameSlots*2, names.asInstanceOf[AnyRef])

  // The media a text serializer's push form can be delivered as: `Text` blocks through `sink`,
  // or UTF-8 `Data` blocks through `utf8`. A format's `emit[medium](value, deliver)` summons
  // one, so the serializer itself is written once, against `Producer[Text]`.
  trait Emission[medium]:
    def run(deliver: medium => Unit)
      ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
      ( using Buffering )
    :   Unit

  object Emission:
    given text: Emission[Text]:
      def run(deliver: Text => Unit)
        ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
        ( using Buffering )
      :   Unit =

        sink[Text](deliver)(body)

    given data: Emission[Data]:
      def run(deliver: Data => Unit)
        ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
        ( using Buffering )
      :   Unit =

        utf8(deliver)(body)

// A producer is a stateful capability: writing requires an exclusive reference, and the
// root classification here lets `Intake` (and every other implementation) mark its
// writes as update methods.
trait Producer[medium] extends caps.ExclusiveCapability, caps.Stateful:
  type Operand
  update def put(source: medium): Unit
  update def put(source: medium, offset: Ordinal, size: Int): Unit

  // Write a single element: a `Char` for `Producer[Text]`, a `Byte` for `Producer[Data]`.
  update def push(operand: Operand): Unit

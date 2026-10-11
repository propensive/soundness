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
package octogenarian

import anticipation.*
import contingency.*
import gastronomy.*
import gossamer.*
import monotonous.*
import pneumatic.*
import rudiments.*
import vacuous.*

import alphabets.hexLowerCase
import cryptoPermits.permitDeprecatedCrypto
import providers.javaBaseProvider

// Packfiles in transit: writing one from a list of objects (undeltified, each zlib-compressed)
// and reading one received whole, resolving its deltas, into a store.
object Packs:
  import Git.Error.Reason as Reason

  private def typeCode(kind: Git.Object.Kind): Int = kind match
    case Git.Object.Kind.Commit => 1
    case Git.Object.Kind.Tree   => 2
    case Git.Object.Kind.Blob   => 3
    case Git.Object.Kind.Tag    => 4

  private def kindOf(code: Int)(using Tactic[Git.Error]): Git.Object.Kind = code match
    case 1 => Git.Object.Kind.Commit
    case 2 => Git.Object.Kind.Tree
    case 3 => Git.Object.Kind.Blob
    case 4 => Git.Object.Kind.Tag
    case _ => abort(Git.Error(Reason.Unexpected, t"an object of type $code in a pack"))

  private def int(output: java.io.ByteArrayOutputStream, value: Int): Unit =
    output.write((value >>> 24) & 0xff)
    output.write((value >>> 16) & 0xff)
    output.write((value >>> 8) & 0xff)
    output.write(value & 0xff)

  // The entry header: the type in bits 4 to 6 of the first byte, the size in its low four bits
  // and then seven bits per continuation byte.
  private def entryHeader(output: java.io.ByteArrayOutputStream, kind: Git.Object.Kind, size: Long): Unit =
    val first = (typeCode(kind) << 4) | (size & 15).toInt
    val rest = size >>> 4

    def continuation(value: Long): Unit =
      if value == 0 then () else
        val more = value >>> 7
        output.write(((value & 0x7f) | (if more == 0 then 0 else 0x80)).toInt)
        continuation(more)

    output.write(if rest == 0 then first else first | 0x80)
    continuation(rest)

  private def sha1(data: Data): Data = Objects.unhex(data.digest[Sha1].serialize[Hex]).asInstanceOf[Data]

  // A pack of `hashes`, each object read through `read`, in the order given.
  def write(hashes: List[Git.Hash], read: Git.Hash => (Git.Object.Kind, Data), activity: Git.Activity^): Data =
    val output = java.io.ByteArrayOutputStream()
    output.write(Objects.bytes(t"PACK").asInstanceOf[scala.Array[Byte]])
    int(output, 2)
    int(output, Transfer.count(hashes))

    def entry(hash: Git.Hash): Unit =
      val (kind, data) = read(hash)
      entryHeader(output, kind, data.length.toLong)
      output.write(data.compress[Zlib].asInstanceOf[scala.Array[Byte]])
      activity.advance()

    hashes.each(entry(_))
    val body = output.toByteArray.nn.asInstanceOf[Data]
    Store.concatenate(body, sha1(body))

  // An entry as read: its type code, its inflated bytes, and for a delta its base (an offset or
  // a hash).
  private case class Entry(offset: Long, code: Int, data: Data, baseOffset: Long, baseHash: Optional[Git.Hash])

  // Reads a whole pack, storing every object through `store` with deltas resolved (against
  // other entries, or through `external` for a thin pack's bases). Returns the object count.
  def read
    ( pack: Data, external: Git.Hash => Optional[(Git.Object.Kind, Data)],
      store: (Git.Object.Kind, Data) => Git.Hash, activity: Git.Activity^ )
    ( using Tactic[Git.Error] )
  :   Int =

    val array = pack.asInstanceOf[scala.Array[Byte]]
    if array.length < 32 || Objects.text(array.slice(0, 4).asInstanceOf[Data]) != t"PACK"
    then abort(Git.Error(Reason.Unexpected, t"the data received is not a packfile"))

    val buffer = java.nio.ByteBuffer.wrap(array).nn
    val version = buffer.getInt(4)
    if version != 2 && version != 3 then abort(Git.Error(Reason.Unsupported(t"packfile version $version")))
    val count = buffer.getInt(8)

    val trailer = array.slice(array.length - 20, array.length).asInstanceOf[Data]
    val checksum = sha1(array.slice(0, array.length - 20).asInstanceOf[Data])
    if Objects.text(trailer) != Objects.text(checksum) && !java.util.Arrays.equals(trailer.asInstanceOf[scala.Array[Byte]], checksum.asInstanceOf[scala.Array[Byte]])
    then abort(Git.Error(Reason.Unexpected, t"the packfile's checksum does not match"))

    activity.begin(Git.Activity.Phase.Unpacking)
    activity.measure(count.toLong)

    def byteAt(position: Int): Int = array(position) & 0xff

    // The size varint after the first byte's low four bits.
    def size(position: Int, value: Long, shift: Int): (Long, Int) =
      val byte = byteAt(position)
      val next = value | ((byte & 0x7f).toLong << shift)
      if (byte & 0x80) == 0 then (next, position + 1) else size(position + 1, next, shift + 7)

    def distance(position: Int, value: Long): (Long, Int) =
      val byte = byteAt(position)
      val next = (value << 7) | (byte & 0x7f)
      if (byte & 0x80) == 0 then (next, position + 1) else distance(position + 1, next + 1)

    def entries(position: Int, remaining: Int, acc: List[Entry]): List[Entry] =
      if remaining == 0 then acc.reverse else
        val first = byteAt(position)
        val code = (first >> 4) & 7
        val (_, afterSize) = if (first & 0x80) == 0 then (0L, position + 1) else size(position + 1, 0L, 4)

        val (baseOffset, baseHash, dataStart) = code match
          case 6 =>
            val (dist, after) = distance(afterSize, (byteAt(afterSize) & 0x7f).toLong)
            // The first byte was already folded in above; recompute cleanly.
            (dist, Unset, after)

          case 7 =>
            (0L, Git.Hash.unsafe(Objects.hex(array, afterSize, 20)), afterSize + 20)

          case _ => (0L, Unset, afterSize)

        val (data, consumed) = Zlib.inflate(pack, dataStart)
        activity.advance()
        entries(dataStart + consumed, remaining - 1, Entry(position.toLong, code, data, baseOffset, baseHash) :: acc)

    val all = entries(12, count, Nil)
    val byOffset = java.util.HashMap[Long, Entry]()
    all.each { entry => byOffset.put(entry.offset, entry) }
    val resolved = java.util.HashMap[Long, (Git.Object.Kind, Data)]()
    val byHash = java.util.HashMap[Git.Hash, (Git.Object.Kind, Data)]()

    activity.begin(Git.Activity.Phase.Resolving)
    activity.measure(count.toLong)

    def resolve(entry: Entry): (Git.Object.Kind, Data) =
      val cached = resolved.get(entry.offset)
      if cached != null then cached else
        val result: (Git.Object.Kind, Data) = entry.code match
          case 6 =>
            val base = byOffset.get(entry.offset - entry.baseOffset)
            if base == null then abort(Git.Error(Reason.Unexpected, t"a delta base offset outside the pack"))
            val (kind, baseData) = resolve(base.nn)
            (kind, Packfile.applyDelta(baseData, entry.data))

          case 7 =>
            val hash = entry.baseHash.or(abort(Git.Error(Reason.Unexpected)))
            val inPack = byHash.get(hash)

            val (kind, baseData) =
              if inPack != null then inPack.nn
              else plainWithHash(all, hash).or(external(hash).or(abort(Git.Error(Reason.NoSuchObject(hash.text)))))

            (kind, Packfile.applyDelta(baseData, entry.data))

          case code => (kindOf(code), entry.data)

        resolved.put(entry.offset, result)
        result

    // A plain (undeltified) entry whose object has the given hash, resolved.
    def plainWithHash(remaining: List[Entry], hash: Git.Hash): Optional[(Git.Object.Kind, Data)] = remaining match
      case entry :: rest =>
        if entry.code > 4 then plainWithHash(rest, hash) else
          val (kind, data) = resolve(entry)
          if Store.hash(kind, data) == hash then (kind, data) else plainWithHash(rest, hash)

      case _ => Unset

    def storeAll(remaining: List[Entry]): Unit = remaining match
      case entry :: rest =>
        val (kind, data) = resolve(entry)
        val hash = store(kind, data)
        byHash.put(hash, (kind, data))
        activity.advance()
        storeAll(rest)

      case _ => ()

    // Plain entries first, so that reference deltas find their bases by hash.
    storeAll(all.filter(_.code <= 4))
    storeAll(all.filter(_.code > 4))
    count

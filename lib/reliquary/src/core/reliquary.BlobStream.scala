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
package reliquary

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import stratiform.*

import Lira.Error.Reason

// The decompressed payload of a `.lira` file (§8.2): a sequence of `uvarint(length) ++ bytes`
// records in ascending bytewise order of their blob hashes, with no duplicates. Hashes are never
// stored: a reader recomputes them while scanning, and that recomputation is the integrity
// check. Writing is deterministic: any permutation of the same blobs serializes identically.
object BlobStream:

  def write(blobs: List[Data]): Data =
    val distinct = scala.collection.mutable.LinkedHashMap[Text, Blob]()

    blobs.each: data =>
      val hash = Lira.Hash(Lira.Hash.Domain.Blob, data)
      distinct.getOrElseUpdate(Lira.Hash.text(hash), Blob(hash, data))

    val sorted = distinct.values.toList.sortWith: (a, b) => Blob.compare(a.hash, b.hash) < 0
    val lengths = sorted.map: blob => Varint.encode(blob.data.length.toLong)
    val total = sorted.zip(lengths).map { (blob, length) => blob.data.length + length.length }.sum
    val buffer = Array.allocate[Byte](total)
    var offset = 0

    sorted.zip(lengths).foreach: (blob, length) =>
      System.arraycopy(Array.unsafeJvm(length), 0, buffer.raw, offset, length.length)
      offset += length.length
      System.arraycopy(Array.unsafeJvm(blob.data), 0, buffer.raw, offset, blob.data.length)
      offset += blob.data.length

    Array.freeze(buffer)

  def read(data: Data): Blobstore raises Lira.Error =
    val blobs = scala.collection.mutable.ListBuffer[Blob]()
    var previous: Data | Null = null
    var offset = 0

    while offset < data.length do
      val decoded =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case _: Varint.Error =>
            Lira.Error(Reason.MalformedPayload(t"a record length is malformed"))

        . protect(Varint.decode(data, offset))

      val length = decoded.value

      if length > Int.MaxValue.toLong || decoded.next + length.toInt > data.length
      then abort(Lira.Error(Reason.MalformedPayload(t"a record overruns the end of the stream")))

      val content = Array.allocate[Byte](length.toInt)
      System.arraycopy(Array.unsafeJvm(data), decoded.next, content.raw, 0, length.toInt)
      val bytes = Array.freeze(content)
      val hash = Lira.Hash(Lira.Hash.Domain.Blob, bytes)

      if previous != null then
        val order = Blob.compare(previous.nn, hash)

        if order == 0
        then abort(Lira.Error(Reason.InvalidBlobStream(t"two records have equal hashes")))

        if order > 0
        then abort(Lira.Error(Reason.InvalidBlobStream(t"records are not in ascending hash order")))

      blobs += Blob(hash, bytes)
      previous = hash
      offset = decoded.next + length.toInt

    Blobstore(List.from(blobs))

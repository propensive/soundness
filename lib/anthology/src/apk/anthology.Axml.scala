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
package anthology

import scala.collection.mutable as scm

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// Android binary XML ("AXML"): the compact, chunked encoding Android's framework parses a
// manifest from — a string pool, an optional resource-map chunk, then a flat stream of
// start-namespace / start-element / end-element / end-namespace events. It is deliberately not a
// Xylophone serialization format: its data model is not text-XML's. Attribute values are typed
// (a boolean, an integer, a string — not all strings), `android:`-namespaced attributes are
// addressed at parse time by a numeric *resource ID* rather than by name, and namespaces are
// first-class chunks rather than `xmlns` attributes — none of which the general XML model
// carries. So AXML has its own small model here, in the Android layer that owns that knowledge.
object Axml:
  // The Android resources namespace, and the prefix a manifest conventionally binds it to.
  val androidUri: Text = t"http://schemas.android.com/apk/res/android"
  val androidPrefix: Text = t"android"

  // A typed attribute value: Android reads each attribute at a specific `TypedValue` type, so
  // the encoder must commit to one rather than emit every value as a string.
  enum Value:
    case Str(text: Text)
    case Num(value: Int)
    case Bool(flag: Boolean)

  // An attribute: its namespace (the android URI, or `Unset` for an unnamespaced attribute such
  // as `package`), its local name, the public resource ID Android identifies it by (for
  // `android:` manifest attributes), and its typed value.
  case class Attribute
    ( namespace: Optional[Text], name: Text, resourceId: Optional[Int], value: Value )

  case class Element(name: Text, attributes: List[Attribute], children: List[Element])

  // `TypedValue` data-type tags (a subset): a string references the pool; an integer or boolean
  // is stored inline.
  private val typeReference: Int = 0x01
  private val typeString:    Int = 0x03
  private val typeIntDec:    Int = 0x10
  private val typeIntBool:   Int = 0x12

  private val noRef: Long = 0xffffffffL

  // The string pool, whose first entries must be exactly the resource-mapped attribute names in
  // resource-map order: Android pairs the i-th pool string with the i-th resource-map ID, so an
  // `android:` attribute is recognized only when its name string and its ID share an index.
  private class Strings:
    private val indices: scm.LinkedHashMap[Text, Int] = scm.LinkedHashMap()
    def intern(string: Text): Int = indices.getOrElseUpdate(string, indices.size)
    def count: Int = indices.size
    def ordered: List[Text] = indices.toList.sortBy(_(1)).map(_(0))

  // A growable little-endian byte buffer with the back-patching the chunked format needs: chunk
  // sizes and the total file size are only known after their contents are written.
  private class Buffer:
    private val bytes: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
    def position: Int = bytes.length

    def u8(value: Int): Unit = bytes += value.toByte

    def u16(value: Int): Unit =
      u8(value & 0xff); u8((value >> 8) & 0xff)

    def u32(value: Long): Unit =
      u16((value & 0xffff).toInt); u16(((value >> 16) & 0xffff).toInt)

    def patchU32(at: Int, value: Long): Unit =
      bytes(at) = (value & 0xff).toByte
      bytes(at + 1) = ((value >> 8) & 0xff).toByte
      bytes(at + 2) = ((value >> 16) & 0xff).toByte
      bytes(at + 3) = ((value >> 24) & 0xff).toByte

    def align4(): Unit = while position%4 != 0 do u8(0)
    def data: Data = bytes.toArray.immutable(using Unsafe)

  def encode(root: Element): Data =
    val strings = Strings()

    // Pass 1: the resource-mapped names first (in first-seen order), so they take the low pool
    // indices the resource map addresses.
    val resourceMap: scm.LinkedHashMap[Text, Int] = scm.LinkedHashMap()

    def collectResources(element: Element): Unit =
      for attribute <- element.attributes do attribute.resourceId.let: id =>
        if !resourceMap.contains(attribute.name) then
          resourceMap(attribute.name) = id
          strings.intern(attribute.name)

      for child <- element.children do collectResources(child)

    collectResources(root)

    // Pass 2: every other referenced string (the namespace prefix and URI, element names,
    // unmapped attribute names, and string values).
    strings.intern(androidPrefix)
    strings.intern(androidUri)

    def collectStrings(element: Element): Unit =
      strings.intern(element.name)

      for attribute <- element.attributes do
        strings.intern(attribute.name)
        attribute.value.only { case Value.Str(text) => strings.intern(text) }

      for child <- element.children do collectStrings(child)

    collectStrings(root)

    val out = Buffer()

    // The XML chunk header; its total-size field is patched once the file is complete.
    out.u16(0x0003)
    out.u16(0x0008)
    val totalSizeAt = out.position
    out.u32(0)

    writeStringPool(out, strings.ordered)
    writeResourceMap(out, resourceMap.values.to(List))

    val android = strings.intern(androidUri)
    val prefix = strings.intern(androidPrefix)

    // START_NAMESPACE
    startChunk(out, 0x0100):
      out.u32(prefix)
      out.u32(android)

    writeElement(out, root, strings)

    // END_NAMESPACE
    startChunk(out, 0x0101):
      out.u32(prefix)
      out.u32(android)

    out.patchU32(totalSizeAt, out.position.toLong)
    out.data

  // A namespace or resource-map-free node chunk with a 16-byte header (type, header size, chunk
  // size, line number, comment); `body` writes the remainder and the size is patched in.
  private def startChunk(out: Buffer, chunkType: Int)(body: => Unit): Unit =
    out.u16(chunkType)
    out.u16(0x0010)
    val sizeAt = out.position
    out.u32(0)
    out.u32(1)          // line number (unimportant; a manifest carries none)
    out.u32(noRef)      // comment
    body
    out.patchU32(sizeAt, (out.position - sizeAt + 4).toLong)

  private def writeElement(out: Buffer, element: Element, strings: Axml.Strings): Unit =
    startChunk(out, 0x0102):
      out.u32(noRef)                       // element namespace (none)
      out.u32(strings.intern(element.name))
      out.u16(0x0014)                      // attribute record start
      out.u16(0x0014)                      // attribute record size
      out.u16(element.attributes.length)
      out.u16(0)                           // id attribute index (none)
      out.u16(0)                           // class attribute index (none)
      out.u16(0)                           // style attribute index (none)

      element.attributes.each: attribute =>
        out.u32(attribute.namespace.let(strings.intern(_)).or(noRef.toInt).toLong)
        out.u32(strings.intern(attribute.name))

        attribute.value match
          case Value.Str(text) =>
            val index = strings.intern(text)
            out.u32(index)                 // raw value (the string itself)
            out.u16(0x0008); out.u8(0); out.u8(typeString)
            out.u32(index)

          case Value.Num(value) =>
            out.u32(noRef)
            out.u16(0x0008); out.u8(0); out.u8(typeIntDec)
            out.u32(value & 0xffffffffL)

          case Value.Bool(flag) =>
            out.u32(noRef)
            out.u16(0x0008); out.u8(0); out.u8(typeIntBool)
            out.u32(if flag then noRef else 0L)

    element.children.each(writeElement(out, _, strings))

    // END_ELEMENT
    startChunk(out, 0x0103):
      out.u32(noRef)
      out.u32(strings.intern(element.name))

  private def writeStringPool(out: Buffer, entries: List[Text]): Unit =
    out.u16(0x0001)
    out.u16(0x001c)
    val sizeAt = out.position
    out.u32(0)
    out.u32(entries.length)
    out.u32(0)                             // style count
    out.u32(0)                             // flags: 0 = UTF-16
    val stringsStartAt = out.position
    out.u32(0)
    out.u32(0)                             // styles start (none)

    // The string data, assembled separately so each entry's offset (from the data start) is
    // known before the offset table is written.
    val data = Buffer()

    val offsets = entries.map: entry =>
      val offset = data.position
      val chars = entry.s
      data.u16(chars.length)
      for index <- 0 until chars.length do data.u16(chars.charAt(index).toInt)
      data.u16(0)                          // NUL terminator
      offset

    for offset <- offsets do out.u32(offset.toLong)
    val stringsStart = out.position - sizeAt + 4
    out.patchU32(stringsStartAt, stringsStart.toLong)
    for byte <- data.data do out.u8(byte.toInt)
    out.align4()
    out.patchU32(sizeAt, (out.position - sizeAt + 4).toLong)

  private def writeResourceMap(out: Buffer, ids: List[Int]): Unit =
    if ids.nonEmpty then
      out.u16(0x0180)
      out.u16(0x0008)
      out.u32((8 + 4*ids.length).toLong)
      for id <- ids do out.u32(id & 0xffffffffL)

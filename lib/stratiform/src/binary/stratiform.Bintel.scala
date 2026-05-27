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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package stratiform

import java.io.ByteArrayOutputStream

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import vacuous.*

// BinTEL §7 node encoding. Serialises a typed `TelElement` tree into
// the binary form defined by `spec/bintel.md` — no magic number, no
// schema signature; the output is exactly the document-root body
// described in §7.1, suitable for §3 value-hashing.
//
// §7.1 forms:
//   - Document root (TelElement.Node with keywordIndex = Unset):
//       child-count : varint, then each child in canonical order.
//   - Struct node (Node with elementType = Tels.Struct):
//       keyword-index : varint, child-count : varint, recursive children.
//   - Flag node (Node with elementType = Tels.Flag):
//       keyword-index : varint.
//   - Scalar node (TelElement.Value):
//       keyword-index : varint, byte-length : varint, UTF-8 value bytes.
//
// Reference types do not appear: the type-assignment phase resolves
// them to Struct / Scalar / Flag before producing TelElement.

extension (tel: Tel)
  // Encode this document's semantic model to BinTEL body bytes (no
  // magic number, no schema signature). Type-assigns `tel` against
  // `schema` first; raises `TelError` on type-assignment failures.
  def bintel(schema: Tels): Data raises TelError =
    Bintel.encode(Tel.Type.assign(tel, schema))

extension (element: TelElement)
  // Encode a pre-assigned semantic-model element to BinTEL body bytes.
  def bintel: Data = Bintel.encode(element)

object Bintel:

  // Encode a `TelElement` tree to its BinTEL body bytes. The element is
  // expected to be the document root (a Node with `keywordIndex = Unset`
  // and `elementType = Tels.Struct`), as produced by `Tel.Type.assign`.
  def encode(element: TelElement): Data =
    val out = new ByteArrayOutputStream
    encodeRoot(out, element)
    out.toByteArray.asInstanceOf[IArray[Byte]]

  private def encodeRoot(out: ByteArrayOutputStream, element: TelElement): Unit = element match
    case TelElement.Node(_, _, children) =>
      writeVarint(out, children.length.toLong)
      var i = 0

      while i < children.length do
        encodeElement(out, children(i))
        i += 1

    case _: TelElement.Value =>
      writeVarint(out, 1L)
      encodeElement(out, element)

  private def encodeElement(out: ByteArrayOutputStream, element: TelElement): Unit =
    element match
      case node: TelElement.Node   => encodeNode(out, node)
      case value: TelElement.Value => encodeValue(out, value)

  private def encodeNode(out: ByteArrayOutputStream, node: TelElement.Node): Unit =
    val kidx = node.keywordIndex.or(0).toLong
    writeVarint(out, kidx)

    node.elementType match
      case _: Tels.Struct =>
        writeVarint(out, node.children.length.toLong)
        var i = 0

        while i < node.children.length do
          encodeElement(out, node.children(i))
          i += 1

      case Tels.Flag =>
        // Flag nodes carry no children and no length.
        ()

      case _: Tels.Scalar | _: Tels.Reference =>
        // Should not appear in a well-formed TelElement.Node after type
        // assignment — scalars are TelElement.Value and references are
        // resolved during assignment. Encode no further bytes.
        ()

  private def encodeValue(out: ByteArrayOutputStream, value: TelElement.Value): Unit =
    writeVarint(out, value.keywordIndex.toLong)
    val bytes = value.text.s.getBytes("UTF-8")
    writeVarint(out, bytes.length.toLong)
    out.write(bytes)

  private def writeVarint(out: ByteArrayOutputStream, value: Long): Unit =
    var n = value

    while n >= 0x80L do
      out.write(((n & 0x7fL) | 0x80L).toInt)
      n >>>= 7

    out.write(n.toInt)

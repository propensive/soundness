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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import gastronomy.*, providers.soundnessProvider
import prepositional.*

// BinTEL §7 node encoding. Serialises a typed `Tel.Element` tree into
// the binary form defined by `spec/bintel.md` — no magic number, no
// schema signature; the output is exactly the document-root body
// described in §7.1, suitable for §3 value-hashing.
//
// §7.1 forms:
//   - Document root (Tel.Element.Node with keywordIndex = Unset):
//       child-count : varint, then each child in canonical order.
//   - Struct node (Node with elementType = Tels.Struct):
//       keyword-index : varint, child-count : varint, recursive children.
//   - Flag node (Node with elementType = Tels.Flag):
//       keyword-index : varint.
//   - Scalar node (Tel.Element.Value):
//       keyword-index : varint, byte-length : varint, UTF-8 value bytes.
//
// Reference types do not appear: the type-assignment phase resolves
// them to Struct / Scalar / Flag before producing Tel.Element.

extension (tel: Tel)
  // Encode this document's semantic model to BinTEL body bytes (no
  // magic number, no schema signature). Type-assigns `tel` against
  // `schema` first; raises `Tel.Error` on type-assignment failures.
  def bintel(schema: Tels)(using Tactic[Tel.Error], Tactic[Bintel.Error]): Data =
    Bintel.encode(Tel.Type.assign(tel, schema), schema)

  // As above, writing scalars with a declared encoding (§21.7) as the
  // bound codec's bytes.
  def bintel(schema: Tels, codecs: Tel.Codec.Bindings)
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    Bintel.encode(Tel.Type.assign(tel, schema, Tel.Validator.Registry.builtins, codecs),
      schema, codecs)

  // BLAKE3 digest of this document's BinTEL body (§3 value hash). The
  // hash is taken over the body bytes only — no magic number, no
  // schema signature — and is therefore a function of the semantic
  // model and the schema alone, independent of presentation form.
  def valueHash(schema: Tels)(using Tactic[Tel.Error], Tactic[Bintel.Error])
  :   Digest in Blake3 =

    tel.bintel(schema).digest[Blake3]

  // Encode this document as a complete §6 BinTEL byte sequence —
  // magic + document length + signature length + signature + body. The signature length
  // must be a valid palimpsest length under some `(H, k_i, k_r)`;
  // otherwise raises `Bintel.Error(BadSignatureLength)`.
  // Declared with explicit tactics rather than stacked `raises`: under capture checking
  // a stacked context-function result whose inner level uses the outer tactic cannot
  // unify its capture with the declared result capability (3.10 toolchain).
  def bintelDocument(schema: Tels, signature: Data)
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    Bintel.frame(tel.bintel(schema), signature)

extension (element: Tel.Element)
  // Encode a pre-assigned semantic-model element to BinTEL body bytes.
  // The schema supplies the member layout needed for §7.2 canonical
  // child order (variant counts of `SelectRef` members).
  def bintel(schema: Tels): Data raises Bintel.Error = Bintel.encode(element, schema)

  // As above, writing scalars with a declared encoding (§21.7) as the
  // bound codec's bytes.
  def bintel(schema: Tels, codecs: Tel.Codec.Bindings)
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    Bintel.encode(element, schema, codecs)

  // BLAKE3 digest of this element's BinTEL body (§3 value hash).
  def valueHash(schema: Tels): Digest in Blake3 raises Bintel.Error =
    element.bintel(schema).digest[Blake3]

  def valueHash(schema: Tels, codecs: Tel.Codec.Bindings)
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Digest in Blake3 =

    element.bintel(schema, codecs).digest[Blake3]

extension [value: Tel.Encodable](value: value)
  // Encode any value to BinTEL body bytes, deriving the schema from its type:
  // `value.bintel` is `value.encode.bintel(Tels.tels[value](…))`. The schema name is
  // internal (a BinTEL body never embeds it), so a decoder that derives the schema from
  // the same type agrees on the layout regardless of the chosen name.
  def bintel
    ( using value is TelSchematic over Tels.Type )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    value.encode.bintel(Tels.tels[value](Text("root")))

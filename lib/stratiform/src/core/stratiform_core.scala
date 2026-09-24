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

import anticipation.*
import contextual.*
import contingency.*
import gastronomy.*, providers.soundnessProvider
import prepositional.*
import rudiments.*
import vacuous.*

// Encodes any value with an `Encodable in Tel` instance to its `Tel` form.
// Mirrors jacinta's `.json`, xylophone's `.xml`, ypsiloid's `.yaml`, etc.
extension [entity: Encodable in Tel](value: entity) def tel: Tel = value.encode

// `tel"…"` extension on StringContext routes through contextual's
// interpolation framework; the actual macro lives in `stratiform.internal`.
// Mirrors `extension (inline context: StringContext) def j` from
// `lib/jacinta/src/core/jacinta_core.scala:230`.
extension (inline context: StringContext)
  transparent inline def tel: Interpolation = interpolation[Tel](context)

// Collection/optic helpers used by the `Tel2` codec and optic givens. They are pure functions of
// their arguments, so they live at package level rather than as members of the `Tel2` trait —
// referencing a trait member would make the codec/optic lambdas capture `Tel2.this`, which the
// pure `Encodable`/`Optic` SAMs reject under capture checking.

// The empty document — an `Optional`'s `Unset` encoding. It contributes no compound to a struct
// body (the same shape an empty collection produces), so the product encoder omits the field and
// both the text and binary formats decode it back to `Unset` via the field's `absent()` path.
private[stratiform] def emptyDocument: Tel =
  Tel(Tel.Document(Unset, Unset, Tel.LineEndings.Lf, 0,
      Array(Tel.Block(Array.empty, Unset, Array.empty, 0))))

// Encodes a collection by flattening each element's compound(s) into one document's children. The
// source is any `Traversable`, so the `List`/`Set`/`Sequence` encodables pass their native
// collections straight through; only the `Array` result at the end is stdlib-shaped.
private[stratiform] def collectionDocument[collection, value](values: collection)
  ( using encodable: value is Encodable in Tel )
  ( using traversable: collection is Traversable by value )
:   Tel =

  val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

  values.each: element =>
    encodable.encoded(element).subtree match
      case compound: Tel.Compound => buffer += compound
      case document: Tel.Document => document.children.each(buffer ++= _.compounds.readable)

  val compounds: Array[Tel.Compound]^{} = Array.from(buffer)

  Tel(Tel.Document(Unset, Unset, Tel.LineEndings.Lf, 0,
      Array(Tel.Block(Array.empty, Unset, compounds, 0))))

// As `collectionDocument`, but embedding each element in its §22.2 canonical child form
// (`constructed`), so nested records keep their inline runs under `Tel.canonical`.
private[stratiform] def constructedDocument[collection, value](values: collection)
  ( using encodable: value is Tel.Encodable )
  ( using traversable: collection is Traversable by value )
:   Tel =

  val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

  values.each: element =>
    encodable.constructed(element).subtree match
      case compound: Tel.Compound => buffer += compound
      case document: Tel.Document => document.children.each(buffer ++= _.compounds.readable)

  val compounds: Array[Tel.Compound]^{} = Array.from(buffer)

  Tel(Tel.Document(Unset, Unset, Tel.LineEndings.Lf, 0,
      Array(Tel.Block(Array.empty, Unset, compounds, 0))))

// Re-keys a replacement compound to the original child's keyword (so a positional optic update
// preserves field identity).
private[stratiform] def rewrap(original: Tel.Compound, replacement: Tel): Tel.Compound =
  replacement.subtree match
    case compound: Tel.Compound =>
      compound.copy(keyword = original.keyword)

    case document: Tel.Document =>
      original.copy(atoms = Array.empty[Tel.Atom], remark = Unset, children = document.children)

// Rebuilds a node with replaced children, preserving its document/compound shape.
private[stratiform] def rebuild(origin: Tel, children: Array[Tel.Block]^{}): Tel = origin.subtree match
  case document: Tel.Document => Tel.make(document.copy(children = children))
  case compound: Tel.Compound => Tel.make(compound.copy(children = children))

// Wraps a value as a compound under the given keyword (used to key map entries' key/value children).
private[stratiform] def reKey(tel: Tel, keyword: Text): Tel.Compound = tel.subtree match
  case c: Tel.Compound => c.copy(keyword = keyword)
  case d: Tel.Document => Tel.Compound(keyword, Array.empty, Unset, d.children)

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

extension [value: Tel.Encodable](value: value)
  // Fulfils an acceptance from this value (§8.4, writer obligations): the value is held under
  // its type's derived schema — base, layers and optional-member atoms — and the first
  // alternative that composition can serve is answered with the richest permitted composition,
  // as a framed document. `Unset` when no alternative can be served. (`fulfil`, as `serve` is
  // urticose's.)
  inline def fulfil
    ( acceptance: Tel.Acceptance, codecs: Tel.Codec.Bindings = Tel.Codec.Bindings.builtins )
    ( using schematic: value is TelSchematic over Tels.Type )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error], Tactic[Tels.Resolution.Error],
            Tactic[Tels.Renderer.Error] )
  :   Optional[Tel.Acceptance.Served] =

    val held = Tel.Acceptance.lineage[value](Tel.Acceptance.nameOf[value])
    val composition = held.base :: held.offered.map(_.hash)
    val element = Tel.Type.assign(value.encode, held.compose(composition))
    Tel.Acceptance.serve(acceptance, held, composition, element, codecs)

extension (acceptance: Tel.Acceptance)
  // The value hash (BinTEL §3) of an acceptance: the BLAKE3-256 digest of its bare form, the
  // same whichever of its forms is sent (§8.4).
  def valueHash(using Tactic[Tel.Error], Tactic[Bintel.Error]): Digest in Blake3 =
    acceptance.bintel.digest[Blake3]

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

import scala.language.dynamics

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import turbulence.*
import vacuous.*

// Presentation model from §17 of the TEL specification. The Scala AST is
// structurally identical to the reference implementation's AST so that
// upstream `.check` fixtures round-trip through a cross-language CheckTree
// (see stratiform.CheckFormat).
//
// `Tel` itself is a thin wrapper around a `Subtree` (either the document
// root or a single compound). Both Subtree variants share a `children:
// IArray[Block]` field, so flat traversal logic can address either form
// without case analysis.

class Tel private[stratiform](private[stratiform] val subtree: Tel.Subtree)
extends scala.Dynamic, Documentary:
  type Self = Tel
  type Metadata = Tel.Metadata

  inline def as[value: Decodable in Tel]: value raises TelError = value.decoded(this)

  // Dynamic field access: `tel.firstName` becomes a lookup for the kebab-
  // case keyword "first-name". Gated by DynamicTelEnabler so opting in is
  // explicit.
  def selectDynamic(field: String)(using erased DynamicTelEnabler)
  :     Tel raises TelError =
    val match0 = childCompounds.find(_.keyword == Tel.camelToKebab(field))
    if match0.isEmpty then Tel.empty else Tel(match0.get)

  // Chained access: `tel.contacts(0)` after the field lookup. Phase-2
  // returns the n-th child compound (any keyword), matching list-style
  // access.
  def applyDynamic(field: String)(index: Int)(using erased DynamicTelEnabler)
  :     Tel raises TelError =
    val parent = selectDynamic(field)
    val cs = parent.childCompounds
    if index < 0 || index >= cs.length then Tel.empty else Tel(cs(index))

  // Keyword of this node — empty for the document root, otherwise the
  // compound's keyword text.
  def keyword: Text = subtree match
    case c: Tel.Compound  => c.keyword
    case _: Tel.Document  => t""

  // Flat list of inline atom texts attached to this node. For the document
  // root this is always empty since the root has no atoms.
  def atomTexts: IArray[Text] = subtree match
    case c: Tel.Compound => c.atoms.collect { case Tel.Atom.Inline(text, _) => text }
    case _: Tel.Document => IArray.empty

  // First inline atom text or empty string if none. Used by primitive
  // Decodable instances which interpret a compound's first atom as its
  // scalar value.
  def primaryAtom: Text =
    if atomTexts.isEmpty then t"" else atomTexts(0)

  // All child compounds, flattened across the node's blocks (presentation-
  // level comments and tabulations are dropped from this view).
  def childCompounds: IArray[Tel.Compound] =
    subtree.children.flatMap(_.compounds)

  // First child compound whose keyword matches `target`, if any.
  def field(target: Text): Optional[Tel] =
    val matched = childCompounds.find(_.keyword == target)
    if matched.isEmpty then Unset else Tel(matched.get)

  // All child compounds whose keyword matches `target`. Useful for
  // schema-repeatable fields that produce multiple compounds with the
  // same keyword in the presentation tree.
  def fields(target: Text): IArray[Tel] =
    childCompounds.filter(_.keyword == target).map(c => Tel(c))

  // Document accessor for downstream operations (printing, mutation). Only
  // meaningful when this Tel wraps a Document.
  private[stratiform] def document: Optional[Tel.Document] = subtree match
    case d: Tel.Document => d
    case _               => Unset

  // Replace (or insert) the child compound with the given field name.
  // Used by the §22 `modify` mutation primitive and the Panopticon Lens
  // given. The new compound carries `value`'s atoms, remark, and
  // children; its keyword is taken from `fieldName` (translated to
  // kebab-case). When the field is missing it is appended to the last
  // block; when present it is replaced in place, preserving surrounding
  // formatting.
  def modify(fieldName: String, value: Tel)(using erased DynamicTelEnabler): Tel =
    val name = Tel.camelToKebab(fieldName)
    val newCompound = value.subtree match
      case c: Tel.Compound => c.copy(keyword = name)
      case d: Tel.Document =>
        Tel.Compound(name, IArray.empty[Tel.Atom], Unset, d.children)

    val newChildren = Tel.replaceOrAppendCompound(subtree.children, name, newCompound)
    val newSubtree = subtree match
      case d: Tel.Document => d.copy(children = newChildren)
      case c: Tel.Compound => c.copy(children = newChildren)

    Tel.make(newSubtree)

object Tel extends Tel2:

  enum LineEndings:
    case Lf, Crlf

  case class Pragma
    ( version: (Int, Int), schema: Optional[Text], sigil: Optional[Char] )

  // Document-level prologue carried alongside a `Tel` value when it is
  // loaded via `text.load[Tel]`. The `Document[Tel]` pair lets callers
  // read the schema identifier, interpreter directive, and chosen line
  // endings without inspecting the presentation AST directly.
  case class Metadata
       ( interpreterDirective: Optional[Text],
         pragma:               Optional[Pragma],
         lineEndings:          LineEndings )

  sealed trait Subtree:
    def children: IArray[Block]

  case class Document
    ( interpreterDirective: Optional[Text],
      pragma:               Optional[Pragma],
      lineEndings:          LineEndings,
      children:             IArray[Block] )
  extends Subtree

  case class Block
    ( comments:           IArray[Comment],
      tabulation:         Optional[Tabulation],
      compounds:          IArray[Compound],
      trailingBlankLines: Int )

  case class Comment(text: Text)

  case class Tabulation(markerOffsets: IArray[Int], headings: IArray[Text])

  case class Compound
    ( keyword:  Text,
      atoms:    IArray[Atom],
      remark:   Optional[Text],
      children: IArray[Block] )
  extends Subtree

  object Atom:
    case class Inline(text: Text, precedingSpaces: Int) extends Atom
    case class Source(text: Text)                       extends Atom
    case class Literal(delimiter: Text, text: Text)     extends Atom

  sealed trait Atom

  // Parse a byte stream into a Tel value wrapping the document. Used
  // internally by the Aggregable and Loadable typeclasses; user code
  // should prefer `bytes.read[Tel]` or `text.load[Tel]`.
  private[stratiform] def parse(bytes: Data): Tel raises TelError =
    Tel(TelParser.parse(bytes))

  // `bytes.read[Tel]` for any Stream[Data] source: concatenates the
  // chunks and parses the result. The metadata (interpreter directive,
  // pragma, line-endings) is *not* surfaced — use `.load[Tel]` to
  // recover those alongside the value.
  given aggregable: Tactic[TelError] => Tel is Aggregable by Data = source =>
    import denominative.nil
    var acc    = IArray.empty[Byte]
    var stream = source
    while !stream.nil do
      acc = acc ++ stream.head
      stream = stream.tail

    parse(acc)

  // `text.load[Tel]` for any Stream[Text] source: concatenates the
  // chunks, UTF-8 encodes, parses, and pairs the resulting Tel with a
  // `Tel.Metadata` carrying the document's prologue.
  given loadable: Tactic[TelError] => Tel is Loadable by Text = stream =>
    import denominative.nil
    val builder = new StringBuilder()
    var s = stream
    while !s.nil do
      builder.append(s.head.s)
      s = s.tail

    val text = builder.toString
    val bytes = text.getBytes("UTF-8").nn
    val doc = TelParser.parse(IArray.unsafeFromArray(bytes))
    val meta = Tel.Metadata(doc.interpreterDirective, doc.pragma, doc.lineEndings)
    turbulence.Document(Tel(doc): Tel, meta)

  // Print the document presentation (presentation-preserving when given a
  // Tel produced by `parse`).
  def show(tel: Tel): Text = tel.document.lay(t"")(TelPrinter.print)

  def show(document: Document): Text = TelPrinter.print(document)

  // Macro-friendly factory: bypasses the private constructor so generated
  // code from the `tel"…"` interpolator can produce Tel values without
  // tripping the inline-private-constructor restriction.
  def make(subtree: Subtree): Tel = new Tel(subtree)

  // camelCase → kebab-case: insert `-` before each interior uppercase
  // letter and lowercase it. `firstName` → `first-name`. Used by the
  // dynamic accessor to map Scala identifier names to TEL keywords.
  private[stratiform] def camelToKebab(s: String): Text =
    val sb = StringBuilder()
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if c >= 'A' && c <= 'Z' then
        if i > 0 then sb.append('-')
        sb.append((c - 'A' + 'a').toChar)
      else sb.append(c)

      i += 1

    Text(sb.toString)

  // Replace the first compound with the given keyword across all
  // blocks; if no compound matches, append the new compound to the
  // last block (creating a fresh block if there are none).
  private[stratiform] def replaceOrAppendCompound
       (blocks: IArray[Block], keyword: Text, compound: Compound)
  :     IArray[Block] =
    var b = 0
    var found = false
    val out = scala.collection.mutable.ArrayBuffer.from(blocks.toList)
    while b < out.length && !found do
      val cs = out(b).compounds
      val idx = cs.indexWhere(_.keyword == keyword)
      if idx >= 0 then
        out(b) = out(b).copy(compounds = cs.updated(idx, compound))
        found = true

      b += 1

    if !found then
      if out.isEmpty then out += Block(IArray.empty, Unset, IArray(compound), 0)
      else
        val lastIdx = out.length - 1
        out(lastIdx) = out(lastIdx).copy(compounds = out(lastIdx).compounds :+ compound)

    IArray.from(out)

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

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
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

class Tel private[stratiform](private[stratiform] val subtree: Tel.Subtree):
  // Keyword of this node — empty for the document root, otherwise the
  // compound's keyword text.
  def keyword: Text = subtree match
    case c: Tel.Compound  => c.keyword
    case _: Tel.Document  => Text("")

  // Flat list of inline atom texts attached to this node. For the document
  // root this is always empty since the root has no atoms.
  def atomTexts: IArray[Text] = subtree match
    case c: Tel.Compound => c.atoms.collect { case Tel.Atom.Inline(text, _) => text }
    case _: Tel.Document => IArray.empty

  // First inline atom text or empty string if none. Used by primitive
  // Decodable instances which interpret a compound's first atom as its
  // scalar value.
  def primaryAtom: Text =
    if atomTexts.isEmpty then Text("") else atomTexts(0)

  // All child compounds, flattened across the node's blocks (presentation-
  // level comments and tabulations are dropped from this view).
  def childCompounds: IArray[Tel.Compound] =
    subtree.children.flatMap(_.compounds)

  // First child compound whose keyword matches `target`, if any.
  def field(target: Text): Optional[Tel] =
    val matched = childCompounds.find(_.keyword == target)
    if matched.isEmpty then Unset else Tel(matched.get)

  // Document accessor for downstream operations (printing, mutation). Only
  // meaningful when this Tel wraps a Document.
  private[stratiform] def document: Optional[Tel.Document] = subtree match
    case d: Tel.Document => d
    case _               => Unset

object Tel:

  enum LineEndings:
    case Lf, Crlf

  case class Pragma
    ( version: (Int, Int), schema: Optional[Text], sigil: Optional[Char] )

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

  // Parse a byte stream into a Tel value wrapping the document. Phase-1
  // contract: untyped, presentation model only.
  def parse(bytes: Data): Tel raises TelError = Tel(TelParser.parse(bytes))

  // Lower-level parse returning the raw Document — used by code that
  // needs the presentation AST directly (e.g. the round-trip printer).
  def parseDocument(bytes: Data): Document raises TelError = TelParser.parse(bytes)

  // Print the document presentation (presentation-preserving when given a
  // Tel produced by `parse`).
  def show(tel: Tel): Text = tel.document.lay(Text(""))(TelPrinter.print)

  def show(document: Document): Text = TelPrinter.print(document)

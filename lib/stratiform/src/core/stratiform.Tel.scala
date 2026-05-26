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
import vacuous.*

// Presentation model defined in §17 of the TEL specification. The Scala AST
// is structurally identical to the reference implementation's AST so that
// upstream `.check` fixtures round-trip through a cross-language CheckTree
// representation (see stratiform.CheckFormat for the comparison reader).

object Tel:

  enum LineEndings:
    case Lf, Crlf

  case class Pragma
    ( version: (Int, Int), schema: Optional[Text], sigil: Optional[Char] )

  case class Document
    ( interpreterDirective: Optional[Text],
      pragma:               Optional[Pragma],
      lineEndings:          LineEndings,
      children:             IArray[Block] )

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

  object Atom:
    case class Inline(text: Text, precedingSpaces: Int) extends Atom
    case class Source(text: Text)                       extends Atom
    case class Literal(delimiter: Text, text: Text)     extends Atom

  sealed trait Atom

  // The entry point. Phase-1 contract: parse an untyped document, returning
  // only the presentation model. Schema-driven type assignment is added in
  // phase 3; the resulting `Document` is unchanged when no schema is in
  // scope.
  def parse(bytes: Data): Document raises TelError = TelParser.parse(bytes)

  // For symmetry with parse; phase 1 prints the presentation model only.
  // `show` mirrors the jacinta naming convention (`json.show`) but is a
  // standalone helper rather than a `Showable` instance to avoid coupling
  // the AST to spectacular for now.
  def show(document: Document): Text = TelPrinter.print(document)

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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package xylophone

import anticipation.*
import contingency.*
import vacuous.*
import zephyrine.*

object XmlReader:
  // Sentinels of `childWord()`; impossible as packed names, whose chars are
  // all 7-bit ASCII.
  inline final val NameEnd = -1L
  inline final val NameOpaque = -2L

  // Only xylophone's read path (`Xml.parseDirect`) constructs readers, so the
  // exclusivity of the wrapped parser and the resolution scope of the carried
  // capabilities are preserved by construction. The wrapped capabilities
  // travel as neutral carriers (jacinta's `JsonReader` pattern): the fields
  // stay pure, and each accessor reasserts the type at the rim — the audited
  // point.
  private[xylophone] def apply
    ( parser:    Xml.XmlParser^,
      tactic:    Tactic[ParseError],
      xmlTactic: Tactic[XmlError],
      foci:      Foci[Xml.Focus] )
  :   XmlReader^ =

    new XmlReader
      ( parser.asInstanceOf[AnyRef],
        tactic.asInstanceOf[AnyRef],
        xmlTactic.asInstanceOf[AnyRef],
        foci.asInstanceOf[AnyRef] )

// The public, restricted rim of the XML parser, handed to `Xml.Parsable`
// instances so they can consume elements straight off the input without an
// intermediate `Xml` tree. `parse` is invoked with the current element just
// *opened* (its name and attributes consumed); exactly one of the content
// consumers — `nextChild` until `Unset`, `text`, `skipElement` or `element` —
// must then consume the element's content and its close tag in full.
//
// The reader carries its own `Tactic[ParseError]`, so malformed input aborts
// through the read call's ambient tactic — and, unlike jacinta's reader, the
// read-site `Tactic[XmlError]` and `Foci[Xml.Focus]` too, so decode errors
// raised by `Parsable` instances accrue to the same `validate` boundary the
// AST path's inline derivation uses, with the same field foci, even when the
// `Parsable` given was instantiated outside the boundary.
//
// An exclusive, stateful capability, like the parser it wraps: it is owned
// by one `Xml.Parsable.parse` call at a time, for the duration of that call,
// and nothing of it may be retained afterwards.
final class XmlReader private
  ( parser0: AnyRef, tactic0: AnyRef, xmlTactic0: AnyRef, foci0: AnyRef )
extends caps.ExclusiveCapability, caps.Stateful:
  private inline def parser: Xml.XmlParser^ = parser0.asInstanceOf[Xml.XmlParser^]

  private[xylophone] inline def parseTactic: Tactic[ParseError] =
    tactic0.asInstanceOf[Tactic[ParseError]]

  // The read-site capabilities, public because staged parsers — generated
  // into user modules — bind them once per record for focus bookkeeping and
  // absent-field raising, exactly as the derived engine does.
  inline def errorTactic: Tactic[XmlError] =
    xmlTactic0.asInstanceOf[Tactic[XmlError]]

  inline def foci: Foci[Xml.Focus] = foci0.asInstanceOf[Foci[Xml.Focus]]

  // The attributes of the just-opened element, valid until the next element
  // is opened. The derived product parser reads them before its child loop,
  // so `@attribute` fields are filled before any child is consumed.
  update def attributes(): Attributes = parser.directAttributes()

  // Steps within the current element: the name of the next child element
  // (opened — its name and attributes consumed), or `Unset` once the close
  // tag is consumed and validated. Character data, comments, CDATA sections
  // and processing instructions between child elements are consumed
  // transparently — the AST derivation looks only at `Element` children.
  update def nextChild(): Optional[Text] =
    val name = parser.directNextChild()(using parseTactic)
    if name == null then Unset else name.nn

  // The next child step in packed form, for parsers that compare names
  // against literal constants (staged parsers compile field names to
  // immediates): the packed low word of the child's name (its high word from
  // `childWordHigh`), `NameEnd` once the close tag is consumed, or
  // `NameOpaque` when the name cannot pack — the child is still opened, and
  // `childLabel` identifies it for a general dispatch. Public because staged
  // parsers are generated into user modules.
  update def childWord(): Long = parser.directNextChildWord()(using parseTactic)

  update def childWordHigh: Long = parser.directChildWordHigh

  update def childLabel: Text = parser.directChildLabel

  // The current element's text content, consumed together with its close
  // tag: `Unset` when the content is not exactly one text run (mirroring
  // `textOf`'s shape rules, under which CDATA is *not* text). Backs the
  // primitive and text-codec parsers.
  update def text(): Optional[Text] =
    val text = parser.directText()(using parseTactic)
    if text == null then Unset else text.nn

  // Skips the current element's entire subtree, validating every close tag
  // on the way, building nothing — for unknown child elements and duplicate
  // occurrences of a field.
  update def skipElement(): Unit = parser.directSkipElement()(using parseTactic)

  // The fallback seam: materialize the current element as an `Xml` tree, for
  // field types that only carry a `Decodable in Xml`.
  update def element(): Xml = parser.directElement()(using parseTactic)

  // Raise an `XmlError` through the read-site tactic and continue — for leaf
  // instances that reject an element's content, preserving the AST
  // primitives' raise-and-continue accrual.
  update def fault(): Unit = raise(XmlError())(using errorTactic)

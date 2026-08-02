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

import soundness.*

import proscenium.compat.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import Tel.given

// The differential property issue #1694 proposes: for a document and a schema
// derived from the corresponding Scala type, decoding through the codecs must
// agree with decoding through `Tel.Type.assign` — a divergence between the
// two layers is caught even when both "succeed", by projecting the semantic
// element tree and comparing it against a hand-written oracle.
object EquivalenceTests extends Suite(m"Stratiform schema/codec equivalence tests"):

  case class Issues(items: List[(Text, TelError)] = Nil)(using Diagnostics)
  extends Error(m"${items.length} validation issues"):
    def +(focus: Text, error: TelError): Issues = Issues(items :+ (focus, error))

  private def validateAssign(tel: Tel, schema: Tels): Issues =
    validate[Tel.Focus](Issues()):
      case error: TelError =>
        // Explicit: `Tel.given` is in scope, whose blanket Tel encodable
        // would otherwise beat `TelPath`'s text form under `.encode`.
        accrual + (prior.let { focus => TelPath.encodable.encoded(focus.pointer) }.or(t"#"), error)
    . protect(Tel.Type.assign(tel, schema))

  // Projects the semantic tree to (path-of-flat-keyword-indexes, text) pairs
  // in depth-first order; a Flag node projects as "+". This is the oracle the
  // decoded value's fields are checked against.
  private def scalars(element: Tel.Element): scala.collection.immutable.List[(String, String)] =
    val buffer = scala.collection.mutable.ListBuffer.empty[(String, String)]

    def recur(element: Tel.Element, prefix: String): Unit = element match
      case Tel.Element.Value(idx, _, text) => buffer += ((prefix + "/" + idx, text.s))

      case Tel.Element.Node(idx, Tels.Flag, _) =>
        buffer += ((prefix + "/" + idx.or(-1), "+"))

      case Tel.Element.Node(idx, _, children) =>
        val next = idx.lay(prefix)(prefix + "/" + _)
        var i = 0

        while i < children.length do
          recur(children.readable(i), next)
          i += 1

    recur(element, "#")
    buffer.toList

  // The full equivalence check for one fixture: both codec paths produce
  // `expected`; type assignment against `schema` accrues nothing and its
  // scalar projection matches `oracle`; and the encoded form of `expected`
  // decodes back and validates cleanly too.
  private inline def equivalent[value](document: Text, schema: Tels, expected: value,
      oracle: scala.collection.immutable.List[(String, String)])
    ( using value is Tel.Parsable, value is Tel.Decodable, value is Tel.Encodable )
  :   Boolean =

    val parsed  = document.read[Tel]
    val encoded = expected.encode

    document.read[Tel].as[value] == expected
    && document.read[value in Tel] == expected
    && validateAssign(parsed, schema).items.length == 0
    && scalars(Tel.Type.assign(parsed, schema)) == oracle
    && encoded.as[value] == expected
    && validateAssign(encoded, schema).items.length == 0

  def run(): Unit =
    suite(m"Codec/assign equivalence (positive)"):
      given PRecipient is Tel.Parsable = Tel.Parsable.derived
      given PDelivery is Tel.Parsable = Tel.Parsable.derived
      given PLog is Tel.Parsable = Tel.Parsable.derived
      given PLogBook is Tel.Parsable = Tel.Parsable.derived
      given PNote is Tel.Parsable = Tel.Parsable.derived

      test(m"positional atoms agree across codecs and type assignment (#1694)"):
        equivalent[PDelivery]
         ( t"recipient  Acme Corporation\n  address  1 Acme Way\n",
           Tels.tels[PDelivery](t"delivery"),
           PDelivery(PRecipient(t"Acme Corporation", t"1 Acme Way")),
           scala.collection.immutable.List
            ( ("#/0/0", "Acme Corporation"),
              ("#/0/1", "1 Acme Way") ) )
      . assert(identity)

      test(m"a repeatable split between atoms and children agrees"):
        equivalent[PLogBook]
         ( t"log lbl 1\n  values 2\n",
           Tels.tels[PLogBook](t"logbook"),
           PLogBook(PLog(t"lbl", List(1, 2))),
           scala.collection.immutable.List
            ( ("#/0/0", "lbl"),
              ("#/0/1", "1"),
              ("#/0/1", "2") ) )
      . assert(identity)

      test(m"the canonical form validates against the derived schema"):
        val doc = Tel.canonical(PDelivery(PRecipient(t"Acme Corporation", t"1 Acme Way")))
        validateAssign(doc, Tels.tels[PDelivery](t"delivery")).items.length
      . assert(_ == 0)

      test(m"a source atom agrees across codecs and type assignment"):
        equivalent[PNote]
         ( t"body\n    line one\n    line two\n",
           Tels.tels[PNote](t"note"),
           PNote(t"line one\nline two"),
           scala.collection.immutable.List(("#/0", "line one\nline two")) )
      . assert(identity)

    suite(m"Codec/assign equivalence (negative)"):
      test(m"both layers reject an atom overflow with E302"):
        val doc = t"recipient a b c\n"
        val schema = Tels.tels[PDelivery](t"delivery")
        val schemaReasons = validateAssign(doc.read[Tel], schema).items.map(_(1).reason).to[Set]
        val codecReason = capture[TelError](doc.read[Tel].as[PDelivery]).reason
        (schemaReasons, codecReason)
      . assert(_ == (Set(TelError.Reason.TooManyAtoms), TelError.Reason.TooManyAtoms))

    suite(m"Codec/assign equivalence (flags, hand-built schema)"):
      // The schema derivation does not yet model Boolean fields as Flag
      // members, so the flag fixture pairs the codecs against a hand-built
      // schema with true Flag members.
      val flagsSchema = Tels(
        name     = t"flags",
        document = Tels.Struct(
          members = Array.of(
            Tels.Field(Tels.Polarity.Loose, Tels.Polarity.Implicit, t"active", Tels.Flag, Unset),
            Tels.Field(Tels.Polarity.Loose, Tels.Polarity.Implicit, t"verbose", Tels.Flag, Unset)),
          validators = Array.empty),
        layers   = Array.empty,
        sigil    = Unset,
        records  = Array.empty,
        scalars  = Array.empty,
        selects  = Array.empty)

      test(m"a bare flag agrees across codecs and type assignment"):
        val doc = t"active\n"
        val decoded = doc.read[Tel].as[PFlags] == PFlags(true, Unset)
        val issues = validateAssign(doc.read[Tel], flagsSchema).items.length
        val oracle = scalars(Tel.Type.assign(doc.read[Tel], flagsSchema))
        (decoded, issues, oracle)
      . assert(_ == (true, 0, scala.collection.immutable.List(("#/0", "+"))))

      test(m"the encoded flag form validates against the flag schema"):
        val encoded = PFlags(true, Unset).encode
        val issues = validateAssign(encoded, flagsSchema).items.length
        (encoded.as[PFlags], issues)
      . assert(_ == (PFlags(true, Unset), 0))

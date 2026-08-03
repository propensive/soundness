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

// Positional-assignment fixtures (§19.2, issue #1694). The positional cases
// nest the record one level down, because the document root never carries
// atoms (§20.2).
case class PRecipient(name: Text, address: Optional[Text]) derives CanEqual
case class PDelivery(recipient: Optional[PRecipient]) derives CanEqual
case class PTriple(one: Text, two: Optional[Text]) derives CanEqual
case class PHolder(item: PTriple) derives CanEqual
case class PLog(label: Text, values: List[Int]) derives CanEqual
case class PLogBook(log: PLog) derives CanEqual
case class PFlags(active: Boolean, verbose: Optional[Boolean]) derives CanEqual
case class PPair(first: Optional[Text], second: Optional[Text]) derives CanEqual
case class PPairBox(pair: PPair) derives CanEqual
case class PNote(body: Text) derives CanEqual
case class PGuard(flag: Boolean, word: Text) derives CanEqual
case class PGuardBox(item: PGuard) derives CanEqual

object PositionalTests extends Suite(m"Stratiform positional assignment tests"):
  def run(): Unit =
    suite(m"Positional atom decoding (§19.2, AST path)"):
      test(m"inline atoms assign positionally to record fields (#1694)"):
        t"recipient  Acme Corporation\n  address  1 Acme Way\n".read[Tel].as[PDelivery]
      . assert(_ == PDelivery(PRecipient(t"Acme Corporation", t"1 Acme Way")))

      test(m"keyword-child form decodes identically to the positional form"):
        val positional = t"recipient  Acme Corporation\n  address  1 Acme Way\n"
        val explicit   = t"recipient\n  name  Acme Corporation\n  address  1 Acme Way\n"
        positional.read[Tel].as[PDelivery] == explicit.read[Tel].as[PDelivery]
      . assert(identity)

      test(m"consecutive atoms fill consecutive scalar members"):
        t"item a xyz\n".read[Tel].as[PHolder]
      . assert(_ == PHolder(PTriple(t"a", t"xyz")))

      test(m"a repeatable field consumes every remaining atom"):
        t"log lbl 1 2 3\n".read[Tel].as[PLogBook]
      . assert(_ == PLogBook(PLog(t"lbl", List(1, 2, 3))))

      test(m"repeatable occurrences split between atoms and children"):
        t"log lbl 1\n  values 2\n".read[Tel].as[PLogBook]
      . assert(_ == PLogBook(PLog(t"lbl", List(1, 2))))

      // A Scala `Boolean` is a value, not a §20 flag: it reads and writes an
      // explicit `true`/`false` atom, the mapping the derived schema and
      // BinTEL also use.
      test(m"a Boolean field reads its explicit atom"):
        t"active true\nverbose false\n".read[Tel].as[PFlags]
      . assert(_ == PFlags(true, false))

      test(m"an absent Optional Boolean reads as Unset"):
        t"active false\n".read[Tel].as[PFlags]
      . assert(_ == PFlags(false, Unset))

      test(m"only the first optional scalar fills positionally (§20.8)"):
        t"pair hello\n".read[Tel].as[PPairBox]
      . assert(_ == PPairBox(PPair(t"hello", Unset)))

      test(m"a second optional scalar arrives as an explicit child"):
        t"pair hello\n  second world\n".read[Tel].as[PPairBox]
      . assert(_ == PPairBox(PPair(t"hello", t"world")))

      test(m"an atom plus a same-keyword child raises E308"):
        capture[TelError](t"log lbl\n  label dup\n".read[Tel].as[PLogBook]).reason
      . assert(_ == TelError.Reason.NonRepeatableTooMany)

      test(m"excess atoms raise E302"):
        capture[TelError](t"item a b c\n".read[Tel].as[PHolder]).reason
      . assert(_ == TelError.Reason.TooManyAtoms)

    suite(m"Positional atom decoding (§19.2, direct path)"):
      given PRecipient is Tel.Parsable = Tel.Parsable.derived
      given PDelivery is Tel.Parsable = Tel.Parsable.derived
      given PTriple is Tel.Parsable = Tel.Parsable.derived
      given PHolder is Tel.Parsable = Tel.Parsable.derived
      given PLog is Tel.Parsable = Tel.Parsable.derived
      given PLogBook is Tel.Parsable = Tel.Parsable.derived
      given PFlags is Tel.Parsable = Tel.Parsable.derived
      given PPair is Tel.Parsable = Tel.Parsable.derived
      given PPairBox is Tel.Parsable = Tel.Parsable.derived

      // The acceptance criterion: the direct read equals the AST-path read.
      inline def parity[value](tel: Text)(using value is Tel.Parsable, value is Tel.Decodable)
      :   Boolean =
        tel.read[value in Tel] == tel.read[Tel].as[value]

      test(m"inline atoms assign positionally, equally on both paths (#1694)"):
        val doc = t"recipient  Acme Corporation\n  address  1 Acme Way\n"
        (doc.read[PDelivery in Tel], parity[PDelivery](doc))
      . assert(_ == (PDelivery(PRecipient(t"Acme Corporation", t"1 Acme Way")), true))

      test(m"consecutive atoms fill consecutive members, equally on both paths"):
        val doc = t"item a xyz\n"
        (doc.read[PHolder in Tel], parity[PHolder](doc))
      . assert(_ == (PHolder(PTriple(t"a", t"xyz")), true))

      test(m"a repeatable field consumes the rest, equally on both paths"):
        val doc = t"log lbl 1 2 3\n"
        (doc.read[PLogBook in Tel], parity[PLogBook](doc))
      . assert(_ == (PLogBook(PLog(t"lbl", List(1, 2, 3))), true))

      test(m"repeatable occurrences split atoms/children, equally on both paths"):
        val doc = t"log lbl 1\n  values 2\n"
        (doc.read[PLogBook in Tel], parity[PLogBook](doc))
      . assert(_ == (PLogBook(PLog(t"lbl", List(1, 2))), true))

      test(m"Boolean fields read explicit atoms, equally on both paths"):
        val doc = t"active true\nverbose false\n"
        (doc.read[PFlags in Tel], parity[PFlags](doc))
      . assert(_ == (PFlags(true, false), true))

      test(m"optional scalars fill per §20.8, equally on both paths"):
        val doc = t"pair hello\n  second world\n"
        (doc.read[PPairBox in Tel], parity[PPairBox](doc))
      . assert(_ == (PPairBox(PPair(t"hello", t"world")), true))

      test(m"an atom plus a same-keyword child raises E308 on the direct path"):
        capture[TelError](t"log lbl\n  label dup\n".read[PLogBook in Tel]).reason
      . assert(_ == TelError.Reason.NonRepeatableTooMany)

      test(m"excess atoms raise E302 on the direct path"):
        capture[TelError](t"item a b c\n".read[PHolder in Tel]).reason
      . assert(_ == TelError.Reason.TooManyAtoms)

    suite(m"Encoder atom forms and flags (§22.3)"):
      given PNote is Tel.Parsable = Tel.Parsable.derived
      given PFlags2: (PFlags is Tel.Parsable) = Tel.Parsable.derived

      test(m"a multi-line Text encodes as a source atom and reparses"):
        val value = PNote(t"line one\nline two")
        val sourceForm =
          value.encode.childCompounds.readable.head.atoms(0).isInstanceOf[Tel.Atom.Source]

        (value.encode.show.s.tt.read[Tel].as[PNote] == value, sourceForm)
      . assert(_ == (true, true))

      test(m"an unsafe payload escalates to a literal atom and reparses"):
        val value = PNote(t"para one\n\npara two")
        val literalForm =
          value.encode.childCompounds.readable.head.atoms(0).isInstanceOf[Tel.Atom.Literal]

        (value.encode.show.s.tt.read[Tel].as[PNote] == value, literalForm)
      . assert(_ == (true, true))

      // A Boolean encodes as its explicit atom, and an absent `Optional`
      // contributes no compound — the form the derived schema and BinTEL
      // both expect.
      test(m"a Boolean encodes as an explicit true/false atom"):
        val doc = PFlags(true, Unset).encode
        (doc.childCompounds.readable.length, doc.childCompounds.readable.head.keyword,
         doc.childCompounds.readable.head.atoms.length)
      . assert(_ == (1, t"active", 1))

      test(m"a false Boolean is written, not omitted"):
        val doc = PFlags(false, false).encode
        (doc.childCompounds.readable.length, doc.field(t"active").vouch.primaryAtom)
      . assert(_ == (2, t"false"))

      test(m"every Boolean combination round-trips through serialized text"):
        val values = List
         ( PFlags(true, Unset), PFlags(false, Unset), PFlags(true, true),
           PFlags(false, false), PFlags(true, false), PFlags(false, true) )

        values.all: value =>
          val printed = value.encode.show.s.tt
          printed.read[Tel].as[PFlags] == value && printed.read[PFlags in Tel] == value
      . assert(identity)

      test(m"a multi-line Text round-trips on the direct path too"):
        val value = PNote(t"line one\nline two")
        value.encode.show.s.tt.read[PNote in Tel] == value
      . assert(identity)

    suite(m"Positional atom decoding (§19.2, staged path)"):
      given PRecipient is Tel.Parsable = Tel.Parsable.staged
      given PDelivery is Tel.Parsable = Tel.Parsable.staged
      given PTriple is Tel.Parsable = Tel.Parsable.staged
      given PHolder is Tel.Parsable = Tel.Parsable.staged
      given PLog is Tel.Parsable = Tel.Parsable.staged
      given PLogBook is Tel.Parsable = Tel.Parsable.staged
      given PFlags is Tel.Parsable = Tel.Parsable.staged
      given PPair is Tel.Parsable = Tel.Parsable.staged
      given PPairBox is Tel.Parsable = Tel.Parsable.staged
      given PNote is Tel.Parsable = Tel.Parsable.staged

      // The acceptance criterion for staged generation: the staged read must
      // equal the AST-path read, for the same input.
      inline def parity[value](tel: Text)(using value is Tel.Parsable, value is Tel.Decodable)
      :   Boolean =
        tel.read[value in Tel] == tel.read[Tel].as[value]

      test(m"inline atoms assign positionally, equally on both paths (#1699)"):
        val doc = t"recipient  Acme Corporation\n  address  1 Acme Way\n"
        (doc.read[PDelivery in Tel], parity[PDelivery](doc))
      . assert(_ == (PDelivery(PRecipient(t"Acme Corporation", t"1 Acme Way")), true))

      test(m"consecutive atoms fill consecutive members, equally on both paths"):
        val doc = t"item a xyz\n"
        (doc.read[PHolder in Tel], parity[PHolder](doc))
      . assert(_ == (PHolder(PTriple(t"a", t"xyz")), true))

      test(m"a repeatable field consumes the rest, equally on both paths"):
        val doc = t"log lbl 1 2 3\n"
        (doc.read[PLogBook in Tel], parity[PLogBook](doc))
      . assert(_ == (PLogBook(PLog(t"lbl", List(1, 2, 3))), true))

      test(m"repeatable occurrences split atoms/children, equally on both paths"):
        val doc = t"log lbl 1\n  values 2\n"
        (doc.read[PLogBook in Tel], parity[PLogBook](doc))
      . assert(_ == (PLogBook(PLog(t"lbl", List(1, 2))), true))

      test(m"Boolean fields read explicit atoms, equally on both paths"):
        val doc = t"active true\nverbose false\n"
        (doc.read[PFlags in Tel], parity[PFlags](doc))
      . assert(_ == (PFlags(true, false), true))

      test(m"optional scalars fill per §20.8, equally on both paths"):
        val doc = t"pair hello\n  second world\n"
        (doc.read[PPairBox in Tel], parity[PPairBox](doc))
      . assert(_ == (PPairBox(PPair(t"hello", t"world")), true))

      test(m"a source atom supplies a staged scalar field"):
        val doc = t"body\n    line one\n    line two\n"
        (doc.read[PNote in Tel], parity[PNote](doc))
      . assert(_ == (PNote(t"line one\nline two"), true))

      test(m"an atom plus a same-keyword child raises E308 on the staged path"):
        capture[TelError](t"log lbl\n  label dup\n".read[PLogBook in Tel]).reason
      . assert(_ == TelError.Reason.NonRepeatableTooMany)

      test(m"excess atoms raise E302 on the staged path"):
        capture[TelError](t"item a b c\n".read[PHolder in Tel]).reason
      . assert(_ == TelError.Reason.TooManyAtoms)

      test(m"an unparseable positional atom raises NotScalar"):
        capture[TelError](t"log lbl notanumber\n".read[PLogBook in Tel]).reason match
          case TelError.Reason.NotScalar(_, expected) => expected
          case _                                      => t"?"
      . assert(_ == t"Int")

    suite(m"Canonical construction (§22.2)"):
      given canonicalRecipient: (PRecipient is Tel.Parsable) = Tel.Parsable.derived
      given canonicalDelivery: (PDelivery is Tel.Parsable) = Tel.Parsable.derived
      given canonicalLog: (PLog is Tel.Parsable) = Tel.Parsable.derived
      given canonicalLogBook: (PLogBook is Tel.Parsable) = Tel.Parsable.derived
      given canonicalPair: (PPair is Tel.Parsable) = Tel.Parsable.derived
      given canonicalPairBox: (PPairBox is Tel.Parsable) = Tel.Parsable.derived
      given canonicalGuard: (PGuard is Tel.Parsable) = Tel.Parsable.derived
      given canonicalGuardBox: (PGuardBox is Tel.Parsable) = Tel.Parsable.derived

      // The canonical text must reparse to the same value on both paths.
      inline def reparses[value](value0: value)
        (using value is Tel.Encodable, value is Tel.Decodable, value is Tel.Parsable)
      :   Boolean =
        val printed = Tel.canonical(value0).show.s.tt
        printed.read[Tel].as[value] == value0 && printed.read[value in Tel] == value0

      test(m"a nested record's leading scalars go inline"):
        Tel.canonical(PDelivery(PRecipient(t"Acme", t"HQ")))
          .childCompounds.readable.head.atoms.length
      . assert(_ == 2)

      test(m"the canonical form reparses to the value on both paths"):
        reparses(PDelivery(PRecipient(t"Acme Corporation", t"1 Acme Way")))
      . assert(identity)

      test(m"a repeatable's occurrences all go inline and reparse"):
        val doc = Tel.canonical(PLogBook(PLog(t"lbl", List(1, 2, 3))))
        (doc.childCompounds.readable.head.atoms.length,
         reparses(PLogBook(PLog(t"lbl", List(1, 2, 3)))))
      . assert(_ == (4, true))

      test(m"a single-occurrence repeatable takes the child form"):
        val doc = Tel.canonical(PLogBook(PLog(t"lbl", List(7))))
        (doc.childCompounds.readable.head.atoms.length,
         reparses(PLogBook(PLog(t"lbl", List(7)))))
      . assert(_ == (1, true))

      test(m"an absent optional scalar terminates the inline run"):
        val doc = Tel.canonical(PPairBox(PPair(Unset, t"x")))
        (doc.childCompounds.readable.head.atoms.length,
         reparses(PPairBox(PPair(Unset, t"x"))))
      . assert(_ == (0, true))

      // A Boolean is a scalar member, so it joins the inline run as its
      // explicit `true`/`false` atom.
      test(m"a false Boolean joins the inline run as its atom"):
        val doc = Tel.canonical(PGuardBox(PGuard(false, t"hello")))
        (doc.childCompounds.readable.head.atoms.length,
         reparses(PGuardBox(PGuard(false, t"hello"))))
      . assert(_ == (2, true))

      test(m"a true Boolean joins the inline run as its atom"):
        val doc = Tel.canonical(PGuardBox(PGuard(true, t"hello")))
        (doc.childCompounds.readable.head.atoms.length,
         reparses(PGuardBox(PGuard(true, t"hello"))))
      . assert(_ == (2, true))

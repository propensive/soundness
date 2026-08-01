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

// Positional-assignment fixtures (§19.2, issue #1694). The positional cases
// nest the record one level down, because the document root never carries
// atoms (§20.2).
case class PRecipient(name: Text, address: Optional[Text]) derives CanEqual
case class PDelivery(recipient: Optional[PRecipient]) derives CanEqual
case class PFlagItem(a: Boolean, b: Optional[Boolean], c: Text) derives CanEqual
case class PHolder(item: PFlagItem) derives CanEqual
case class PLog(label: Text, values: List[Int]) derives CanEqual
case class PLogBook(log: PLog) derives CanEqual
case class PFlags(active: Boolean, verbose: Optional[Boolean]) derives CanEqual
case class PPair(first: Optional[Text], second: Optional[Text]) derives CanEqual
case class PPairBox(pair: PPair) derives CanEqual

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

      test(m"an atom skips an optional flag and fills the scalar (worked example)"):
        t"item a xyz\n".read[Tel].as[PHolder]
      . assert(_ == PHolder(PFlagItem(true, Unset, t"xyz")))

      test(m"a repeatable field consumes every remaining atom"):
        t"log lbl 1 2 3\n".read[Tel].as[PLogBook]
      . assert(_ == PLogBook(PLog(t"lbl", List(1, 2, 3))))

      test(m"repeatable occurrences split between atoms and children"):
        t"log lbl 1\n  values 2\n".read[Tel].as[PLogBook]
      . assert(_ == PLogBook(PLog(t"lbl", List(1, 2))))

      test(m"a bare flag keyword decodes true and an absent flag false"):
        t"active\n".read[Tel].as[PFlags]
      . assert(_ == PFlags(true, Unset))

      test(m"a bare optional flag decodes as present true"):
        t"active\nverbose\n".read[Tel].as[PFlags]
      . assert(_ == PFlags(true, true))

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
        capture[TelError](t"item a xyz extra\n".read[Tel].as[PHolder]).reason
      . assert(_ == TelError.Reason.TooManyAtoms)

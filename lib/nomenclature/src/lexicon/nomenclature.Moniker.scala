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
package nomenclature

import scala.caps

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import fulminate.*

object Moniker:
  opaque type Moniker = Int

  private def wrap[transport](ordinal: Int): Moniker over transport =
    ordinal.asInstanceOf[Moniker over transport]

  def apply[transport](ordinal: Int)(using Vocabulary over transport): Moniker over transport =
    wrap(ordinal)

  extension (moniker: Moniker) def ordinal: Int = moniker

  // An honest capability: the instance retains the resolution-scoped tactic
  // (every given that includes a tactic is a capability; Jon, 2026-07-13).
  given encodable: [transport] => (vocabulary: Vocabulary over transport, tactic: Tactic[Moniker.Error])
  =>  (((Moniker over transport) is Encodable in Text)^{tactic, caps.any}) =
    new Encodable:
      type Self = Moniker over transport
      type Form = Text

      def encoded(moniker: Self): Text = vocabulary.name(moniker.ordinal)

  given decodable: [transport] => (vocabulary: Vocabulary over transport, tactic: Tactic[Moniker.Error])
  =>  (((Moniker over transport) is Decodable in Text)^{tactic}) =
    text => wrap(vocabulary.number(text))

  // MonikerError → Moniker.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Unreadable               extends Reason(1)
      case OutOfRange(value: Int)   extends Reason(2)
      case Malformed(moniker: Text) extends Reason(3)
      case UnknownWord(word: Text)  extends Reason(4)

    given communicable: Reason is Communicable =
      case Reason.Unreadable        => m"the vocabulary could not be read"
      case Reason.OutOfRange(n)     => m"the number $n is outside the representable range"
      case Reason.Malformed(name)   => m"$name is not of the form <adjective>-<animal>"
      case Reason.UnknownWord(word) => m"the word $word does not appear in the vocabulary"

  case class Error(reason: Moniker.Error.Reason)(using Diagnostics)
  extends fulminate.Error(80, reason.number)(m"the moniker is not valid because $reason")


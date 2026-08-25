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
package exoskeleton

import scala.caps

import java.util.concurrent.atomic as juca

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import parasite.*
import profanity.*
import quantitative.*
import rudiments.*
import symbolism.*
import vacuous.*

object Cli:
  @scala.caps.unsafe.untrackedCaptures
  private var messages: List[Text] = Nil
  @scala.caps.unsafe.untrackedCaptures
  private var trigger: Promise[Unit] = Promise()

  def prepare(): Unit =
    messages = Nil
    trigger = Promise()

  def done(): Unit = trigger.offer(())
  def log(input: Text): Unit = messages ::= input
  def await()(using Monitor^): List[Text] =
    safely(trigger.await(10.0*Second)) yet messages.reverse


  def arguments
    ( textArguments: Iterable[Text],
      focus:         Optional[Int]     = Unset,
      position:      Optional[Int]     = Unset,
      tab:           Optional[Ordinal] = Unset )
  :   List[Argument] =


      textArguments.toList.padTo(focus.let(_ + 1).or(0), t"").zipWithIndex.map: (text, index) =>
        Argument(index, text, if focus == index then position else Unset, tab, Argument.Format.Full)
      . to(List)


// A `Cli` is a *capability*: it carries the live stdio, signal-dispatch and completion state of
// one command-line invocation, whose lifetime is the `process` scope that introduces it.
// `Exclusive` because an invocation has a single owner; nothing may retain it past the exit.
trait Cli extends Console, caps.ExclusiveCapability:
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory
  def proceed: Boolean
  def login: Login
  def register(flag: Flag, discoverable: Discoverable, operand: Optional[Text]): Unit = ()
  def record(statuses: List[Status]): Unit = ()
  def present(flag: Flag): Unit = ()
  def explain(update: (Optional[Text] aka "prior") ?=> Optional[Text]): Unit = ()

  // Records a successful `Subcommand` match during dispatch; `matches` is the contiguous
  // sequence of subcommand names matched so far, from the first argument onwards.
  def matched(argument: Argument): Unit = ()
  def matches: List[Text] = Nil

  private val signalHandlers:
  juca.AtomicReference[List[PartialFunction[UnixSignal | WindowsSignal, SignalResponse]]] =
    juca.AtomicReference(Nil)

  override def trap
    ( handler: PartialFunction[UnixSignal | WindowsSignal, SignalResponse] )
  :   Unit =

    signalHandlers.updateAndGet(list => (handler :: list.nn.stdlib).to(List))


  def dispatchSignal(signal: UnixSignal | WindowsSignal): SignalResponse =
    def loop(handlers: List[PartialFunction[UnixSignal | WindowsSignal, SignalResponse]])
    :   SignalResponse =

      handlers match
        case Nil =>
          SignalResponse.Reject

        case pf :: rest =>
          if pf.isDefinedAt(signal) then pf(signal) match
            case SignalResponse.Defer => loop(rest)
            case decided              => decided
          else loop(rest)

    loop(signalHandlers.get.nn)

  def parameter[operand: Interpretable](flag: Flag)(using (? <: operand) is Discoverable)
  :   Optional[operand]


  def suggest
    ( argument: Argument,
      update:   (List[Suggestion] aka "prior") ?=> List[Suggestion],
      prefix:   Text,
      suffix:   Text )
  :   Unit =

    return

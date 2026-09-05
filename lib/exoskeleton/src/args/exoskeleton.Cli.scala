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

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import gossamer.*
import parasite.*
import profanity.*
import quantitative.*
import rudiments.*
import symbolism.*
import turbulence.Stdio
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
    ( textArguments: List[Text],
      focus:         Optional[Int]     = Unset,
      position:      Optional[Int]     = Unset,
      tab:           Optional[Ordinal] = Unset )
  :   List[Argument] =

    val target = focus.let(_ + 1).or(0)

    val padded =
      if textArguments.size >= target then textArguments
      else textArguments + List.fill(target - textArguments.size)(t"")

    padded.indexed.map: (text, ordinal) =>
      val index = ordinal.n0
      Argument(index, text, if focus == index then position else Unset, tab, Argument.Format.Full)


// A `Cli` is a *capability*: it carries the live stdio, signal-dispatch and completion state of
// one command-line invocation, whose lifetime is the `process` scope that introduces it.
// `Exclusive` because an invocation has a single owner; nothing may retain it past the exit.
trait Cli
extends Console, caps.ExclusiveCapability, WorkingDirectory.Provider, Environment.Provider,
    Stdio.Provider:
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory
  def proceed: Boolean
  def login: Login
  def register(flag: Flag, discoverable: Discoverable, operand: Optional[Text]): Unit = ()
  def record(statuses: List[Status]): Unit = ()
  def present(flag: Flag): Unit = ()

  // Records that `flag` is required, and whether it was actually specified. Requirements are
  // accrued, never failed fast: an `Invocation` collects the missing flags for `execute` to
  // report together, and a `Completion` notes the requiredness for help, while continuing to
  // offer suggestions — completing a command whose required flags are not yet all present is
  // the common case.
  def demand(flag: Flag, present: Boolean): Unit = ()

  // Records that `flag` was specified but its operand could not be decoded, with the reason.
  // Like `demand`, faults are accrued for `execute` to report together, and never preclude
  // completions.
  def fault(flag: Flag, message: Text): Unit = ()

  // The flag's raw operand arguments, or `Unset` if the flag was not specified at all.
  def locate(flag: Flag): Optional[List[Argument]] = Unset
  def explain(update: (Optional[Text] aka "prior") ?=> Optional[Text]): Unit = ()

  // Records a successful `Subcommand` match during dispatch; `matches` is the contiguous
  // sequence of subcommand names matched so far, from the first argument onwards.
  def matched(argument: Argument): Unit = ()
  def matches: List[Text] = Nil

  // `Atomic.Ref`, not `Atomic[…]`: the match type does not reduce over an opaque type such as
  // `proscenium.List`, because its final arm needs the scrutinee proved distinct from `Int`,
  // `Long` and `Boolean`, and an opaque type's representation is not visible here to prove it.
  private val signalHandlers:
  Atomic.Ref[List[PartialFunction[UnixSignal | WindowsSignal, SignalResponse]]] =
    Atomic.Ref(Nil)

  override def trap
    ( handler: PartialFunction[UnixSignal | WindowsSignal, SignalResponse] )
  :   Unit =

    signalHandlers.since(handler :: _)


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

    loop(signalHandlers())

  def parameter[operand: Interpretable](flag: Flag)(using (? <: operand) is Discoverable)
  :   Optional[operand]


  def suggest
    ( argument: Argument,
      update:   (List[Suggestion] aka "prior") ?=> List[Suggestion],
      prefix:   Text,
      suffix:   Text )
  :   Unit =

    return

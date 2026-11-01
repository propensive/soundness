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
package guillotine

import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import zephyrine.*

object Stderr:
  given computable: Stderr is Computable = Computable((process, _) => Stderr(process.errorText()))

case class Stderr(text: Text)

object Computable:
  // The instances are made by this factory rather than written as lambdas: a method with a
  // `using` clause is not a single abstract method. The tactic is a plain second argument: a
  // context-function result would freshen its capture set at each adaptation.
  def apply[self](lambda: (Subprocess^, Tactic[Truncation.Error]) => self)
  :   (self is Computable)^{lambda} =

    new Computable:
      type Self = self

      def compute(process: Subprocess^)(using tactic: Tactic[Truncation.Error]): self =
        lambda(process, tactic)

  given lineStream: Buffering => (Chain[Text] is Computable) =
    // Line-oriented output as a lazy chain of lines, pulled from the process as the chain is
    // forced and memoized thereafter. An exclusive iterator cannot be the `Self` of an erased
    // given (a trait holding it would have to be a `Capability`), so the chain -- pure, lazy --
    // is the honest line-at-a-time type here. `lines()` needs a `Tactic[Truncation.Error]` and
    // `.records` a `Buffering`.
    Computable: (job, tactic) =>
      given Tactic[Truncation.Error] = tactic
      val records: Iterator[Text]^ = job.lines().records
      records.chain

  given list: List[Text] is Computable = Computable: (job, tactic) =>
    given Tactic[Truncation.Error] = tactic
    job.lines().records.to[List]

  given text: Text is Computable = Computable((process, _) => process.text())

  given string: String is Computable = Computable((process, _) => process.text().s)

  given data: Buffering => (Data is Computable) =
    // The whole of standard output as one block of bytes; the streaming byte
    // endpoint is `Subprocess.stdout()` for callers who want it lazy.
    Computable: (job, tactic) =>
      given Tactic[Truncation.Error] = tactic
      job.stdout().memoize

  given exitStatus: Exit is Computable = Computable: (process, _) =>
    process.status() match
      case 0     => Exit.Ok
      case other => Exit.Fail(other)

  given unit: Unit is Computable = exitStatus.map(_ => ())

  given instantiable: [instantiable]
  =>  ( evidence: (instantiable is Instantiable across Paths from Text)^ )
  =>  ( (instantiable is Computable)^{evidence} ) =

    text.map: text => evidence(text.trim)

// A read of the process's output which is cut short — the stream ending before its framing
// says it should — is raised through the tactic; `exec` reports it as the command's failure.
trait Computable extends Typeclass:
  def compute(process: Subprocess^)(using Tactic[Truncation.Error]): Self

  def map[self2](lambda: Self => self2): (self2 is Computable)^{this, lambda} =
    Computable((process, tactic) => lambda(compute(process)(using tactic)))

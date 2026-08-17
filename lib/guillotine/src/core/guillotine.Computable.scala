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
  given computable: Stderr is Computable = process => Stderr(process.errorText())

case class Stderr(text: Text)

object Computable:
  given lineStream: Buffering => (Iterator[Text] is Computable) =
    // Line-oriented output as a single-consumer line iterator (the K-B
    // adapter), replacing the memoizing `Chain` bridge. `lines()` needs a
    // `Tactic[Truncation.Error]` and `.records` a `Buffering`; the endpoint the
    // iterator pulls from is laundered to fit the pure `Self` slot, so the
    // caller must drain it once. A read failure throws, as the `BufferedReader`
    // this replaced did.
    job => unsafely(job.lines().records.asInstanceOf[Iterator[Text]])

  given list: List[Text] is Computable = job => unsafely(List.of(job.lines().records.toList))

  given text: Text is Computable = _.text()

  given string: String is Computable = _.text().s

  given data: Buffering => (Data is Computable) =
    // The whole of standard output as one block of bytes; the streaming byte
    // endpoint is `Subprocess.stdout()` for callers who want it lazy.
    job => unsafely(job.stdout().memoize)

  given exitStatus: Exit is Computable = _.status() match
    case 0     => Exit.Ok
    case other => Exit.Fail(other)

  given unit: Unit is Computable = exitStatus.map(_ => ())

  given instantiable: [instantiable]
  =>  ( evidence: (instantiable is Instantiable across Paths from Text)^ )
  =>  ((instantiable is Computable)^{evidence}) =

    text.map: text => evidence(text.trim)

trait Computable extends Typeclass:
  def compute(process: Subprocess^): Self

  def map[self2](lambda: Self => self2): (self2 is Computable)^{this, lambda} =
    process => lambda(compute(process))

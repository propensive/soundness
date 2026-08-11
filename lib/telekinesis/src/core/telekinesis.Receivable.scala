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
package telekinesis

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import turbulence.*
import vacuous.*
import zephyrine.*

trait Receivable2:
  given instantiable: [content: Instantiable across HttpRequests from Text]
  =>  (tactic: Tactic[Http.Error])
  =>  ((content is Receivable)^{tactic}) =

    Receivable:
      body => content(body.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^].memoize.utf8)

object Receivable extends Receivable2:
  // A named SAM rather than a function type: a function type may not take a
  // capability-typed (`^`) parameter (the `Spring` precedent), and SAM
  // conversion keeps `Receivable(stream => ...)` call sites unchanged. The
  // parameter is NOT `consume` (a SAM-lambda cannot provide one, on the
  // `-scalajs` row): consuming readers cross it through a neutral carrier,
  // per the `accept` convention.
  trait Reader[result]:
    def read(stream: (Stream[Data] over Credit)^): result

  // The reader receives the response body as a single-owner pull endpoint;
  // whole-value consumers go through their `Aggregable`'s `accept`.
  def apply[result](lambda: Reader[result]^)(using tactic: Tactic[Http.Error])
  :   ((result is Receivable)^{lambda, tactic}) =
    response =>
      if response.status.category != Http.Status.Category.Successful
      then abort(Http.Error(response.status, response.textHeaders))
      else lambda.read(response.body.stream)

  given text: (tactic: Tactic[Http.Error])
  =>  ((Text is Receivable)^{tactic}) =
    Receivable(_.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^].memoize.utf8)

  given streamable: [stream] => (aggregable: (stream is Aggregable by Data)^)
  =>  (tactic: Tactic[Http.Error])
  =>  ((stream is Receivable)^{aggregable, tactic}) =
    Receivable(aggregable.accept(_))

  given httpStatus: Http.Status is Receivable = _.status

trait Receivable extends Typeclass:
  // Widened (`Response^`): a reader may consume a response whose streamed body
  // retains the live connection it arrived on.
  def read(response: Http.Response^): Self
  def map[self2](lambda: Self => self2): (self2 is Receivable)^{this, lambda} = response => lambda(read(response))

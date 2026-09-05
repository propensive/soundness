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

import scala.caps

import scala.language.dynamics

import anticipation.*
import contingency.*
import fulminate.Hazard
import distillate.*
import honeycomb.*
import legerdemain.*
import gossamer.*
import prepositional.*
import vacuous.*

import htmlDoms.whatwg, whatwg.*

// An `Orchestrate` is a capability: it retains the caller's `process`/`render` function, which
// may itself capture capabilities — a value constructed from capabilities is a capability
// (Jon, 2026-07-06; see rep/DECISIONS.md).
class Orchestrate[value: Encodable in Query, result](initial: Optional[value] = Unset)
  ( process: (form: Text => Html of Flow) => Optional[value] ->{form, caps.any} result )
extends caps.ExclusiveCapability:
  def otherwise(validate: (query: Query) ?=> Validation)(using Formulation, value is Formulaic)
    ( using decodable: Tactic[Hazard] ?=> value is Decodable in Query )
    ( using request: Http.Request, tactic: Tactic[Query.Error] )
  :   result =

    request.method match
      case Http.Post =>
        val result: Optional[value] = safely(decodable.decoded(request.query))
        val validation = if result.absent then validate(using request.query) else Validation()

        process(elicit[value](request.query, validation, _))(result)

      case _ =>
        process(elicit[value](initial.let(_.encode).or(Query()), Validation(), _))(Unset)


def orchestrate[value: Encodable in Query](initial: Optional[value] = Unset)[result]
  ( render: (form: Text => Html of Flow) ?=> (value: Optional[value]) => result )
:   Orchestrate[value, result]^{render, caps.any} =

  new Orchestrate[value, result](initial)(render(using _))

// `Submission.form`, which renders the submission as an HTML form. It is an extension here
// rather than a member of `Submission` so that `telekinesis.core` — the HTTP client and server
// vocabulary — does not depend on honeycomb and legerdemain's widgets.
extension [value](submission: Submission[value])
  def form
    ( submit:     Optional[Text]       = Unset,
      value:      Optional[value]      = Unset,
      validation: Optional[Validation] = Unset )
    ( using value is Formulaic, value is Encodable in Query, Formulation )
  :   Html of Flow =

    // FIXME: Check why `data` isn't used
    val data: Optional[Query] = submission.query.or(value.let(_.encode))
    elicit[value](submission.query, validation.or(Validation()), submit)

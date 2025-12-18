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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package legerdemain

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import honeycomb.*
import prepositional.*
import vacuous.*

import doms.whatwg
import whatwg.*
import attributives.attributiveText

private given realm: Realm = realm"legerdemain"


def elicit[value: Formulaic]
   (query: Optional[Query] = Unset, validation: Validation, submit: Optional[Text])
   (using formulation: Formulation)
: Html of Flow =

    formulation.form
     (value.fields(Pointer.Self, t"", query.or(Query.empty), validation, formulation), submit)


extension [formulaic: {Formulaic, Encodable in Query}](value: formulaic)
  def edit(validation: Validation, submit: Optional[Text])(using formulation: Formulation)
  : Html of Flow =

      formulation.form
       (formulaic.fields(Pointer.Self, t"", formulaic.encoded(value), validation, formulation),
        submit)


package formulations:
  given default: Formulation:
    def form(content: Seq[Html of Flow], submit: Optional[Text]): Html of Flow =
      Form(action = t".", method = t"post")(Fragment(content*), Input.Submit(value = submit.or(t"Submit")))

    def element
         (widget:     Html of Phrasing,
          legend:     Text,
          validation: Optional[Message],
          required:   Boolean)
    : Html of Flow =
        given alertClass: (CssClass of "alert") = CssClass()
        given requiredClass: (CssClass of "required") = CssClass()
        Div(P.alert(validation.let(_.html)), Label(legend, widget), Span.required(t"*"))

        // Div
        //  (validation.let(_.html).let(P.alert(_)),
        //   Label(legend, widget),
        //   Span.required(t"*").unless(!required))

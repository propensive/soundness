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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import wisteria.*

import doms.html.whatwg, whatwg.*

object Formulaic extends ProductDerivable[Formulaic]:
  given elicitable: [value]
  =>  ( elicitable: value is Elicitable )
  =>  ( renderable: elicitable.Operand is Renderable in Phrasing )
  =>  value is Formulaic:

    def fields
      ( pointer:     Pointer,
        legend:      Text,
        query:       Query,
        validation:  Validation,
        formulation: Formulation )
    :   Seq[Html of Flow] =

      val message: Optional[Message] = validation(pointer)
      val widget = renderable.render(elicitable.widget(pointer.text, legend, query().or(t"")))
      val required = false//safely(decodable.decoded(t"")).absent

      List(formulation.element(widget, legend, message, required))


  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Formulaic =
    (pointer, legend, query, errors, formulation) =>
      val content: Seq[Html of Flow] =
        contexts:
          [field] => context =>
            val label2 = if pointer == Pointer.Self then Pointer(label) else pointer(label)
            val legend = label.uncamel.map(_.lower.capitalize).spaced
            context.fields(label2, legend, query(label), errors, formulation)

        . flatten

      List(Fieldset(Legend(legend): Html of Flow, Fragment(content*): Html of Flow))


trait Formulaic extends Typeclass:
  def fields
    ( pointer:     Pointer,
      legend:      Text,
      query:       Query,
      validation:  Validation,
      formulation: Formulation )
  :   Seq[Html of Flow]

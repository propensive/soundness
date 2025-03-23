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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import distillate.*
import fulminate.*
import gossamer.*
import honeycomb.*
import nomenclature.*
import prepositional.*
import spectacular.*
import vacuous.*
import wisteria.*

import html5.*

given Realm = realm"legerdemain"

def elicit[ValueType: Formulable](legend: Text, query: Optional[Query] = Unset)
   (using formulation: Formulation)
:     Html[Flow] =
  formulation.form(ValueType.elements(t"", legend, query.or(Query())))

extension [ValueType: {Formulable, Encodable in Query}](value: ValueType)
   (using formulation: Formulation)
  def edit(legend: Text): Html[Flow] =
    formulation.form(ValueType.elements(t"", legend, Query(value)))

package formulations:
  given default: Formulation:
    def form(content: List[Html[Flow]]): Html[Flow] =
      Form(action = t".", method = Method.Post)(content*)

trait Widget:
  def name: Text
  def value: Text
  def entered: Boolean = value != t""

object Field:
  given Field is Renderable into Html[Flow] = field =>
    List(Label(field.label, Input.Text(name = field.name, value = field.value)))

case class Field(label: Text, name: Text, value: Text) extends Widget

object Autocomplete:
  given renderable: Autocomplete is Renderable into Html[Flow] = autocomplete => List:
    Label
     (Input(list = DomId(autocomplete.name)),
      Datalist(id = DomId(autocomplete.name)):
        autocomplete.options.map: option =>
          html5.Option(value = option))

case class Autocomplete(name: Text, options: List[Text], value: Text) extends Widget

object RadioGroup:
  given renderable: RadioGroup is Renderable into Html[Flow] = group =>
    List:
      Fieldset
       (Legend(group.label),
        group.options.map: option =>
          Label(Input.Radio(name = group.name, value = option.value), option.label))

case class RadioGroup
   (name:    Text,
    label:   Text,
    options: List[(key: Text, value: Text, label: Text)],
    value:   Text)
extends Widget

object Checkbox:
  given renderable: Checkbox is Renderable into Html[Flow] = checkbox =>
    List(Input.Checkbox(name = checkbox.name, checked = (checkbox.value != t"")))

case class Checkbox(name: Text, value: Text) extends Widget

object Selection:
  given renderable: Selection is Renderable into Html[Flow] = selection =>
    List(Label(Select(name = selection.name):
      selection.options.map: option =>
        html5.Option(value = option.key, label = option.value)))

case class Selection(name: Text, options: List[(key: Text, value: Text)], value: Text)
extends Widget

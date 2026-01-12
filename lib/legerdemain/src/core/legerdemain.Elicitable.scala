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
┃    Soundness, version 0.50.0.                                                                    ┃
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

import anamnesis.*
import anticipation.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

trait Elicitable2:
  given decodable: [value: Encodable in Text] => value is Elicitable:
    type Operand = Field
    def input(value: value): Text = value.encode
    def widget(id: Text, label: Text, value: Text): Field = Field(label, id, value)

object Elicitable extends Elicitable2:
  given boolean: Boolean is Elicitable:
    type Operand = Checkbox
    def input(value: Boolean): Text = if value then t"on" else t""
    def widget(id: Text, label: Text, value: Text): Checkbox = Checkbox(id, value)

  given enumerable: [enumerable: Enumerable] => enumerable is Elicitable:
    type Operand = Combobox
    def input(value: enumerable): Text = enumerable.name(value)

    def widget(id: Text, label: Text, value: Text): Combobox =
      Combobox(id, enumerable.values.to(List).map(enumerable.name(_)), value)

  given reference: [entity <: Entity: {Listable, Showable, Referenceable}]
        => entity.Operand is Encodable in Text
        => Reference[entity] is Elicitable:
    type Operand = Dropdown

    def input(ref: Reference[entity]): Text = ref.encode

    def widget(id: Text, label: Text, value: Text): Dropdown =
      val items = every[entity].map { item => item.encode -> item().show }.to(List)
      Dropdown(id, items, value)

trait Elicitable extends Typeclass, Operable:
  def input(value: Self): Text
  def widget(id: Text, label: Text, value: Text): Operand

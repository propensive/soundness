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
package jacinta

import scala.annotation.*

import anticipation.*
import prepositional.*
import proscenium.*
import urticose.*
import vacuous.*

object Schematic:
  given byte: Byte is Schematic in JsonSchema = () => JsonSchema.Integer()
  given short: Short is Schematic in JsonSchema = () => JsonSchema.Integer()
  given int: Int is Schematic in JsonSchema = () => JsonSchema.Integer()
  given long: Long is Schematic in JsonSchema = () => JsonSchema.Integer()
  given float: Float is Schematic in JsonSchema = () => JsonSchema.Number()
  given double: Double is Schematic in JsonSchema = () => JsonSchema.Number()
  given text: Text is Schematic in JsonSchema = () => JsonSchema.String()
  given email: EmailAddress is Schematic in JsonSchema = () => JsonSchema.String()
  given boolean: Boolean is Schematic in JsonSchema = () => JsonSchema.Boolean()

  given optional: [value: Schematic in JsonSchema] => Optional[value] is Schematic in JsonSchema =
    () =>
      value.schema() match
        case entity: JsonSchema.Object => entity.copy(optional = true)
        case entity: JsonSchema.Integer  => entity.copy(optional = true)
        case entity: JsonSchema.Number   => entity.copy(optional = true)
        case entity: JsonSchema.String   => entity.copy(optional = true)
        case entity: JsonSchema.Array    => entity.copy(optional = true)
        case entity: JsonSchema.Boolean  => entity.copy(optional = true)
        case entity: JsonSchema.Null   => entity.copy(optional = true)

  given list: [value: Schematic in JsonSchema] => List[value] is Schematic in JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given set: [value: Schematic in JsonSchema] => Set[value] is Schematic in JsonSchema =
    () => JsonSchema.Array(items = value.schema())


  given map: [key: Encodable in Text, value: Schematic in JsonSchema]
  =>  Map[key, value] is Schematic in JsonSchema =

    () => JsonSchema.Object(additionalProperties = true)


trait Schematic extends Typeclass, Formal:
  def schema(): Form

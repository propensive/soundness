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
import urticose.*
import vacuous.*
import wisteria.*

// `Schematic` describes the schema of a value's encoding; `Transport` is the
// schema's own representation (e.g. `JsonSchema`), so an instance reads `X is
// Schematic over JsonSchema`. It carries no `Form` of its own: when fused with an
// encoder as `Encodable & Schematic in Json over JsonSchema`, the wire format
// (`Form = Json`) comes from `Encodable`.
object Schematic:
  given byte: Byte is Schematic over JsonSchema = () => JsonSchema.Integer()
  given short: Short is Schematic over JsonSchema = () => JsonSchema.Integer()
  given int: Int is Schematic over JsonSchema = () => JsonSchema.Integer()
  given long: Long is Schematic over JsonSchema = () => JsonSchema.Integer()
  given float: Float is Schematic over JsonSchema = () => JsonSchema.Number()
  given double: Double is Schematic over JsonSchema = () => JsonSchema.Number()
  given text: Text is Schematic over JsonSchema = () => JsonSchema.String()
  given email: EmailAddress is Schematic over JsonSchema = () => JsonSchema.String()
  given boolean: Boolean is Schematic over JsonSchema = () => JsonSchema.Boolean()

  given optional: [value: Schematic over JsonSchema]
  =>  Optional[value] is Schematic over JsonSchema =
    () =>
      value.schema() match
        case entity: JsonSchema.Object  => entity.copy(optional = true)
        case entity: JsonSchema.Integer => entity.copy(optional = true)
        case entity: JsonSchema.Number  => entity.copy(optional = true)
        case entity: JsonSchema.String  => entity.copy(optional = true)
        case entity: JsonSchema.Array   => entity.copy(optional = true)
        case entity: JsonSchema.Boolean => entity.copy(optional = true)
        case entity: JsonSchema.Null    => entity.copy(optional = true)

  given list: [value: Schematic over JsonSchema]
  =>  List[value] is Schematic over JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given set: [value: Schematic over JsonSchema]
  =>  Set[value] is Schematic over JsonSchema =
    () => JsonSchema.Array(items = value.schema())

  given map: [key: Encodable in Text, value: Schematic over JsonSchema]
  =>  Map[key, value] is Schematic over JsonSchema =
    () => JsonSchema.Object(additionalProperties = true)

  // Auto-derivation of a schema for any product or sum, mirroring the encoder /
  // decoder auto-givens. Gated on `Reflection` so it never competes with the
  // primitive givens above.
  inline given derived: [value: Reflection] => value is Schematic over JsonSchema =
    JsonSchema.derived

// `Schematic` describes the schema of a value's encoding. `Transport` is the
// schema's own representation (e.g. `JsonSchema`). The wire format the schema is
// *for* is not recorded here — in the fused `Encodable & Schematic in Json over
// JsonSchema` it comes from `Encodable`'s `Form`.
trait Schematic extends Typeclass, Transportive:
  def schema(): Transport

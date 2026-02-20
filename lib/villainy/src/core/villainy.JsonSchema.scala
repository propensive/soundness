/*
    Villainy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package villainy

import anticipation.*
import contingency.*
import fulminate.*
import inimitable.*
import jacinta.*
import kaleidoscope.*
import merino.*
import urticose.*
import polyvinyl.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

import scala.compiletime.*

import strategies.throwUnsafely

object JsonSchema:

  def intensional[name <: Label, value](accessor: Json => value)
  :   name is Intensional in Json to value =
    new Intensional:
      type Self = name
      type Form = Json
      type Result = value

      def access(value: Json): value = accessor(value)
      def transform(value: Json, params: List[Text]): value = access(value)

  case class Property
     (`type`:     Text,
      properties: Optional[Map[Text, Json]],
      items:      Optional[Map[Text, Json]],
      required:   Optional[Set[Text]],
      minimum:    Optional[Int],
      maximum:    Optional[Int],
      format:     Optional[Text],
      pattern:    Optional[Text]):

    def requiredFields: Set[Text] = required.or(Set())

    def arrayFields =
      items.let(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key)))

      . or:
          panic(m"Some items were missing")

    def objectFields =
      properties.let(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key)))

      . or:
          panic(m"Some properties were missing")

    def field(required: Boolean): Member = `type` match
      case "array"  => Member.Record(if required then "array" else "array?", arrayFields)
      case "object" => Member.Record(if required then "object" else "object?", objectFields)

      case "string" =>
        val suffix = if required then "" else "?"

        pattern.let(Member.Value("pattern"+suffix, _)).or:
          Member.Value(format.or("string".tt)+suffix)

      case "integer" =>
        val end = if minimum.absent && maximum.absent then (if required then "" else "?") else "!"

        Member.Value("integer"+end, minimum.let(_.toString).or(""), maximum.let(_.toString).or(""))

      case other =>
        Member.Value(if required then other else other+"?")

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Specification:
  type Form = Json
  type Plane = JsonRecord
  def access(name: Text, json: Json): Json = json(name)

  def make(data: Json, access: Text => Json => Any): JsonRecord = JsonRecord(data, access)

  def fields: Map[Text, Member] = unsafely(doc.fields)

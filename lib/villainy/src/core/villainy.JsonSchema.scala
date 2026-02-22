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
import distillate.*
import fulminate.*
import gossamer.*
import inimitable.*
import jacinta.*
import kaleidoscope.*
import merino.*
import polyvinyl.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import urticose.*
import vacuous.*

import scala.compiletime.*

import strategies.throwUnsafely

object JsonSchema:

  given boolean: ("boolean" is Intensional in JsonSchema from Json to Boolean) =
    JsonSchema.intensional(_.as[Boolean])

  given optionalBoolean
  :   ("boolean?" is Intensional in JsonSchema from Json to Optional[Boolean]) =

    (value, params) => value.as[Optional[Boolean]]


  given string: ("string" is Intensional in JsonSchema from Json to Text) =
    JsonSchema.intensional(_.as[Text])

  given optionalText: ("string?" is Intensional in JsonSchema from Json to Optional[Text]) =
    (value, params) => value.as[Optional[Text]]

  given integer: ("integer" is Intensional in JsonSchema from Json to Int) =
    JsonSchema.intensional(_.as[Int])

  given optionalInteger: ("integer?" is Intensional in JsonSchema from Json to Optional[Int]) =
    (value, params) => value.as[Optional[Int]]

  given boundedInteger
  :   ("integer!" is Intensional in JsonSchema from Json to (Int raises BoundsError)) =

    new Intensional:
      type Self = "integer!"
      type Origin = Json
      type Form = JsonSchema
      type Result = Int raises BoundsError

      def transform(json: Json, params: List[Text] = Nil): Int raises BoundsError =
        val int = json.as[Int]

        params.absolve match
          case As[Int](min) :: As[Int](max) :: Nil =>
            if int < min || int > max then abort(BoundsError(int, min, max)) else int

          case As[Int](min) :: _ :: Nil =>
            if int < min then abort(BoundsError(int, min, Double.MaxValue)) else int

          case _ :: As[Int](max) :: Nil =>
            if int > max then abort(BoundsError(int, Double.MinValue, max)) else int


  given number: ("number" is Intensional in JsonSchema from Json to Double) =
    JsonSchema.intensional(_.as[Double])

  given optionalNumber: ("number?" is Intensional in JsonSchema from Json to Optional[Double]) =
    (value, params) => value.as[Optional[Double]]


  given dateTime: [instant: Instantiable across Instants from Text]
  =>  ("date-time" is Intensional in JsonSchema from Json to instant) =

    JsonSchema.intensional(_.as[Text].instantiate)

  given optionalDateTime: [instant: Instantiable across Instants from Text]
  =>  ("date-time?" is Intensional in JsonSchema from Json to Optional[instant]) =

    JsonSchema.intensional(_.as[Optional[Text]].let(_.instantiate))


  given date: [date: Instantiable across Dates from Text]
  =>  ("date" is Intensional in JsonSchema from Json to date) =

    JsonSchema.intensional(_.as[Text].instantiate)


  given optionalDate: [date: Instantiable across Dates from Text]
  =>  ("date?" is Intensional in JsonSchema from Json to Optional[date]) =

    JsonSchema.intensional(_.as[Optional[Text]].let(_.instantiate))



  given time: [time: Instantiable across Times from Text]
  =>  ("time" is Intensional in JsonSchema from Json to time) =

    JsonSchema.intensional(_.as[Text].instantiate)

  given optionalTime: [time: Instantiable across Times from Text]
  =>  ("time?" is Intensional in JsonSchema from Json to Optional[time]) =

    JsonSchema.intensional(_.as[Optional[Text]].let(_.instantiate))


  given duration: [duration: Instantiable across Durations from Text]
  =>  ("duration" is Intensional in JsonSchema to duration) =

    JsonSchema.intensional(_.as[Text].instantiate)

  given optionalDuration: [duration: Instantiable across Durations from Text]
  =>  ("duration?" is Intensional in JsonSchema to Optional[duration]) =

    JsonSchema.intensional(_.as[Optional[Text]].let(_.instantiate))


  given uriReference: ("uri-reference" is Intensional in JsonSchema from Json to Text) =
    JsonSchema.intensional(_.as[Text])


  given optionalUriReference
  :   ("uri-reference?" is Intensional in JsonSchema from Json to Optional[Text]) =
    JsonSchema.intensional(_.as[Optional[Text]])


  given email
  :   ("email" is Intensional in JsonSchema from Json to (EmailAddress raises EmailAddressError)) =

    JsonSchema.intensional(_.as[EmailAddress])


  given optionalEmail
  :   ( "email?" is Intensional in JsonSchema from Json to
            (Optional[EmailAddress] raises EmailAddressError) ) =

    JsonSchema.intensional(_.as[Optional[EmailAddress]])


  given idnEmail
  :   ( "idn-email" is Intensional in JsonSchema from Json to
            (EmailAddress raises EmailAddressError) ) =

    JsonSchema.intensional(_.as[EmailAddress])

  given optionalIdnEmail
  :   ( "idn-email?" is Intensional in JsonSchema from Json to
            (Optional[EmailAddress] raises EmailAddressError) ) =

    JsonSchema.intensional(_.as[Optional[EmailAddress]])


  given hostname
  :   ("hostname" is Intensional in JsonSchema from Json to (Hostname raises HostnameError)) =

    JsonSchema.intensional(_.as[Hostname])

  given optionalHostname
  :   ( "hostname?" is Intensional in JsonSchema from Json to
            (Optional[Hostname] raises HostnameError) ) =

    JsonSchema.intensional(_.as[Hostname])


  given ipv4: ("ipv4" is Intensional in JsonSchema from Json to (Ipv4 raises IpAddressError)) =
    JsonSchema.intensional(_.as[Ipv4])


  given optionalIpv4
  :   ("ipv4?" is Intensional in JsonSchema from Json to (Optional[Ipv4] raises IpAddressError)) =

    JsonSchema.intensional(_.as[Optional[Ipv4]])


  given ipv6: ("ipv6" is Intensional in JsonSchema from Json to (Ipv6 raises IpAddressError)) =
    JsonSchema.intensional(_.as[Ipv6])

  given optionalIpv6
  :   ("ipv6?" is Intensional in JsonSchema from Json to (Optional[Ipv6] raises IpAddressError)) =

    JsonSchema.intensional(_.as[Optional[Ipv6]])


  given uri: [url: Instantiable across Urls from Text]
  =>   ( "uri" is Intensional in JsonSchema from Json to url ) =

    JsonSchema.intensional: value => url.instantiate(value.as[Text])

  given optionalUri: [url: Instantiable across Urls from Text]
  =>   ( "uri?" is Intensional in JsonSchema from Json to Optional[url] ) =

    JsonSchema.intensional(_.as[Optional[Text]].let(_.instantiate))


  given iri: [url: Instantiable across Urls from Text]
  =>  ("iri" is Intensional in JsonSchema from Json to url) =

    JsonSchema.intensional:
      value => url.instantiate(value.as[Text])


  given optionalIri: [url: Instantiable across Urls from Text]
  =>   ( "iri?" is Intensional in JsonSchema from Json to Optional[url] ) =

    JsonSchema.intensional(_.as[Optional[Text]].let(_.instantiate))



  given iriReference: ("iri-reference" is Intensional in JsonSchema from Json to Text) =
    JsonSchema.intensional(_.as[Text])

  given optionalIriReference
  :   ("iri-reference?" is Intensional in JsonSchema from Json to Optional[Text]) =
    JsonSchema.intensional(_.as[Optional[Text]])


  given uuid: ("uuid" is Intensional in JsonSchema from Json to (Uuid raises UuidError)) =
    JsonSchema.intensional(_.as[Uuid])

  given optionalUuid
  :   ("uuid?" is Intensional in JsonSchema from Json to (Optional[Uuid] raises UuidError)) =
    JsonSchema.intensional(_.as[Optional[Uuid]])

  given uriTemplate: ("uri-template" is Intensional in JsonSchema from Json to Text) =
    JsonSchema.intensional(_.as[Text])


  given optionalUriTemplate
  :   ("uri-template?" is Intensional in JsonSchema from Json to Optional[Text]) =

    JsonSchema.intensional(_.as[Optional[Text]])


  given jsonPointer: ("json-pointer" is Intensional in JsonSchema from Json to JsonPointer) =
    JsonSchema.intensional(_.as[JsonPointer])

  given optionalJsonPointer
  :   ("json-pointer?" is Intensional in JsonSchema from Json to Optional[JsonPointer]) =

    JsonSchema.intensional(_.as[Optional[JsonPointer]])


  given regex: ("regex" is Intensional in JsonSchema from Json to Regex) =
    JsonSchema.intensional: value => Regex(value.as[Text])

  given optionalRegex: ("regex?" is Intensional in JsonSchema from Json to Optional[Regex]) =
    JsonSchema.intensional(_.as[Optional[Text]].let(Regex(_)))

  given array: ("array" is Structural[List] in JsonSchema from Json) = _.as[List[Json]].map(_)

  given optionalArray
  :   ("array?" is Structural[[element] =>> Optional[List[element]]] in JsonSchema from Json) =

    (value, make) => value.as[List[Json]].map(make)

  given module: ("object" is Structural[[Type] =>> Type] in JsonSchema from Json) =
    (value, make) => make(value)


  given optionalModule
  :   ("object?" is Structural[[value] =>> Optional[value]] in JsonSchema from Json) =

    (value, make) => make(value)


  given pattern: ("pattern" is Intensional):
    type Origin = Json
    type Form = JsonSchema
    type Result = Text

    def transform(value: Json, params: List[Text]): Text = params.absolve match
      case List(pattern: Text) =>
        val regex = Regex(pattern)
        if regex.matches(value.as[Text]) then value.as[Text]
        else abort(JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(value.as[Text], regex)))

  given optionalPattern: ("pattern?" is Intensional):
    type Origin = Json
    type Form = JsonSchema
    type Result = Optional[Text]

    def transform(value: Json, params: List[Text] = Nil): Optional[Text] = params.absolve match
      case pattern :: Nil =>
        val regex = Regex(pattern)
        if regex.matches(value.as[Text]) then value.as[Text]
        else abort(JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(value.as[Text], regex)))

  def record(data0: Json, access0: Text => Json => Any): Record = new Record:
    type Origin = Json
    val data: Json = data0
    def access: Text => Json => Any = access0

  def intensional[name <: Label, value](accessor: Json => value)
  :   name is Intensional in JsonSchema from Json to value =
    new Intensional:
      type Self = name
      type Origin = Json
      type Form = JsonSchema
      type Result = value

      def access(value: Json): value = accessor(value)
      def transform(value: Json, params: List[Text]): value = access(value)

  case class Property
    ( `type`:     Text,
      properties: Optional[Map[Text, Json]],
      items:      Optional[Map[Text, Json]],
      required:   Optional[Set[Text]],
      minimum:    Optional[Int],
      maximum:    Optional[Int],
      format:     Optional[Text],
      pattern:    Optional[Text] ):

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
  type Origin = Json
  type Form = JsonSchema
  def access(name: Text, json: Json): Json = json(name)
  def make(data: Json, access: Text => Json => Any): Record = JsonSchema.record(data, access)
  def fields: Map[Text, Member] = unsafely(doc.fields)

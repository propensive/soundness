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
import urticose.*
import polyvinyl.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

import scala.compiletime.*

import strategies.throwUnsafely

object JsonRecord:
  given boolean: ("boolean" is Intensional in Json on JsonRecord to Boolean) =
    JsonSchema.intensional(_.as[Boolean])

  given string: ("string" is Intensional in Json on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given integer: ("integer" is Intensional in Json on JsonRecord to Int) =
    JsonSchema.intensional(_.as[Int])

  given number: ("number" is Intensional in Json on JsonRecord to Double) =
    JsonSchema.intensional(_.as[Double])

  given dateTime: ("date-time" is Intensional in Json on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given date: ("date" is Intensional in Json on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given time: ("time" is Intensional in Json on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given duration: ("duration" is Intensional in Json on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given uriReference: ("uri-reference" is Intensional in Json on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given email
  :   ("email" is Intensional in Json on JsonRecord to
          (EmailAddress raises EmailAddressError)) =

    JsonSchema.intensional(_.as[EmailAddress])

  given idnEmail
  :   ("idn-email" is Intensional in Json on JsonRecord to
          (EmailAddress raises EmailAddressError)) =

    JsonSchema.intensional(_.as[EmailAddress])

  given hostname
  :   ("hostname" is Intensional in Json on JsonRecord to (Hostname raises
          HostnameError)) =

    JsonSchema.intensional(_.as[Hostname])

  given ipv4
  :   ("ipv4" is Intensional in Json on JsonRecord to (Ipv4 raises IpAddressError)) =

    JsonSchema.intensional(_.as[Ipv4])

  given ipv6
  :   ("ipv6" is Intensional in Json on JsonRecord to (Ipv6 raises IpAddressError)) =

    JsonSchema.intensional(_.as[Ipv6])

  given uri[url: Instantiable across Urls from Text]: ("uri" is Intensional on JsonRecord to url) =
    JsonSchema.intensional: value => url.instantiate(value.as[Text])

  given iri: [url: Instantiable across Urls from Text]
  =>  ("iri" is Intensional in Json on JsonRecord to url) =

    JsonSchema.intensional:
      value => url.instantiate(value.as[Text])


  given iriReference: ("iri-reference" is Intensional on JsonRecord in Json to Text) =
    JsonSchema.intensional(_.as[Text])

  given uuid: ("uuid" is Intensional on JsonRecord in Json to (Uuid raises UuidError)) =
    JsonSchema.intensional(_.as[Uuid])

  given uriTemplate: ("uri-template" is Intensional on JsonRecord in Json to Text) =
    JsonSchema.intensional(_.as[Text])

  given jsonPointer: ("json-pointer" is Intensional on JsonRecord in Json to Text) =
    JsonSchema.intensional(_.as[Text])


  given relativeJsonPointer
  :   ("relative-json-pointer" is Intensional on JsonRecord in Json to Text) =

    JsonSchema.intensional(_.as[Text])

  given regex: ("regex" is Intensional on JsonRecord in Json to Regex) =
    JsonSchema.intensional: value => Regex(value.as[Text])

  given array: ("array" is Accessor[List] in Json on JsonRecord) = _.as[List[Json]].map(_)

  given obj: ("object" is Accessor[[Type] =>> Type] in Json on JsonRecord) =
    (value, make) => make(value)


  given optionalBoolean
  :   ("boolean?" is Intensional in Json on JsonRecord to Optional[Boolean]) =

    (value, params) => value.as[Optional[Boolean]]


  given optionalText: ("string?" is Intensional in Json on JsonRecord to Optional[Text]) =
    (value, params) => value.as[Optional[Text]]

  given pattern: ("pattern" is Intensional):
    type Form = Json
    type Plane = JsonRecord
    type Result = Text

    def transform(value: Json, params: List[Text]): Text = params.absolve match
      case List(pattern: Text) =>
        val regex = Regex(pattern)
        if regex.matches(value.as[Text]) then value.as[Text]
        else abort(JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(value.as[Text], regex)))

  given optionalPattern: ("pattern?" is Intensional):
    type Form = Json
    type Plane = JsonRecord
    type Result = Optional[Text]

    def transform(value: Json, params: List[Text] = Nil): Optional[Text] = params.absolve match
      case pattern :: Nil =>
        val regex = Regex(pattern)
        if regex.matches(value.as[Text]) then value.as[Text]
        else abort(JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(value.as[Text], regex)))

  given optionalInteger: ("integer?" is Intensional in Json on JsonRecord to Optional[Int]) =
    (value, params) => value.as[Optional[Int]]

  given boundedInteger: ("integer!" is Intensional in Json on JsonRecord to (Int raises BoundsError)) =
    new Intensional:
      type Self = "integer!"
      type Form = Json
      type Plane = JsonRecord
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

  given optionalNumber: ("number?" is Intensional in Json on JsonRecord to Optional[Double]) =
    (value, params) => value.as[Optional[Double]]

  given optionalArray
  :   ("array?" is Accessor[[element] =>> Optional[List[element]]] in Json on JsonRecord) =

    (value, make) => value.as[List[Json]].map(make)

  given optionalObject: ("object?" is Accessor[[value] =>> Optional[value]] in Json on JsonRecord) =
    (value, make) => make(value)

class JsonRecord(data0: Json, access0: Text => Json => Any) extends Record:
  type Form = Json
  val data: Json = data0
  def access: Text => Json => Any = access0

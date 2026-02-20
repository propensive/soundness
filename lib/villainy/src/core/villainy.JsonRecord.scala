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
  given boolean: ("boolean" is Intensional in Optional[Json] on JsonRecord to Boolean) =
    JsonSchema.intensional(_.as[Boolean])

  given string: ("string" is Intensional in Optional[Json] on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given integer: ("integer" is Intensional in Optional[Json] on JsonRecord to Int) =
    JsonSchema.intensional(_.as[Int])

  given number: ("number" is Intensional in Optional[Json] on JsonRecord to Double) =
    JsonSchema.intensional(_.as[Double])

  given dateTime: ("date-time" is Intensional in Optional[Json] on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given date: ("date" is Intensional in Optional[Json] on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given time: ("time" is Intensional in Optional[Json] on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given duration: ("duration" is Intensional in Optional[Json] on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given uriReference: ("uri-reference" is Intensional in Optional[Json] on JsonRecord to Text) =
    JsonSchema.intensional(_.as[Text])

  given email
  :   ("email" is Intensional in Optional[Json] on JsonRecord to
          (EmailAddress raises EmailAddressError)) =

    JsonSchema.intensional(_.as[EmailAddress])

  given idnEmail
  :   ("idn-email" is Intensional in Optional[Json] on JsonRecord to
          (EmailAddress raises EmailAddressError)) =

    JsonSchema.intensional(_.as[EmailAddress])

  given hostname
  :   ("hostname" is Intensional in Optional[Json] on JsonRecord to (Hostname raises
          HostnameError)) =

    JsonSchema.intensional(_.as[Hostname])

  given ipv4
  :   ("ipv4" is Intensional in Optional[Json] on JsonRecord to (Ipv4 raises IpAddressError)) =

    JsonSchema.intensional(_.as[Ipv4])

  given ipv6
  :   ("ipv6" is Intensional in Optional[Json] on JsonRecord to (Ipv6 raises IpAddressError)) =

    JsonSchema.intensional(_.as[Ipv6])

  given uri[url: Instantiable across Urls from Text]: ("uri" is Intensional on JsonRecord to url) =
    JsonSchema.intensional: value => url.instantiate(value.as[Text])

  given iri: [url: Instantiable across Urls from Text]
  =>  ("iri" is Intensional in Optional[Json] on JsonRecord to url) =

    JsonSchema.intensional:
      value => url.instantiate(value.as[Text])


  given iriReference: ("iri-reference" is Intensional on JsonRecord in Optional[Json] to Text) =
    JsonSchema.intensional(_.as[Text])

  given uuid: ("uuid" is Intensional on JsonRecord in Optional[Json] to (Uuid raises UuidError)) =
    JsonSchema.intensional(_.as[Uuid])

  given uriTemplate: ("uri-template" is Intensional on JsonRecord in Optional[Json] to Text) =
    JsonSchema.intensional(_.as[Text])

  given jsonPointer: ("json-pointer" is Intensional on JsonRecord in Optional[Json] to Text) =
    JsonSchema.intensional(_.as[Text])


  given relativeJsonPointer
  :   ("relative-json-pointer" is Intensional on JsonRecord in Optional[Json] to Text) =

    JsonSchema.intensional(_.as[Text])

  given regex: ("regex" is Intensional on JsonRecord in Optional[Json] to Regex) =
    JsonSchema.intensional: value => Regex(value.as[Text])

  given array: Accessor[JsonRecord, Optional[Json], "array", List] =
    _.vouch.as[List[Json]].map(_)

  given obj: Accessor[JsonRecord, Optional[Json], "object", [Type] =>> Type] =
    (value, make) => make(value.vouch)


  given maybeBoolean
  :   ("boolean?" is Intensional in Optional[Json] on JsonRecord to Optional[Boolean]) =

    (value, params) => value.let(_.as[Boolean])


  given maybeString: ("string?" is Intensional in Optional[Json] on JsonRecord to Optional[Text]) =
    (value, params) => value.let(_.as[Text])

  given pattern: ("pattern" is Intensional):
    type Form = Optional[Json]
    type Plane = JsonRecord
    type Result = Text

    def transform(value: Optional[Json], params: List[Text]): Text =
      value.let: value =>
        (params: @unchecked) match
          case List(pattern: Text) =>
            val regex = Regex(pattern)
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(value.as[Text], regex)))

      . lest(JsonSchemaError(JsonSchemaError.Reason.MissingValue))

  given maybePattern: ("pattern?" is Intensional):
    type Form = Optional[Json]
    type Plane = JsonRecord
    type Result = Optional[Text]
    def transform(value: Optional[Json], params: List[Text] = Nil): Optional[Text] =
      value.let: value =>
        (params: @unchecked) match
          case pattern :: Nil =>
            val regex = Regex(pattern)
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonSchemaError(JsonSchemaError.Reason.PatternMismatch(value.as[Text], regex)))

  given maybeInteger: ("integer?" is Intensional in Optional[Json] on JsonRecord to Optional[Int]) =
    (value, params) => value.let(_.as[Int])

  given boundedInteger: ("integer!" is Intensional in Optional[Json] on JsonRecord to (Int raises IntRangeError)) =
    new Intensional:
      type Self = "integer!"
      type Form = Optional[Json]
      type Plane = JsonRecord
      type Result = Int raises IntRangeError

      def transform(json: Optional[Json], params: List[Text] = Nil): Int raises IntRangeError =
        val int = json.vouch.as[Int]

        params.absolve match
          case As[Int](min) :: As[Int](max) :: Nil =>
            if int < min || int > max then abort(IntRangeError(int, min, max)) else int

          case As[Int](min) :: _ :: Nil =>
            if int < min then abort(IntRangeError(int, min, Unset)) else int

          case _ :: As[Int](max) :: Nil =>
            if int > max then abort(IntRangeError(int, Unset, max)) else int

  given maybeNumber: ("number?" is Intensional in Optional[Json] on JsonRecord to Optional[Double]) =
    (value, params) => value.let(_.as[Double])

  given maybeArray: Accessor[JsonRecord, Optional[Json], "array?", [T] =>> Optional[List[T]]] =
    (value, make) => value.let(_.as[List[Json]].map(make))

  given maybeObject: Accessor[JsonRecord, Optional[Json], "object?", [T] =>> Optional[T]] =
    (value, make) => value.let(make(_))

class JsonRecord(data0: Optional[Json], access0: Text => Optional[Json] => Any) extends Record:
  type Form = Optional[Json]
  val data: Optional[Json] = data0
  def access: Text => Optional[Json] => Any = access0

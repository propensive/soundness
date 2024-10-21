/*
    Telekinesis, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import nettlesome.*
import turbulence.*
import vacuous.*

object HttpResponse:
  given (using Tactic[HttpError]) => HttpResponse is Readable by Bytes as readable = response =>
    val body = response.status match
      case status: (HttpStatus & FailureCase) =>
        raise(HttpError(response.url, response.method, status), response.body)

      case status =>
        response.body

    body.stream

case class HttpResponse
    (url:     HttpUrl,
     method:  HttpMethod,
     status:  HttpStatus,
     headers: Map[ResponseHeader[?], List[Text]],
     body:    LazyList[Bytes]):

  def as[BodyType: HttpReadable as readable]: BodyType raises HttpError = (status: @unchecked) match
    case status: FailureCase => abort(HttpError(url, method, status))
    case status              => readable.read(status, body)

  def apply[ValueType](header: ResponseHeader[ValueType])
      (using decoder: HttpHeaderDecoder[ValueType])
          : List[ValueType] =

    headers.at(header).or(Nil).map(decoder.decode(_))
